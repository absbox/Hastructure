{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module AssetClass.Mortgage  
  (projectMortgageFlow,projectScheduleFlow,updateOriginDate,getOriginInfo
  ,buildARMrates)
  where

import qualified Data.Time as T ()
import qualified Cashflow as CF 
import qualified Assumptions as A
import Asset as Ast
import Types
import Lib
import Util
import DateUtil
import InterestRate as IR

import qualified Data.Map as Map
import Data.List
import Data.Ratio ()
import Data.Maybe
import GHC.Generics ()
import Data.Aeson ()
import Language.Haskell.TH ()
import Data.Aeson.TH ()
import Data.Aeson.Types ()

import AssetClass.AssetBase
import AssetClass.AssetCashflow
import Assumptions (AssetPerfAssumption(MortgageAssump))
import GHC.Float.RealFracMethods ()
import Cashflow (extendTxns)
import Control.Lens hiding (element)
import Control.Lens.TH ()
import qualified Data.DList as DL


projectMortgageFlow :: (Balance, Balance, Date, Maybe BorrowerNum, AmortPlan, DayCount, IRate, Period, Int) -> (Dates, [DefaultRate],[PrepaymentRate],[IRate],[Int]) -> (DL.DList CF.TsRow, Balance, Balance)
projectMortgageFlow (originBal, startBal, lastPayDate, mbn, pt, dc, startRate, p, oTerms) (cfDates, defRates, ppyRates, rateVector, remainTerms) = 
  let 
    initRow = CF.MortgageFlow lastPayDate startBal 0.0 0.0 0.0 0.0 0.0 0.0 startRate Nothing Nothing Nothing
  in 
    foldl 
      (\(acc, begBal, lastOriginBal) (pDate, defRate, ppyRate, intRate, rt)
          -> let 
               -- begBal = view CF.tsRowBalance (last acc) 
               -- lastPaidDate = getDate (last acc) -- `debug` ("beg bal"++ show begBal)
               newDefault = mulBR begBal defRate -- `debug` ("new default"++ show defRate++ ">>"++ show begBal)
               newPrepay = mulBR (begBal - newDefault) ppyRate
               -- performing balance
               _balAfterPpy = begBal - newDefault - newPrepay -- `debug` ("new ppy "++ show newPrepay ++ "beg bal"++ show (begBal - newDefault) ++ "ppy rate"++ show ppyRate)
               -- performing original balance 
               amortBal = mulBR lastOriginBal $ (1-defRate) * (1-ppyRate)  
               amortTerm =  case pt of
                              Balloon aTerm -> aTerm
                              _ -> oTerms
                
               (newInt,newPrin) = calcAssetPrinInt pt _balAfterPpy (periodRateFromAnnualRate p intRate) oTerms rt (amortBal, amortTerm) -- `debug` ("using bal for pmt"++ show _balAfterPpy)
               endBal = _balAfterPpy - newPrin
               newMbn = decreaseBorrowerNum begBal endBal mbn -- `debug` ("rt in mortgage proj"++ show rt)
             in 
               (DL.snoc acc (CF.MortgageFlow pDate endBal newPrin newInt newPrepay newDefault 0.0 0.0 intRate newMbn Nothing Nothing), endBal ,amortBal)
      )
      (DL.singleton initRow, startBal, originBal)
      (zip5 cfDates defRates ppyRates rateVector remainTerms)            
             

projectDelinqMortgageFlow :: ([CF.TsRow],[CF.TsRow]) -> Balance -> Maybe Int -> Date -> [Date] -> [Rate] -> [PrepaymentRate] -> [IRate] -> (Rate,Lag,Rate,Lag,Period,AmortPlan,Int) -> ([Balance],[Balance],[Balance]) -> [CF.TsRow]
projectDelinqMortgageFlow (trs, backToPerfs) beginBal mbn lastDate pDates delinqRates ppyRates rates assump vectors =
  go (DL.fromList trs) backToPerfs beginBal mbn lastDate pDates delinqRates ppyRates rates assump vectors
  where
    go acc [] _ _ _ [] _ _ _ _ _ = CF.dropTailEmptyTxns (DL.toList acc)
    go acc backToPerfs _ _ _ [] _ _ _ _ _ =
      let
        consolTxn = sort backToPerfs
        trsList = DL.toList acc
        (trsKeep,trsMerge) = splitByDate trsList (getDate (head backToPerfs)) EqToRight
        mergedTrs = CF.combineTss [] trsMerge consolTxn
      in
        trsKeep ++ mergedTrs

    go acc backToPerfs beginBal mbn lastDate (pDate:pDates) (delinqRate:delinqRates) (ppyRate:ppyRates) (rate:rates)
       (defaultPct,defaultLag,recoveryRate,recoveryLag,p,prinType,ot)
       (dBal:defaultVec,rAmt:recoveryVec,lAmt:lossVec)
      = go (DL.snoc acc tr) (CF.combineTss [] backToPerfs newPerfCfs) endingBal newMbn pDate pDates delinqRates ppyRates rates
           (defaultPct,defaultLag,recoveryRate,recoveryLag,p,prinType,ot)
           (newDefaultVec,newRecoveryVec,newLossVec)
      where
        remainTerms = succ $ max 0 (length pDates - recoveryLag - defaultLag)
        delinqBal = mulBR beginBal delinqRate

        defaultBal = mulBR delinqBal defaultPct
        recBal = mulBR defaultBal recoveryRate
        lossBal = mulBR defaultBal (1 - recoveryRate)

        newDefaultVec = replace defaultVec (pred defaultLag) defaultBal
        newRecoveryVec = replace recoveryVec (pred recoveryLag + defaultLag) recBal
        newLossVec = replace lossVec (pred recoveryLag + defaultLag) lossBal

        backToPerfBal = mulBR delinqBal (1 - defaultPct)

        restPerfVector = replicate (succ (length delinqRates)) 0
        restPerfBal = fromRational <$> restPerfVector
        newPerfCfs = if backToPerfBal > 0.0 then
                       projectDelinqMortgageFlow ([],[]) backToPerfBal Nothing (pDates!!defaultLag) (drop defaultLag (pDate:pDates))
                                                 restPerfVector restPerfVector
                                                 (drop defaultLag (rate:rates))
                                                 (0,0,0,0,p,prinType,ot)
                                                 (restPerfBal,restPerfBal,restPerfBal)
                     else
                       []

        balAfterDelinq = beginBal - delinqBal
        ppyAmt = mulBR balAfterDelinq ppyRate
        balAfterPpy  = balAfterDelinq - ppyAmt
        periodRate = periodRateFromAnnualRate p rate
        amortTerm =  case prinType of
                       Balloon aTerm -> aTerm
                       _ -> ot
        (intAmt, prinAmt) = calcAssetPrinInt prinType balAfterPpy periodRate ot remainTerms (0,amortTerm)

        endingBal = beginBal - prinAmt - ppyAmt - delinqBal
        downFactor = divideBB beginBal endingBal
        newMbn = decreaseBorrowerNum beginBal endingBal mbn
        tr = CF.MortgageDelinqFlow pDate endingBal prinAmt intAmt ppyAmt delinqBal dBal rAmt lAmt rate newMbn Nothing Nothing

projectScheduleFlow :: [CF.TsRow] -> Rate -> Balance -> [CF.TsRow] -> [DefaultRate] -> [PrepaymentRate] -> [Amount] -> [Amount] -> (Int, Rate) -> [CF.TsRow]
projectScheduleFlow trs bal_factor last_bal flows defRates ppyRates recV lossV params =
  DL.toList (go (DL.fromList trs) (lastMaybe trs) bal_factor last_bal flows defRates ppyRates recV lossV params)
  where
    lastMaybe [] = Nothing
    lastMaybe xs = Just (last xs)

    go acc _ _ last_bal [] _ _ [] [] (_,_) = acc
    go acc _ bal_factor last_bal (flow:flows) (defRate:defRates) (ppyRate:ppyRates) recV lossV (recoveryLag,recoveryRate)
      = let
          startBal = last_bal
          defAmt = mulBR startBal defRate
          ppyAmt = mulBR (startBal - defAmt) ppyRate
          afterBal = startBal - defAmt - ppyAmt

          surviveRate = (1 - defRate) * (1 - ppyRate) * bal_factor
          schedulePrin = mulBR (CF.mflowPrincipal flow) surviveRate
          scheduleInt = mulBR (CF.mflowInterest flow) surviveRate

          newRec = mulBR defAmt recoveryRate
          newLoss = mulBR defAmt (1 - recoveryRate)

          recVector = replace recV recoveryLag newRec
          lossVector = replace lossV recoveryLag newLoss

          endBal = max 0 $ afterBal - schedulePrin

          tr = CF.MortgageFlow (CF.getDate flow) endBal schedulePrin scheduleInt ppyAmt defAmt (head recVector) (head lossVector) 0.0 Nothing Nothing Nothing
        in
          go (DL.snoc acc tr) (Just tr) surviveRate endBal flows defRates ppyRates (tail recVector) (tail lossVector) (recoveryLag,recoveryRate)

    go acc mLast b_factor lastBal [] _ _ (r:rs) (l:ls) (recovery_lag,recovery_rate)
      = let
          lastDate = getDate (fromMaybe (error "Prelude.last: empty list") mLast)
          flowDate = nextDate lastDate Lib.Monthly
          tr = CF.MortgageFlow flowDate lastBal 0 0 0 0 r l 0.0 Nothing Nothing Nothing
        in
          go (DL.snoc acc tr) (Just tr) b_factor lastBal [] [] [] rs ls (recovery_lag - 1,recovery_rate)

type DelinqRate = Rate
projectScheduleDelinqFlow :: ([CF.TsRow],[CF.TsRow]) -> Rate -> Balance -> [CF.TsRow] -> [DelinqRate] -> [PrepaymentRate] -> [Amount] -> [Amount] -> [Amount] -> (Rate,Int,Rate,Int) -> [CF.TsRow]
projectScheduleDelinqFlow (trs, backToPerfCfs) surviveRate begBal flows delinqRates ppyRates defaultBals recoveryBals lossBals params =
  DL.toList (go (DL.fromList trs) backToPerfCfs surviveRate begBal flows delinqRates ppyRates defaultBals recoveryBals lossBals params)
  where
    go acc [] _ begBal flows [] [] defaults recoveries losses _ =
      let
        trsList = DL.toList acc
        patchedFlows = [ CF.MortgageDelinqFlow d begBal prin int prepay delinq defVal recVal lossVal rate mB mPPN Nothing
                        | (CF.MortgageDelinqFlow d bal prin int prepay delinq _ _ _ rate mB mPPN Nothing,defVal,recVal,lossVal) <- zip4 flows defaults recoveries losses]
        r1 = sort $ trsList ++ patchedFlows
      in
        DL.fromList r1

    go acc newPerfs _ begBal flows [] [] defaults recoveries losses _ =
      let
        trsList = DL.toList acc
        patchedFlows = [ CF.MortgageDelinqFlow d begBal prin int prepay delinq defVal recVal lossVal rate mB mPPN Nothing
                        | (CF.MortgageDelinqFlow d bal prin int prepay delinq _ _ _ rate mB mPPN Nothing,defVal,recVal,lossVal) <- zip4 flows defaults recoveries losses]
        r1 = sort $ trsList ++ patchedFlows
        r3 = CF.aggregateTsByDate [] $ sort newPerfs
        (r1keep, r1merge) = splitByDate r1 (getDate (head r3)) EqToRight
        r4 = CF.combineTss [] r1merge r3
      in
        DL.fromList (r1keep ++ r4)

    go acc backToPerfCfs surviveRate begBal (flow:flows) (delinqRate:delinqRates) (ppyRate:ppyRates) (defaultBal:defaultBals) (recoveryBal:recoveryBals) (lossBal:lossBals) (defaultPct,defaultLag,recoveryRate,recoveryLag)
      = go (DL.snoc acc tr) (CF.combineTss [] backToPerfCfs currentBackToPerfCfs) newSurviveRate endBal flows delinqRates ppyRates newDefaultBals newRecoveryBals newLossBals (defaultPct,defaultLag,recoveryRate,recoveryLag)
      where
        delinqAmt = mulBR begBal delinqRate
        ppyAmt = mulBR (begBal - delinqAmt) ppyRate
        newSurviveRate = (1-delinqRate) * (1-ppyRate) * surviveRate

        scheduleBal = view CF.tsRowBalance flow
        schedulePrin = mulBR (CF.mflowPrincipal flow) surviveRate
        scheduleInt = mulBR (CF.mflowInterest flow) surviveRate

        newDefaultBal = mulBR delinqAmt defaultPct
        endBal = max 0 $ (begBal - delinqAmt - ppyAmt - schedulePrin)
        currentBackToPerfCfs = let
                                 futureDs = drop (defaultLag+recoveryLag) $ getDates (flow:flows)
                                 splitPct = divideBB (mulBR delinqAmt (1-defaultPct)) begBal
                                 perfFlows = take (length flows - defaultLag - recoveryLag + 1) $ CF.splitTrs splitPct (flow:flows)
                               in
                                 [ set CF.tsDate d f | (d,f) <- zip futureDs perfFlows ]

        newDefaultBals = replace defaultBals (pred defaultLag) newDefaultBal
        newRecoveryBals = replace recoveryBals (recoveryLag + pred defaultLag) (mulBR newDefaultBal recoveryRate)
        newLossBals =  replace lossBals (recoveryLag + pred defaultLag) (mulBR newDefaultBal (1-recoveryRate))
        tr = CF.MortgageDelinqFlow (CF.getDate flow) endBal schedulePrin scheduleInt ppyAmt delinqAmt defaultBal recoveryBal lossBal (CF.mflowRate flow) Nothing
                                   Nothing Nothing

-- | implementation on projection via default balance amount
projCashflowByDefaultAmt :: (Balance, Date, AmortPlan, Period,IRate,Maybe BorrowerNum) -> (Dates, ([Balance],[Balance]), [Rate], [IRate], [Int]) -> [CF.TsRow]
projCashflowByDefaultAmt (cb,lastPayDate,pt,p,cr,mbn) (cfDates,(expectedDefaultBals,unAppliedDefaultBals), ppyRates, rateVector, remainTerms) = 
  go (DL.singleton initRow) cb mbn (zip5 cfDates (zip expectedDefaultBals unAppliedDefaultBals) ppyRates rateVector remainTerms)
  where
    initRow = CF.MortgageFlow lastPayDate cb 0.0 0.0 0.0 0.0 0.0 0.0 cr mbn Nothing Nothing

    go acc _ _ [] = DL.toList acc
    go acc !begBal !mBorrower ((pDate, (defaultBal,futureDefualtBal), ppyRate, rate, rt):rest) =
      let
        newDefault = if begBal <= (defaultBal+futureDefualtBal) then
                        begBal
                      else
                        defaultBal
        newPrepay = mulBR (max 0 (begBal - newDefault)) ppyRate
        newInt = mulBI (max 0 (begBal - newDefault - newPrepay)) (periodRateFromAnnualRate p rate)
        intBal = max 0 $ begBal - newDefault - newPrepay
        newPrin = case (rt,pt) of
                    (0,_) -> intBal
                    (_,Level) -> let
                               pmt = calcPmt intBal (periodRateFromAnnualRate p rate) rt
                             in
                               pmt - newInt
                    (_,Even) -> intBal / fromIntegral rt
                    _ -> error ("Unsupport Prin type for mortgage"++ show pt)
        endBal = intBal - newPrin
        newMbn = decreaseBorrowerNum begBal endBal mBorrower
      in
        go (DL.snoc acc (CF.MortgageFlow pDate endBal newPrin newInt newPrepay newDefault 0.0 0.0 rate newMbn Nothing Nothing))
           endBal newMbn rest

-- TODO to fix here , hard code on Left
calcScheduleBalaceToday :: Mortgage -> Maybe [RateAssumption] -> Date -> Balance 
calcScheduleBalaceToday m mRates asOfDay
  = let 
      sd = Ast.getOriginDate m
    in 
      case calcCashflow (resetToOrig m) sd mRates of
        Right (CF.CashFlowFrame _ scheduleTxn) ->
          case getByDate asOfDay scheduleTxn of
            Just f -> view CF.tsRowBalance f
            Nothing -> error "Failed to find schedule balance"
        Left _ -> 0


-- | implementation on projection via default balance amount
projScheduleCashflowByDefaultAmt :: (Balance, Date,IRate,Maybe BorrowerNum) -> ([CF.TsRow], ([Balance],[Balance]), [Rate] ) -> ([CF.TsRow], Rate)
projScheduleCashflowByDefaultAmt (cb,lastPayDate,cr,mbn) (scheduleFlows,(expectedDefaultBals,unAppliedDefaultBals), ppyRates) = 
  go (DL.singleton initRow) 1.0 cb mbn (zip3 scheduleFlows (zip expectedDefaultBals unAppliedDefaultBals) ppyRates)
  where
    initRow = CF.MortgageFlow lastPayDate cb 0.0 0.0 0.0 0.0 0.0 0.0 cr mbn Nothing Nothing

    go acc !factor _ _ [] = (DL.toList acc, factor)
    go acc !factor !begBal !mBorrower ((cflow, (defaultBal,futureDefualtBal), ppyRate):rest) =
      let
        pDate = getDate cflow
        newDefault = if begBal <= (defaultBal+futureDefualtBal) then
                       begBal
                     else
                       defaultBal
        newPrepay = mulBR (max 0 (begBal - newDefault)) ppyRate

        intBal = max 0 $ begBal - newDefault - newPrepay
        defRate = if (begBal - newPrepay) /= 0 then
                    divideBB newDefault (begBal - newPrepay)
                  else
                    0
        newFactor = (1 - ppyRate) * (1 - defRate) * factor
        newInt = mulBR (CF.mflowInterest cflow) newFactor
        newPrin = mulBR (CF.mflowPrincipal cflow) newFactor

        endBal = intBal - newPrin
        newMbn = decreaseBorrowerNum begBal endBal mBorrower
      in
        go (DL.snoc acc (CF.MortgageFlow pDate endBal newPrin newInt newPrepay newDefault 0.0 0.0
                          cr newMbn Nothing Nothing))
           newFactor endBal newMbn rest

buildARMrates :: IR.RateType -> (ARM,Date,Date,Date,IRate) -> Maybe [RateAssumption] -> Ts
buildARMrates (IR.Fix _ _ ) _ _ = error "ARM should have floater rate"
buildARMrates or@(IR.Floater _ idx sprd initRate dp _ _ mRoundBy ) 
              (arm, startDate, firstResetDate, lastCfDate, beginRate) mRates
  = let 
      resetDates = genSerialDatesTill2 IE firstResetDate dp lastCfDate
      projectFutureActualCurve = runInterestRate2 arm (startDate,beginRate) or resetDates
    in 
      case A.getRateAssumption (fromMaybe [] mRates) idx of
        Just (RateCurve idx curve) 
          -> projectFutureActualCurve curve 
        Just (RateFlat idx v) 
          -> projectFutureActualCurve (mkRateTs [(startDate, v),(lastCfDate,v)]) -- `debug` ("lpd"++show last_pay_date++"lpd"++ show (last cf_dates))
        Nothing -> error $ "Failed to find index"++ show idx

instance Ast.Asset Mortgage where
  calcCashflow m@(Mortgage (MortgageOriginalInfo ob or ot p sd ptype _ _)  _bal _rate _term _mbn _) d mRates
    = fst <$> (projCashflow m d (MortgageAssump Nothing Nothing Nothing Nothing
                                  ,A.DummyDelinqAssump
                                  ,A.DummyDefaultAssump) mRates)

  calcCashflow s@(ScheduleMortgageFlow beg_date [] _)  d _ 
    = Right $ CF.CashFlowFrame (0.0, beg_date, Nothing) []
  calcCashflow s@(ScheduleMortgageFlow beg_date flows _)  d _ 
    = Right $ CF.CashFlowFrame ( ((view CF.tsRowBalance) . head) flows, beg_date, Nothing ) flows

  calcCashflow m@(AdjustRateMortgage _origin _arm  _bal _rate _term _mbn _status) d mRates 
    = fst <$> (projCashflow m d (MortgageAssump Nothing Nothing Nothing Nothing
                                 ,A.DummyDelinqAssump
                                 ,A.DummyDefaultAssump) mRates)


  
  getCurrentBal (Mortgage _ _bal _ _ _ _) = _bal
  getCurrentBal (AdjustRateMortgage _ _ _bal _ _ _ _) = _bal
  getCurrentBal (ScheduleMortgageFlow _ flows _) =
    case flows of
      [] -> 0
      (f:_) -> CF.mflowBegBalance f

  getOriginBal (Mortgage (MortgageOriginalInfo _bal _ _ _ _ _ _ _) _ _ _ _ _ ) = _bal
  getOriginBal (AdjustRateMortgage (MortgageOriginalInfo _bal _ _ _ _ _ _ _) _ _ _ _ _ _ ) = _bal
  
  getOriginRate m
    = let 
        (MortgageOriginalInfo _ or _ _ _ _ _ _) = getOriginInfo m
      in  
        case or of
          IR.Fix _ _r -> _r
          IR.Floater _ _ _ _r _ _ _ _ -> _r 

  getCurrentRate (Mortgage _ _ r _ _ _) = r
  getCurrentRate (AdjustRateMortgage _ _ _ r _ _ _) = r
  getCurrentRate (ScheduleMortgageFlow _ flows _) = 0.0

  resetToOrig m@(Mortgage (MortgageOriginalInfo ob or ot p sd pt pp obr) cb cr rt mBn st)
    = Mortgage (MortgageOriginalInfo ob or ot p sd pt pp obr) 
                ob 
                (getOriginRate m)
                ot 
                mBn
                st  --TODO borrowerNum is not being updated
  resetToOrig m@(AdjustRateMortgage (MortgageOriginalInfo ob or ot p sd pt pp obr) arm cb cr rt mBn st)
    = AdjustRateMortgage (MortgageOriginalInfo ob or ot p sd pt pp obr) 
                         arm 
                         ob 
                         (getOriginRate m)
                         ot 
                         mBn
                         st  --TODO borrowerNum is not being updated
  resetToOrig m@(ScheduleMortgageFlow begDate flows dp) = m

  getPaymentDates (Mortgage (MortgageOriginalInfo _ _ ot p sd _ _ _) _ _ _ _ _) extra = genDates sd p (ot+extra)
  getPaymentDates (AdjustRateMortgage (MortgageOriginalInfo _ _ ot p sd _ _ _) _ _ _ _ _ _) extra = genDates sd p (ot+extra)
  getPaymentDates (ScheduleMortgageFlow _ [] _) extra = []
  getPaymentDates (ScheduleMortgageFlow begDate flows dp) extra 
    = let 
        lastPayDay = (getDate . last) flows
        extDates = genSerialDates dp Exc lastPayDay extra 
      in 
        getDates flows ++ extDates

  isDefaulted (Mortgage _ _ _ _ _ (Defaulted _)) = True
  isDefaulted (AdjustRateMortgage _ _ _ _ _ _ (Defaulted _)) = True
  isDefaulted Mortgage {} = False
  isDefaulted AdjustRateMortgage {} = False
  
  getOriginDate (Mortgage (MortgageOriginalInfo _ _ ot p sd _ _ _) _ _ ct _ _) = sd
  getOriginDate (AdjustRateMortgage (MortgageOriginalInfo _ _ ot p sd _ _ _) _ _ _ ct _ _) = sd
  getOriginDate (ScheduleMortgageFlow begDate _ _) = begDate

  getRemainTerms (Mortgage (MortgageOriginalInfo _ _ ot p sd _ _ _) _ _ ct _ _) = ct
  getRemainTerms (AdjustRateMortgage (MortgageOriginalInfo _ _ ot p sd _ _ _) _ _ _ ct _ _) = ct

  getOriginInfo (Mortgage oi _ _ _ _ _) = oi
  getOriginInfo (AdjustRateMortgage oi _ _ _ _ _ _) = oi

  updateOriginDate (Mortgage (MortgageOriginalInfo ob or ot p sd _type mpn obr) cb cr ct mbn st) nd 
    = Mortgage (MortgageOriginalInfo ob or ot p nd _type mpn obr) cb cr ct mbn st 
  updateOriginDate (AdjustRateMortgage (MortgageOriginalInfo ob or ot p sd _type mpn obr) arm cb cr ct mbn st) nd 
    = AdjustRateMortgage (MortgageOriginalInfo ob or ot p nd _type mpn obr) arm cb cr ct mbn st

  -- project current mortgage with total default amt 
  projCashflow (ScheduleMortgageFlow _ [] _) _ _ _ = Left "projCashflow: empty schedule mortgage flow"
  projCashflow m@(Mortgage (MortgageOriginalInfo ob or ot p sd prinPayType mpn _) cb cr rt mbn Current) 
               asOfDay 
               mars@(A.MortgageAssump (Just (A.DefaultByAmt (dBal,vs))) amp amr ams ,_ ,_) 
               mRates =
      let
        recoveryLag = maybe 0 getRecoveryLag amr
        lastPayDate:cfDates = lastN (succ (recoveryLag + rt)) $ sd:getPaymentDates m recoveryLag
        expectedDefaultBals = paddingDefault 0 (mulBR dBal <$> vs) (length cfDates)
        unAppliedDefaultBals = tail $ scanl (-) dBal expectedDefaultBals
        remainTerms = paddingDefault 0 (reverse [0..(length cfDates - recoveryLag)]) (length cfDates)
      in
        do 
          rateVector <- A.projRates cr or mRates cfDates 
          ppyRates <- Ast.buildPrepayRates m (lastPayDate:cfDates) amp
          let txns = projCashflowByDefaultAmt (cb,lastPayDate,prinPayType,p,cr,mbn) 
                                              (cfDates,(expectedDefaultBals,unAppliedDefaultBals),ppyRates,rateVector,remainTerms)
          txns' <- (patchLossRecovery txns amr)
          let (futureTxns,historyM)= CF.cutoffTrs asOfDay txns'
          let begBal = CF.buildBegBal futureTxns
          return $ (applyHaircut ams $ patchPrepayPenaltyFlow (ot,mpn) (CF.CashFlowFrame (begBal,asOfDay,Nothing) futureTxns) ,historyM)
  
  -- project current adjMortgage with total default amt
  projCashflow m@(AdjustRateMortgage (MortgageOriginalInfo ob or ot p sd prinPayType mpn _) arm cb cr rt mbn Current) 
               asOfDay 
               mars@(A.MortgageAssump (Just (A.DefaultByAmt (dBal,vs))) amp amr ams,_,_) 
               mRates =
      let
        ARM initPeriod initCap periodicCap lifeCap lifeFloor = arm
        passInitPeriod = (ot - rt) >= initPeriod 
        firstResetDate = monthsAfter sd (toInteger (succ initPeriod))

        lastPayDate:cfDates = sliceDates (SliceOnAfterKeepPrevious asOfDay)  $ lastN (rt + recoveryLag + 1) $ sd:getPaymentDates m recoveryLag 
        rateCurve = buildARMrates or (arm, sd, firstResetDate, case cfDates of [] -> lastPayDate; _ -> last cfDates, getOriginRate m) mRates
        rateVector = fromRational <$> getValByDates rateCurve Inc cfDates 
        expectedDefaultBals = paddingDefault 0 (mulBR dBal <$> vs) (length cfDates)
        unAppliedDefaultBals = tail $ scanl (-) dBal expectedDefaultBals
        recoveryLag = maybe 0 getRecoveryLag amr
        remainTerms = paddingDefault 0 (reverse [0..(length cfDates - recoveryLag)]) (length cfDates)
      in
        do
          ppyRates <- Ast.buildPrepayRates m (lastPayDate:cfDates) amp
          let txns = projCashflowByDefaultAmt (cb,lastPayDate,prinPayType,p,cr,mbn) (cfDates,(expectedDefaultBals,unAppliedDefaultBals),ppyRates,rateVector,remainTerms)
          txns' <- (patchLossRecovery txns amr)
          let (futureTxns,historyM)= CF.cutoffTrs asOfDay txns'
          let begBal = CF.buildBegBal futureTxns
          return $ (applyHaircut ams $ patchPrepayPenaltyFlow (ot,mpn) (CF.CashFlowFrame (begBal,asOfDay,Nothing) futureTxns) ,historyM)
  -- project schedule cashflow with total default amount
  projCashflow m@(ScheduleMortgageFlow begDate flows dp) asOfDay 
              assumps@(pAssump@(A.MortgageAssump (Just (A.DefaultByAmt (dBal,vs))) amp amr ams ),dAssump,fAssump) _
    = let
        begBal =  CF.mflowBegBalance $ head flows
        begDate = getDate $ head flows 
        begRate = CF.mflowRate $ head flows 
        begMbn = CF.mflowBorrowerNum $ head flows 
        originCfDates = CF.getDate <$> flows 
        originFlowSize = length flows
        recoveryLag = maybe 0 getRecoveryLag amr
        totalLength = recoveryLag + originFlowSize
        expectedDefaultBals = paddingDefault 0 (mulBR dBal <$> vs) totalLength
        unAppliedDefaultBals = tail $ scanl (-) dBal expectedDefaultBals
        endDate = (CF.getDate . last) flows
        extraDates = genSerialDates dp Exc endDate recoveryLag
        flowsWithEx = flows ++ extendTxns (last flows) extraDates -- `debug` (">> end date"++ show endDate++">>> extra dates"++show extraDates)
      in
        do 
          _ppyRate <- Ast.buildPrepayRates m (begDate:originCfDates) amp
          let ppyRates = paddingDefault 0.0 _ppyRate totalLength
          let (txns,_) = projScheduleCashflowByDefaultAmt 
                          (begBal,begDate,begRate,begMbn) 
                          (flowsWithEx,(expectedDefaultBals,unAppliedDefaultBals),ppyRates) -- `debug` ("exted flows"++ show flowsWithEx)
          txns' <- (patchLossRecovery txns amr)
          let (futureTxns,historyM) = CF.cutoffTrs asOfDay txns' -- `debug` ("txn"++show txns)
          let begBalAfterCut = CF.buildBegBal futureTxns
          return $ (applyHaircut ams (CF.CashFlowFrame (begBalAfterCut,asOfDay,Nothing) futureTxns) ,historyM)  -- `debug` ("Future txn"++ show futureTxns)

  -- project current mortgage(without delinq)
  projCashflow m@(Mortgage (MortgageOriginalInfo ob or ot p sd prinPayType mpn _) cb cr rt mbn Current) 
               asOfDay 
               mars@(A.MortgageAssump amd amp amr ams,_ ,_) 
               mRates =
    let
      recoveryLag = maybe 0 getRecoveryLag amr
      lastPayDate:cfDates = lastN (rt + 1) $ sd:getPaymentDates m 0
      cfDatesLength = length cfDates 
      remainTerms = reverse [0..rt]
      dc = getDayCount or -- `debug` ("day count"++ show dc)
      recoveryDates = lastN recoveryLag $ sd:getPaymentDates m recoveryLag
    in  
      do
        rateVector <- A.projRates cr or mRates cfDates 
        defRates <- Ast.buildDefaultRates m (lastPayDate:cfDates) amd
        ppyRates <- Ast.buildPrepayRates m (lastPayDate:cfDates) amp
        let (txns',_,_) = projectMortgageFlow 
                          (ob, cb,lastPayDate,mbn,prinPayType,dc,cr,p,ot) 
                          (cfDates, defRates, ppyRates,rateVector,remainTerms)
        let txns = DL.toList txns'
        let extraTxns = case txns of 
                          [] -> []
                          _ -> [ CF.emptyTsRow d (last txns)  | d <- recoveryDates ]
        txns'' <- (patchLossRecovery (txns++extraTxns) amr)
        let (futureTxns,historyM)= CF.cutoffTrs asOfDay txns''
        let begBal = CF.buildBegBal futureTxns
        return $ (applyHaircut ams $ patchPrepayPenaltyFlow (ot,mpn) (CF.CashFlowFrame (begBal,asOfDay,Nothing) futureTxns) ,historyM)

  -- project current mortgage(with delinq)
  projCashflow m@(Mortgage (MortgageOriginalInfo ob or ot p sd prinPayType mpn _) cb cr rt mbn Current) 
               asOfDay 
               mars@(A.MortgageDeqAssump amd amp amr ams
                    ,_
                    ,_) 
               mRates =
    let
      (recoveryRate, recoveryLag) = Ast.getRecoveryLagAndRate amr
      lastPayDate:cfDates = lastN (recoveryLag + defaultLag + rt + 1) $ sd:getPaymentDates m (recoveryLag+defaultLag)
      (_,defaultLag,defaultPct) = Ast.getDefaultDelinqAssump amd cfDates
      cfDatesLength = length cfDates + recoveryLag + defaultLag
    in
      do 
        rateVector <- A.projRates cr or mRates cfDates
        (ppyRates,delinqRates,(_,_),_,_) <- Ast.buildAssumptionPpyDelinqDefRecRate m (lastPayDate:cfDates) (A.MortgageDeqAssump amd amp amr ams)
        let txns = projectDelinqMortgageFlow ([],[]) cb mbn lastPayDate cfDates delinqRates ppyRates rateVector 
                                         (defaultPct,defaultLag,recoveryRate,recoveryLag,p,prinPayType,ot) 
                                         (replicate cfDatesLength 0.0,replicate cfDatesLength 0.0,replicate cfDatesLength 0.0)
        let (futureTxns,historyM)= CF.cutoffTrs asOfDay txns
        let begBal = CF.buildBegBal futureTxns
        return $ (applyHaircut ams $ patchPrepayPenaltyFlow (ot,mpn) (CF.CashFlowFrame (begBal,asOfDay, Nothing) futureTxns) ,historyM)

  -- project defaulted Mortgage    
  projCashflow m@(Mortgage (MortgageOriginalInfo ob or ot p sd prinPayType mpn _) cb cr rt mbn (Defaulted (Just defaultedDate)) ) 
               asOfDay
               (_,_,A.DefaultedRecovery rr lag timing) _ =
    let 
      (emptyDates,recoveryDates) = splitAt (pred lag) $ genDates defaultedDate p (lag + length timing)
      beforeRecoveryTxn = [ CF.MortgageFlow d 0 0 0 0 0 0 0 cr mbn Nothing Nothing | d <- emptyDates ]
      recoveries = calcRecoveriesFromDefault cb rr timing
      txns = [ CF.MortgageFlow d 0 0 0 0 0 r 0 cr mbn Nothing Nothing | (d,r) <- zip recoveryDates recoveries ]
      futureTxns = cutBy Inc Future asOfDay $ beforeRecoveryTxn ++ txns
      begBal = CF.buildBegBal futureTxns
    in 
      Right $ (CF.CashFlowFrame (begBal,asOfDay,Nothing) futureTxns ,Map.empty)

  -- project defaulted adjMortgage with a defaulted Date   
  projCashflow m@(AdjustRateMortgage mo arm cb cr rt mbn (Defaulted (Just defaultedDate)) ) asOfDay assumps mRates
    = projCashflow (Mortgage mo cb cr rt mbn  (Defaulted (Just defaultedDate))) asOfDay assumps mRates
  -- project defaulted adjMortgage without a defaulted Date   
  projCashflow m@(AdjustRateMortgage _ _ cb cr rt mbn (Defaulted Nothing) ) asOfDay assumps _
    = Right $ (CF.CashFlowFrame (cb,asOfDay,Nothing) [ CF.MortgageFlow asOfDay 0 0 0 0 0 0 0 cr mbn Nothing Nothing] ,Map.empty)
  -- project defaulted Mortgage    
  projCashflow m@(Mortgage _ cb cr rt mbn (Defaulted Nothing) ) asOfDay assumps _
    = Right $ (CF.CashFlowFrame (cb,asOfDay,Nothing) [ CF.MortgageFlow asOfDay 0 0 0 0 0 0 0 cr mbn Nothing Nothing] ,Map.empty)

  -- project current AdjMortgage
  projCashflow m@(AdjustRateMortgage (MortgageOriginalInfo ob or ot p sd prinPayType mpn _) arm cb cr rt mbn Current) 
               asOfDay 
               mars@(A.MortgageAssump amd amp amr ams,_,_) 
               mRates =
    let
      ARM initPeriod initCap periodicCap lifeCap lifeFloor = arm
      passInitPeriod = (ot - rt) >= initPeriod 
      firstResetDate = monthsAfter sd (toInteger (succ initPeriod))
      (recoveryRate,recoveryLag) = Ast.getRecoveryLagAndRate amr
      lastPayDate:cfDates = sliceDates (SliceOnAfterKeepPrevious asOfDay)  $ lastN (rt + recoveryLag + 1) $ sd:getPaymentDates m recoveryLag 
      cfDatesLength = length cfDates -- `debug` (" cf dates >>" ++ show (last_pay_date:cf_dates ))
      rateCurve = buildARMrates or (arm, sd, firstResetDate, last cfDates, getOriginRate m) mRates
      rateVector = fromRational <$> getValByDates rateCurve Inc cfDates -- `debug` ("RateCurve"++ show rate_curve)
      scheduleBalToday = calcScheduleBalaceToday m mRates asOfDay
      dc = getDayCount or
    in
      do 
        (ppyRates,defRates,recoveryRate,recoveryLag) <- buildAssumptionPpyDefRecRate m (lastPayDate:cfDates) (A.MortgageAssump amd amp amr ams)
        let remainTerms = reverse $ replicate recoveryLag 0 ++ [0..rt]
        let (txns,_,_) = projectMortgageFlow (scheduleBalToday, cb,lastPayDate,mbn,prinPayType,dc,cr,p,ot) (cfDates, defRates, ppyRates,rateVector,remainTerms)
        txns' <- (patchLossRecovery (DL.toList txns) amr)
        let (futureTxns,historyM)= CF.cutoffTrs asOfDay txns'
        let begBal = CF.buildBegBal futureTxns
        return $ (applyHaircut ams $ patchPrepayPenaltyFlow (ot,mpn) (CF.CashFlowFrame (begBal,asOfDay,Nothing) futureTxns) ,historyM)
  
  -- project current AdjMortgage with delinq
  projCashflow m@(AdjustRateMortgage (MortgageOriginalInfo ob or ot p sd prinPayType mpn _) arm cb cr rt mbn Current) 
               asOfDay 
               mars@(A.MortgageDeqAssump amd amp amr ams,_,_) 
               mRates 
    = let
        ARM initPeriod initCap periodicCap lifeCap lifeFloor = arm
        passInitPeriod = (ot - rt) >= initPeriod 
        firstResetDate = monthsAfter sd (toInteger (succ initPeriod))
        (recoveryRate,recoveryLag) = Ast.getRecoveryLagAndRate amr
        -- Ast.getDefaultDelinqAssump amd
        lastPayDate:cfDates = lastN (recoveryLag + defaultLag + rt + 1) $ sd:getPaymentDates m recoveryLag  
        (_,defaultLag,defaultPct) = Ast.getDefaultDelinqAssump amd cfDates
        cfDatesLength = length cfDates 
        rateCurve = buildARMrates or (arm, sd, firstResetDate, case cfDates of [] -> lastPayDate; _ -> last cfDates, getOriginRate m) mRates
        rateVector = fromRational <$> getValByDates rateCurve Inc cfDates -- `debug` ("RateCurve"++ show rate_curve)                                  
      in
        do
          (ppyRates, delinqRates,(_,_),_,_) <- Ast.buildAssumptionPpyDelinqDefRecRate m (lastPayDate:cfDates) (A.MortgageDeqAssump amd amp amr ams)
          let txns = projectDelinqMortgageFlow ([],[]) cb mbn lastPayDate cfDates delinqRates ppyRates rateVector 
                                           (defaultPct,defaultLag,recoveryRate,recoveryLag,p,prinPayType,ot) 
                                           (replicate cfDatesLength 0.0,replicate cfDatesLength 0.0,replicate cfDatesLength 0.0)
          let (futureTxns,historyM)= CF.cutoffTrs asOfDay txns 
          let begBal = CF.buildBegBal futureTxns
          return $ (applyHaircut ams $ patchPrepayPenaltyFlow (ot,mpn) (CF.CashFlowFrame (begBal,asOfDay,Nothing) futureTxns) ,historyM)
  
  -- schedule mortgage flow without delinq
  projCashflow m@(ScheduleMortgageFlow begDate flows dp) asOfDay 
               assumps@(pAssump@(A.MortgageAssump _ _ mRa ams ),dAssump,fAssump) _
    = let
        begBal =  CF.mflowBegBalance $ head flows 
        endDate = CF.getDate (last flows)
        (recoveryRate,recoveryLag) = Ast.getRecoveryLagAndRate mRa
        curveDatesLength =  recoveryLag + length flows
        extraDates = genSerialDates dp Exc endDate recoveryLag
        cfDates = (CF.getDate <$> flows) ++ extraDates
      in
        do
          (ppyRates,defRates,recoveryRate,recoveryLag) <- buildAssumptionPpyDefRecRate m (begDate:cfDates) pAssump 
          let txns = projectScheduleFlow [] 1.0 begBal flows defRates ppyRates
                                     (replicate curveDatesLength 0.0)
                                     (replicate curveDatesLength 0.0)
                                     (recoveryLag,recoveryRate) 
          let (futureTxns,historyM) = CF.cutoffTrs asOfDay txns 
          let begBalAfterCutoff = CF.buildBegBal futureTxns
          return $ (applyHaircut ams (CF.CashFlowFrame (begBalAfterCutoff,asOfDay,Nothing) futureTxns) ,historyM)
  
  -- schedule mortgage flow WITH delinq
  projCashflow smf@(ScheduleMortgageFlow begDate flows dp) asOfDay assumps@(pAssump@(A.MortgageDeqAssump _ _ _ ams),dAssump,fAssump) mRates
    = 
      let
        begBal =  CF.mflowBegBalance $ head flows -- `debug` ("beg date"++show beg_date)
      in
        do
          (ppyRates, delinqRates,(defaultPct,defaultLag),recoveryRate,recoveryLag) <- Ast.buildAssumptionPpyDelinqDefRecRate smf (begDate:getDates flows) pAssump
          let curveDatesLength = defaultLag + recoveryLag + length flows -- `debug` ("Length of rates"++show (length delinqRates)++">>"++show (length ppyRates))
          let extraPeriods = defaultLag + recoveryLag -- `debug` ("lags "++show defaultLag++">>"++show recoveryLag)
          let endDate = CF.getDate (last flows) 
          let extraDates = genSerialDates dp Exc endDate extraPeriods
          let extraFlows = [ CF.emptyTsRow d r | (d,r) <- zip extraDates (replicate extraPeriods (last flows)) ] 
          let flowWithExtraDates = flows ++ extraFlows
          let cfDates = getDates flowWithExtraDates -- `debug` ("CF dates"++ show flowWithExtraDates)
          let txns = projectScheduleDelinqFlow ([],[]) 1.0 begBal flowWithExtraDates delinqRates ppyRates
                   (replicate curveDatesLength 0.0) (replicate curveDatesLength 0.0)
                   (replicate curveDatesLength 0.0) (defaultPct,defaultLag,recoveryRate,recoveryLag)  -- `debug` ("Delinq rates"++ show delinqRates++">>ppy rates"++ show ppyRates)
          let (futureTxns,historyM) = CF.cutoffTrs asOfDay txns 
          let begBalAfterCutoff = CF.buildBegBal futureTxns
          return $ (applyHaircut ams (CF.CashFlowFrame (begBalAfterCutoff, asOfDay,Nothing) futureTxns) ,historyM)
  
  projCashflow a b c d = Left $ "Failed to match when proj mortgage with assumption >>" ++ show a ++ show b ++ show c ++ show d

  getBorrowerNum m@(Mortgage _ cb cr rt mbn _ ) = fromMaybe 1 mbn
  getBorrowerNum m@(AdjustRateMortgage _ _ cb cr rt mbn _ ) = fromMaybe 1 mbn

  splitWith (Mortgage (MortgageOriginalInfo ob or ot p sd prinPayType mpn obr) cb cr rt mbn st ) rs 
    = [ Mortgage (MortgageOriginalInfo (mulBR ob ratio) or ot p sd prinPayType mpn obr) (mulBR cb ratio) cr rt mbn st 
       | ratio <- rs ]
  
  splitWith (AdjustRateMortgage (MortgageOriginalInfo ob or ot p sd prinPayType mpn obr) arm cb cr rt mbn st ) rs 
    = [ AdjustRateMortgage (MortgageOriginalInfo (mulBR ob ratio) or ot p sd prinPayType mpn obr) arm (mulBR cb ratio) cr rt mbn st 
       | ratio <- rs ]
  

