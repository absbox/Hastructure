import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import qualified UT.AssetTest as AT
import qualified UT.AccountTest as AccT
import qualified UT.CashflowTest as CFT
import qualified UT.BondTest as BT
import qualified UT.LibTest as LT
import qualified UT.ExpTest as ET
import qualified UT.DealTest as DT
import qualified UT.DealTest2 as DT2
import qualified UT.QueryTest as QT
import qualified UT.StmtTest as ST
import qualified UT.UtilTest as UtilT
import qualified UT.AnalyticsTest as AnalyticsT
import qualified UT.InterestRateTest as IRT
import qualified UT.RateHedgeTest as RHT
import qualified UT.CeTest as CET


import qualified DealTest.DealTest as DealTest
import qualified DealTest.RevolvingTest as RevolvingTest
import qualified DealTest.MultiPoolDealTest as DealMultiTest

import qualified Accounts as A
import qualified Lib as L
import qualified Stmt as S
import qualified Data.Time as T
import qualified Data.Vector as UtilT
import qualified UT.UtilTest as RH
import GHC.Generics (U1(U1))

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [AT.mortgageTests
                           ,AT.mortgageCalcTests
                           ,AT.btlMortgageTest
                           ,AT.loanTests
                           ,AT.leaseTests
                           ,AT.installmentTest
                           ,AT.armTest
                           ,AT.ppyTest
--                           ,AT.delinqScheduleCFTest
                           ,AT.delinqMortgageTest
                           ,AT.nonPayMortgageTest
                           ,AT.receivableTest
                           ,AT.fixedAssetTest
                           ,CFT.cfTests
                           ,CFT.tsSplitTests
                           ,CFT.testMergePoolCf
                           ,CFT.combineTest
                           ,CFT.testHaircut
                           ,CFT.testMergeTsRowsFromTwoEntities
                           ,CFT.testCumStat
                           ,CFT.testClawIntTest
                           ,CFT.testPoolAggTest
                           ,BT.pricingTests
                           ,BT.bndConsolTest
                           ,BT.writeOffTest
                           ,LT.curveTests
                           ,LT.periodCurveTest
                           ,LT.pvTests
                           ,LT.seqFunTest
                           -- --,LT.queryStmtTests
                           ,LT.datesTests
                           ,LT.prorataTests
                           ,LT.tsOperationTests
                           ,ET.expTests
                           ,DT.queryTests
                           ,DT.triggerTests
                           ,DT.dateTests
                           ,DT.liqProviderTest
                           ,DT.poolFlowTest
                           ,DT2.queryTests
                           ,UtilT.daycountTests1
                           ,UtilT.daycountTests2
                           ,UtilT.daycountTests3
                           ,UtilT.daycountTests4
                           ,UtilT.tsTest
                           ,UtilT.ts2Test
                           ,UtilT.ts3Test
                           ,UtilT.dateVectorPatternTest
                           ,UtilT.paddingTest
                           ,UtilT.dateSliceTest
                           ,UtilT.capTest
                           ,UtilT.roundingTest
                           ,UtilT.sliceTest
                           ,UtilT.splitTsTest
                           ,UtilT.tableTest
                           ,UtilT.lastOftest
                           ,UtilT.paySeqTest
                           ,UtilT.scaleListTest
                           ,AccT.intTests
                           ,AccT.investTests
                           ,AccT.reserveAccTest
                           ,QT.queryTest
                           ,ST.txnTest
                           -- ,ST.txnCalcTest
                           ,IRT.armResetTests
                           ,IRT.interestRoundingTest
                           ,AnalyticsT.walTest
                           ,AnalyticsT.durationTest
                           ,AnalyticsT.fvTest
                           ,AnalyticsT.assetPricingTest
                           ,AnalyticsT.irrTest
                           ,AnalyticsT.survivorTest
                           ,DealTest.baseTests
                           ,RevolvingTest.baseTests
                           ,DealMultiTest.mPoolbaseTests
                           ,RHT.capRateTests
                           ,CET.liqTest
                           ]
