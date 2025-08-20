# Changelog for Hastructure

<!-- towncrier release notes start -->


## 0.51.5
### 2025-08-20
* NEW: New accrue-type waterfall action
* ENHANCE: add assumption validation on `prepayment` `default` `recovery` .
* FIX: pricing IRR: engine will use wrong position when `Buy Bond`


## 0.51.2
### 2025-08-02
* NEW: new asset `ProjectedByFactor` and `ProjectedCashflow` .
* ENHANCE: refactor on waterfall action, throw error when `name` is missing from the deal object
* ENHANCE: throw error when divide by zero
* ENHANCE: refactor on `credit support` for `liquidity suppport.
* FIX: fix zero cashflow when holding position is too small

## 0.50.0
### 2025-07-14
* NEW: add `stopBy` in run assumption ,which stop deal run by a list of `Condition`
* NEW: expose `asset level` cashflow ,with a toggle
* NEW: expose `Un-Used Pool cashflow`
* ENHANCE: parameterized the `tweaks`
* ENHANCE: update dependency to accomodate publishment to `Hackage`
* FIX: enable `Lease` deal for `Financial reports`


## 0.46.4
### 2025-06-10
* ENHANCE: add error message when calculation IRR for bond with non cashflow
* ENHANCE: add `tweak`: `Stress Prepayment` 
* ENHANCE: add `stop`: `Bond Principal Loss` `Bond Interest Loss`


## 0.46.2
### 2025-06-08
* ENHANCE: add `tweak`: `Balance Split` and `stop`: `Bond Met Target IRR`

## 0.46.1
### 2025-06-07
* ENHANCE: add 2 more `leaseEndType` assumptions: `Earlier` `Later` which will end the lease projection base on two input `End date` and `extention times`.
* ENHANCE: expose `new bond rate type` in `trigger effects`. Now bond rate type can be changed during the projection.
* REFACTOR: with new refactor `root finder` endpoint and signature. In the long term, the refactor of signature lays down fundation for `deal structuring` domain, now it would be easy to implement all kinds of structuring features.

## 0.45.7
### 2025-05-26
* ENHANCE: add `BaseByVec` for vector-based rental change


## 0.45.5
### 2025-05-20
* NEW: `MaxSpread` feature for structuring stage: get max possible bond coupon rate !
* ENHANCE: Transfer from `stack` to `cabal` as build tool
* ENHANCE: Apply `DList` to trigger log
* ENHANCE: Enable `Double Decline Balance` in `FixedAsset`
* REFACTOR: Refactor `Leasing` asset type
  * Add `Default` assumption
  * Add `Period-based` rental ,in addition to `Day-based` rental calculation


## 0.45.2
### 2025-04-01
* ENHANCE: Performance optimization by replace `List` with `DList`.
* ENHANCE: In `inspection` ,expose `IsOutstanding` `HasPassedMaturity` in `Pre`

## 0.45.1
### 2025-03-25
* FIX: in `Pricing/IRR`, error when holding position is too small
* ENHANCE: engine will auto patch `interest start date` for bonds if it is not modeled. In `PreClosing` status, engine will use `closing date` as bond interest begin date ; In `Non-PreClosing` status, it defaults to use last waterfall distribution date as bond interest begin date.

## 0.45.0
### 2025-03-21
* BREAK: remove unused `DealDates` : `FixInterval`, `CustomDates` and `PatternInterval`. Since all these can be replace by new `GenericDates` in type `DateDesp`
* ENHANCE: now bond with `No last interest accure day` will begin accrue interest from `closing date` if the deal is in `PreClosing` mode, while the bond will use `last bond day` otherwise.
* FIX: `IsPaidOff` now can be queried in inspection formula


## 0.44.0
### 2025-03-11
* BREAK: Add `PAC` `PAC Anchor` to `BondGroup`, now `BondGroup` is `Map String L.Bond (Maybe PrinType)`
* NEW: add formula `bondTargetBalance` to query target amortized balance
* ENHANCE: expose `PAC Anchor` which is same to `PAC` except that the balance schedule will be ineffective if `Anchor Bonds` are paid off.


## 0.43.0
### 2025-03-08
* NEW: new interest type `BalRef` which bond will accrue its interest by a `Formula`, which is being used to model `IO` bond
* ENHANCE: in `FirstLoss` ,the stress will be applied to `revolving assumption` as well
* FIX: add `interest accrued` in bond pricing result
* BREAK: In waterfall ,the action `CalcBondInt` now only accepts a list of bond names
* BREAK: asset modeling and analytics `lease` has been refactored


## 0.42.10
### 2025-02-15
* NEW: expose new bond pricing : calculate IRR for `holding` a bond, `hold and sell` a bond, or `buy a bond`.
* ENHANCE: lift `Pricing` to expose error message.
* ENHANCE: change compare symbol in response from `GT` to `>` and others as well.
* ENHANCE: auto patch `bond paid periods` and `pool collection periods` for `preClosing` deal.


## 0.42.8
### 2025-02-13
* FIX: Enable `byTerm` assumption on `Installment`
* FIX: cap the default rate vector with 100% geneated by `root.finder`
* NEW: add `PeriodBased` rate curve or balance curve in `Pre`, i.e. easy to build default rate trigger in structuring stage

## 0.42.4
### 2025-02-06
* NEW: `FirstLoss` as new endpoint, which will stress on `Default` assumption till 0.01 loss on input tranche.
* NEW: New prepayment /default assumption via `byTerm`, which vector curves are being applied via term of the assets.


## 0.42.3
### 2025-02-04
* NEW: `Multi-thread` on pool cashflow projection
* NEW: Expose `convexity` on bond/asset
* NEW: Add new prepayment assumption `PSA` for Monthly mortgage
* NEW: Add new prepayment/default vector assumption based on asset origin term

## 0.42.1
### 2025-02-02
* NEW: add custom fee flow by `BondPaidPeriod` `PoolCollectedPeriod` index


## 0.42.0
### 2025-02-01
* ENHANCE: refactor `calcPmt` to boost 15x performance for mortgage cashflow projection. 
* NEW: add `ScheduleByIndex` for bonds
* FIX: `fundWith` shall increase the bond balance
* ENHANCE: refactor Z-spread calc logic with numeric.root.finder


## 0.41.1
### 2025-01-11
* NEW: `Multi Interest Bond` which used to model in bond with `step up` feature ( sub ordinated interest) in European 
* NEW: new assumption ,which used to funding existing bond.
* NEW: new query `totalFunding` for bond,which records all funding amount of bond.
* NEW: new query `AmountRequiredForIRR` for bond,which return the amount to be paid out make bond met the target IRR.
* NEW: in `Rate Swap` , the notional can be set by `Formula`
* ENHANCE: when account is being used as a source for `support`, it has option to book ledger with both `credit` and `debit` direction.
* FIX: `payPrinBySeq` was not paying out principal.


## 0.40.13
### 2024-12-17
* NEW: new formula `totalFunded` for bond with extra funding amount
* NEW: new deal run assumption `FundBond`, which records a time series funding amount for a single bond.
* ENHANCE: When booking account from `support` action, now user can book on `Credit` or `Debit` side
* FIX: `payPrinBySeq` was not working



## 0.40.9
### 2024-12-11
* ENHANCE:  Ensure `limit` always return positive ,otherwise engine will throw error
* NEW: add new action  `changeStatus` in waterfall, with optional `Pre` as condition to trigger the status change


## 0.40.6
### 2024-12-06 
* NEW: new formula `ledgerBalanceBy`, which return either `Credit` or `Debit` balance of a ledger
* FIX: step-up coupon bond which has a floater index will increase forever
* ENHANCE: refactor on `PDL` book type.


## 0.40.1
### 2024-11-05
* NEW: break changes on API ,now the engine is able to throw out error message instead of just hanging.


## 0.31.0
### 2024-11-05
* NEW: new Call options assumption ,which specifies `dates` to be tested
* ENHANCE: transform financial report to a `Tree` from a `Table`

## 0.30.5
### 2024-11-02
* NEW: Expose bond factor formula for single bond
* FIX: Enable balanceSheet support for multiple pools
* FIX: Include logs from clean up waterfall
* FIX: Include logs from trigger/actions
* ENHANCE: query borrower number by Pool Id
* ENHANCE: query current pool balance by Pool Id


## 0.30.3
### 2024-10-20
* NEW: Expose combo sensitivity endpoint, 
* NEW: Expose single clear ledger function
* NEW: Expose `writeoffBySeq` which write a list of bonds by sequence.
* NEW: Add new assumption curve with padding last value to rest
* NEW: Expose extra Stress on ppy/def curve, use can impose time-series based stress on prepayment and default.
* NEW: Expose `transferMultiple`, with one action transfer multiple account to single account
* ENHANCE: Expose pricing for bond groups
* ENHANCE: Instead of liquidating all pools but users now have the option to select pool to liquidate
* FIX: Revolve buy when building balance
* FIX: avoid duplicate run waterfall in call


## 0.28.21
### 2024-8-25
* ENHANCE: Expose pricing function with options of `include` or `not include` accrued interest.
* NEW: Ballon Mortgage
* NEW: Expose Assumption: defaultAtEnd with rates
* NEW: Expose revolving buy asset from multiple pools
* NEW: Expose which waterfall is run at each payment date

## 0.28.16
### 2024-8-6
* FIX: correct `runPool` cashflow order and add UT 

## 0.28.15
### 2024-7-31
* FIX: enable compound formula on `weighted average` formula.


## 0.28.14
### 2024-07-06
* FIX: enable `annualized rate fee type` with formula `bondbalance` on `bondGroup`

## 0.28.13
###  2024-06-30
* NEW: new assumption `issue bond` which allow funding by issuing new bonds during cashflow projection.
* NEW: new asset class `projectScheduleFlow` which can be divided projected cashflow with fix portion and float portions. The interest from the float portion will be affected by interest rate assumption.
* ENHANCE: enable formula `bondRate`/`bondWaRate` on `bondGroup`
* FIX: `formula` will return `inf` if a `divide` with zero instead of just throw exception
* FIX: `financial reports` was failing because it can't access to `interest due` on bond group.
* FIX: enable formula query on `bond groups` 


## 0.28.8
### 2024-06
* FIX: `limit` on `payFee` was not working with `duePct`
* ENHANCE: expose `transaction statement` for `triggers`

## 0.28.2
### 2024-05-27
* NEW: enable `trigger` to run waterfall `actions`
* FIX: the `result log` used to be doubled each pool collection period
* FIX: `payPrinResidual` will use all cash from account regardless principal due of bonds, which may caused negative balance of bonds( cash of account > principal due of bond)

## 0.28.1
### 2024-05-26
* BREAK : add `bondGroup`, which group bonds and pay with prorata/sequential/by coupon rate/by maturity/by start date
* BREAK : add `begin balance`/`accure interest`/`as of date` for cashflow frame
* BREAK : add `interest arrears` `interest over interest` on bond cashflow
* NEW: add `interest over interest` settings on bonds and expose `interest over interest` `interest due` flow
* ENHANCE: add tabular representation of cashflow frame
* FIX: fix rolling default rate query


## 0.27.21
### 2024-05-15
* NEW: add `weekday <n>` in the date pattern
* ENHANCE: expose `weekly` /`biweekly` in `Period`
* NEW: now allow new `first N period without Fee` feature to model cashflow of type `Installment` 
* FIX: negative pool balance for (revolving pool asset >= 2)

## 0.27.13
### 2024-05-05
* ENHANCE: enable all `Combination Type` formula (via patching dates)
* ENHANCE: add capability to query txn in (Fee/Bond/Account) via a `comment`


## 0.27.12
### 2024-05-04
* ENHANCE: deal will return how it was ended in projection

## 0.27.11
### 2024-05-04
* NEW: Formula:  `originalBondBalance`,`BondDuePrin`
* NEW: Waterfall Action: `CalcBondPrin`,`PayPrinWithDue`
* ENHANCE: fix Formula: `PoolFactor`
* NEW: Enable `*` between formulas
* FIX: Unlimit Liquidity Provider has wrong available balance

## 0.27.7
### 2024-05-01
* ENHANCE: Enable pricing on asset via a constant rate/rate curve; add `duration` for asset pricing (curve only)


## 0.27.4
### 2024-04-15
* ENHANCE: Pool run: enhance multip-scenario run and mulitple-assets type run
* ENHANCE: Enable revovling on `Receivable`
* ENHANCE: add `RecoveryByDays` to `Receivable` ,which describes recovery cash received after default.
* FIX: Fix single asset run on `lease`
* FIX: Failed to include cumulative stats on revolving buy assets
* FIX: Multi-asset run was failure due to including schedule cashflow run.
* ENHANCE: upgrade stack resolver from `lts-18.22` to `lts-22.6`


## 0.27.0
### 2024-04-01
* NEW: Now docker image ship with `Apple silicon` chip ! Happy hacking Mac users !


## 0.26.2
### 2024-03-24
* FIX: patch recoveries for `Mortgage` type cashflow
* NEW: add new asset class `Receivable` which represent a `invoice factored`,`trading receivable`
* NEW: `DefaultAtEnd` assumption, which assumes asset default at last payment(For `Receivable`)

## 0.26.1
### 2024-03-09
* NEW: `fundWith` which will increate the balance of bond and deposit cash to account.
* NEW: `writeOff` which will write off balance of bond via a formula
* NEW: a new predicate `passMaturity` which True if bonds has passed their expected maturity/pay off date.
* NEW: `Not` as composite boolean test.
* NEW: add new `OAS` pricing assumption, which return OAS spread given input scenarios.

## 0.26.0
### 2024-02-27
* NEW: add `NO_FirstN` as type of `Mortgage` which implies no payment for first N period and interest due will be capitalized.
* NEW: add `IO_FirstN` as type of `Mortgage` which implies no principal payment for first N period.
* NEW: add `Make Whole` Feature, which allow user to set a <Date>,<Spread>,<WAL/Spread> Table. The bond will be componsate with PV from spread determined by WAL remaining.

## 0.25.0
### 2024-02-16
* NEW: add `resec deal`, which allow to use bonds as underlying assets and allow user to set assumption on underlying deals.


## 0.24.1
### 2023-12-17
* NEW: add `payIntBySeq` which pay interest to bonds sequentially with optional limit
* NEW: add condition to "ExtraSupport" ,which support only available if a <predicate> is satisfied 
* NEW: add `Nothing` to trigger effects
* NEW: add `payFeeBySeq` to which pay a list of fees sequentially with optional limit
* NEW: add a fee type which due amount is X per pool collection period
* NEW: add a fee type which is a lookup table with look up value from a formula
* NEW: add override feature `rate` and `balance` to `calcDueInt` action in waterfall.
* NEW: multiple pool support !! now engine support multiple pools in a deal with mixed assets.
* NEW: add query on `present value on schedule pool cashflow`, which enable `Yield Maitenance Overcollaterisaztion` supports



## 0.23.1
### 2023-11-16
* NEW: new asset class `FixAsset` type , which yield cashflow given a `capacity` and assumption called `utilization rate curve`. The new asset type is applicable to Hotel booking/EV Charge station/Solar Panel/Wind Power type.
* NEW: new rate hedge instrument `RateCap` which yield cash if `rateCurve` is higher than a `strike rate`
* NEW: add `accruedInterest` field in pool stats, which will be deducted from pool cash flow 
* NEW: add `payPrinBySeq` in waterfall action, now user can pay prin bond via a simple list.
* NEW: add an assumption `fireTrigger` which mannualy fire a trigger at point of projection
* NEW: add pool collection type `totalCash` will aggregate all pool cash field
* NEW: `payInt` now accept a `limit` which constrain how much interset to be paid via a `formula`
* NEW: add `bookBy` a ledger via `formula`
* NEW: add `I_P` to `Mortgage` type ,which models `Buy To Let` type mortgage( interest only and principal at last period)
* ENHANCE: include `Lens` and code clean up
* BREAK: refactor `StepUp` out of `interest` part of bond.


## 0.22.2
### 2023-10-27
* ENHANCE: expose cumulative stats on pool cashflow returned by `runDeal`


## 0.22.1
### 2023-10-26
* NEW: add `default by amount` assumption, which enable user to set a `total amount of default` alongside with a vector.
* ENHANCE: misc refactors


## 0.22.0
### 2023-10-15
* BREAK: cashflow now with `Cumulative Stats` ( cumulative default/delinq/loss/prepayment/principal/recovery)
* NEW: expose `inspect` in waterfall action to observe variables during a waterfall execution
* NEW: `stepup` now accpet a `pre` instead of a `date` to switch rate
* ENHANCE: auto patch `issuance balance` for `PreClosing` Deal
* ENHANCE: implement `pre-run check` and `post-run check`
  * IssuanceBalance check : `Ongoing Deal` shall have a IssuanceBalance value in Pool
  * Interest Rate check : index required by deal should be found in assumption
  * Waterfall action check : actions in waterfall ( source/target) should exist in deal object
* FIX: fix bug on `prepay penalty` when using `stepDown`
* FIX: fix project cashflow for `Loan`

## 0.21.5
### 2023-10-8
* ENHANCE: in the revolving buy , now buy amount is no longer a multipler of revolving assets face value
* FIX: now revolving asset may have remtain term ==  original term

## 0.21.4
### 2023-9-27
* ENHANCE: require a new status when defining a deal in `preClosing` stage
* FIX: fix a bug when reading financial report logs 

## 0.21.3
### 2023-9-26
* NEW: include a `default`/`delinq`/`loss` status map when projecting cashflow
* NEW: implement `haircut` as extra stress projecting `mortgage` 
* ENHANCE: include `called` deal status, which will be set when deal was triggered with a clean up call assumption
* ENHANCE: expose `runAsset` endpoint
* ENHANCE: expose formula query on `deal status` as well as `trigger status`
* ENHANCE: add `rampUp` deal status
* FIX: adjust bond reset date from `cutoff date` to `closing date`

## 0.21.1
### 2023-9-21
* BREAK: seperate `performance assumption`
* BREAK: add `delinquency` projection on mortgage as well as schedule mortgage cashflow

## 0.20.3
### 2023-9-4
* ENHANCE: now user can include boolean/int/balance/rate type query in `inspect` field

## 0.20.2
### 2023-8-31
* BREAK: move `Trigger` from `list` into a `map` with a name
* ENHANCE: add `CumulatiePoolDefaultedRateTill` to query default rate as of collection period N , then support query last one,last two default rates in the past as a rolling basis..
* ENHANCE: add `queryBool` with test logic of `any` `or` which will test all predicates or any predicates are/is satisfied. With new included aforementioned formula above, the engine can have a predicate like `last 2 period cumulative defaulte rates are all lower than 5%`, `any last 2 period cumulative defaulte rates is higher than 5%`  

## 0.20.1
### 2023-8-29
* ENHANCE: add `LedgerTxnAmt` , allow user to query transaction amount for a ledger by `comment`
* ENHANCE: expose `Abs` in formula , which will get absolute value of another formula

## 0.20.0
### 2023-8-25
* BREAK: refactor `payInt` and `payFee` which includes `extraSupport` from either another `account` or `liquidation provider`, with option to book `PDL draw` on ledger
* NEW: expose `Cumulative Net Loss` `Cumulative Net Loss Ratio` `Bond Rate` `Bond Weight Average Rate` in formula
* NEW: expose `Avg` in formula ,which can calculate average value from a list of deal stats.
* NEW: expose `RefRate` in bond , now bond can be setup interest rate which reference to a value of deal , could be like 100% of Pool WAC coupon , or average of bond rate of bonds etc.
* ENHANCE: add `liquidity provider` `interest swap` to `balance sheet repot`
* ENHANCE: add new bool query `is_most_senior_bond`
* ENHANCE: add new balance query `PoolCurCollection` returns target pool source balance in last collected period
* ENHANCE: refactor account transfer by `target reserve amount`


## 0.19.15
### 2023-8-20
* ENHANCE: add `reserve account excess/gap` to formula
* ENHANCE: refactor bond `step up` coupon by date ,which pertains to Euro deals
* ENHANCE: add comments to souce code and prepare to release to `Hackage`

## 0.19.12
### 2023-8-17
* NEW: Add `Step Up By Date` /`Cap`/`Floor` coupon type for bond
* NEW: Add `Prepay Penalty` attribute on `Mortgage`, penalty types includes:
  * `rate0` before `term N` and `rate1` after `term N`
  * Fixed amount in lifetime or before `term N`
  * Fixed pct in life time or bfore `term N`
  * Sliding from `rate0` by step of `rate1`
  * Ladder type like first `12` periods with Pct of `Rate0` , next `12` periods with Pct of `Rate1`
* ENHANCE: refactor `liquidity provider`
  * include a maybe `valid date` of the agreement
  * include floater index

## 0.19.11
### 2023-8-14
* ENHANCE: add `calcAndPay` action for fee
* ENHANCE: expose new assumption on expense projection
* ENHANCE: include a new `NO_IE` type to generate dates vector
* FIX: Fix missing periods of `recurr` type of fee

## 0.19.10
### 2023-8-7
* NEW : add a new expense type: `TargetBalanceFee`, which due amount = `<formula 1> - <formula 2>`
* ENHANCE: add query total txn amount for account/bond/expense with optional `comment` as a filter
* ENHANCE: expoese query on `cumulative pool` on `recoveries` `principal` `interest` `prepayment` 
* ENHANCE: expoese query on `beg balance` on pool


## 0.19.8
### 2023-7-24
* ENHANCE: trancate payments records for bond with 0 balance and 0 due interest/due pricipal

## 0.19.7
### 2023-7-19
* ENHANCE: expose query on `cumulative pool recoveries`
* ENHANCE: expose `factor` in query
* ENHANCE: ensure principal payment is cap via bond oustanding balance 

## 0.19.6
### 2023-7-18
* FIX: update PDL Ledger balance after `bookBy` action

## 0.19.4
#### 2023-7-17
* FIX: fix pricing error if bond flow size is 0

## 0.19.0
#### 2023-7-1
* BREAK : seperate `payInt` action and `accrueInt` action
* ENHANCE: optimize `Z-spread` calculation
* ENHANCE: re arrange `deal.hs` , break down code logic into seperate files.
* NEW: include `ledgers` to accomodate `PDL` feature (Principal Deficiency Ledger)
* ENHANCE: expose `rounding` on `deal stats`, which rounds interest rate change by a factor of fix amount ,or pay principal on balance by a factor of fix amount.
* ENAHNCE: expose `runDate` endpoint as sandbox for user to play with `<datePattern>`


## 0.18.9
#### 2023-6-23
* NEW : add `floorAndCap` formula to set upper or lower bound of formula value
* NEW : add formula based fee rate for `pct` and `annual` type of fee

## 0.18.1
#### 2023-6-21
* NEW : Project cashflow for a list of asset, with performance assumption
* ENHANCE : Add `limit` for revolving buy action
* ENHANCE : Add `default` waterfall 
* NEW : Add "IF-ELSE" in waterfall action

## 0.18.0
#### 2023-6-8
* NEW "Major" : expose revolving assumption !
* NEW : `Pre` now support comparing with a `balance` type formula ,not limited to a balance number


## 0.17.2
#### 2023-5-24
* NEW: expose `exclude dates` and `offset by days` <date pattern>


## 0.17.1
#### 2023-5-21
* NEW: expose trigger status in `Inspect`

## 0.17.0
#### 2023-5-21
* NEW: expose `BalanceSheet Report` and `Cashflow Report`, user can query them via set flags in assumption
* BREAK: normalized some account comments to be analysed when compling `Cashflow Report`


## 0.16.0
#### 2023-5-13
* NEW: expose bond with `Step-Up coupon` feature 
* BREAK:using `DatePattern` to annotate reset date for floater bonds
* FIX: data query in the trigger

## 0.15.4
#### 2023-5-6
* FIX: Fix cashflow projection logic for `Installment`
* Include `DefaultedRecovery` assumption for defaulted assets.

## 0.15
#### 2023-5-1
* Introduce new asset : `AdjustRateMortgage` with assumption:
    * init period,first reset cap, periodic reset cap,lifetime cap, lifetime floor
* Docker hub will host each stable releases of Hastructure



