[![Actions Status](https://github.com/absbox/Hastructure/workflows/Haskell%20CI/badge.svg)](https://github.com/absbox/Hastructure/actions)
[![Docker Build](https://img.shields.io/docker/v/absbox/hastructure?color=green&label=docker)](https://hub.docker.com/r/absbox/hastructure)
[![Pulls from DockerHub](https://img.shields.io/docker/pulls/absbox/hastructure.svg)](https://hub.docker.com/r/absbox/hastructure)
![Hackage Version](https://img.shields.io/hackage/v/Hastructure)


# What is Hastructure ?

``Hastructure`` names after ``Haskell`` and ``Structured Finance``, aims to provide cashflow projection for deal/transactions described in either Haskell structure or ``JSON`` via RESTful Service, with inputs from below:

* deal components (bonds,assets,accounts,waterfall,trigger,fees etc.) 
* pool performance assumption input as well as interest rate assumption

``Hastructure`` will generate outputs:

* cashflow of bonds/accounts/fees
* pricing of bonds
* or other outputs make your lose money faster :sunglasses:

# Why Hastructure ?

* :bricks: A collection of building blocks to build cashflows model for structured product. User just need to `compose` them together.
* :car: In-house and white-label friendly.
* :flags: No lock-in risk, all JSONs input/output, no proprietary file formats.

# I'm using language XXX

* :snake: [Python wrapper](https://github.com/yellowbean/PyABS) is in ``Beta`` now !
* :coffee: Easy integration with ``Java/C#/C++/JavaScript/Python`` with ``RESTful`` interface and Docker image are ready. 
  * C/Java : [here](https://github.com/absbox/Hastructure/issues/106)

### Documentation

* see what `Hastructure` is capable of -> [Here](https://absbox-doc.readthedocs.io/en/latest/)
  * [Where is `Hastructure` doc ? ](https://github.com/absbox/Hastructure/wiki/Where-is-documentation-of-Hastructure-%3F)

### Features
* Integration
  * Built-in REST API services
    * Language independent, integration friendly.
    * Swagger -> [here](https://github.com/absbox/Hastructure/blob/master/swagger.json)
    * Public server status -> [here](https://absbox.org)
  * Docker Support 
* Asset class coverage (Mortgage/Student Loan/Auto Loan/Rentals/Corp Loan/Consumer Installment)
* Pool Assumptions
  * Mortgage (Prepay, Prepay Penalty, Deliquency, Default,Recovery Lag/Rate)
  * Installment (Prepay Default Recovery Lag/Rate) 
  * Corp Loan (Prepay Default Recovery Lag/Rate)
  * Receivable (Default Recovery Lag/Rate)
  * Rentals (Gaps between leases,Rental Curve Assumption) 
  * Fixed Asset ( Uitility Rate)
* Multiple Waterfalls
  * Clean up waterfall/ Pre,Post Enforcement waterfall
  * Pool collection waterfall
* Accounts
  * Reserve Account/Bank Account (with interest)/Cash Account/ Ledger(PDL)
* Bonds/Tranches
  * Float Index rate / Step Up coupon type / Fix Rate
  * Sequential / Prepay Lockout /PAC Bond Support /Z Bond Support 
  * Bond Pricing (IRR /WAL /Duration /Accrual Int)
* Call
  * call by Pool/Bond Balance amount;Bond/Pool Factor;On Date/or after
* Fees
  * Pool / Bond balanced based fees 
  * Fix Amount Fees / Custom Fee Flow / Number Type Fee of a deal / Formula based fee rate 
* Liquidity Provider 
  * line of credit/ Unlimit support 
  * interest charge or fee charge on the credit used & unused
* Trigger 
  * Base on Date 
  * Base on Free Formula, Bond /Pool metrics
  * Base on Pool performance, like Cumulative Default Rate, last 3 periods delinquency rates.
  * Base on any combination above
* Interest Swap
  * Float to Float/ Fix to Float
  * formula based notional balance
* Scenario Analysis
  * Running multiple scenarios on single deal
  * Pricing on single asset 
  * Revoving Buy Analysis 
* Free Formula Support 
  * User is able to using statistics of deal ( Pool Balance,Account balance ,total Bond Balance of , A factor of .. ) to construct formula which used to specify the amount of cash to transfer , pay out to fee or liabilities etc.
* Misc
  * Support user define pay dates & pool collection dates 


### Online Demo

The demo only cover very limit features of this engine and subject to UI performance issue due to rapid prototype design of web component

**Pls noted that the web demo is far behind latest development/stable version**

* [Here](https://deal-bench.xyz)


### Others
* [Why yet another cashflow engine](https://github.com/absbox/Hastructure/wiki/Why-Yet-Anohter-Cashflow-Engine)
