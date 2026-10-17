[![Actions Status](https://github.com/absbox/Hastructure/workflows/Haskell%20CI/badge.svg)](https://github.com/absbox/Hastructure/actions)
[![Docker Build](https://img.shields.io/docker/v/yellowbean/hastructure?color=green&label=docker)](https://hub.docker.com/r/yellowbean/hastructure)
[![Pulls from DockerHub](https://img.shields.io/docker/pulls/yellowbean/hastructure.svg)](https://hub.docker.com/r/yellowbean/hastructure)
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

### Installation

#### From Docker

Pull from [Docker Hub](https://hub.docker.com/r/yellowbean/hastructure) using the image path `yellowbean/hastructure` (full registry path `docker.io/yellowbean/hastructure`; `latest` follows the `v*` release tags, `dev` the `a*` dev tags):

```bash
docker pull yellowbean/hastructure:latest
docker run -p 8081:8081 yellowbean/hastructure:latest
```

Or build the checkout with compose:

```bash
docker compose up --build
```

The image ships `config.yml` and `swagger.json` in `/opt/myapp`; mount your own config over it to change the server settings:

```bash
docker run -p 9090:9090 -v "$PWD/config.yml:/opt/myapp/config.yml" yellowbean/hastructure:latest
```

#### From nixpkgs

```bash
nix-shell shell.nix                     # dev shell: GHC, HLS, cabal
nix-build release.nix -A hastructure    # build the engine -> result/bin/Hastructure-exe
```

#### From release executables

Prebuilt executables for Linux/macOS/Windows are attached to [GitHub releases](https://github.com/absbox/Hastructure/releases) as `Hastructure-<version>-<OS>-ghc-<ghc-version>`. Download one, then:

```bash
chmod +x Hastructure-<version>-Linux-ghc-9.10.2
./Hastructure-<version>-Linux-ghc-9.10.2
```

Run it from a directory containing `config.yml`: the engine reads the file at startup and refuses to start when it is missing, malformed or contains an invalid value.

#### Configuration (`config.yml`)

See the sample [`config.yml`](https://github.com/absbox/Hastructure/blob/master/config.yml) in the repository. The engine reads it from the working directory once at startup (restart to apply changes); it holds the following fields:

* `port` (required) — HTTP listen port; a missing `port` aborts startup.
* `workerNum` (optional) — in-flight evaluation cap applied to every multi-run request. Precedence: `HASTRUCTURE_WORKERS` environment variable (when it parses to a positive integer), then `workerNum`, then the RTS capability count (`+RTS -N`). A present-but-unusable value falls back to the next layer, since this is only a performance knob.
* `maxRunNum` (optional) — per-endpoint total-run caps configured as `maxRunNum.<endpoint>`, default `1000` each; a non-positive value aborts startup:
  * `runPoolByScenarios` — number of pool scenarios
  * `runDealByScenarios` — number of deal scenarios
  * `runMultiDeals` — number of deals
  * `runDealByRunScenarios` — number of non-performance assumption scenarios
  * `runByCombo` — size of the deals × assumptions × non-performance assumptions cartesian product (sized before materialising, so oversized grids are rejected without allocation)
* Exceeding a cap — rejected with HTTP 400 in the standard JSON error envelope (`{"Left":"..."}`), naming the endpoint, the configured limit and the `maxRunNum.<endpoint>` key to raise. Invalid config values abort startup with an `invalid config.yml: ...` error naming the offending key.


### Documentation

* see what `Hastructure` is capable of -> [Here](https://absbox-doc.readthedocs.io/en/latest/)
  * [Where is `Hastructure` doc ? ](https://github.com/absbox/Hastructure/wiki/Where-is-documentation-of-Hastructure-%3F)

### Features

<details>
<summary><b>Integration</b></summary>

* Built-in REST API services
  * Language independent, integration friendly.
  * Swagger -> [here](https://github.com/absbox/Hastructure/blob/master/swagger.json)
  * Public server status -> [here](https://absbox.org)
* Docker Support 

</details>

<details>
<summary><b>Asset class coverage</b></summary>

* Mortgage/Student Loan/Auto Loan/Rentals/Corp Loan/Consumer Installment

</details>

<details>
<summary><b>Pool Assumptions</b></summary>

* Mortgage (Prepay, Prepay Penalty, Deliquency, Default,Recovery Lag/Rate)
* Installment (Prepay Default Recovery Lag/Rate) 
* Corp Loan (Prepay Default Recovery Lag/Rate)
* Receivable (Default Recovery Lag/Rate)
* Rentals (Gaps between leases,Rental Curve Assumption) 
* Fixed Asset ( Uitility Rate)

</details>

<details>
<summary><b>Multiple Waterfalls</b></summary>

* Clean up waterfall/ Pre,Post Enforcement waterfall
* Pool collection waterfall

</details>

<details>
<summary><b>Accounts</b></summary>

* Reserve Account/Bank Account (with interest)/Cash Account/ Ledger(PDL)

</details>

<details>
<summary><b>Bonds/Tranches</b></summary>

* Float Index rate / Step Up coupon type / Fix Rate
* Sequential / Prepay Lockout /PAC Bond Support /Z Bond Support 
* Bond Pricing (IRR /WAL /Duration /Accrual Int)

</details>

<details>
<summary><b>Call</b></summary>

* call by Pool/Bond Balance amount;Bond/Pool Factor;On Date/or after

</details>

<details>
<summary><b>Fees</b></summary>

* Pool / Bond balanced based fees 
* Fix Amount Fees / Custom Fee Flow / Number Type Fee of a deal / Formula based fee rate 

</details>

<details>
<summary><b>Liquidity Provider</b></summary>

* line of credit/ Unlimit support 
* interest charge or fee charge on the credit used & unused

</details>

<details>
<summary><b>Trigger</b></summary>

* Base on Date 
* Base on Free Formula, Bond /Pool metrics
* Base on Pool performance, like Cumulative Default Rate, last 3 periods delinquency rates.
* Base on any combination above

</details>

<details>
<summary><b>Interest Swap</b></summary>

* Float to Float/ Fix to Float
* formula based notional balance

</details>

<details>
<summary><b>Scenario Analysis</b></summary>

* Running multiple scenarios on single deal
* Pricing on single asset 
* Revoving Buy Analysis 

</details>

<details>
<summary><b>Free Formula Support</b></summary>

* User is able to using statistics of deal ( Pool Balance,Account balance ,total Bond Balance of , A factor of .. ) to construct formula which used to specify the amount of cash to transfer , pay out to fee or liabilities etc.

</details>

<details>
<summary><b>Misc</b></summary>

* Support user define pay dates & pool collection dates 

</details>


### Others
* [Why yet another cashflow engine](https://github.com/absbox/Hastructure/wiki/Why-Yet-Anohter-Cashflow-Engine)
