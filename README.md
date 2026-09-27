<h1 align="center">Yield Atlas</h1>
<h3 align="center">Global government bond screening and holding-period scenarios</h3>

<p align="center">
  <a href="https://cran.r-project.org/">
    <img src="https://img.shields.io/badge/R-%3E%3D%204.3+-blue.svg" alt="R version" />
  </a>
  <a href="#">
    <img src="https://img.shields.io/badge/status-portfolio%20prototype-informational.svg" alt="Status" />
  </a>
</p>

---

Yield Atlas is an R Shiny app for exploring **government bond yields** and testing a simple investment scenario before checking the final details with a broker. It brings country benchmarks, market structure, individual bond terms, inflation, FX and estimated cash flows into one place.

The app is intended for screening and general research. It does not recommend which bond to buy and does not provide live executable prices.

---

## Features

- Global context from monthly 10-year government benchmark yields
- 43 actively covered countries in the current OECD snapshot
- 163 US Treasury notes and Australian Treasury bonds
- Budget, settlement date, holding period, price and FX assumptions
- Estimated face value, coupons, sale/redemption proceeds and holding-period return
- Price/FX sensitivity chart and downloadable cash-flow table
- PCA factor map and clustering of markets with similar observed features
- Historical panel GAM with fitted uncertainty and residual diagnostics
- Data source, retrieval date and methodology shown inside the app

## Feed datasets

The downloaded snapshots are included in this repository, so the app can run without calling the source websites on every launch.

Main sources:

1. [OECD long-term interest rates](https://www.oecd.org/en/data/indicators/long-term-interest-rates.html)
2. [US TreasuryDirect](https://www.treasurydirect.gov/marketable-securities/treasury-notes/)
3. [Australian Office of Financial Management](https://www.aofm.gov.au/data-hub)
4. [ECB reference exchange rates](https://www.ecb.europa.eu/stats/policy_and_exchange_rates/euro_reference_exchange_rates/html/index.en.html)
5. [World Bank consumer inflation](https://data.worldbank.org/indicator/FP.CPI.TOTL.ZG)

Download URLs, retrieval times and checksums are stored under `data/market/` and `data/instruments/`.

## Quick start

Run these commands from the project folder:

```r
source("scripts/setup.R")
shiny::runApp(".")
```

Or from a terminal:

```sh
Rscript scripts/setup.R
Rscript -e "shiny::runApp('.', launch.browser = TRUE)"
```

## Refreshing the data

```sh
Rscript scripts/fetch_market_data.R
Rscript scripts/fetch_instrument_data.R
```

Restart the app after refreshing. The scripts validate downloaded files before replacing the existing data. If a fetch fails, no synthetic observations are used as a replacement.

## Method (brief)

- Country context: monthly 10-year benchmark yields from the OECD SDMX feed
- Inflation context: annual consumer price inflation from the World Bank
- FX context: recent ECB reference cross-rates
- Bond terms: settled US and Australian government security auctions from 2024 onward
- Purchase amount: budget less entry costs, divided by dirty price and rounded down to the assumed face-value increment
- Coupons: fixed annual coupon divided into semiannual payments
- Exit: assumed clean sale price before maturity, or principal redemption at 100 at maturity
- Sensitivity: recalculation across different sale-price and FX assumptions
- Market structure: standardized PCA using yield level, 12-month change, monthly-change volatility and previous-year inflation
- Market groups: K-means or Ward hierarchical clustering, with the automatic group count chosen by average silhouette width
- Historical model: panel GAM with a common smooth time effect, lagged inflation and a country random effect

The result is based on user assumptions, not predicted market data. Missing source observations remain missing and are not carried forward.

## Current coverage

The country feed found 46 countries and one Euro Area aggregate. The app excludes the aggregate and any country with data older than three months. This left 43 active country benchmarks when the current snapshot was retrieved.

Individual security coverage is more limited:

- United States: nominal Treasury notes
- Australia: nominal Treasury bonds

TIPS, floating-rate notes, Treasury bills, bond ETFs and securities from other markets are not included in this version.

## Main assumptions and limitations

- Auction references are dated prices, not today's broker quotes
- Users can replace the reference price and FX rate with their own quote
- Tax is represented by simplified effective rates
- Coupons and remaining cash are not reinvested
- Returns are for the selected holding period and are not annualised
- Default, recovery, liquidity and bid-ask spread are not modelled
- Actual availability, denomination, eligibility, fees and tax treatment still need to be checked externally
- PCA, clusters and fitted model relationships depend on the chosen month, window and available observations

The local market snapshot will eventually become stale under the three-month freshness rule. Run the refresh scripts before using an older clone of the repository.

## Stack & packages

| Functionality | Details |
| --- | --- |
| App | `R`, `shiny`, `DT`, `plotly` |
| Data download | `httr2`, `xml2`, `jsonlite`, `readxl` |
| Market processing | Base R, `cluster`, `mgcv` |
| Charts | `plotly` |
| Storage | CSV, JSON, XML, XLSX and RDS source snapshots |

## Tests

```sh
Rscript tests/market_dashboard.R
Rscript tests/bond_scenarios.R
Rscript tests/market_structure.R
```

## Project notes

The earlier referral-model experiment is retained under `legacy/` for reference. It is separate from Yield Atlas and is not used by the current Shiny app.

## Acknowledgments

Built with R Shiny and Plotly. Market data are provided by the OECD, US Treasury, AOFM, ECB and World Bank through their public data services.
