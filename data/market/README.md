# Actual government bond benchmark data

- `oecd_inventory.csv`: unmodified OECD SDMX CSV response for all reference areas, monthly long-term rates from 2015.
- `market_yields.csv`: normalized observations and explicit NA gaps; no interpolation or generated values.
- `coverage.csv`: every returned area, latest observation, inclusion status and source URL.
- `manifest.json`: exact query, UTC retrieval time, raw-response MD5 and coverage counts.
- `fred_yields.csv`: original 12-country snapshot retained for reconciliation, not used by the app.

Source: OECD Financial market data, monthly long-term government bond benchmark yields, percent per annum. Definition: https://www.oecd.org/en/data/indicators/long-term-interest-rates.html

Citation: OECD (2026), Financial market data, https://data-explorer.oecd.org/, retrieved directly on the date in manifest.json. Retain source attribution.

At the September 2026 snapshot, 46 countries and one aggregate were returned; 43 countries pass the three-calendar-month freshness rule. That rule is re-evaluated at app startup. Exclusions remain visible in the audit. Historical observations can be revised, and refreshes must be run explicitly with Rscript scripts/fetch_market_data.R.

Country and currency metadata is maintained in config/market_metadata.csv. Unknown new areas cause validation to stop for metadata review, rather than silently disappear. Currency transitions for Croatia and Bulgaria are applied by observation date. Benchmarks are not tradable tickers or investment returns.
