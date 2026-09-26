# Official security and economic snapshots

`bundle.rds` contains normalized bond terms, dated auction observations, ECB FX and World Bank annual CPI. `provenance.json` records raw URLs, original download timestamps and MD5 checksums. The app can run offline from this bundle.

| File | Origin and meaning |
| --- | --- |
| `us_raw.json` | TreasuryDirect securities API: nominal fixed-rate notes; published clean auction prices, coupon, yield, settlement and maturity |
| `au_raw.xlsx` | AOFM Data Hub Treasury Bond Issuance workbook: actual transaction dates, ISINs, coupons, yields, allotted face and settlement proceeds |
| `fx_raw.xml` | ECB 90-day daily reference rates per euro; cross-rates use observations from the same date |
| `inflation_raw.json` | World Bank FP.CPI.TOTL.ZG (IMF source): annual consumer inflation since 2020; missing values retained |

At initial retrieval on 26 September 2026 there are 163 eligible securities from two countries. Auction references are not daily secondary-market quotes. The AOFM file is updated in place: the date in its URL is not the latest observation date. If the official workbook URL changes, update it from https://www.aofm.gov.au/data-hub; do not replace it with a fabricated source.

Refresh: `Rscript scripts/fetch_instrument_data.R`. Reparse existing raw files without changing their retrieval timestamps: `Rscript scripts/fetch_instrument_data.R --from-cache`. Failed requests or schema validation leave the previous app bundle intact. `--from-cache` assumes the supplied raw files are the documented official downloads; it does not fetch new observations.

US notes are filtered to nominal, non-floating, normal first interest periods and completed settlement. Australian auctions with unusable proceeds, or reference settlement inside the unsupported 14-day pre-coupon boundary, are excluded. Latest eligible auction per identifier supplies the reference; stale dates are shown, never advertised as live prices.

Source attribution: U.S. Treasury / TreasuryDirect; Australian Office of Financial Management; European Central Bank; World Bank World Development Indicators / IMF International Financial Statistics (CPI, CC BY 4.0). Source links are available in the application and README. User-generated scenarios are calculations, not source observations or forecasts.
