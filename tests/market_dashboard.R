# Tests use the downloaded observations, never generated price/yield fixtures.
source("app.R")
d <- read_market_data()
months <- available_months(d)
stopifnot(length(unique(d$country)) == 43L, length(months) > 12L,
          !anyDuplicated(d[c("date", "series_id")]))
month <- as.character(months[1])
ranking <- rank_markets(d, month)
stopifnot(nrow(ranking) >= 5L, all(diff(ranking$yield) <= 0),
          all(ranking$date == as.Date(month)))
euro <- rank_markets(d, month, currency = "EUR")
stopifnot(all(euro$currency == "EUR"), nrow(euro) >= 5L)
empty <- rank_markets(d, month, region = "Americas", currency = "JPY")
stopifnot(nrow(empty) == 0L)
selected <- c("United States", "Australia", "Japan")
changes <- comparison_data(d, selected, month, 3L, "Change (basis points)")
stopifnot(all(changes$value[changes$date == min(changes$date)] == 0))
for (country in selected) {
  rows <- changes[changes$country == country, ]
  stopifnot(isTRUE(all.equal(rows$value, (rows$yield - rows$yield[1]) * 100)))
}
raw <- read.csv("data/market/fred_yields.csv", na.strings = c("", "."))
actual <- d[d$country == "United States", ]
stopifnot(isTRUE(all.equal(actual$yield, raw$IRLTLT01USM156N[match(as.character(actual$date), raw[[1]])])))
shiny::testServer(server, {
  session$setInputs(rank_month = month, end_month = month, region = "All regions",
    currency = "All currencies", countries = selected, metric = "Yield (%)", history = "3")
  stopifnot(!is.null(output$custom_chart), !is.null(output$sources))
  session$setInputs(metric = "Change (basis points)")
  stopifnot(all(custom()$value[custom()$date == min(custom()$date)] == 0))
  session$setInputs(countries = character())
  stopifnot(inherits(tryCatch(custom(), error = identity), "shiny.silent.error"))
})
cat("Real-source reconciliation, rankings, currency/region filters, basis-point changes, empty selections and Shiny outputs passed.\n")

# Audit against the actual downloaded OECD payload.
audit <- parse_oecd_inventory("data/market/oecd_inventory.csv", as.Date("2026-09-26"))
stopifnot(nrow(audit$coverage) == 47L, sum(audit$coverage$is_country) == 46L,
          sum(audit$coverage$status == "Active") == 43L,
          all(audit$coverage$status[audit$coverage$ref_area %in% c("IDN", "ISL", "RUS")] == "Excluded: stale observations"),
          audit$coverage$status[audit$coverage$ref_area == "EA20"] == "Excluded: regional aggregate",
          nrow(rank_markets(d, "2026-08-01")) == 39L)
stopifnot(all(is.na(d$yield[d$ref_area == "PRT" & d$date == as.Date("2026-08-01")])));
stopifnot(all(d$currency[d$ref_area == "BGR" & d$date < as.Date("2026-01-01")] == "BGN"),
          all(d$currency[d$ref_area == "HRV" & d$date >= as.Date("2023-01-01")] == "EUR"))
raw_oecd <- read.csv("data/market/oecd_inventory.csv")
observed <- audit$data[is.finite(audit$data$yield), ]
idx <- match(paste(observed$ref_area, format(observed$date, "%Y-%m")), paste(raw_oecd$REF_AREA, raw_oecd$TIME_PERIOD))
stopifnot(!anyNA(idx), identical(observed$yield, raw_oecd$OBS_VALUE[idx]))
cat("Full OECD observation reconciliation, exclusions, missing months and historical currencies passed.\n")
