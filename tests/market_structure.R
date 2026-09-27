# Tests use the downloaded OECD and World Bank observations. No generated market data is used.
source("R/market_data.R")
source("R/instrument_data.R")
source("R/market_structure.R")

benchmarks <- read_market_data()
instruments <- read_instruments()
month <- available_months(benchmarks)[1]

structure <- analyse_market_structure(
  benchmarks, instruments$inflation, month,
  lookback_months = 36L, method = "K-means", requested_k = "Auto"
)
stopifnot(
  nrow(structure$features) >= 30L,
  nrow(structure$scores) == nrow(structure$features),
  isTRUE(all.equal(sum(structure$variance), 1)),
  structure$clustering$k == structure$clustering$diagnostics$k[which.max(structure$clustering$diagnostics$silhouette)],
  all(is.finite(as.matrix(structure$loadings[, c("PC1", "PC2")]))),
  sum(structure$profiles$markets) == nrow(structure$features)
)

ward <- analyse_market_structure(
  benchmarks, instruments$inflation, month,
  lookback_months = 36L, method = "Ward hierarchical", requested_k = "3"
)
stopifnot(ward$clustering$k == 3L, length(unique(ward$scores$cluster)) == 3L)

panel <- fit_market_panel_gam(benchmarks, instruments$inflation, month, years = 5L)
stopifnot(
  panel$metrics$observations >= 1000L,
  panel$metrics$markets >= 30L,
  all(panel$data$date <= as.Date(month)),
  all(is.finite(panel$data$fitted)),
  is.finite(panel$metrics$deviance_explained),
  is.finite(panel$metrics$residual_lag1)
)

cat("PCA, automatic and fixed clustering, Ward clustering, and panel GAM checks passed.\n")
