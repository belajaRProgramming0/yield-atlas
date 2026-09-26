source("R/oecd_inventory.R")
market_coverage <- function(as_of = Sys.Date()) {
  classify_coverage(read.csv("data/market/coverage.csv", stringsAsFactors = FALSE), as_of)
}
market_catalog <- function() {
  x <- market_coverage()
  x[x$status == "Active", ]
}
read_market_data <- function(path = "data/market/market_yields.csv") {
  if (!file.exists(path)) stop("Run Rscript scripts/fetch_market_data.R. No simulated fallback is used.")
  x <- read.csv(path, stringsAsFactors = FALSE)
  x$date <- as.Date(x$date)
  if (anyNA(x$date) || anyDuplicated(x[c("date", "series_id")])) stop("Invalid or duplicate observations.")
  x <- x[x$series_id %in% market_catalog()$series_id, ]
  if (!any(is.finite(x$yield))) stop("No recent source data. Refresh the dataset.")
  x[order(x$date, x$country), ]
}

available_months <- function(data, minimum = 5L) {
  valid <- data[is.finite(data$yield), ]
  counts <- table(as.character(valid$date))
  as.Date(sort(names(counts)[counts >= minimum], decreasing = TRUE))
}
rank_markets <- function(data, month, region = "All regions", currency = "All currencies") {
  x <- data[data$date == as.Date(month) & is.finite(data$yield), ]
  if (region != "All regions") x <- x[x$region == region, ]
  if (currency != "All currencies") x <- x[x$currency == currency, ]
  x <- x[order(-x$yield, x$country), ]
  rownames(x) <- NULL
  x
}

comparison_data <- function(data, countries, end, years = 3L, mode = "Yield (%)") {
  end <- as.Date(end)
  start <- if (years == 0L) min(data$date) else seq(end, by = paste0("-", years, " years"), length.out = 2)[2]
  x <- data[data$country %in% countries & data$date >= start & data$date <= end, ]
  x$value <- x$yield
  if (mode == "Change (basis points)") {
    # Changes use a common starting observation, never a different start per line.
    counts <- table(as.character(x$date[is.finite(x$yield)]))
    common <- names(counts)[counts == length(unique(countries))]
    if (!length(common)) stop("No common starting observation for this selection.")
    baseline_date <- min(as.Date(common))
    x <- x[x$date >= baseline_date, ]
    baseline <- x[x$date == baseline_date, c("country", "yield")]
    x$value <- (x$yield - baseline$yield[match(x$country, baseline$country)]) * 100
  }
  x
}
