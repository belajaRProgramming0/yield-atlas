# Full OECD universe, actual monthly observations only.
source("R/oecd_inventory.R")
dir.create("data/market", recursive = TRUE, showWarnings = FALSE)
staged <- tempfile(fileext = ".csv")
request <- httr2::request(oecd_download_url())
request <- httr2::req_headers(request, Accept = "text/csv")
request <- httr2::req_timeout(request, 120)
httr2::req_perform(request, path = staged)
bundle <- parse_oecd_inventory(staged)
stopifnot(sum(bundle$coverage$status == "Active") >= 5)
manifest <- list(source = "OECD Financial market data (direct SDMX API)",
  retrieved_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  source_url = oecd_download_url(), frequency = "Monthly", unit = "Percent per annum",
  latest_observation = as.character(max(bundle$data$date[is.finite(bundle$data$yield)])),
  countries_in_inventory = sum(bundle$coverage$is_country),
  active_countries = sum(bundle$coverage$status == "Active"), freshness_months = 3,
  md5 = unname(tools::md5sum(staged)))
# Download and dimension validation finish before replacing the cache.
write.csv(bundle$data, "data/market/market_yields.csv", row.names = FALSE, na = "")
write.csv(bundle$coverage, "data/market/coverage.csv", row.names = FALSE, na = "")
stopifnot(file.copy(staged, "data/market/oecd_inventory.csv", overwrite = TRUE))
jsonlite::write_json(manifest, "data/market/manifest.json", pretty = TRUE, auto_unbox = TRUE)
cat("Saved", manifest$active_countries, "active countries out of", manifest$countries_in_inventory, "countries inventoried.\n")
