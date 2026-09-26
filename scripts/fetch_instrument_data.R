source("R/instrument_data.R")
cached <- "--from-cache" %in% commandArgs(trailingOnly = TRUE)
destination <- "data/instruments"
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
stage <- tempfile("yield-atlas-")
dir.create(stage)
urls <- instrument_urls()
fetch <- function(url, filename) {
  if (grepl("^https://www.aofm.gov.au/", url) && .Platform$OS.type == "windows" && nzchar(Sys.which("curl.exe"))) {
    # Windows' system curl succeeds where this host resets R/libcurl connections.
    status <- system2(Sys.which("curl.exe"), c("-f", "-L", "--silent", "--show-error",
      "--connect-timeout", "15", "--max-time", "90", shQuote(url), "-o", shQuote(file.path(stage, filename))))
    if (status != 0) stop("Official AOFM download failed; existing cache retained.")
    return(invisible(NULL))
  }
  request <- httr2::req_timeout(httr2::request(url), 120)
  request <- httr2::req_options(request, http_version = 2L) # HTTP/1.1: AOFM rejects some HTTP/2 clients.
  httr2::req_perform(request, path = file.path(stage, filename))
  invisible(NULL)
}
files <- c("us_raw.json", "au_raw.xlsx", "fx_raw.xml", "inflation_raw.json")
if (cached) {
  stopifnot(all(file.copy(file.path(destination, files), stage, copy.date = TRUE)))
} else {
  # AOFM updates this workbook in place (the URL's folder date is not its observation date).
  # If its URL changes, locate the issuance workbook on au_hub; a failed request preserves the cache.
  fetch(urls$us, files[1]); fetch(urls$au, files[2])
  fetch(urls$fx, files[3]); fetch(urls$inflation, files[4])
}
bundle <- parse_instruments(stage)
bundle$provenance <- data.frame(file = files,
  url = unlist(urls[c("us", "au", "fx", "inflation")], use.names = FALSE),
  retrieved_at_utc = format(file.info(file.path(stage, files))$mtime, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  md5 = unname(tools::md5sum(file.path(stage, files))))
saveRDS(bundle, file.path(stage, "bundle.rds"))
jsonlite::write_json(bundle$provenance, file.path(stage, "provenance.json"), pretty = TRUE)
# Nothing in the live cache is replaced until all providers and parsers succeed.
stopifnot(all(file.copy(file.path(stage, c(files, "provenance.json")), destination, overwrite = TRUE, copy.date = TRUE)))
stopifnot(file.copy(file.path(stage, "bundle.rds"), destination, overwrite = TRUE))
cat("Saved", nrow(bundle$bonds), "real securities from", length(unique(bundle$bonds$country)),
    "markets; FX through", as.character(max(bundle$fx$date)), "and dated annual inflation.\n")
