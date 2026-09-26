oecd_download_url <- function() paste0(
  "https://sdmx.oecd.org/public/rest/v1/data/OECD.SDD.STES,DSD_STES@DF_FINMARK,4.0/",
  ".M.IRLT.PA.....?startPeriod=2015-01&dimensionAtObservation=AllDimensions")

oecd_series_url <- function(code) paste0(
  "https://data-explorer.oecd.org/vis?df[ag]=OECD.SDD.STES&df[id]=DSD_STES%40DF_FINMARK&df[vs]=4.0&dq=",
  code, ".M.IRLT.PA.....")

month_number <- function(date) as.integer(format(as.Date(date), "%Y")) * 12L +
  as.integer(format(as.Date(date), "%m"))

classify_coverage <- function(coverage, as_of = Sys.Date(), max_age_months = 3L) {
  coverage$lag_months <- month_number(as_of) - month_number(coverage$latest_month)
  coverage$status <- ifelse(!coverage$is_country, "Excluded: regional aggregate",
    ifelse(is.na(coverage$latest_month), "Excluded: no observations",
      ifelse(coverage$lag_months < 0, "Excluded: future-dated",
        ifelse(coverage$lag_months <= max_age_months, "Active", "Excluded: stale observations"))))
  coverage$evaluated_on <- as.character(as_of)
  coverage
}

parse_oecd_inventory <- function(path, as_of = Sys.Date()) {
  raw <- read.csv(path, na.strings = c("", "NA"), stringsAsFactors = FALSE)
  required <- c("REF_AREA", "FREQ", "MEASURE", "UNIT_MEASURE", "ACTIVITY", "ADJUSTMENT",
                "TRANSFORMATION", "TIME_HORIZ", "METHODOLOGY", "TIME_PERIOD", "OBS_VALUE", "UNIT_MULT")
  if (!all(required %in% names(raw)) || !nrow(raw)) stop("Invalid OECD CSV response.")
  expected <- list(FREQ = "M", MEASURE = "IRLT", UNIT_MEASURE = "PA", ACTIVITY = "_Z",
                   ADJUSTMENT = "_Z", TRANSFORMATION = "_Z", TIME_HORIZ = "_Z", METHODOLOGY = "N", UNIT_MULT = 0)
  for (name in names(expected)) {
    if (anyNA(raw[[name]]) || any(raw[[name]] != expected[[name]])) stop("Unexpected OECD dimension: ", name)
  }
  if (!is.numeric(raw$OBS_VALUE)) stop("Yield values must be numeric.")
  if (any(!is.na(raw$OBS_VALUE) & !is.finite(raw$OBS_VALUE))) stop("Non-finite yields.")
  if (anyDuplicated(raw[c("REF_AREA", "TIME_PERIOD")])) stop("Multiple series per country/month: review dimensions.")
  meta <- read.csv("config/market_metadata.csv", stringsAsFactors = FALSE)
  unknown <- setdiff(unique(raw$REF_AREA), meta$ref_area)
  if (length(unknown)) stop("New OECD areas require metadata mapping: ", paste(unknown, collapse = ", "))
  # Preserve the full queried inventory, including stale series and regional aggregates.
  meta <- meta[meta$ref_area %in% unique(raw$REF_AREA), ]
  raw$date <- as.Date(paste0(raw$TIME_PERIOD, "-01"))
  if (anyNA(raw$date)) stop("Invalid source dates.")
  coverage <- meta
  coverage$series_id <- paste0("OECD.IRLT.", meta$ref_area, ".M")
  coverage$latest_month <- vapply(meta$ref_area, function(code) {
    dates <- raw$date[raw$REF_AREA == code & is.finite(raw$OBS_VALUE)]
    if (!length(dates)) NA_character_ else as.character(max(dates))
  }, character(1))
  coverage$observations <- vapply(meta$ref_area, function(code) sum(raw$REF_AREA == code & is.finite(raw$OBS_VALUE)), integer(1))
  coverage$source_url <- oecd_series_url(meta$ref_area)
  coverage <- classify_coverage(coverage, as_of)
  row_meta <- coverage[match(raw$REF_AREA, coverage$ref_area), ]
  data <- data.frame(date = raw$date, series_id = row_meta$series_id, country = row_meta$country,
                     currency = row_meta$currency, region = row_meta$region, yield = raw$OBS_VALUE,
                     source_url = row_meta$source_url, ref_area = raw$REF_AREA)
  # Currency filters refer to currency in effect at the observation date.
  data$currency[data$ref_area == "HRV" & data$date < as.Date("2023-01-01")] <- "HRK"
  data$currency[data$ref_area == "BGR" & data$date < as.Date("2026-01-01")] <- "BGN"
  # Materialise absent months as NA (not invented observations) to stop lines bridging gaps.
  full_dates <- seq(min(raw$date), max(raw$date), by = "month")
  grid <- expand.grid(date = full_dates, ref_area = meta$ref_area, stringsAsFactors = FALSE)
  keys <- paste(data$date, data$ref_area)
  idx <- match(paste(grid$date, grid$ref_area), keys)
  complete <- data[idx, ]
  absent <- is.na(idx)
  if (any(absent)) {
    m <- coverage[match(grid$ref_area[absent], coverage$ref_area), ]
    complete$date[absent] <- grid$date[absent]
    complete$ref_area[absent] <- m$ref_area
    complete$series_id[absent] <- m$series_id
    complete$country[absent] <- m$country
    complete$currency[absent] <- m$currency
    complete$region[absent] <- m$region
    complete$source_url[absent] <- m$source_url
    complete$currency[complete$ref_area == "HRV" & complete$date < as.Date("2023-01-01")] <- "HRK"
    complete$currency[complete$ref_area == "BGR" & complete$date < as.Date("2026-01-01")] <- "BGN"
  }
  complete <- complete[order(complete$date, complete$country), ]
  rownames(complete) <- NULL
  list(data = complete, coverage = coverage)
}
