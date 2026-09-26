source("R/bond_model.R")

instrument_urls <- function() list(
  us = "https://www.treasurydirect.gov/TA_WS/securities/search?format=json&type=Note",
  au_hub = "https://www.aofm.gov.au/data-hub",
  au = "https://www.aofm.gov.au/sites/default/files/2025-06-20/treasury%20bonds%20-%20issuance.xlsx",
  fx = "https://www.ecb.europa.eu/stats/eurofxref/eurofxref-hist-90d.xml",
  inflation = paste0("https://api.worldbank.org/v2/country/all/indicator/FP.CPI.TOTL.ZG?format=json&date=2020:",
                     format(Sys.Date(), "%Y"), "&per_page=20000"))

parse_instruments <- function(directory, as_of = Sys.Date()) {
  us <- jsonlite::fromJSON(file.path(directory, "us_raw.json"))
  required <- c("cusip", "auctionDate", "issueDate", "maturityDate", "interestRate", "pricePer100",
                "highYield", "tips", "floatingRate", "firstInterestPeriod")
  if (!all(required %in% names(us))) stop("Treasury response schema changed.")
  date_us <- function(x) as.Date(substr(x, 1, 10))
  numeric_us <- function(x) suppressWarnings(as.numeric(x))
  keep <- us$tips == "No" & us$floatingRate == "No" & us$firstInterestPeriod == "Normal" &
    date_us(us$auctionDate) >= as.Date("2024-01-01") & date_us(us$issueDate) <= as_of &
    date_us(us$maturityDate) > as_of & is.finite(numeric_us(us$pricePer100))
  us <- us[which(keep), ]
  american <- data.frame(id = us$cusip, country = "United States", currency = "USD",
    coupon = numeric_us(us$interestRate), maturity = date_us(us$maturityDate),
    auction_date = date_us(us$auctionDate), settlement = date_us(us$issueDate),
    clean_price = numeric_us(us$pricePer100), auction_yield = numeric_us(us$highYield),
    price_basis = "Published auction clean price",
    source_url = paste0("https://www.treasurydirect.gov/auctions/auction-query/?cusip=", us$cusip))

  header <- readxl::read_excel(file.path(directory, "au_raw.xlsx"), skip = 1, n_max = 1, col_names = FALSE, .name_repair = "minimal")
  if (!identical(as.character(unlist(header[c(1, 3, 4, 5, 7, 10, 19, 20)])),
                 c("Date Held", "Maturity", "Coupon", "ISIN", "Amount Allotted", "Weighted Average Issue Yield", "Settlement Proceeds", "Date Settled")))
    stop("AOFM workbook schema changed.")
  au <- as.data.frame(readxl::read_excel(file.path(directory, "au_raw.xlsx"), skip = 3,
                       col_names = FALSE, col_types = "text", .name_repair = "minimal"))
  num <- function(i) suppressWarnings(as.numeric(au[[i]]))
  excel_date <- function(i) as.Date(num(i), origin = "1899-12-30")
  australian <- data.frame(id = au[[5]], country = "Australia", currency = "AUD",
    coupon = num(4), maturity = excel_date(3), auction_date = excel_date(1), settlement = excel_date(20),
    clean_price = num(19) / num(7) * 100, auction_yield = num(10),
    price_basis = "Derived from settlement proceeds / allotted face, less accrued interest",
    source_url = "https://www.aofm.gov.au/data-hub")
  keep <- !is.na(australian$id) & australian$auction_date >= as.Date("2024-01-01") &
    australian$settlement <= as_of & australian$maturity > as_of & is.finite(australian$clean_price)
  australian <- australian[which(keep), ]
  # Only normalize prices outside the ex-interest boundary supported by the model.
  supported <- vapply(seq_len(nrow(australian)), function(i)
    !inherits(try(check_au_boundary(australian[i, ], australian$settlement[i]), silent = TRUE), "try-error"), logical(1))
  australian <- australian[supported, ]
  australian$clean_price <- australian$clean_price - vapply(seq_len(nrow(australian)),
    function(i) accrued_per100(australian[i, ], australian$settlement[i]), numeric(1))
  history <- rbind(american, australian)
  if (!nrow(american) || !nrow(australian) || anyNA(history) ||
      any(!is.finite(history$clean_price) | history$clean_price <= 0 | history$coupon <= 0))
    stop("Incomplete or invalid bond observations.")
  history <- history[order(history$auction_date, decreasing = TRUE), ]
  bonds <- history[!duplicated(history$id), ]
  bonds$label <- paste(bonds$country, bonds$id, paste0(bonds$coupon, "%"), bonds$maturity)

  doc <- xml2::read_xml(file.path(directory, "fx_raw.xml"))
  days <- xml2::xml_find_all(doc, "//*[@time]")
  fx <- do.call(rbind, lapply(days, function(day) {
    nodes <- xml2::xml_children(day)
    data.frame(date = as.Date(xml2::xml_attr(day, "time")),
      currency = c("EUR", xml2::xml_attr(nodes, "currency")),
      per_eur = c(1, as.numeric(xml2::xml_attr(nodes, "rate"))))
  }))
  if (!nrow(fx) || anyNA(fx) || any(fx$per_eur <= 0) || anyDuplicated(fx[c("date", "currency")])) stop("Invalid ECB data.")
  fx <- fx[fx$date <= as_of, ]
  wb <- jsonlite::fromJSON(file.path(directory, "inflation_raw.json"))
  if (length(wb) != 2 || wb[[1]]$pages != 1) stop("Incomplete World Bank response; pagination required.")
  rows <- wb[[2]]
  inflation <- data.frame(ref_area = rows$countryiso3code, year = as.integer(rows$date), inflation = rows$value)
  inflation <- inflation[inflation$ref_area != "" & inflation$year < as.integer(format(as_of, "%Y")), ]
  list(bonds = bonds, history = history, fx = fx, inflation = inflation,
       wb_updated = wb[[1]]$lastupdated, evaluated_on = as.character(as_of))
}

read_instruments <- function() {
  path <- "data/instruments/bundle.rds"
  if (!file.exists(path)) stop("Bond data unavailable. Run Rscript scripts/fetch_instrument_data.R.")
  readRDS(path)
}
