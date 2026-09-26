# Deterministic cash-flow scenarios for nominal, fixed-rate, semiannual bonds.
# Market observations and user assumptions remain separate throughout.
shift_months <- function(date, months, end_of_month = FALSE) {
  date <- as.Date(date)
  first <- as.Date(format(date, "%Y-%m-01"))
  target <- seq(first, by = paste(months, "months"), length.out = 2L)[2L]
  last <- seq(target, by = "month", length.out = 2L)[2L] - 1
  if (end_of_month) last else target + min(as.integer(format(date, "%d")), as.integer(format(last, "%d"))) - 1L
}

coupon_dates <- function(bond, from, through = as.Date(bond$maturity)) {
  maturity <- as.Date(bond$maturity)
  eom <- bond$country == "United States" && format(maturity + 1, "%d") == "01"
  index <- as.integer(format(maturity, "%Y")) * 12L + as.integer(format(maturity, "%m")) - 1L - 6L * (0:100)
  first <- as.Date(sprintf("%04d-%02d-01", index %/% 12L, index %% 12L + 1L))
  next_first <- as.Date(sprintf("%04d-%02d-01", (index + 1L) %/% 12L, (index + 1L) %% 12L + 1L))
  dates <- if (eom) next_first - 1L else first + pmin(as.integer(format(maturity, "%d")), as.integer(next_first - first)) - 1L
  sort(dates[dates > as.Date(from) & dates <= as.Date(through)])
}

accrued_per100 <- function(bond, date) {
  date <- as.Date(date)
  if (date >= as.Date(bond$maturity)) return(0)
  dates <- coupon_dates(bond, date - 400)
  previous <- max(dates[dates <= date])
  next_date <- min(dates[dates > date])
  if (!is.finite(previous) || !is.finite(next_date)) stop("Cannot determine coupon period.")
  bond$coupon / 2 * as.numeric(date - previous) / as.numeric(next_date - previous)
}

check_au_boundary <- function(bond, date) {
  if (bond$country != "Australia" || as.Date(date) >= as.Date(bond$maturity)) return(invisible(TRUE))
  next_date <- min(coupon_dates(bond, date))
  # Avoid ex-interest/record-date holiday rules rather than guess entitlement.
  if (as.numeric(next_date - as.Date(date)) <= 14)
    stop("Australian settlement or sale within 14 days before a coupon is unsupported. Choose another date; ex-interest entitlement needs broker confirmation.")
}

fx_history <- function(fx, base, local) {
  a <- fx[fx$currency == base, c("date", "per_eur")]
  b <- fx[fx$currency == local, c("date", "per_eur")]
  x <- merge(a, b, by = "date", suffixes = c("_base", "_local"))
  x$rate <- x$per_eur_local / x$per_eur_base
  x[order(x$date), c("date", "rate")]
}

bond_scenario <- function(bond, capital, base_currency, entry_fx, start, months,
                          clean_price, exit_price, fx_change = 0, entry_fee = 0,
                          exit_fee = 0, coupon_tax = 0, gain_tax = 0, lot = 100) {
  inputs <- c(capital, entry_fx, months, clean_price, exit_price, fx_change,
              entry_fee, exit_fee, coupon_tax, gain_tax, lot)
  if (length(inputs) != 11L || any(!is.finite(inputs))) stop("Complete all scenario inputs with finite numbers.")
  if (capital <= 0 || entry_fx <= 0 || clean_price <= 0 || exit_price <= 0 || lot <= 0)
    stop("Capital, FX, prices and face-value increment must be positive.")
  if (months < 1 || months > 600 || months != floor(months)) stop("Holding period must be 1 to 600 whole months.")
  if (fx_change <= -100 || any(c(entry_fee, exit_fee, coupon_tax, gain_tax) < 0) ||
      coupon_tax > 100 || gain_tax > 100 || entry_fee >= capital)
    stop("Check fees, tax assumptions and FX change (must exceed -100%).")
  start <- as.Date(start)
  maturity <- as.Date(bond$maturity)
  if (is.na(start) || start < as.Date(bond$settlement) || start >= maturity)
    stop("Settlement must be on/after the reference settlement and before maturity.")
  same_currency <- base_currency == bond$currency
  if (same_currency) { entry_fx <- 1; fx_change <- 0 }
  requested_end <- shift_months(start, months)
  end <- min(requested_end, maturity)
  check_au_boundary(bond, start)
  check_au_boundary(bond, end)
  entry_accrued <- accrued_per100(bond, start)
  dirty <- clean_price + entry_accrued
  face <- floor((capital - entry_fee) * entry_fx / (dirty / 100) / lot) * lot
  if (face <= 0) stop("Budget is too small for this face-value increment after fees.")
  invested <- face * dirty / 100 / entry_fx
  residual <- capital - entry_fee - invested
  exit_fx <- entry_fx / (1 + fx_change / 100)
  dates <- coupon_dates(bond, start, end)
  gross_coupon <- rep(face * bond$coupon / 200 / exit_fx, length(dates))
  tax_coupon <- gross_coupon * coupon_tax / 100
  matured <- end == maturity
  exit_clean <- if (matured) 100 else exit_price
  exit_accrued <- if (matured) 0 else accrued_per100(bond, end)
  proceeds <- face * (exit_clean + exit_accrued) / 100 / exit_fx
  # Deliberately simplified effective tax assumption, not jurisdictional tax advice.
  taxable_gain <- max(0, face * exit_clean / 100 / exit_fx - face * clean_price / 100 / entry_fx)
  capital_tax <- taxable_gain * gain_tax / 100
  flows <- data.frame(date = start, event = "Purchase + entry costs", gross = -invested,
                      tax = 0, fee = entry_fee, net = -invested - entry_fee)
  if (length(dates)) flows <- rbind(flows, data.frame(date = dates, event = "Coupon",
    gross = gross_coupon, tax = tax_coupon, fee = 0, net = gross_coupon - tax_coupon))
  flows <- rbind(flows, data.frame(date = end, event = if (matured) "Principal redemption" else "Sale incl. accrued interest",
    gross = proceeds, tax = capital_tax, fee = exit_fee, net = proceeds - capital_tax - exit_fee))
  flows <- flows[order(flows$date), ]
  flows$currency <- base_currency
  flows$cumulative_net <- cumsum(flows$net)
  final <- residual + sum(gross_coupon - tax_coupon) + proceeds - capital_tax - exit_fee
  list(flows = flows, face = face, residual = residual, invested = invested, final = final,
       profit = final - capital, return_pct = 100 * (final / capital - 1),
       coupons = sum(gross_coupon), net_coupons = sum(gross_coupon - tax_coupon),
       taxes = sum(tax_coupon) + capital_tax, fees = entry_fee + exit_fee,
       start = start, end = end, requested_end = requested_end, matured = matured,
       entry_accrued = entry_accrued, exit_accrued = exit_accrued,
       entry_fx = entry_fx, exit_fx = exit_fx, exit_clean = exit_clean)
}
