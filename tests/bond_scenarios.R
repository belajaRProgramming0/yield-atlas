# Real downloaded instruments; hypothetical prices/fees below are explicit test scenarios.
library(shiny)
source("R/investment_ui.R")
d <- read_instruments()
us <- d$bonds[d$bonds$country == "United States", ][1, ]
au <- d$bonds[d$bonds$country == "Australia", ][1, ]
stopifnot(all(c("United States", "Australia") %in% d$bonds$country), !anyDuplicated(d$bonds$id))
raw <- jsonlite::fromJSON("data/instruments/us_raw.json")
observed <- d$history[d$history$country == "United States", ]
idx <- match(paste(observed$id, observed$auction_date), paste(raw$cusip, substr(raw$auctionDate, 1, 10)))
stopifnot(!anyNA(idx), identical(observed$clean_price, as.numeric(raw$pricePer100[idx])),
          identical(observed$coupon, as.numeric(raw$interestRate[idx])))
published_accrued <- suppressWarnings(as.numeric(raw$accruedInterestPer1000[idx])) / 10
available <- which(is.finite(published_accrued))
calculated_accrued <- vapply(available, function(i) accrued_per100(observed[i, ], observed$settlement[i]), numeric(1))
stopifnot(length(available) > 0, all(abs(calculated_accrued - published_accrued[available]) < 1e-6))

ab <- fx_history(d$fx, "AUD", "USD"); ba <- fx_history(d$fx, "USD", "AUD")
stopifnot(all(abs(ab$rate * ba$rate - 1) < 1e-12), nrow(fx_history(d$fx, "AUD", "COP")) == 0)
args <- list(bond = us, capital = 50000, base_currency = "AUD", entry_fx = tail(ab$rate, 1),
             start = as.Date("2026-09-26"), months = 12, clean_price = us$clean_price, exit_price = us$clean_price)
run <- function(...) do.call(bond_scenario, utils::modifyList(args, list(...)))
s <- run()
stopifnot(s$face > 0, s$residual >= 0, s$residual < 100 * (us$clean_price + s$entry_accrued) / 100 / s$entry_fx,
          abs(s$profit - sum(s$flows$net)) < 1e-8,
          abs(s$final - (50000 + sum(s$flows$net))) < 1e-8)
coupons <- s$flows[s$flows$event == "Coupon", ]
stopifnot(nrow(coupons) == 2, all(abs(coupons$gross - s$face * us$coupon / 200 / s$exit_fx) < 1e-9))
stopifnot(run(fx_change = -20)$final < s$final, run(fx_change = 20)$final > s$final,
          run(exit_price = us$clean_price * 0.9)$final < s$final,
          run(entry_fee = 100, exit_fee = 100, coupon_tax = 20)$final < s$final,
          run(coupon_tax = 100)$net_coupons == 0)
same <- run(base_currency = "USD", entry_fx = 0.2, fx_change = 50)
same2 <- run(base_currency = "USD", entry_fx = 1, fx_change = 0)
stopifnot(identical(same$final, same2$final), same$exit_fx == 1)
maturity <- run(months = 240, exit_price = 1)
maturity2 <- run(months = 240, exit_price = 200)
stopifnot(maturity$matured, maturity$end == us$maturity, maturity$exit_clean == 100,
          identical(maturity$final, maturity2$final), maturity$exit_accrued == 0)
stopifnot(inherits(try(run(capital = 1), silent = TRUE), "try-error"),
          inherits(try(run(entry_fx = 0), silent = TRUE), "try-error"),
          inherits(try(run(coupon_tax = 101), silent = TRUE), "try-error"),
          inherits(try(run(start = us$maturity), silent = TRUE), "try-error"))
next_au <- min(coupon_dates(au, as.Date("2026-09-26")))
stopifnot(inherits(try(check_au_boundary(au, next_au - 7), silent = TRUE), "try-error"))

# Month-end schedule must match Treasury's actual first contractual coupon.
eom <- d$bonds[d$bonds$country == "United States" & format(d$bonds$maturity + 1, "%d") == "01", ][1, ]
r <- raw[raw$cusip == eom$id & substr(raw$issueDate, 1, 10) == as.character(eom$settlement), ][1, ]
stopifnot(min(coupon_dates(eom, eom$settlement)) == as.Date(substr(r$firstInterestPaymentDate, 1, 10)))

testServer(bond_lab_server, args = list(state = d), {
  session$setInputs(country = "United States", bond = us$id, base = "AUD", capital = 50000,
    start = as.Date("2026-09-26"), months = 12, price_mode = "reference", fx_mode = "reference",
    exit_change = 0, fx_change = 0, entry_fee = 0, exit_fee = 0, coupon_tax = 0, gain_tax = 0, lot = 100)
  stopifnot(abs(scenario()$final - s$final) < 1e-8,
    !is.null(output$details), !is.null(output$results), !is.null(output$cash_chart), !is.null(output$risk_chart))
  exported <- read.csv(output$download)
  stopifnot(nrow(exported) == nrow(scenario()$flows),
            all(exported$security_id == us$id),
            all(abs(exported$scenario_final_total - scenario()$final) < 1e-7),
            all(exported$assumed_capital == 50000))
  session$setInputs(fx_change = -20)
  stopifnot(scenario()$return_pct < s$return_pct)
  session$setInputs(base = "USD")
  stopifnot(scenario()$entry_fx == 1, scenario()$exit_fx == 1)
  session$setInputs(price_mode = "manual", price = 95)
  stopifnot(arguments()$clean_price == 95)
  session$setInputs(country = "Australia", bond = au$id, base = "AUD", price_mode = "reference")
  stopifnot(scenario()$entry_fx == 1, !is.null(output$cash_chart))
})
cat("Passed: real security reconciliation, FX direction, coupons, accrued costs, fees/tax, maturity, month-end schedules, AU boundaries and Shiny scenarios.\n")
