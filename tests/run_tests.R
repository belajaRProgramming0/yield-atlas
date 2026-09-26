source("config/policy.R")
for (file in c("data", "database", "engine")) source(paste0("R/", file, ".R"))
checks <- 0L
check <- function(condition, message) {
  if (!isTRUE(condition)) stop(message)
  checks <<- checks + 1L
}
expect_error <- function(expr) inherits(tryCatch(force(expr), error = identity), "error")

raw <- make_demo_credit(200, 42)
check(identical(raw, make_demo_credit(200, 42)), "Demo must be reproducible")
check(expect_error(validate_credit(raw[, -1])), "Missing columns must fail")
bad <- raw; bad$loan_status[1] <- 2
check(expect_error(validate_credit(bad)), "Invalid target must fail")
tables <- build_tables(raw, "synthetic_demo", policy)
flipped <- raw; flipped$loan_status <- 1 - flipped$loan_status
other <- build_tables(flipped, "synthetic_demo", policy)
check(identical(tables$pawn_history, other$pawn_history) &&
        identical(tables$applications, other$applications), "Outcomes must not affect augmentation")
small <- raw[1, ]; small$person_income <- 100000
no_history_policy <- policy
no_history_policy$seed <- 1L
small_tables <- build_tables(small, "external_credit_csv", no_history_policy)
check(nrow(small_tables$pawn_history) == 0L, "Single customer with no pawn history must be supported")
con <- open_database(":memory:")
tryCatch({
  load_database(con, tables)
  f <- read_query(con, "sql/features.sql")
  check(nrow(f) == nrow(raw) && !anyDuplicated(f$application_id), "Join must preserve application grain")
  check(!("default_label" %in% names(f)), "Outcomes must not enter routing features")
  check(all(is.na(f$pawn_repayment_rate[f$pawn_trans_count_2yr == 0])), "Absent history must remain unknown")
  check(sum(f$pawn_trans_count_2yr) == nrow(tables$pawn_history), "History counts must reconcile")
  check(expect_error(DBI::dbExecute(con, "INSERT INTO outcomes VALUES ('UNKNOWN', 1)")), "Foreign keys must be enforced")
  DBI::dbExecute(con, "INSERT INTO pawn_history VALUES ('FUTURE', 'C000001', '2027-01-01', 1, 1)")
  check(identical(f, read_query(con, "sql/features.sql")), "Future records must not leak")

  cases <- f[rep(1, 8), ]
  cases$application_id <- paste0("TEST", 1:8)
  cases$annual_income <- 10000; cases$requested_amount <- 3000
  cases$employment_years <- 2; cases$prior_default <- "N"
  cases$collateral_value <- 0; cases$consent <- 1L
  cases$requested_amount[2:3] <- 4000
  cases$collateral_value[2] <- 10000
  cases$consent[4] <- 0L
  cases$annual_income[5] <- NA_real_
  cases$prior_default[6] <- "UNKNOWN"
  cases$prior_default[7] <- "Y"
  cases$requested_amount[8] <- -1
  d <- route_applications(cases, policy)
  check(identical(d$route, c("LENDING_REFERRAL", "PAWN_APPRAISAL", "MANUAL_REVIEW",
    "NO_REFERRAL", rep("MANUAL_REVIEW", 4))), "Routing boundaries and invalid input behavior")
  requests <- make_mock_requests(cases, d)
  check(length(requests) == 2L, "Only eligible referrals become mock requests")
  check(all(vapply(requests, function(x) x$simulation && x$status == "NOT_SENT", logical(1))), "Mock requests cannot imply sending")
  check(all(vapply(requests, function(x) !any(c("annual_income", "prior_default", "default_label") %in% names(x)), logical(1))), "Requests must minimize data")
  monthly <- read_query(con, "sql/monthly_pawn.sql")
  check(is.na(monthly$previous_month_transactions[1]) &&
    identical(monthly$previous_month_transactions[-1], head(monthly$closed_transactions, -1)), "LAG must reference previous month")
}, finally = DBI::dbDisconnect(con))
cat(checks, "checks passed.\n")
