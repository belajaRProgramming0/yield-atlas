route_applications <- function(features, policy) {
  needed <- c("application_id", "customer_id", "annual_income", "employment_years",
              "prior_default", "requested_amount", "collateral_value", "consent")
  if (!all(needed %in% names(features))) stop("Incomplete routing inputs")
  f <- features
  ratio <- f$requested_amount / f$annual_income
  invalid <- !is.finite(f$annual_income) | f$annual_income <= 0 |
    !is.finite(f$requested_amount) | f$requested_amount <= 0 |
    !is.finite(f$employment_years) | f$employment_years < 0 |
    !is.finite(f$collateral_value) | f$collateral_value < 0 |
    !(f$prior_default %in% c("Y", "N"))
  consent <- !is.na(f$consent) & f$consent == 1
  lending <- !invalid & consent & f$prior_default == "N" &
    ratio <= policy$max_amount_income_ratio &
    f$employment_years >= policy$min_employment_years
  lending[is.na(lending)] <- FALSE
  pawn <- !invalid & consent & !lending &
    f$requested_amount <= f$collateral_value * policy$pawn_ltv
  pawn[is.na(pawn)] <- FALSE
  route <- rep("MANUAL_REVIEW", nrow(f))
  reason <- rep("NO_ROUTE_MATCH", nrow(f))
  route[lending] <- "LENDING_REFERRAL"
  reason[lending] <- "INCOME_AND_HISTORY_POLICY_MET"
  route[pawn] <- "PAWN_APPRAISAL"
  reason[pawn] <- "COLLATERAL_COVERAGE_PENDING_APPRAISAL"
  reason[invalid] <- "INVALID_OR_MISSING_INPUT"
  route[!consent] <- "NO_REFERRAL"
  reason[!consent] <- "CONSENT_NOT_GRANTED"
  data.frame(application_id = f$application_id, route = route, reason_code = reason,
             policy_version = policy$version, amount_income_ratio = ratio)
}

make_mock_requests <- function(features, decisions) {
  indices <- match(decisions$application_id, features$application_id)
  if (anyNA(indices)) stop("Unmatched decision")
  eligible <- which(decisions$route %in% c("LENDING_REFERRAL", "PAWN_APPRAISAL"))
  lapply(eligible, function(i) list(
    schema_version = "0.1.0", simulation = TRUE,
    request_id = paste0(decisions$application_id[i], "-", decisions$policy_version[i]),
    customer_ref = features$customer_id[indices[i]],
    destination = if (decisions$route[i] == "LENDING_REFERRAL") "MOCK_LENDER" else "MOCK_PAWN_PARTNER",
    amount = features$requested_amount[indices[i]], amount_unit = "SOURCE_UNITS",
    reason_code = decisions$reason_code[i], status = "NOT_SENT"
  ))
}
