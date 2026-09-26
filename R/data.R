make_demo_credit <- function(n = 10000L, seed = 42L) {
  stopifnot(length(n) == 1L, is.finite(n), n >= 20, n == as.integer(n))
  set.seed(seed)
  income <- round(exp(rnorm(n, log(45000), 0.6)))
  amount <- round(runif(n, 1000, 30000))
  employment <- sample(0:25, n, replace = TRUE)
  prior <- sample(c("Y", "N"), n, replace = TRUE, prob = c(0.15, 0.85))
  probability <- plogis(-3 + 3 * amount / income + 0.9 * (prior == "Y") -
                         0.035 * employment)
  data.frame(person_income = income, loan_amnt = amount,
             person_emp_length = employment, cb_person_default_on_file = prior,
             loan_status = rbinom(n, 1, probability))
}

validate_credit <- function(d) {
  required <- c("person_income", "loan_amnt", "person_emp_length",
                "cb_person_default_on_file", "loan_status")
  missing <- setdiff(required, names(d))
  if (length(missing)) stop("Missing columns: ", paste(missing, collapse = ", "))
  if (!nrow(d)) stop("Input contains no customers")
  for (name in c("person_income", "loan_amnt", "person_emp_length", "loan_status")) {
    if (!is.numeric(d[[name]])) stop("Column must be numeric: ", name)
  }
  # Missing financial inputs are retained and routed for manual review.
  if (any(!is.na(d$loan_status) & !(d$loan_status %in% c(0, 1)))) {
    stop("loan_status must be 0, 1, or missing")
  }
  d
}

build_tables <- function(d, source_name, policy) {
  d <- validate_credit(d)
  set.seed(policy$seed + 1L)
  n <- nrow(d)
  ids <- sprintf("C%06d", seq_len(n))
  # Everything generated below is a scenario, never an observed customer fact.
  income <- d$person_income
  income[!is.finite(income) | income <= 0] <- 45000
  counts <- rpois(n, ifelse(income < 40000, 3, 0.5))
  owners <- rep(seq_len(n), counts)
  redeemed <- rbinom(length(owners), 1, 0.8)
  pawn <- data.frame(
    pawn_id = sprintf("P%07d", seq_along(owners)), customer_id = ids[owners],
    closed_date = as.character(policy$snapshot_date - sample(1:730, length(owners), TRUE)),
    redeemed = redeemed, synthetic = rep(1L, length(owners))
  )
  # One customer snapshot: amounts preserve source units, not converted to IDR.
  customers <- data.frame(
    customer_id = ids, annual_income = d$person_income,
    employment_years = d$person_emp_length,
    prior_default = d$cb_person_default_on_file, source = source_name
  )
  applications <- data.frame(
    application_id = sprintf("A%06d", seq_len(n)), customer_id = ids,
    application_date = as.character(policy$snapshot_date), requested_amount = d$loan_amnt,
    collateral_value = round(runif(n, 0, 50000) * rbinom(n, 1, 0.55)),
    consent = rbinom(n, 1, 0.95), scenario_synthetic = 1L
  )
  outcomes <- data.frame(application_id = applications$application_id,
                         default_label = d$loan_status)
  list(customers = customers, applications = applications, pawn_history = pawn,
       outcomes = outcomes)
}
