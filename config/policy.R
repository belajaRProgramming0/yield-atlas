# Illustrative policy assumptions, not calibrated credit criteria.
policy <- list(
  version = "demo-0.1.0",
  seed = 42L,
  n_customers = 10000L,
  snapshot_date = as.Date("2026-01-01"),
  max_amount_income_ratio = 0.30,
  min_employment_years = 1,
  pawn_ltv = 0.65,
  referral_fee_rate = 0.015
)
