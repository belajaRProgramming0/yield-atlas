CREATE TABLE customers (
  customer_id TEXT PRIMARY KEY,
  annual_income REAL,
  employment_years REAL,
  prior_default TEXT,
  source TEXT NOT NULL
);
CREATE TABLE applications (
  application_id TEXT PRIMARY KEY,
  customer_id TEXT NOT NULL REFERENCES customers(customer_id),
  application_date TEXT NOT NULL,
  requested_amount REAL,
  collateral_value REAL,
  consent INTEGER NOT NULL CHECK (consent IN (0,1)),
  scenario_synthetic INTEGER NOT NULL CHECK (scenario_synthetic = 1)
);
CREATE TABLE pawn_history (
  pawn_id TEXT PRIMARY KEY,
  customer_id TEXT NOT NULL REFERENCES customers(customer_id),
  closed_date TEXT NOT NULL,
  redeemed INTEGER NOT NULL CHECK (redeemed IN (0,1)),
  synthetic INTEGER NOT NULL CHECK (synthetic = 1)
);
CREATE TABLE outcomes (
  application_id TEXT PRIMARY KEY REFERENCES applications(application_id),
  default_label INTEGER CHECK (default_label IN (0,1))
);
CREATE TABLE decisions (
  application_id TEXT PRIMARY KEY REFERENCES applications(application_id),
  route TEXT NOT NULL CHECK (route IN ('LENDING_REFERRAL','PAWN_APPRAISAL','MANUAL_REVIEW','NO_REFERRAL')),
  reason_code TEXT NOT NULL,
  policy_version TEXT NOT NULL,
  amount_income_ratio REAL
);
CREATE INDEX pawn_customer_date ON pawn_history(customer_id, closed_date);
