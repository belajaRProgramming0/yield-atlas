# Run from the repository root. Optional argument: a local Kaggle CSV.
source("config/policy.R")
for (file in c("data", "database", "engine")) source(paste0("R/", file, ".R"))
for (package in c("DBI", "RSQLite", "jsonlite")) {
  if (!requireNamespace(package, quietly = TRUE)) stop("Run Rscript scripts/setup.R first")
}
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 1L) stop("Usage: Rscript scripts/run_pipeline.R [credit_risk_dataset.csv]")
source_name <- if (length(args)) "external_credit_csv" else "synthetic_demo"
raw <- if (length(args)) read.csv(args[1], stringsAsFactors = FALSE) else
  make_demo_credit(policy$n_customers, policy$seed)
tables <- build_tables(raw, source_name, policy)
dir.create("outputs", showWarnings = FALSE)
# Stage SQLite in memory; save only after all computations succeed.
con <- open_database(":memory:")
tryCatch({
  load_database(con, tables)
  features <- read_query(con, "sql/features.sql")
  decisions <- route_applications(features, policy)
  DBI::dbAppendTable(con, "decisions", decisions)
  stopifnot(nrow(features) == nrow(raw), !anyDuplicated(features$application_id))
  requests <- make_mock_requests(features, decisions)
  monthly <- read_query(con, "sql/monthly_pawn.sql")
  summary <- aggregate(features$requested_amount,
                       list(route = decisions$route), function(x) sum(x[is.finite(x) & x > 0]))
  names(summary)[2] <- "requested_volume_source_units"
  summary$applications <- as.integer(table(factor(decisions$route, levels = summary$route)))
  summary$hypothetical_fee_if_all_referred_funded <- ifelse(
    summary$route == "LENDING_REFERRAL", summary$requested_volume_source_units * policy$referral_fee_rate, 0)
  write.csv(features, "outputs/customer_features.csv", row.names = FALSE, na = "")
  write.csv(decisions, "outputs/routing_decisions.csv", row.names = FALSE, na = "")
  write.csv(summary, "outputs/route_summary.csv", row.names = FALSE)
  write.csv(monthly, "outputs/monthly_pawn.csv", row.names = FALSE)
  jsonlite::write_json(requests, "outputs/mock_requests.json", auto_unbox = TRUE, pretty = TRUE, na = "null")
  jsonlite::write_json(list(source = source_name, rows = nrow(raw), seed = policy$seed,
    snapshot_date = as.character(policy$snapshot_date), policy = policy,
    input_md5 = if (length(args)) unname(tools::md5sum(args[1])) else NA_character_,
    synthetic_fields = c("pawn_history", "collateral_value", "consent", "application_date"),
    R_version = R.version.string), "outputs/manifest.json", auto_unbox = TRUE, pretty = TRUE, na = "null")
  RSQLite::sqliteCopyDatabase(con, "outputs/orchestration.sqlite")
  writeLines(capture.output(sessionInfo()), "outputs/session-info.txt")
  print(summary, row.names = FALSE)
  cat("\nSimulation complete. Outputs saved under outputs/. No requests sent.\n")
}, finally = DBI::dbDisconnect(con))
