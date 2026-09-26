# Exercise external import in an isolated temporary workspace.
source("R/data.R")
workspace <- tempfile("orchestration-smoke-")
dir.create(workspace)
for (directory in c("R", "scripts", "config", "sql")) {
  dir.create(file.path(workspace, directory))
  file.copy(list.files(directory, full.names = TRUE), file.path(workspace, directory))
}
fixture <- make_demo_credit(200, 7)
fixture$person_income[1] <- NA_real_
write.csv(fixture, file.path(workspace, "input.csv"), row.names = FALSE, na = "")
original_directory <- getwd()
setwd(workspace)
tryCatch({
  runner <- file.path(R.home("bin"), "Rscript")
  logs <- system2(runner, c("scripts/run_pipeline.R", "input.csv"), stdout = TRUE, stderr = TRUE)
  if (!is.null(attr(logs, "status"))) stop(paste(logs, collapse = "\n"))
  manifest <- jsonlite::read_json("outputs/manifest.json")
  decisions <- read.csv("outputs/routing_decisions.csv")
  stopifnot(manifest$source == "external_credit_csv", manifest$rows == 200,
            manifest$input_md5 == unname(tools::md5sum("input.csv")),
            nrow(decisions) == 200,
            decisions$route[1] %in% c("MANUAL_REVIEW", "NO_REFERRAL"))
  requests <- jsonlite::read_json("outputs/mock_requests.json")
  stopifnot(length(requests) == sum(decisions$route %in% c("LENDING_REFERRAL", "PAWN_APPRAISAL")))
  con <- DBI::dbConnect(RSQLite::SQLite(), "outputs/orchestration.sqlite")
  tryCatch(stopifnot(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM decisions")$n == 200),
           finally = DBI::dbDisconnect(con))
  cat("External CSV pipeline smoke test passed.\n")
}, finally = setwd(original_directory))
