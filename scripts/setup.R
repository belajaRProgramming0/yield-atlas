packages <- c("DBI", "RSQLite", "jsonlite", "shiny", "DT", "plotly", "httr2", "xml2", "readxl")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
cat("Dependencies ready. Run Rscript scripts/fetch_market_data.R, then launch Shiny.\n")
