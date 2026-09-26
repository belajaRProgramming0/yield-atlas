open_database <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  con
}

load_database <- function(con, tables, schema_path = "sql/schema.sql") {
  sql <- paste(readLines(schema_path, warn = FALSE), collapse = "\n")
  DBI::dbWithTransaction(con, {
    for (statement in strsplit(sql, ";", fixed = TRUE)[[1]]) {
      if (nzchar(trimws(statement))) DBI::dbExecute(con, statement)
    }
    for (name in names(tables)) DBI::dbAppendTable(con, name, tables[[name]])
  })
}

read_query <- function(con, path) {
  DBI::dbGetQuery(con, paste(readLines(path, warn = FALSE), collapse = "\n"))
}
