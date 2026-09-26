# Run after the pipeline. Uses Shiny's built-in mock session, no testthat needed.
source("legacy/referral-app.R")
stopifnot(calculate_scenario(c(100, 200, NA, Inf, -5), 50, 2)$fee == 3,
          calculate_scenario(100, 0, 2)$fee == 0,
          calculate_scenario(numeric(), 100, 5)$fee == 0)
shiny::testServer(server, {
  session$setInputs(route = "ALL", funding = 50, fee = 1.5)
  stopifnot(nrow(filtered()) == nrow(loaded()$data))
  expected <- valid_volume(loaded()$data$requested_amount[loaded()$data$route == "LENDING_REFERRAL"])
  stopifnot(isTRUE(all.equal(scenario()$fee, expected * 0.5 * 0.015)))
  stopifnot(grepl("APPLICATIONS", output$metrics$html),
            grepl("Lending referral", output$route_mix$html),
            grepl("Select an application", output$case_detail$html))
  session$setInputs(route = "MANUAL_REVIEW")
  stopifnot(all(filtered()$route == "MANUAL_REVIEW"))
  if (nrow(filtered())) {
    session$setInputs(queue_rows_selected = 1)
    stopifnot(grepl(filtered()$application_id[1], output$case_detail$html),
              grepl("Policy reference", output$case_detail$html))
  }
  session$setInputs(funding = 0, fee = 0)
  stopifnot(scenario()$fee == 0, !is.null(output$sensitivity))
  session$setInputs(funding = 100, fee = 5)
  stopifnot(isTRUE(all.equal(scenario()$fee, expected * 0.05)))
  stopifnot(!is.null(output$pawn), !is.null(output$queue))
})
cat("Dashboard calculations, filtering, detail selection and chart rendering passed.\n")
