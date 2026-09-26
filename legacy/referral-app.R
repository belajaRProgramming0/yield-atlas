library(shiny)
source("R/dashboard.R")
route_labels <- c(LENDING_REFERRAL = "Lending referral", PAWN_APPRAISAL = "Pawn appraisal", MANUAL_REVIEW = "Manual review", NO_REFERRAL = "No referral")
route_colors <- c(LENDING_REFERRAL = "#198575", PAWN_APPRAISAL = "#608eaa", MANUAL_REVIEW = "#cb9549", NO_REFERRAL = "#9ca8b6")
reason_labels <- c(INCOME_AND_HISTORY_POLICY_MET = "Income and credit history meet the referral policy.", COLLATERAL_COVERAGE_PENDING_APPRAISAL = "Collateral coverage meets the threshold; a partner appraisal is required.", INVALID_OR_MISSING_INPUT = "Financial information is missing or invalid.", CONSENT_NOT_GRANTED = "Referral consent has not been granted.", NO_ROUTE_MATCH = "The application does not meet either referral pathway's rules.")
metric <- function(label, value, note, accent = FALSE) {
  div(class = paste("metric", if (accent) "metric-accent"), div(class = "eyebrow", label), div(class = "metric-value", value), div(class = "muted", note))
}
panel_heading <- function(title, subtitle) div(class = "panel-heading-custom", h3(title), p(subtitle))

ui <- fluidPage(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "dashboard.css")),
  div(class = "workspace",
    tags$aside(class = "sidebar",
      div(class = "brand", span(class = "brand-mark", "s/s"), div(strong("Senang & Sulit"), tags$small("FINANCIAL ORCHESTRATION"))),
      div(class = "sidebar-label", "ANALYST WORKSPACE"),
      div(class = "sidebar-current", span(class = "nav-dot"), "Referral intelligence"),
      div(class = "sidebar-story", h4("One entry point.", tags$br(), "Two financing pathways."), p("Understand where applications go, investigate exceptions, and explore referral economics.")),
      div(class = "sidebar-bottom", span(class = "demo-pill", "SIMULATION"), uiOutput("sidebar_meta"), p("Internal analysis workspace", tags$br(), "No requests sent to partners."))
    ),
    tags$main(class = "main-content",
      div(class = "topline", span("WORKSPACE / REFERRAL INTELLIGENCE"), uiOutput("snapshot", inline = TRUE)),
      div(class = "page-heading", div(h1("Every application. A clear next step."), p("Monitor financing pathways and understand the decisions behind them.")), span(class = "status-pill", span(class = "status-dot"), "Local simulation")),
      tabsetPanel(id = "workspace_tab", type = "tabs",
        tabPanel("Portfolio overview", value = "overview",
          uiOutput("metrics"),
          div(class = "overview-grid",
            div(class = "panel-card", panel_heading("Where applications go", "Current policy outcomes across the full portfolio"), uiOutput("route_mix"), div(class = "panel-footnote", "Referrals require a partner decision. They are not approvals.")),
            div(class = "panel-card focus-card", span(class = "eyebrow", "OPERATIONS FOCUS"), h3("What needs attention?"), uiOutput("attention"), actionButton("open_review", "Explore review queue", class = "primary-button"))
          ),
          div(class = "panel-card history-panel", panel_heading("Pawn activity over time", "Synthetic closed transactions across all customers / prior 24 months"), plotOutput("pawn", height = "220px"), div(class = "panel-footnote", "Historical activity is simulated and does not represent observed demand or cash flow."))
        ),
        tabPanel("Application review", value = "review",
          div(class = "section-intro", h2("Follow the decision trail"), p("Search an application, select a row, and inspect the inputs behind its route.")),
          div(class = "review-toolbar", selectInput("route", "Financing pathway", choices = c("All pathways" = "ALL", setNames(names(route_labels), route_labels))), downloadButton("export", "Export filtered decisions", class = "secondary-button")),
          div(class = "review-grid", div(class = "panel-card table-panel", DT::DTOutput("queue")), div(class = "panel-card detail-panel", uiOutput("case_detail")))
        ),
        tabPanel("Fee scenarios", value = "scenarios",
          div(class = "section-intro", h2("Explore referral economics"), p("Adjust two assumptions to estimate fees from the current lending referral volume.")),
          div(class = "scenario-grid",
            div(class = "panel-card", panel_heading("Scenario assumptions", "Applies to all lending referrals in this run"), sliderInput("funding", "Share of referred amount funded", min = 0, max = 100, value = 50, step = 5, post = "%"), sliderInput("fee", "Referral fee rate", min = 0, max = 5, value = 1.5, step = 0.1, post = "%"), div(class = "assumption-note", "These assumptions do not change routing decisions. Fees exclude costs and taxes.")),
            div(class = "panel-card scenario-result", span(class = "eyebrow", "HYPOTHETICAL REFERRAL FEE"), uiOutput("scenario_result"), plotOutput("sensitivity", height = "220px"))
          )
        )
      ),
      tags$footer(uiOutput("data_note"), span("SENANG & SULIT / RESEARCH WORKSPACE"))
    )
  )
)

server <- function(input, output, session) {
  loaded <- reactive({
    paths <- file.path("outputs", c("customer_features.csv", "routing_decisions.csv", "monthly_pawn.csv", "manifest.json"))
    validate(need(all(file.exists(paths)), "No portfolio loaded. Run Rscript scripts/run_pipeline.R, then restart the app."))
    f <- read.csv(paths[1], stringsAsFactors = FALSE)
    d <- read.csv(paths[2], stringsAsFactors = FALSE)
    list(data = merge(f, d, by = "application_id"), monthly = read.csv(paths[3]), manifest = jsonlite::read_json(paths[4]))
  })
  filtered <- reactive({
    x <- loaded()$data
    if (is.null(input$route) || input$route == "ALL") x else x[x$route == input$route, , drop = FALSE]
  })
  output$snapshot <- renderUI(span(paste("SNAPSHOT", loaded()$manifest$snapshot_date)))
  output$sidebar_meta <- renderUI({
    m <- loaded()$manifest
    div(class = "sidebar-meta", div(paste(format_number(m$rows), "applications")), div(paste("Policy", m$policy$version)))
  })
  output$data_note <- renderUI({
    label <- if (loaded()$manifest$source == "synthetic_demo") "Synthetic demonstration data." else "Imported credit data; pawn, collateral and consent are synthetic."
    span(label, " Amounts are in source units, not IDR.")
  })
  output$metrics <- renderUI({
    x <- loaded()$data
    referred <- sum(x$route %in% c("LENDING_REFERRAL", "PAWN_APPRAISAL"))
    div(class = "metric-grid",
      metric("APPLICATIONS", format_number(nrow(x)), "Full portfolio snapshot"),
      metric("REFERRED FOR ASSESSMENT", format_number(referred), paste(format_percent(referred / max(1, nrow(x))), "of applications"), TRUE),
      metric("REQUIRE MANUAL REVIEW", format_number(sum(x$route == "MANUAL_REVIEW")), "No automatic referral available"),
      metric("REQUESTED VOLUME", format_compact(valid_volume(x$requested_amount)), "Source units / not disbursed"))
  })
  output$route_mix <- renderUI({
    counts <- table(factor(loaded()$data$route, levels = names(route_labels)))
    n <- max(1, sum(counts))
    tagList(lapply(names(route_labels), function(route) {
      share <- as.numeric(counts[route]) / n
      div(class = "route-row", div(class = "route-row-label", span(route_labels[[route]]), span(strong(format_number(counts[route])), span(class = "route-share", format_percent(share)))), div(class = "route-track", div(class = "route-fill", style = paste0("width:", 100 * share, "%;background:", route_colors[[route]], ";"))))
    }))
  })
  output$attention <- renderUI({
    x <- loaded()$data
    n <- sum(x$route == "MANUAL_REVIEW")
    div(div(class = "attention-number", format_percent(n / max(1, nrow(x)))), p("of applications require manual review under the current policy."),
        div(class = "attention-row", span("Incomplete or invalid inputs"), strong(format_number(sum(x$reason_code == "INVALID_OR_MISSING_INPUT")))),
        div(class = "attention-row", span("No pathway match"), strong(format_number(sum(x$reason_code == "NO_ROUTE_MATCH")))),
        div(class = "attention-row", span("No consent / excluded from referral"), strong(format_number(sum(x$route == "NO_REFERRAL")))))
  })
  observeEvent(input$open_review, {
    updateSelectInput(session, "route", selected = "MANUAL_REVIEW")
    updateTabsetPanel(session, "workspace_tab", selected = "review")
  })
  output$pawn <- renderPlot({
    x <- loaded()$monthly
    validate(need(nrow(x) > 0, "No pawn history in this scenario."))
    dates <- as.Date(paste0(x$month, "-01"))
    par(mar = c(3, 3, 1, 1), family = "sans", fg = "#68778a", col.axis = "#68778a", cex.axis = 0.85)
    plot(dates, x$closed_transactions, type = "n", ylim = c(0, max(1, x$closed_transactions) * 1.15), xlab = "", ylab = "", axes = FALSE)
    abline(h = axTicks(2), col = "#edf0f3")
    polygon(c(dates[1], dates, tail(dates, 1)), c(0, x$closed_transactions, 0), col = "#e6f2ef", border = NA)
    lines(dates, x$closed_transactions, col = "#198575", lwd = 2.5)
    points(dates, x$closed_transactions, pch = 16, col = "#198575", cex = 0.5)
    positions <- unique(round(seq(1, length(dates), length.out = min(6, length(dates)))))
    axis.Date(1, at = dates[positions], format = "%b %Y", tick = FALSE)
    axis(2, las = 1, tick = FALSE)
  }, res = 110)
  output$queue <- DT::renderDT({
    x <- filtered()
    view <- data.frame(Application = x$application_id, Requested = x$requested_amount, Pathway = unname(route_labels[x$route]), Reason = gsub("_", " ", tolower(x$reason_code)))
    DT::formatRound(DT::datatable(view, rownames = FALSE, selection = "single", options = list(pageLength = 8, lengthChange = FALSE, scrollX = TRUE, language = list(search = "Search", emptyTable = "No applications in this pathway."))), "Requested", digits = 0)
  }, server = TRUE)
  output$case_detail <- renderUI({
    index <- input$queue_rows_selected
    x <- filtered()
    if (!length(index) || index[1] > nrow(x)) return(div(class = "empty-detail", span(class = "eyebrow", "DECISION EXPLAINER"), h3("Look behind the route"), p("Select an application in the table to inspect its financial inputs and policy checks.")))
    row <- x[index[1], ]
    policy <- loaded()$manifest$policy
    reason <- reason_labels[row$reason_code]
    if (is.na(reason)) reason <- row$reason_code
    datum <- function(label, value) div(class = "detail-datum", span(label), strong(value))
    tagList(span(class = "eyebrow", "APPLICATION DETAIL"), h3(row$application_id),
      div(class = "case-badge", style = paste0("color:", route_colors[[row$route]]), route_labels[[row$route]]), p(class = "case-reason", reason),
      datum("Requested amount", format_number(row$requested_amount)), datum("Annual income", format_number(row$annual_income)),
      datum("Amount / income", format_percent(row$amount_income_ratio)), datum("Employment", paste(format_number(row$employment_years), "years")),
      datum("Prior default", ifelse(row$prior_default == "Y", "Yes", ifelse(row$prior_default == "N", "No", "Unknown"))),
      datum("Collateral (synthetic)", format_number(row$collateral_value)), datum("Consent (synthetic)", ifelse(row$consent == 1, "Granted", "Not granted")),
      div(class = "policy-note", strong("Policy reference"), p(paste0("Lending: amount / income at most ", format_percent(policy$max_amount_income_ratio), "; employment at least ", policy$min_employment_years, " year; no prior default.")), p(paste0("Pawn: requested amount at most ", format_percent(policy$pawn_ltv), " of collateral value.")), tags$small(paste("Decision version", row$policy_version))))
  })
  observeEvent(input$route, { DT::selectRows(DT::dataTableProxy("queue"), NULL) }, ignoreInit = TRUE)
  output$export <- downloadHandler(filename = function() "filtered-decisions.csv", content = function(file) {
    x <- filtered()
    rows <- input$queue_rows_all
    if (!is.null(rows)) x <- x[rows, , drop = FALSE]
    write.csv(x[c("application_id", "requested_amount", "route", "reason_code", "policy_version")], file, row.names = FALSE)
  })
  scenario <- reactive({
    x <- loaded()$data
    calculate_scenario(x$requested_amount[x$route == "LENDING_REFERRAL"], input$funding, input$fee)
  })
  output$scenario_result <- renderUI({
    s <- scenario()
    tagList(div(class = "scenario-value", format_number(s$fee)), p(class = "muted", "Source units / scenario estimate, not revenue"),
      div(class = "calculation", span(paste(format_compact(s$referred), "referred")), span("x"), span(paste0(input$funding, "% funded")), span("x"), span(paste0(input$fee, "% fee"))),
      div(class = "chart-caption", "Fee sensitivity to funded share, at the selected fee rate"))
  })
  output$sensitivity <- renderPlot({
    s <- scenario()
    shares <- seq(0, 100, 10)
    fees <- s$referred * shares / 100 * input$fee / 100
    par(mar = c(3, 4.5, 1, 1), fg = "#68778a", col.axis = "#68778a", cex.axis = 0.85)
    plot(shares, fees, type = "n", axes = FALSE, xlab = "", ylab = "", ylim = c(0, max(1, fees)))
    abline(h = axTicks(2), col = "#edf0f3")
    lines(shares, fees, col = "#198575", lwd = 2.5)
    points(input$funding, s$fee, pch = 21, bg = "#198575", col = "white", cex = 1.8, lwd = 2)
    axis(1, at = seq(0, 100, 25), labels = paste0(seq(0, 100, 25), "%"), tick = FALSE)
    ticks <- axTicks(2)
    axis(2, at = ticks, labels = vapply(ticks, format_compact, character(1)), las = 1, tick = FALSE)
  }, res = 110)
}
shinyApp(ui, server)
