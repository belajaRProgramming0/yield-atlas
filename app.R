library(shiny)
source("R/market_data.R")
source("R/investment_ui.R")
instrument_state <- tryCatch(read_instruments(), error = function(e) list(error = conditionMessage(e)))
catalog <- market_catalog()
palette <- setNames(grDevices::hcl.colors(nrow(catalog), "Dark 3"), catalog$country)
market_state <- tryCatch(list(data = read_market_data(), manifest = jsonlite::read_json("data/market/manifest.json")), error = function(e) list(error = conditionMessage(e)))

market_chart <- function(x, mode = "Yield (%)") {
  chart <- plotly::plot_ly()
  for (country in unique(x$country)) {
    part <- x[x$country == country, ]
    part <- part[order(part$date), ]
    unit <- if (mode == "Yield (%)") "%" else " bp"
    chart <- plotly::add_trace(chart, data = part, x = ~date, y = ~value,
      type = "scatter", mode = "lines", name = country, connectgaps = FALSE,
      line = list(color = palette[[country]], width = 2.5),
      hovertemplate = paste0("%{x|%b %Y}<br>%{y:.2f}", unit, "<extra>", country, "</extra>"))
  }
  chart <- plotly::layout(chart, paper_bgcolor = "transparent", plot_bgcolor = "white",
    font = list(family = "Segoe UI, Arial", color = "#637487", size = 12),
    margin = list(l = 60, r = 25, b = 70, t = 45), hovermode = "x unified",
    legend = list(orientation = "h", x = 0, y = -0.2),
    xaxis = list(title = "", gridcolor = "#f1f4f6", showline = FALSE,
      rangeselector = list(buttons = list(
        list(count = 6, label = "6M", step = "month", stepmode = "backward"),
        list(count = 1, label = "1Y", step = "year", stepmode = "backward"),
        list(count = 1, label = "YTD", step = "year", stepmode = "todate"),
        list(count = 3, label = "3Y", step = "year", stepmode = "backward"),
        list(step = "all", label = "ALL")), bgcolor = "#f0f4f4", activecolor = "#d5ebe4")),
    yaxis = list(title = mode, gridcolor = "#eaf0f3", zerolinecolor = "#cad5dd", ticksuffix = if (mode == "Yield (%)") "%" else ""))
  plotly::config(chart, displaylogo = FALSE, responsive = TRUE, modeBarButtonsToRemove = c("select2d", "lasso2d"))
}

ui <- fluidPage(
  tags$head(tags$link(rel = "stylesheet", href = "dashboard.css"), tags$link(rel = "stylesheet", href = "market.css")),
  div(class = "workspace",
    tags$aside(class = "sidebar",
      div(class = "brand", span(class = "brand-mark", "ya"), div(strong("Yield Atlas"), tags$small("GLOBAL FIXED INCOME"))),
      div(class = "sidebar-label", "MARKET EXPLORER"),
      div(class = "sidebar-current", span(class = "nav-dot"), "Government bonds"),
      div(class = "sidebar-story", h4("A wider view.", tags$br(), "A clearer comparison."),
          p("Start with your budget and timeframe. Explore a bond, then see how its payments fit your plans.")),
      div(class = "sidebar-bottom", span(class = "demo-pill", "OBSERVED DATA"),
          p("Official source data", tags$br(), "Dated references", tags$br(), "Transparent assumptions"))
    ),
    tags$main(class = "main-content",
      div(class = "topline", span("FIXED INCOME / GOVERNMENT BONDS"), uiOutput("retrieved", inline = TRUE)),
      div(class = "page-heading", div(h1("Your money. A clearer picture."), p("Explore government bonds, understand their payments, and test your assumptions.")), span(class = "status-pill", span(class = "status-dot"), "Public-source data")),
      tabsetPanel(id = "market_tab", type = "tabs", selected = "bond_lab",
        bond_lab_ui("bonds"),
        country_context_ui("context", catalog, extra_content =           div(id = "custom_section", class = "panel-card market-panel",
            div(class = "section-heading", div(span(class = "eyebrow", "COMPARE COUNTRIES"), h2("Build your own comparison"), p("Select markets, a metric and a history window. Click a legend label to hide a line.")),
                downloadButton("download", "Download chart data", class = "secondary-button")),
            selectizeInput("countries", "Markets", choices = catalog$country,
                           selected = c("United States", "United Kingdom", "Australia"), multiple = TRUE),
            div(class = "market-filters",
              selectInput("metric", "Metric", choices = c("Yield (%)", "Change (basis points)")),
              selectInput("history", "History window", choices = c("1 year" = 1, "3 years" = 3, "5 years" = 5, "All available" = 0), selected = 3),
              selectInput("end_month", "Through month", choices = NULL)),
            plotly::plotlyOutput("custom_chart", height = "390px"), uiOutput("custom_note"))),
        tabPanel("Data & methodology", value = "sources",
          div(class = "section-intro", h2("Trace every series to its source"), p("No simulated observations. Missing data stays missing.")),
          div(class = "panel-card", uiOutput("source_description"), DT::DTOutput("sources")),
          div(class = "panel-card market-panel", h3("How the charts work"),
            tags$ul(
              tags$li("Universe: all countries in the OECD feed with an observation within three calendar months of today. This freshness rule does not guarantee future updates. Excluded areas appear in the coverage audit."),
              tags$li("Measure: OECD long-term government bond yields, 10-year main/benchmark series; monthly, percent per annum, not seasonally adjusted."),
              tags$li("Basis-point changes start on the first month with an observation for every selected country in the chosen window. 100 basis points = 1 percentage point."),
              tags$li("The cached data is refreshed with Rscript scripts/fetch_market_data.R. Restart the app after a refresh. No unattended refresh service is installed."),
              tags$li("Monthly data does not support 1D or 5D comparisons. Chart buttons operate on the loaded history window; ALL means all loaded observations."))))
      ),
      tags$footer(span("OECD / TreasuryDirect / AOFM / ECB / World Bank"), span("YIELD ATLAS / DATA EXPLORER"))
    )
  )
)

server <- function(input, output, session) {
  country_context_server("context", instrument_state, market_state, catalog, function(country) {
    updateSelectInput(session, "bonds-country", selected = country)
    updateTabsetPanel(session, "market_tab", selected = "bond_lab")
  })
  bond_lab_server("bonds", instrument_state)
  data <- reactive({
    validate(need(is.null(market_state$error), market_state$error))
    market_state$data
  })
  observeEvent(data(), {
    months <- available_months(data())
    choices <- setNames(as.character(months), format(months, "%B %Y"))
    updateSelectInput(session, "end_month", choices = choices, selected = as.character(months[1]))
  })
  custom <- reactive({
    req(input$end_month, input$history, input$metric)
    validate(need(length(input$countries) > 0, "Select at least one market to start comparing."))
    result <- tryCatch(comparison_data(data(), input$countries, input$end_month, as.integer(input$history), input$metric), error = function(e) e)
    validate(need(!inherits(result, "error"), if (inherits(result, "error")) conditionMessage(result) else ""))
    validate(need(any(is.finite(result$value)), "No observations for this selection."))
    result
  })
  output$retrieved <- renderUI({
    data()
    span(paste("BENCHMARK SNAPSHOT", substr(market_state$manifest$retrieved_at_utc, 1, 10), "UTC"))
  })
  output$custom_chart <- plotly::renderPlotly(market_chart(custom(), input$metric))
  output$custom_note <- renderUI({
    x <- custom()
    text <- if (input$metric == "Change (basis points)") paste("Changes from common baseline", format(min(x$date), "%B %Y"), "/") else "Observed yields /"
    div(class = "panel-footnote", paste(text, sum(is.finite(x$value)), "observations. Gaps are not interpolated. Chart range buttons do not change the downloaded dataset."))
  })
  output$download <- downloadHandler(filename = function() paste0("yield-atlas-", input$end_month, ".csv"), content = function(file) {
    x <- custom()
    x$display_metric <- input$metric
    x$retrieved_at_utc <- market_state$manifest$retrieved_at_utc
    write.csv(x, file, row.names = FALSE, na = "")
  })
  output$source_description <- renderUI({
    x <- data()
    tagList(h3("OECD long-term interest rates: coverage audit"),
      p(paste(format(sum(is.finite(x$yield)), big.mark = ","), "observed values across", length(unique(x$country)), "markets.")),
      p(paste("Downloaded:", market_state$manifest$retrieved_at_utc)),
      p("Source: OECD, Main Economic Indicators / Financial market data. Retrieved directly from the OECD SDMX API. Currency follows observation date: Croatia adopted EUR in January 2023; Bulgaria in January 2026."),
      p(tags$a(href = "https://www.oecd.org/en/data/indicators/long-term-interest-rates.html", target = "_blank", rel = "noopener", "OECD definition"), " / ",
        tags$a(href = "https://fred.stlouisfed.org/series/IRLTLT01USM156N", target = "_blank", rel = "noopener", "FRED series notes and attribution")))
  })
  output$sources <- DT::renderDT({
    x <- data()
    rows <- market_coverage()
    rows <- rows[, c("country", "currency", "region", "latest_month", "status", "observations", "evaluated_on", "source_url")]
    DT::datatable(rows, rownames = FALSE, options = list(pageLength = 10, dom = "ftp", scrollX = TRUE))
  })
}
ui <- tagList(ui, tags$script(HTML("Shiny.addCustomMessageHandler('scroll-custom', function() { document.getElementById('custom_section').scrollIntoView({behavior:'smooth'}); });")))
shinyApp(ui, server)
