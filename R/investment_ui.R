source("R/instrument_data.R")

investment_plot <- function(chart, y_title = "", height = NULL) {
  chart <- plotly::layout(chart, paper_bgcolor = "transparent", plot_bgcolor = "white",
    font = list(family = "Segoe UI, Arial", color = "#637487", size = 12),
    margin = list(l = 65, r = 25, b = 60, t = 25),
    xaxis = list(title = "", gridcolor = "#f1f4f6"),
    yaxis = list(title = y_title, gridcolor = "#eaf0f3"),
    legend = list(orientation = "h", y = -0.25))
  plotly::config(chart, displaylogo = FALSE, responsive = TRUE)
}
investment_card <- function(label, value, detail) {
  div(class = "metric", div(class = "eyebrow", label), div(class = "metric-value", value), div(class = "muted", detail))
}
money_label <- function(x, currency) paste(currency, format(round(x, 2), big.mark = ",", nsmall = 2, trim = TRUE))
base_currencies <- c("AUD", "USD", "EUR", "GBP", "CAD", "SGD", "JPY", "CHF", "NZD", "IDR")

country_context_ui <- function(id, catalog, extra_content = NULL) {
  ns <- NS(id)
  tabPanel("Market context", value = "context",
    div(class = "section-intro", h2("Put the yield in context"),
      p(paste(nrow(catalog), "markets covered. Explore yields, inflation and exchange rates when you need the wider picture."))),
    div(class = "panel-card market-panel",
      div(class = "market-filters",
        selectInput(ns("country"), "Country", choices = sort(catalog$country), selected = "United States"),
        selectInput(ns("base"), "Your currency", choices = base_currencies, selected = "AUD"),
        div(class = "context-action", actionButton(ns("open_lab"), "Explore specific bonds", class = "secondary-button"))),
      uiOutput(ns("coverage")), uiOutput(ns("facts"))),
    div(class = "panel-card market-panel", h3("Government benchmark yield / monthly"),
      plotly::plotlyOutput(ns("yield"), height = "280px")),
    div(class = "investment-two-col",
      div(class = "panel-card market-panel", h3("Consumer inflation / annual"),
        plotly::plotlyOutput(ns("inflation"), height = "280px"),
        p(class = "panel-footnote", "World Bank / IMF, annual CPI change. Historical context, not an inflation forecast or a real-return calculation.")),
      div(class = "panel-card market-panel", h3("Exchange rate / daily reference"),
        plotly::plotlyOutput(ns("fx"), height = "280px"),
        p(class = "panel-footnote", "ECB reference cross-rates over the available 90-day window. Informational rates; broker spreads are not included."))),
    tags$details(class = "panel-card market-panel", tags$summary("Compare countries"), extra_content))
}
country_context_server <- function(id, state, market_state, catalog, open_lab) {
  moduleServer(id, function(input, output, session) {
    bundle <- reactive({ validate(need(is.null(state$error), state$error)); state })
    country <- reactive({ req(input$country); catalog[catalog$country == input$country, ][1, ] })
    cpi <- reactive({ x <- bundle()$inflation; x <- x[x$ref_area == country()$ref_area, ]; x[order(x$year), ] })
    fx <- reactive({ req(input$base); fx_history(bundle()$fx, input$base, country()$currency) })
    output$coverage <- renderUI({
      supported <- input$country %in% bundle()$bonds$country
      div(class = "panel-footnote", if (supported) "Specific bond data available. Open Bond lab to examine a security." else
        "Market context only: individual bond data is not connected for this market. No bond prices or coupons are inferred from its benchmark.")
    })
    output$facts <- renderUI({
      a <- cpi(); a <- a[is.finite(a$inflation), ]; f <- fx()
      div(class = "investment-two-col",
        investment_card("LATEST ANNUAL INFLATION", if (nrow(a)) sprintf("%.2f%%", tail(a$inflation, 1)) else "Unavailable",
          if (nrow(a)) paste("Calendar year", tail(a$year, 1)) else "No observation in this feed"),
        investment_card("LATEST REFERENCE FX", if (nrow(f)) sprintf("%.4f", tail(f$rate, 1)) else "Unavailable",
          if (nrow(f)) paste(country()$currency, "per", input$base, "/", tail(f$date, 1)) else "Currency not covered by ECB"))
    })
    output$yield <- plotly::renderPlotly({
      validate(need(is.null(market_state$error), market_state$error))
      x <- market_state$data; x <- x[x$country == input$country, ]
      investment_plot(plotly::plot_ly(x, x = ~date, y = ~yield, type = "scatter", mode = "lines",
        connectgaps = FALSE, line = list(color = "#137e72")), "Yield (%)")
    })
    output$inflation <- plotly::renderPlotly({
      x <- cpi(); validate(need(any(is.finite(x$inflation)), "No annual CPI data for this country."))
      investment_plot(plotly::plot_ly(x, x = ~year, y = ~inflation, type = "scatter", mode = "lines+markers",
        connectgaps = FALSE, line = list(color = "#b17b2c")), "Annual CPI change (%)")
    })
    output$fx <- plotly::renderPlotly({
      x <- fx(); validate(need(nrow(x) > 0, "ECB does not cover this currency pair. No substitute rate is used."))
      investment_plot(plotly::plot_ly(x, x = ~date, y = ~rate, type = "scatter", mode = "lines",
        line = list(color = "#366cad")), paste(country()$currency, "per", input$base))
    })
    observeEvent(input$open_lab, {
      if (input$country %in% bundle()$bonds$country) open_lab(input$country) else
        showNotification("Individual bond coverage currently includes the United States and Australia.", type = "message")
    })
  })
}

bond_lab_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Bond lab", value = "bond_lab",
    div(class = "bond-welcome",
      div(class = "eyebrow", "START WITH YOUR PLANS"),
      h2("See what a bond could mean for your money."),
      p("Explore payment schedules and investment scenarios in your currency."),
      div(class = "market-filters welcome-inputs",
        numericInput(ns("capital"), "Your budget", 50000, min = 1),
        selectInput(ns("base"), "Your currency", base_currencies, selected = "AUD"),
        numericInput(ns("months"), "When you need the money (months)", 12, min = 1, max = 600, step = 1)),
      tags$a(href = paste0("#", ns("selection")), class = "btn welcome-button", "Explore bonds"),
      tags$a(href = paste0("#", ns("selection")), class = "welcome-link", "Already have a bond in mind? Search by code below."),
      p(class = "panel-footnote", "Explore a scenario, not a recommendation. Your timeframe does not automatically filter securities.")),
    div(id = ns("selection"), class = "panel-card market-panel bond-selection",
      h3("Choose a bond to explore"),
      p("Official auction references from the United States and Australia. Confirm access with your broker."),
      span(class = "data-badge", "OBSERVED / OFFICIAL SOURCES"),
      div(class = "market-filters",
        selectInput(ns("country"), "Issuer market", c("United States", "Australia"))),
      selectizeInput(ns("bond"), "Security / coupon / maturity", choices = NULL, options = list(placeholder = "Choose a security or type its code")),
      uiOutput(ns("details")),
      tags$details(tags$summary("View this security's auction history"),
        plotly::plotlyOutput(ns("auction_chart"), height = "270px"),
        p(class = "panel-footnote", "Each dot is an observed auction. Prices exclude accrued interest. These are not daily secondary-market quotes."))),
    conditionalPanel(sprintf("!input['%s']", ns("bond")),
      div(class = "payment-preview", h3("Your payment story starts here"),
        div(class = "payment-steps", span("Invest your budget"), span("Receive coupon payments"), span("Sell or reach maturity")),
        p("Choose a bond above to see its actual terms and calculate your scenario. No result is assumed before you select one."))),
    conditionalPanel(sprintf("!!input['%s']", ns("bond")),
    div(class = "panel-card market-panel scenario-panel",
      uiOutput(ns("assumption_summary")),
      tags$details(class = "advanced-assumptions",
      tags$summary("Adjust price, currency, costs and tax"),
      span(class = "data-badge assumption-badge", "YOUR ASSUMPTIONS / NOT A FORECAST"),
      h3("Set up your investment scenario"),
      div(class = "market-filters",
        dateInput(ns("start"), "Assumed settlement date", Sys.Date())),
      div(class = "market-filters",
        radioButtons(ns("price_mode"), "Purchase price assumption", c("Use dated auction reference" = "reference", "Enter broker / other quote" = "manual")),
        conditionalPanel(sprintf("input['%s'] == 'manual'", ns("price_mode")),
          numericInput(ns("price"), "Clean purchase price per 100 face", NA_real_, min = 0.01)),
        numericInput(ns("lot"), "Assumed face-value increment", 100, min = 1)),
      uiOutput(ns("purchase_note")),
      div(class = "market-filters",
        radioButtons(ns("fx_mode"), "Entry FX assumption", c("Use latest ECB reference" = "reference", "Enter conversion quote" = "manual")),
        conditionalPanel(sprintf("input['%s'] == 'manual'", ns("fx_mode")),
          numericInput(ns("entry_fx"), "Bond currency per 1 of your currency", NA_real_, min = 0.000001)),
        numericInput(ns("fx_change"), "Bond currency value change vs yours (%)", 0, min = -99, max = 500, step = 1)),
      uiOutput(ns("fx_note")),
      div(class = "market-filters",
        numericInput(ns("exit_change"), "Clean sale price change vs purchase (%)", 0, min = -99, max = 500),
        numericInput(ns("entry_fee"), "All entry costs in your currency", 0, min = 0),
        numericInput(ns("exit_fee"), "All exit costs in your currency", 0, min = 0)),
      div(class = "market-filters",
        numericInput(ns("coupon_tax"), "Effective haircut on gross coupons (%)", 0, min = 0, max = 100),
        numericInput(ns("gain_tax"), "Effective tax on positive capital gain (%)", 0, min = 0, max = 100)),
      p(class = "panel-footnote", "Zero costs and tax are initial assumptions, not a tax exemption. Include broker, custody and FX charges in the cost inputs. Face-value increment is a broker assumption, not verified trading availability."))),
    div(class = "scenario-results", uiOutput(ns("results"))),
    div(class = "panel-card market-panel", h3("Contractual cash flows under your assumptions"),
      checkboxInput(ns("include_principal"), "Include purchase and sale / redemption", FALSE),
      plotly::plotlyOutput(ns("cash_chart"), height = "320px"),
      uiOutput(ns("cash_note")),
      tags$details(tags$summary("Inspect cash flows and download the scenario"),
        DT::DTOutput(ns("cash_table")), downloadButton(ns("download"), "Download scenario CSV"))),
    div(class = "panel-card market-panel", h3("What if price and currency move?"),
      p("Holding-period total return in your currency. Each cell is a deterministic assumption, not a probability or prediction."),
      plotly::plotlyOutput(ns("risk_chart"), height = "360px"),
      p(class = "panel-footnote", "Price changes are relative to purchase price; FX changes measure the bond currency's value in your currency. At maturity, redemption stays at 100 and price shocks have no effect. Same-currency investments have no FX shock.")),
    div(class = "panel-card market-panel",
      tags$details(tags$summary("Calculation rules and coverage limits"),
        tags$ul(
          tags$li("Supported instruments: US nominal fixed-rate Treasury notes and Australian nominal Treasury bonds, with completed settlements and usable auction records from 2024. Latest observation per security is used; maturity is checked again when the app opens."),
          tags$li("Purchase cost includes clean price plus accrued interest. Coupons are based on face value, paid semiannually, never on the investment budget or benchmark yield. Whole assumed face-value increments leave uninvested cash."),
          tags$li("Contractual coupon dates are shown without weekend or holiday payment adjustments. Australian settlement/sale within 14 days before a coupon is blocked because ex-interest entitlement needs broker confirmation."),
          tags$li("Exit before maturity uses your clean-price assumption plus accrued interest. At maturity, face value is redeemed. Payment is assumed in full: default, recovery and liquidity are not modelled."),
          tags$li("A single assumed future FX rate converts all coupons and exit proceeds. Coupons and residual cash earn no reinvestment interest. There is no forecast path between observations."),
          tags$li("Tax inputs are simplified effective assumptions: a haircut on gross coupons, and a tax on positive clean-price gain measured in your currency. No allowances, loss offsets, tax treaties, accrued-interest deduction, or jurisdictional tax calculations are applied."),
          tags$li("The output is nominal, not inflation-adjusted. Annual country inflation in Market context is historical and is not substituted for the investor's future purchasing-power inflation."),
          tags$li("Auction prices and ECB rates are dated references, not executable quotes. Confirm availability, denomination, settlement, costs and tax with your broker before acting."))),
      uiOutput(ns("provenance")))))
}

bond_lab_server <- function(id, state, selected_country = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    bundle <- reactive({ validate(need(is.null(state$error), state$error)); state })
    observeEvent(selected_country(), updateSelectInput(session, "country", selected = selected_country()))
    observeEvent(list(input$country, bundle()), {
      x <- bundle()$bonds
      x <- x[x$country == input$country & x$maturity > Sys.Date(), ]
      updateSelectizeInput(session, "bond", choices = c("Choose a bond" = "", setNames(x$id, x$label)), selected = character(0), server = TRUE)
    })
    bond <- reactive({
      req(input$bond, input$country)
      x <- bundle()$bonds
      x <- x[x$id == input$bond & x$country == input$country, ]
      validate(need(nrow(x) == 1, "Select an available security."))
      x
    })
    reference_fx <- reactive({
      req(input$base)
      x <- fx_history(bundle()$fx, input$base, bond()$currency)
      validate(need(nrow(x) > 0, "No ECB reference rate. Enter your conversion quote."))
      tail(x, 1)
    })
    observeEvent(bond(), updateNumericInput(session, "price", value = bond()$clean_price))
    observeEvent(reference_fx(), updateNumericInput(session, "entry_fx", value = reference_fx()$rate))
    arguments <- reactive({
      req(input$price_mode, input$fx_mode, input$start, input$base)
      price <- if (input$price_mode == "reference") bond()$clean_price else input$price
      rate <- if (input$base == bond()$currency) 1 else if (input$fx_mode == "reference") reference_fx()$rate else input$entry_fx
      req(!is.null(price), !is.null(input$exit_change))
      list(bond = bond(), capital = input$capital, base_currency = input$base, entry_fx = rate,
        start = input$start, months = input$months, clean_price = price,
        exit_price = price * (1 + input$exit_change / 100), fx_change = input$fx_change,
        entry_fee = input$entry_fee, exit_fee = input$exit_fee, coupon_tax = input$coupon_tax,
        gain_tax = input$gain_tax, lot = input$lot)
    })
    scenario <- reactive({
      result <- tryCatch(do.call(bond_scenario, arguments()), error = identity)
      validate(need(!inherits(result, "error"), if (inherits(result, "error")) conditionMessage(result) else ""))
      result
    })
    output$assumption_summary <- renderUI({
      req(input$bond)
      b <- bond()
      p(class = "reference-note", paste(
        if (input$price_mode == "reference") paste("Using auction price dated", b$auction_date) else "Using your purchase-price input",
        "/ settlement", input$start,
        "/ entry costs", input$entry_fee, input$base, "/ exit costs", input$exit_fee, input$base,
        "/ coupon tax", input$coupon_tax, "% / capital-gain tax", input$gain_tax, "%.",
        if (input$base == b$currency) "Same currency; no FX conversion." else
          if (input$fx_mode == "reference") paste("ECB FX reference dated", reference_fx()$date, "; assumed currency change", input$fx_change, "%.") else
            paste("User-supplied entry FX; assumed currency change", input$fx_change, "%."),
        "Assumed sale-price change", input$exit_change, "% (ignored at maturity).",
        "Prices are not live quotes. Review these assumptions below."))
    })
    output$details <- renderUI({
      b <- bond()
      tagList(div(class = "metric-grid",
        investment_card("ANNUAL COUPON", sprintf("%.3f%%", b$coupon), "Of face value / semiannual payments"),
        investment_card("REFERENCE CLEAN PRICE", sprintf("%.4f", b$clean_price), paste(b$currency, "per 100 face")),
        investment_card("MATURITY", as.character(b$maturity), paste("Identifier:", b$id)),
        investment_card("AUCTION YIELD", sprintf("%.3f%%", b$auction_yield), paste("Observed", b$auction_date))),
        p(class = "reference-note", paste("Reference auction:", b$auction_date, "/ settlement:", b$settlement,
          "/", as.integer(Sys.Date() - b$auction_date), "days old. No live market quote.")),
        p(class = "panel-footnote", b$price_basis, ". ", tags$a(href = b$source_url, target = "_blank", rel = "noopener", "Official source")))
    })
    output$purchase_note <- renderUI({
      b <- bond()
      p(class = "reference-note", if (input$price_mode == "reference")
        paste("Assuming a purchase at the", b$auction_date, "auction reference, not today's market price. Accrued interest at your settlement date is added.") else
        "Your clean-price input is an assumption / supplied quote. Accrued interest is calculated separately.")
    })
    output$fx_note <- renderUI({
      if (input$base == bond()$currency) return(p(class = "panel-footnote", "Same currency: FX is fixed at 1; FX-change inputs are ignored."))
      f <- reference_fx()
      p(class = "panel-footnote", paste("ECB reference:", signif(f$rate, 7), bond()$currency, "per", input$base, "on", f$date,
        ". Positive FX change means the bond currency strengthens. The same future conversion rate is applied to every receipt."))
    })
    output$auction_chart <- plotly::renderPlotly({
      x <- bundle()$history; x <- x[x$id == bond()$id, ]; x <- x[order(x$auction_date), ]
      investment_plot(plotly::plot_ly(x, x = ~auction_date, y = ~clean_price, type = "scatter", mode = "markers",
        marker = list(color = "#137e72", size = 8)), "Clean price / 100")
    })
    output$results <- renderUI({
      x <- scenario(); a <- arguments()
      tagList(div(class = "metric-grid",
        investment_card("SCENARIO FINAL TOTAL", money_label(x$final, input$base), "Exit proceeds + net coupons + residual cash"),
        investment_card("HOLDING-PERIOD RETURN", sprintf("%+.2f%%", x$return_pct), money_label(x$profit, input$base)),
        investment_card("NET COUPONS", money_label(x$net_coupons, input$base), "After assumed coupon haircut"),
        investment_card("FACE VALUE BOUGHT", money_label(x$face, bond()$currency), paste("Residual cash:", money_label(x$residual, input$base)))),
        p(class = "reference-note", paste("Scenario ends", x$end,
          if (x$matured) "at maturity: principal redeems at 100. No sale-price shock applies." else paste("at assumed clean sale price", round(x$exit_clean, 4), "per 100."),
          "Total costs:", money_label(x$fees, input$base), "/ assumed tax:", money_label(x$taxes, input$base),
          ". This is a holding-period return, not annualized.")))
    })
    output$cash_chart <- plotly::renderPlotly({
      x <- scenario()$flows
      if (!isTRUE(input$include_principal)) x <- x[x$event == "Coupon", ]
      validate(need(nrow(x) > 0, "No coupon falls inside this holding period. Enable principal flows to view the exit."))
      investment_plot(plotly::plot_ly(x, x = ~date, y = ~net, color = ~event, type = "bar",
        hovertemplate = paste0("%{x|%d %b %Y}<br>%{y:,.2f} ", input$base, "<extra></extra>")), paste("Net cash /", input$base))
    })
    output$cash_note <- renderUI({
      x <- scenario()
      p(class = "panel-footnote", paste("Contractual dates; no holiday adjustment. Entry accrued interest:",
        round(x$entry_accrued, 6), "per 100 face. Coupons are cash payments, not total return. No reinvestment assumed."))
    })
    output$cash_table <- DT::renderDT({
      DT::formatRound(DT::datatable(scenario()$flows, rownames = FALSE,
        options = list(pageLength = 8, dom = "tp", scrollX = TRUE)), c("gross", "tax", "fee", "net", "cumulative_net"), 2)
    })
    output$risk_chart <- plotly::renderPlotly({
      scenario()
      a <- arguments()
      price_moves <- if (scenario()$matured) 0 else c(-20, -10, 0, 10, 20)
      fx_moves <- if (input$base == bond()$currency) 0 else c(-20, -10, 0, 10, 20)
      z <- matrix(NA_real_, nrow = length(fx_moves), ncol = length(price_moves))
      for (i in seq_along(fx_moves)) for (j in seq_along(price_moves)) {
        a$fx_change <- fx_moves[i]; a$exit_price <- a$clean_price * (1 + price_moves[j] / 100)
        z[i, j] <- do.call(bond_scenario, a)$return_pct
      }
      chart <- plotly::plot_ly(x = paste0(price_moves, "%"), y = paste0(fx_moves, "%"), z = z, type = "heatmap",
        colorscale = list(c(0, "#c76868"), c(0.5, "#f5f4ed"), c(1, "#168576")), zmid = 0,
        colorbar = list(title = "Return %"), hovertemplate = "Price: %{x}<br>FX: %{y}<br>Return: %{z:.2f}%<extra>Scenario</extra>")
      plotly::layout(investment_plot(chart, "Bond currency value change"), xaxis = list(title = "Clean sale price change", type = "category"), yaxis = list(type = "category"))
    })
    output$provenance <- renderUI({
      p <- bundle()$provenance
      tagList(h4("Downloaded source snapshots"), tags$ul(lapply(seq_len(nrow(p)), function(i)
        tags$li(tags$a(href = p$url[i], target = "_blank", rel = "noopener", p$file[i]), " / retrieved ", p$retrieved_at_utc[i]))))
    })
    output$download <- downloadHandler(filename = function() paste0("bond-scenario-", bond()$id, ".csv"), content = function(file) {
      x <- scenario()$flows; a <- arguments(); s <- scenario()
      x$security_id <- bond()$id; x$source_url <- bond()$source_url
      x$reference_auction_date <- bond()$auction_date; x$price_mode <- input$price_mode; x$fx_mode <- input$fx_mode
      x$fx_reference_date <- reference_fx()$date
      for (name in setdiff(names(a), "bond")) x[[paste0("assumed_", name)]] <- a[[name]]
      x$effective_entry_fx <- s$entry_fx; x$effective_exit_fx <- s$exit_fx
      x$effective_exit_clean <- s$exit_clean; x$face_value <- s$face; x$residual_cash <- s$residual
      x$scenario_final_total <- s$final; x$holding_return_pct <- s$return_pct
      x$source_retrievals <- paste(bundle()$provenance$retrieved_at_utc, collapse = " / ")
      write.csv(x, file, row.names = FALSE, na = "")
    })
    list(scenario = scenario, bond = bond, arguments = arguments)
  })
}
