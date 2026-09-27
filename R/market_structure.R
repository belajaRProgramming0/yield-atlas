month_index <- function(date) {
  date <- as.Date(date)
  as.integer(format(date, "%Y")) * 12L + as.integer(format(date, "%m"))
}

previous_year_date <- function(date) {
  date <- as.Date(date)
  as.Date(sprintf("%04d-%s", as.integer(format(date, "%Y")) - 1L, format(date, "%m-%d")))
}

structure_feature_labels <- c(
  yield_level = "Latest available yield (%)",
  change_12m_bp = "12-month change (bp)",
  volatility_bp = "Monthly-change volatility (bp)",
  lagged_inflation = "Previous-year inflation (%)"
)

build_market_features <- function(data, inflation, end_month, lookback_months = 36L) {
  end_month <- as.Date(end_month)
  data <- data[data$date <= end_month & is.finite(data$yield), ]
  if (!nrow(data)) stop("No benchmark observations through the selected month.")
  rows <- lapply(split(data, data$country), function(part) {
    part <- part[order(part$date), ]
    current <- tail(part, 1)
    prior_date <- previous_year_date(current$date)
    prior <- part[part$date == prior_date, ]
    start_index <- month_index(current$date) - as.integer(lookback_months) + 1L
    history <- part[month_index(part$date) >= start_index, ]
    changes <- diff(history$yield) * 100
    consecutive <- diff(month_index(history$date)) == 1L
    changes <- changes[consecutive & is.finite(changes)]
    cpi <- inflation[inflation$ref_area == current$ref_area &
      inflation$year <= as.integer(format(current$date, "%Y")) - 1L &
      is.finite(inflation$inflation), ]
    cpi <- cpi[order(cpi$year), ]
    data.frame(
      country = current$country,
      ref_area = current$ref_area,
      as_of = current$date,
      yield_level = current$yield,
      change_12m_bp = if (nrow(prior)) (current$yield - prior$yield[1]) * 100 else NA_real_,
      volatility_bp = if (length(changes) >= 12L) stats::sd(changes) else NA_real_,
      lagged_inflation = if (nrow(cpi)) tail(cpi$inflation, 1) else NA_real_,
      inflation_year = if (nrow(cpi)) tail(cpi$year, 1) else NA_integer_,
      observations = nrow(history)
    )
  })
  features <- do.call(rbind, rows)
  model_columns <- names(structure_feature_labels)
  features <- features[stats::complete.cases(features[, model_columns]), ]
  rownames(features) <- NULL
  if (nrow(features) < 8L) stop("Fewer than eight markets have complete yield and inflation features.")
  if (any(vapply(features[, model_columns], stats::sd, numeric(1)) == 0))
    stop("At least one feature has no variation in this window.")
  features
}

cluster_market_features <- function(scaled, method = "K-means", requested_k = "Auto") {
  if (!requireNamespace("cluster", quietly = TRUE)) stop("Install the cluster package.")
  max_k <- min(6L, nrow(scaled) - 1L)
  candidates <- 2L:max_k
  distance <- stats::dist(scaled)
  hierarchy <- if (method == "Ward hierarchical") stats::hclust(distance, method = "ward.D2") else NULL
  solutions <- lapply(candidates, function(k) {
    groups <- if (method == "Ward hierarchical") {
      stats::cutree(hierarchy, k = k)
    } else {
      set.seed(2026)
      stats::kmeans(scaled, centers = k, nstart = 50L, iter.max = 100L)$cluster
    }
    width <- mean(cluster::silhouette(groups, distance)[, "sil_width"])
    list(k = k, groups = groups, silhouette = width)
  })
  diagnostics <- data.frame(
    k = vapply(solutions, `[[`, integer(1), "k"),
    silhouette = vapply(solutions, `[[`, numeric(1), "silhouette")
  )
  chosen_k <- if (identical(requested_k, "Auto")) diagnostics$k[which.max(diagnostics$silhouette)] else as.integer(requested_k)
  selected <- solutions[[match(chosen_k, diagnostics$k)]]
  if (is.null(selected)) stop("The requested cluster count is unavailable for this selection.")
  # Cluster numbers are identifiers. Reordering by the first standardized feature keeps labels stable.
  order_ids <- order(tapply(scaled[, 1], selected$groups, mean))
  relabel <- setNames(seq_along(order_ids), order_ids)
  selected$groups <- unname(relabel[as.character(selected$groups)])
  selected$diagnostics <- diagnostics
  selected$method <- method
  selected
}

analyse_market_structure <- function(data, inflation, end_month, lookback_months = 36L,
                                     method = "K-means", requested_k = "Auto") {
  features <- build_market_features(data, inflation, end_month, lookback_months)
  variables <- names(structure_feature_labels)
  pca <- stats::prcomp(features[, variables], center = TRUE, scale. = TRUE)
  scaled <- scale(features[, variables], center = pca$center, scale = pca$scale)
  clustering <- cluster_market_features(scaled, method, requested_k)
  variance <- pca$sdev^2 / sum(pca$sdev^2)
  scores <- data.frame(
    country = features$country,
    PC1 = pca$x[, 1],
    PC2 = pca$x[, 2],
    cluster = factor(paste("Group", clustering$groups), levels = paste("Group", seq_len(clustering$k)))
  )
  loadings <- data.frame(
    feature = unname(structure_feature_labels[rownames(pca$rotation)]),
    PC1 = pca$rotation[, 1],
    PC2 = pca$rotation[, 2],
    row.names = NULL
  )
  profile_data <- cbind(features[, variables], group = clustering$groups)
  profiles <- stats::aggregate(profile_data[, variables], list(group = profile_data$group), mean)
  counts <- table(clustering$groups)
  profiles$markets <- as.integer(counts[as.character(profiles$group)])
  profiles <- profiles[, c("group", "markets", variables)]
  profiles$group <- paste("Group", profiles$group)
  list(features = features, pca = pca, variance = variance, scores = scores,
       loadings = loadings, clustering = clustering, profiles = profiles)
}

fit_market_panel_gam <- function(data, inflation, end_month, years = 5L) {
  if (!requireNamespace("mgcv", quietly = TRUE)) stop("Install the mgcv package.")
  end_month <- as.Date(end_month)
  start_month <- seq(end_month, by = paste0("-", as.integer(years), " years"), length.out = 2)[2]
  panel <- data[data$date >= start_month & data$date <= end_month & is.finite(data$yield), ]
  panel$inflation_year <- as.integer(format(panel$date, "%Y")) - 1L
  cpi <- inflation[, c("ref_area", "year", "inflation")]
  names(cpi) <- c("ref_area", "inflation_year", "lagged_inflation")
  panel <- merge(panel, cpi, by = c("ref_area", "inflation_year"), all = FALSE)
  panel <- panel[is.finite(panel$lagged_inflation), ]
  panel <- panel[order(panel$country, panel$date), ]
  if (nrow(panel) < 200L || length(unique(panel$country)) < 8L)
    stop("Not enough matched yield and lagged-inflation observations for the panel model.")
  panel$country <- factor(panel$country)
  panel$time_index <- as.numeric(panel$date - min(panel$date)) / 365.25
  time_k <- min(12L, length(unique(panel$date)) - 1L)
  inflation_k <- min(5L, length(unique(panel$lagged_inflation)) - 1L)
  s <- mgcv::s
  model <- mgcv::gam(
    yield ~ s(time_index, k = time_k) + s(lagged_inflation, k = inflation_k) + s(country, bs = "re"),
    data = panel, method = "REML"
  )
  prediction <- stats::predict(model, newdata = panel, se.fit = TRUE)
  panel$fitted <- as.numeric(prediction$fit)
  panel$lower <- panel$fitted - 1.96 * as.numeric(prediction$se.fit)
  panel$upper <- panel$fitted + 1.96 * as.numeric(prediction$se.fit)
  panel$residual <- stats::residuals(model, type = "response")
  lag_pairs <- do.call(rbind, lapply(split(panel, panel$country), function(part) {
    part <- part[order(part$date), ]
    if (nrow(part) < 2L) return(NULL)
    keep <- diff(month_index(part$date)) == 1L
    data.frame(previous = head(part$residual, -1)[keep], current = tail(part$residual, -1)[keep])
  }))
  summary_model <- summary(model)
  list(
    model = model,
    data = panel,
    metrics = list(
      observations = nrow(panel),
      markets = length(unique(panel$country)),
      adjusted_r2 = summary_model$r.sq,
      deviance_explained = summary_model$dev.expl,
      rmse = sqrt(mean((panel$yield - panel$fitted)^2)),
      residual_lag1 = if (nrow(lag_pairs) > 2L) stats::cor(lag_pairs$previous, lag_pairs$current) else NA_real_
    )
  )
}

market_structure_ui <- function(id, catalog) {
  ns <- NS(id)
  tabPanel("Market structure", value = "structure",
    div(class = "section-intro", h2("Find structure without turning it into a recommendation"),
      p("Reduce several market features into common factors, group similar markets, and inspect a historical statistical model.")),
    div(class = "panel-card market-panel",
      div(class = "section-heading", div(span(class = "eyebrow", "PCA + UNSUPERVISED LEARNING"),
        h2("How are sovereign markets positioned?"),
        p("Every feature is standardized before PCA and clustering. Group numbers are identifiers, not rankings."))),
      div(class = "market-filters",
        selectInput(ns("end_month"), "Observation month", choices = NULL),
        selectInput(ns("lookback"), "Volatility window", choices = c("24 months" = 24, "36 months" = 36, "60 months" = 60), selected = 36),
        selectInput(ns("method"), "Clustering method", choices = c("K-means", "Ward hierarchical"))),
      selectInput(ns("clusters"), "Number of groups", choices = c("Choose by silhouette" = "Auto", 2:6), selected = "Auto"),
      uiOutput(ns("metrics")),
      p(class = "panel-footnote", "Each market uses its latest reported yield at or before the selected month. Values are not carried forward into missing months.")),
    div(class = "investment-two-col",
      div(class = "panel-card market-panel", h3("Market factor map"),
        plotly::plotlyOutput(ns("factor_map"), height = "430px"),
        p(class = "panel-footnote", "Nearby markets have similar standardized yield, 12-month change, volatility and lagged inflation features.")),
      div(class = "panel-card market-panel", h3("Variance retained"),
        plotly::plotlyOutput(ns("scree"), height = "260px"),
        h3("What drives the first two components"),
        plotly::plotlyOutput(ns("loadings"), height = "260px"))),
    div(class = "panel-card market-panel structure-table", h3("Group profiles"),
      DT::DTOutput(ns("profiles")),
      p(class = "panel-footnote", "Profile values are group means in their original units. The clustering itself uses standardized features.")),
    div(class = "panel-card market-panel",
      div(class = "section-heading", div(span(class = "eyebrow", "ADVANCED STATISTICAL MODELLING"),
        h2("Explain the historical pattern"),
        p("A panel GAM combines a common smooth time pattern, previous-year inflation and a country random effect."))),
      div(class = "market-filters",
        selectInput(ns("model_country"), "Market to inspect", choices = sort(catalog$country), selected = "United States"),
        selectInput(ns("model_years"), "Model history", choices = c("3 years" = 3, "5 years" = 5), selected = 5)),
      uiOutput(ns("model_metrics")),
      plotly::plotlyOutput(ns("model_chart"), height = "360px"),
      p(class = "panel-footnote", "The fitted line explains observed history. It is not extended beyond the data and is not a yield forecast."),
      tags$details(tags$summary("Model diagnostics and interpretation limits"),
        plotly::plotlyOutput(ns("residuals"), height = "280px"),
        uiOutput(ns("model_note")))),
    div(class = "market-disclosure",
      p("Method limits: PCA directions and clusters can change with the observation month, feature set and window. Inflation is lagged by one calendar year to avoid using future annual data, but mixed frequencies remain. The GAM describes association rather than causation, and residual serial dependence is reported rather than hidden.")))
}

market_structure_server <- function(id, state, market_state) {
  moduleServer(id, function(input, output, session) {
    bundle <- reactive({ validate(need(is.null(state$error), state$error)); state })
    benchmark <- reactive({ validate(need(is.null(market_state$error), market_state$error)); market_state$data })
    observeEvent(benchmark(), {
      months <- available_months(benchmark())
      updateSelectInput(session, "end_month", choices = setNames(as.character(months), format(months, "%B %Y")), selected = as.character(months[1]))
    })
    analysis <- reactive({
      req(input$end_month, input$lookback, input$method, input$clusters)
      result <- tryCatch(analyse_market_structure(benchmark(), bundle()$inflation, input$end_month,
        as.integer(input$lookback), input$method, input$clusters), error = identity)
      validate(need(!inherits(result, "error"), if (inherits(result, "error")) conditionMessage(result) else ""))
      result
    })
    panel_model <- reactive({
      req(input$end_month, input$model_years)
      result <- tryCatch(fit_market_panel_gam(benchmark(), bundle()$inflation, input$end_month,
        as.integer(input$model_years)), error = identity)
      validate(need(!inherits(result, "error"), if (inherits(result, "error")) conditionMessage(result) else ""))
      result
    })
    output$metrics <- renderUI({
      a <- analysis()
      div(class = "metric-grid",
        investment_card("MARKETS MODELLED", nrow(a$features), "Complete observed features"),
        investment_card("PC1 + PC2", sprintf("%.1f%%", 100 * sum(a$variance[1:2])), "Variance retained"),
        investment_card("GROUPS", a$clustering$k, a$clustering$method),
        investment_card("SILHOUETTE", sprintf("%.2f", a$clustering$silhouette), "Higher means clearer separation"))
    })
    output$factor_map <- plotly::renderPlotly({
      a <- analysis(); x <- a$scores
      plotly::plot_ly(x, x = ~PC1, y = ~PC2, color = ~cluster, text = ~country,
        type = "scatter", mode = "markers", marker = list(size = 11, line = list(color = "white", width = 1)),
        hovertemplate = "%{text}<br>PC1 %{x:.2f}<br>PC2 %{y:.2f}<extra>%{fullData.name}</extra>") |>
        plotly::layout(paper_bgcolor = "transparent", plot_bgcolor = "white",
          xaxis = list(title = sprintf("PC1 (%.1f%%)", 100 * a$variance[1]), gridcolor = "#eef2f3"),
          yaxis = list(title = sprintf("PC2 (%.1f%%)", 100 * a$variance[2]), gridcolor = "#eef2f3"),
          legend = list(orientation = "h", y = -0.18), margin = list(l = 60, r = 20, b = 70, t = 20)) |>
        plotly::config(displaylogo = FALSE, responsive = TRUE)
    })
    output$scree <- plotly::renderPlotly({
      a <- analysis(); x <- data.frame(component = paste0("PC", seq_along(a$variance)), variance = 100 * a$variance)
      investment_plot(plotly::plot_ly(x, x = ~component, y = ~variance, type = "bar", marker = list(color = "#4c8e80")), "Variance explained (%)")
    })
    output$loadings <- plotly::renderPlotly({
      a <- analysis(); x <- rbind(
        data.frame(feature = a$loadings$feature, component = "PC1", loading = a$loadings$PC1),
        data.frame(feature = a$loadings$feature, component = "PC2", loading = a$loadings$PC2))
      investment_plot(plotly::plot_ly(x, x = ~loading, y = ~feature, color = ~component,
        type = "bar", orientation = "h"), "Loading") |>
        plotly::layout(barmode = "group")
    })
    output$profiles <- DT::renderDT({
      x <- analysis()$profiles
      names(x) <- c("Group", "Markets", unname(structure_feature_labels))
      DT::datatable(x, rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE)) |>
        DT::formatRound(columns = 3:ncol(x), digits = 2)
    })
    output$model_metrics <- renderUI({
      m <- panel_model()$metrics
      div(class = "metric-grid",
        investment_card("OBSERVATIONS", format(m$observations, big.mark = ","), paste(m$markets, "markets")),
        investment_card("DEVIANCE EXPLAINED", sprintf("%.1f%%", 100 * m$deviance_explained), "In-sample description"),
        investment_card("MODEL RMSE", sprintf("%.2f", m$rmse), "Yield percentage points"),
        investment_card("RESIDUAL LAG-1", sprintf("%.2f", m$residual_lag1), "Remaining monthly dependence"))
    })
    output$model_chart <- plotly::renderPlotly({
      req(input$model_country); x <- panel_model()$data
      x <- x[as.character(x$country) == input$model_country, ]
      validate(need(nrow(x) > 0, "This market has no matched observations in the model window."))
      chart <- plotly::plot_ly(x, x = ~date) |>
        plotly::add_ribbons(ymin = ~lower, ymax = ~upper, name = "95% model interval",
          fillcolor = "rgba(19,126,114,0.12)", line = list(color = "transparent"), hoverinfo = "skip") |>
        plotly::add_lines(y = ~fitted, name = "Fitted history", line = list(color = "#137e72", width = 3)) |>
        plotly::add_markers(y = ~yield, name = "Observed yield", marker = list(color = "#334f65", size = 5),
          hovertemplate = "%{x|%b %Y}<br>%{y:.2f}%<extra>Observed</extra>")
      investment_plot(chart, "Yield (%)")
    })
    output$residuals <- plotly::renderPlotly({
      x <- panel_model()$data
      investment_plot(plotly::plot_ly(x, x = ~fitted, y = ~residual, text = ~country,
        type = "scatter", mode = "markers", marker = list(size = 5, opacity = .45, color = "#557789"),
        hovertemplate = "%{text}<br>Fitted %{x:.2f}<br>Residual %{y:.2f}<extra></extra>"), "Residual")
    })
    output$model_note <- renderUI({
      m <- panel_model()$metrics
      tags$ul(
        tags$li("Response: observed monthly 10-year government benchmark yield."),
        tags$li("Terms: common smooth time effect, smooth previous-calendar-year inflation effect, and a country random intercept."),
        tags$li(paste("Adjusted R-squared:", sprintf("%.3f.", m$adjusted_r2), "Residual lag-1 correlation:", sprintf("%.3f.", m$residual_lag1))),
        tags$li("Confidence bands describe uncertainty around the fitted historical mean. They are not prediction intervals for future yields."),
        tags$li("Serial correlation, omitted macro variables and revisions limit inference. Coefficients are not interpreted as causal effects."))
    })
  })
}
