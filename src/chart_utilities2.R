# chart_utilities.R

library(dplyr)
library(xts)
library(highcharter)
library(magrittr)

# --- Load JavaScript formatters ---
load_js_tooltips <- function(js_dir = "src/js") {
  list(
    ohlc_tooltip_js = readChar(file.path(js_dir, "ohlc_tooltip.js"),
                               file.info(file.path(js_dir, "ohlc_tooltip.js"))$size),
    currency_tooltip_js = readChar(file.path(js_dir, "currency_tooltip.js"),
                                   file.info(file.path(js_dir, "currency_tooltip.js"))$size),
    percent_tooltip_js = readChar(file.path(js_dir, "percent_tooltip.js"),
                                  file.info(file.path(js_dir, "percent_tooltip.js"))$size),
    axis_label_percent_js = readChar(file.path(js_dir, "axis_label_percent.js"),
                                     file.info(file.path(js_dir, "axis_label_percent.js"))$size)
  )
}

# --- Create y-axis configuration ---
create_yaxis_config <- function(include_technical_indicator = FALSE, axis_label_percent_js = NULL) {
  yaxis_list <- list()
  
  if (include_technical_indicator) {
    yaxis_list[["technical_indicator_axis"]] <- list(
      nid = 0L,
      title = list(text = "Indicator"),
      relative = 1)
    price_axis_nid <- 1L
    equity_axis_nid <- 2L
    drawdown_axis_nid <- 3L
  } else {
    price_axis_nid <- 0L
    equity_axis_nid <- 1L
    drawdown_axis_nid <- 2L
  }
  
  yaxis_list[["price_axis"]] <- list(
    nid = price_axis_nid,
    title = list(text = "Prices"),
    relative = 3,
    type = "logarithmic")
  yaxis_list[["equity_axis"]] <- list(
    nid = equity_axis_nid,
    title = list(text = "Equity"),
    relative = 1)
  yaxis_list[["drawdown_axis"]] <- list(
    nid = drawdown_axis_nid,
    title = list(text = "Drawdown"),
    relative = 1,
    labels = list(formatter = if (!is.null(axis_label_percent_js)) JS(axis_label_percent_js) else NULL)
  )
  
  attr(yaxis_list, "price_axis_nid") <- price_axis_nid
  return(yaxis_list)
}

# --- Create series configuration ---
create_series_config <- function(chart_data2,
                                 technical_indicator_series = NULL,
                                 optional_price_series = NULL,
                                 js_tooltips) {
  series_list <- list()
  y_axis_offset <- 0L
  
  if (!is.null(technical_indicator_series)) {
    y_axis_offset <- 1L
    for (series_name in names(technical_indicator_series)) {
      series_list[[series_name]] <- technical_indicator_series[[series_name]]
    }
  }
  
  price_axis_nid <- 0L + y_axis_offset
  
  series_list[["trade_open_series"]] <- list(
    data = chart_data2[,"Trade.Open"],
    yAxis = price_axis_nid,
    name = "Open Trade",
    type = "scatter",
    color = "green",
    marker = list(symbol = "triangle"),
    tooltip = list(pointFormat = NULL)
  )
  series_list[["trade_close_series"]] <- list(
    data = chart_data2[,"Trade.Close"],
    yAxis = price_axis_nid,
    name = "Close Trade",
    type = "scatter",
    color = "red",
    marker = list(symbol = "triangle-down"),
    tooltip = list(pointFormat = NULL)
  )
  series_list[["ohlc_series"]] <- list(
    data = chart_data2[,c("Open", "High", "Low", "Close")],
    yAxis = price_axis_nid,
    id = 1,
    dataGrouping = list(enabled = FALSE),
    color = "black",
    lineColor = "black",
    name = "SPY",
    tooltip = list(pointFormatter = JS(js_tooltips$ohlc_tooltip_js))
  )
  
  if (!is.null(optional_price_series)) {
    for (series_name in names(optional_price_series)) {
      current_series_config <- optional_price_series[[series_name]]
      current_series_config$yAxis <- price_axis_nid
      series_list[[series_name]] <- current_series_config
    }
  }
  
  series_list[["equity_series"]] <- list(
    data = chart_data2[,"Equity"],
    yAxis = 1L + y_axis_offset,
    name = "Equity",
    type = "area",
    color = "green",
    negativeColor = "red",
    threshold = 10000,
    opacity = 0.75,
    tooltip = list(pointFormatter = JS(js_tooltips$currency_tooltip_js))
  )
  series_list[["buy_and_hold_series"]] <- list(
    data = chart_data2[,"BuyAndHold"],
    yAxis = 1L + y_axis_offset,
    name = "Buy & Hold",
    color = "blue",
    type = "line",
    tooltip = list(pointFormatter = JS(js_tooltips$currency_tooltip_js))
  )
  series_list[["drawdown_series"]] <- list(
    data = chart_data2[,"Drawdown"],
    yAxis = 2L + y_axis_offset,
    name = "Drawdown",
    color = "darkred",
    type = "area",
    tooltip = list(pointFormatter = JS(js_tooltips$percent_tooltip_js))
  )
  
  return(series_list)
}

build_stock_chart <- function(
    chart_data2,
    chart_title = "Backtest",
    chart_theme = hc_theme_hcrt(),
    technical_indicator_config = NULL,
    optional_price_series_config = NULL,
    js_dir = "src/js"
) {
  # --- Load JavaScript formatters ---
  js_tooltips <- load_js_tooltips(js_dir)
  
  # --- Determine y-axis config ---
  include_technical_indicator <- !is.null(technical_indicator_config)
  yaxis_config <- create_yaxis_config(
    include_technical_indicator,
    axis_label_percent_js = js_tooltips$axis_label_percent_js
  )
  
  # --- Create series config ---
  series_config <- create_series_config(
    chart_data2,
    technical_indicator_series = technical_indicator_config,
    optional_price_series = optional_price_series_config,
    js_tooltips = js_tooltips
  )
  
  # --- Initialize chart ---
  hc <- highchart(type = "stock") |>
    hc_title(text = chart_title) |>
    hc_add_theme(chart_theme) |>
    hc_rangeSelector(
      selected = 1,
      buttons = list(
        list(type = 'year', count = 1, text = '1y'),
        list(type = 'year', count = 5, text = '5y'),
        list(type = 'year', count = 10, text = '10y'),
        list(type = 'year', count = 15, text = '15y'),
        list(type = 'all', text = 'All')
      )
    ) |>
    hc_navigator(height = 0)
  
  # --- Add y-axes ---
  for (axis_name in names(yaxis_config)) {
    if (!inherits(yaxis_config[[axis_name]], "list")) next
    hc <- add_yaxis_hc(hc, yaxis_config[[axis_name]])
  }
  
  # --- Add series ---
  for (series_name in names(series_config)) {
    hc <- add_series_hc(hc, series_config[[series_name]])
  }
  
  return(hc)
}

#' Convert tibble with Date + indicator columns to xts aligned with reference xts
#' @param indicator_tbl A tibble with a Date column
#' @param reference_xts Reference xts to align index with
#' @return xts object
convert_indicator_tbl_to_xts <- function(indicator_tbl, reference_xts) {
  stopifnot("Date" %in% colnames(indicator_tbl))
  
  indicator_xts <- xts::xts(
    indicator_tbl %>% dplyr::select(-Date),
    order.by = as.POSIXct(indicator_tbl$Date, tz = "UTC")
  )
  
  # Ensure proper alignment with main chart xts
  indicator_xts <- merge(reference_xts[, 0], indicator_xts, join = "left")
  return(indicator_xts)
}

#' Merge user indicators (as tibbles or xts) into the main chart xts
#' @param main_xts Main chart xts
#' @param indicators Named list of tibbles or xts (must contain `Date` if tibble)
#' @return xts with indicators merged
merge_indicators_into_xts <- function(main_xts, indicators) {
  for (ind_name in names(indicators)) {
    ind_data <- indicators[[ind_name]]
    ind_xts <- if (is.data.frame(ind_data)) {
      convert_indicator_tbl_to_xts(ind_data, main_xts)
    } else if (xts::is.xts(ind_data)) {
      merge(main_xts[, 0], ind_data, join = "left")
    } else {
      stop(paste("Unsupported format for indicator:", ind_name))
    }
    
    if (any(colnames(ind_xts) %in% colnames(main_xts))) {
      stop(paste("Duplicate column name found in indicator:", ind_name))
    }
    
    main_xts <- merge(main_xts, ind_xts)
  }
  return(main_xts)
}

#' Generate series config from indicator xts
#' @param indicator_xts An xts object with one or more columns
#' @param yAxis Index for yAxis
#' @param type Chart type (default: "line")
#' @param colors Named list of colors (optional)
#' @return Named list of highcharter series config entries
generate_technical_series_config <- function(indicator_xts, yAxis = 0L, type = "line", colors = NULL) {
  col_names <- colnames(indicator_xts)
  
  lapply(seq_along(col_names), function(i) {
    series_name <- col_names[i]
    list(
      data = indicator_xts[, series_name],
      yAxis = yAxis,
      name = series_name,
      type = type,
      color = if (!is.null(colors) && !is.null(colors[[series_name]])) colors[[series_name]] else NULL
    )
  }) %>% setNames(col_names)
}
