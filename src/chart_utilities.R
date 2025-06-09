# chart_utilities.R

library(dplyr)
library(xts)
library(highcharter)
library(magrittr)

#' Load JavaScript tooltip formatters from files
#' 
#' Reads all tooltip JavaScript files used in chart rendering into memory.
#' 
#' @return Named list of JS strings
load_js_tooltips <- function(base_path = "src/js/") {
  read_js <- function(filename) {
    full_path <- file.path(base_path, filename)
    readChar(full_path, file.info(full_path)$size)
  }
  
  list(
    ohlc_tooltip_js = read_js("ohlc_tooltip.js"),
    currency_tooltip_js = read_js("currency_tooltip.js"),
    percent_tooltip_js = read_js("percent_tooltip.js"),
    axis_label_percent_js = read_js("axis_label_percent.js")
  )
}

# --- Create y-axis configuration ---
create_yaxis_config <- function(include_technical_indicator = FALSE, axis_label_percent_js = NULL) {
  yaxis_list <- list()
  
  # Common layout tweaks
  base_axis_style <- list(
    labels = list(x = -10),  # Shift labels to avoid overlap
    offset = 30,             # Offset entire axis from edge
    lineWidth = 1
  )
  
  if (include_technical_indicator) {
    yaxis_list[["technical_indicator_axis"]] <- c(
      base_axis_style,
      list(
        nid = 0L,
        title = list(text = "Indicator"),
        relative = 1
      )
    )
    price_axis_nid <- 1L
    equity_axis_nid <- 2L
    drawdown_axis_nid <- 3L
  } else {
    price_axis_nid <- 0L
    equity_axis_nid <- 1L
    drawdown_axis_nid <- 2L
  }
  
  yaxis_list[["price_axis"]] <- c(
    base_axis_style,
    list(
      nid = price_axis_nid,
      title = list(text = "Prices"),
      relative = 3,
      type = "logarithmic"
    )
  )
  yaxis_list[["equity_axis"]] <- c(
    base_axis_style,
    list(
      nid = equity_axis_nid,
      title = list(text = "Equity"),
      relative = 1
    )
  )
  yaxis_list[["drawdown_axis"]] <- c(
    base_axis_style,
    list(
      nid = drawdown_axis_nid,
      title = list(text = "Drawdown"),
      relative = 1,
      labels = list(
        formatter = if (!is.null(axis_label_percent_js)) JS(axis_label_percent_js) else NULL,
        x = -10  # Needed again here to override any default
      )
    )
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
    chart_data, 
    chart_title = "Backtest",
    chart_theme = hc_theme_hcrt(),
    technical_indicator_config = NULL,
    optional_price_series_config = NULL,
    js_tooltips = load_js_tooltips()  # Default to loading JS tooltips
) {
  include_technical_indicator <- !is.null(technical_indicator_config) 
  
  hc <- highchart(type = "stock") |>
    hc_title(text = chart_title) |>
    hc_add_theme(chart_theme) |>
    hc_rangeSelector(selected = 1, buttons = list(
      list(type = 'year', count = 1, text = '1y'),
      list(type = 'year', count = 5, text = '5y'),
      list(type = 'year', count = 10, text = '10y'),
      list(type = 'year', count = 15, text = '15y'),
      list(type = 'all', text = 'All'))) |>
    hc_navigator(height = 0) |>
    # hc_yAxis(
    #   labels = list(x = -10),  # Shift labels left
    #   offset = 30,             # Offset whole axis from edge
    #   lineWidth = 1
    # ) |>          # Prevents label overlap
    hc_exporting(
      enabled = TRUE,
      buttons = list(
        contextButton = list(
          menuItems = c("viewFullscreen", "printChart", "separator", "downloadPNG",
                        "downloadJPEG", "downloadPDF", "downloadSVG", "separator",
                        "resetZoom", "resetZoomButton")
        )
      )
    )
  
  yaxis_config <- create_yaxis_config(include_technical_indicator)
  series_config <- create_series_config(
    chart_data,
    technical_indicator_series = technical_indicator_config,
    optional_price_series = optional_price_series_config,
    js_tooltips = js_tooltips  # Automatically loaded
  )
  
  # for (axis_name in names(yaxis_config)) {
  #   if (!inherits(yaxis_config[[axis_name]], "list")) next
  #   hc <- add_yaxis_hc(hc, yaxis_config[[axis_name]])
  # }
  
  cleaned_axes <- lapply(yaxis_config, function(axis) {
    if (!is.list(axis)) return(NULL)
    axis[!(names(axis) %in% c("nid", "relative"))]
  })
  cleaned_axes <- Filter(Negate(is.null), cleaned_axes)
  
  hc <- hc %>% hc_yAxis_multiples(cleaned_axes)
  str(cleaned_axes)
  
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

#' Generate configuration for technical indicator series
#'
#' @param data An xts/data.frame with one or more columns for plotting.
#' @param yAxis Integer specifying the yAxis index to use.
#' @param colors Named list of colors per series (optional).
#' @param tooltip_js Either a single JS string, or a named list of JS strings per series.
#' @param dash_styles Named list of dash styles per series (optional).
#' @param line_widths Named list of line widths per series (optional).
#' @param markers Named list of marker configs per series (optional).
#'
#' @return Named list of series configuration entries for use in Highcharter
generate_technical_series_config <- function(
    data,
    yAxis = 0L,
    colors = NULL,
    tooltip_js = NULL,
    dash_styles = NULL,
    line_widths = NULL,
    markers = NULL
) {
  series_list <- list()
  
  for (series_name in colnames(data)) {
    series_config <- list(
      data = data[, series_name],
      yAxis = yAxis,
      name = series_name,
      type = "line"
    )
    
    # Optional styling
    if (!is.null(colors) && !is.null(colors[[series_name]])) {
      series_config$color <- colors[[series_name]]
    }
    
    if (!is.null(dash_styles) && !is.null(dash_styles[[series_name]])) {
      series_config$dashStyle <- dash_styles[[series_name]]
    }
    
    if (!is.null(line_widths) && !is.null(line_widths[[series_name]])) {
      series_config$lineWidth <- line_widths[[series_name]]
    }
    
    if (!is.null(markers) && !is.null(markers[[series_name]])) {
      series_config$marker <- markers[[series_name]]
    }
    
    # Handle tooltip configuration
    if (!is.null(tooltip_js) && series_name %in% names(tooltip_js)) {
      tooltip_value <- tooltip_js[[series_name]]
      
      if (is.character(tooltip_value) && length(tooltip_value) == 1L) {
        # It's a JavaScript formatter string
        series_config$tooltip <- list(pointFormatter = JS(tooltip_value))
      } else if (is.list(tooltip_value)) {
        # It's a raw tooltip list (e.g., list(enabled = FALSE))
        series_config$tooltip <- tooltip_value
      } else {
        warning(paste("Unrecognized tooltip format for series", series_name))
      }
    }
    
    series_list[[series_name]] <- series_config
  }
  
  return(series_list)
}

add_blank_bars_xts <- function(xts_data, n = 3) {
  if (!xts::is.xts(xts_data)) {
    stop("Input must be an xts object")
  }
  
  # Get the index (timestamps)
  dates <- zoo::index(xts_data)
  
  if (length(dates) < 2) {
    stop("Need at least 2 timestamps to compute interval")
  }
  
  # Compute interval based on last 2 bars
  interval <- dates[length(dates)] - dates[length(dates) - 1]
  
  # Create new timestamps
  new_dates <- seq(from = dates[length(dates)] + interval, by = interval, length.out = n)
  
  # Create empty xts rows with the same columns (filled with NAs)
  blank_matrix <- matrix(NA, nrow = n, ncol = ncol(xts_data))
  colnames(blank_matrix) <- colnames(xts_data)
  blank_xts <- xts::xts(blank_matrix, order.by = new_dates)
  
  # Combine original and blank rows
  combined_xts <- rbind(xts_data, blank_xts)
  
  return(combined_xts)
}