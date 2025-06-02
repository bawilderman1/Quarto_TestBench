# chart_utilities.R

library(dplyr)
library(xts)
library(highcharter)
library(magrittr)

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
