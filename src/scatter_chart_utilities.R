# Creates a Highcharter scatter series configuration from given column mappings
create_scatter_series_config <- function(data, x_col, y_col, group_col) {
  hchart(
    data,
    "scatter",
    hcaes(
      x = !!rlang::sym(x_col),
      y = !!rlang::sym(y_col),
      group = !!rlang::sym(group_col)
    )
  )
}

# Example usage:
# create_scatter_series_config(my_data, "trd_pnl_pct", "max_trd_drawdn", "trd_rslt")

# Generates a default HTML tooltip table for scatter points
# Arguments:
#   y_axis_tooltip: Tooltip for Y-axis (default: "Y Axis Tooltip")
generate_tooltip_scatter <- function(y_axis_tooltip = "Y Axis Tooltip") {
  x_labels <- c("Trade #", "Trade Date", "Profit/Loss", y_axis_tooltip)
  y_values <- c("{point.trd_num}", "{point.trd_st_dt}", "{point.x}%", "{point.y}%")
  tooltip_table(x_labels, y_values)
}

# Example usage:
# tooltip_html <- generate_tooltip_scatter()

# Builds a full Highcharter scatter chart for backtest trade metrics
# Arguments:
#   data: Data frame with trade results
#   x_col, y_col: Column names for x and y values
#   group_col: Column name for grouping/coloring
#   chart_title: Main chart title (default: "Scatter Chart")
#   y_axis_title: Title for Y-axis (default: "Y Axis Title")
#   y_axis_tooltip: Tooltip for Y-axis (default: "Y Axis Tooltip")
#   chart_theme: Highcharter theme object (default: hc_theme_hcrt())
build_scatter_chart <- function(data, x_col, y_col, group_col,
                                chart_title = "Scatter Chart",
                                y_axis_title = "Y Axis Title",
                                y_axis_tooltip = "Y Axis Tooltip",
                                chart_theme = hc_theme_hcrt()) {
  
  # Create new column names for the scaled data
  # This makes it explicit that we're creating new, scaled versions of the columns
  x_col_scaled <- paste0(x_col, "_scaled")
  y_col_scaled <- paste0(y_col, "_scaled")
  
  # Prepare data by rounding key metrics to percentages into NEW columns
  data <- data |>
    dplyr::mutate(
      !!sym(x_col_scaled) := round(.data[[x_col]] * 100, 2),
      !!sym(y_col_scaled) := round(.data[[y_col]] * 100, 2)
    )
  
  tooltip_html <- generate_tooltip_scatter(y_axis_tooltip)
  
  # Pass the NEW scaled column names to create_scatter_series_config
  chart <- create_scatter_series_config(data, x_col_scaled, y_col_scaled, group_col)
  
  # Build final chart with title, axes, theme, and tooltip
  chart |>
    hc_add_theme(chart_theme) |>
    hc_title(text = chart_title) |>
    hc_xAxis(
      title = list(text = "Trade Profit and Loss (%)"),
      plotLines = list(
        list(dashStyle = "Dash", value = 0, color = "black", zIndex = 3)
      ),
      labels = list( # X-axis label formatter (expects scaled data)
        formatter = JS("function() {
          return Highcharts.numberFormat(this.value, 2) + '%';
        }")
      )
    ) |>
    hc_yAxis(
      title = list(text = paste0(y_axis_title, " (%)")),
      plotLines = list(
        list(dashStyle = "Dash", value = 0, color = "black", zIndex = 3)
      ),
      labels = list( # Y-axis label formatter (expects scaled data)
        formatter = JS("function() {
          return Highcharts.numberFormat(this.value, 2) + '%';
        }")
      )
    ) |>
    hc_tooltip(
      useHTML = TRUE,
      pointFormat = tooltip_html, # This correctly uses {point.x}% and {point.y}%
      headerFormat = paste0(
        "<span style=\"color:{series.color}\">\u25cf</span> ",
        "<b style=\"color:{series.color};font-size:1rem;\">{series.name}</b>"
      )
    )
}

# Example usage:
# build_scatter_chart(
#   data = bt_metrics$data_trd,
#   x_col = "trd_pnl_pct",
#   y_col = "max_trd_drawdn",
#   group_col = "trd_rslt",
#   chart_title = "Backtest - PnL vs. Max Drawdown",
#   y_axis_title = "Max Trade Drawdown",
#   y_axis_tooltip = "Max Drawdown",
#   chart_theme = hc_theme_hcrt()
# )
