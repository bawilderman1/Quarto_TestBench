#include "include/macd.h" // Include your header file
#include <numeric>
#include <vector>

// Function to calculate Exponential Moving Average (EMA)
std::vector<double> ema(const std::vector<double>& data, int period) {
  std::vector<double> ema_values(data.size());
  
  if (data.empty()) {
    return ema_values;  // Return empty vector if input is empty
  }
  
  double k = 2.0 / (period + 1);
  ema_values[0] = data[0]; // Initialize the first EMA value with the first data point
  
  for (size_t i = 1; i < data.size(); ++i) {
    ema_values[i] = (data[i] * k) + (ema_values[i - 1] * (1 - k));
  }
  
  return ema_values;
}

// Function to calculate MACD
// [[Rcpp::export]] // Important: Export the MACD function!
Rcpp::List macd(const Rcpp::NumericVector& data, int short_period, int long_period, int signal_period) {
  std::vector<double> data_vec = Rcpp::as<std::vector<double>>(data);
  size_t n = data.size();
  
  std::vector<double> ema_short = ema(data_vec, short_period);
  std::vector<double> ema_long = ema(data_vec, long_period);
  
  std::vector<double> macd_line(n);
  for (size_t i = 0; i < n; ++i) {
    macd_line[i] = ema_short[i] - ema_long[i];
  }
  
  std::vector<double> signal_line = ema(macd_line, signal_period);
  
  // Round the MACD and Signal lines to 3 decimal places
  for (size_t i = 0; i < n; ++i) {
    macd_line[i] = std::round(macd_line[i] * 1000.0) / 1000.0;
    signal_line[i] = std::round(signal_line[i] * 1000.0) / 1000.0;
  }
  
  std::vector<double> histogram(n);
  for (size_t i = 0; i < n; ++i) {
    histogram[i] = macd_line[i] - signal_line[i];
  }
  
  // Create the Zero line
  std::vector<double> zero_line(n, 0.0);
  
  return Rcpp::List::create(
    Rcpp::Named("MACD") = Rcpp::wrap(macd_line),
    Rcpp::Named("Signal") = Rcpp::wrap(signal_line),
    Rcpp::Named("Histogram") = Rcpp::wrap(histogram),
    Rcpp::Named("Zero") = Rcpp::wrap(zero_line) // Add the Zero line to the output
  );
}

// Function to calculate MACD as a Tibble
// [[Rcpp::export]]
Rcpp::DataFrame macdTbl(const Rcpp::NumericVector& data, int short_period, int long_period, int signal_period) {
  std::vector<double> data_vec = Rcpp::as<std::vector<double>>(data);
  size_t n = data.size();
  
  std::vector<double> ema_short = ema(data_vec, short_period);
  std::vector<double> ema_long = ema(data_vec, long_period);
  
  std::vector<double> macd_line(n);
  for (size_t i = 0; i < n; ++i) {
    macd_line[i] = ema_short[i] - ema_long[i];
  }
  
  std::vector<double> signal_line = ema(macd_line, signal_period);
  
  // Round the MACD and Signal lines to 3 decimal places
  for (size_t i = 0; i < n; ++i) {
    macd_line[i] = std::round(macd_line[i] * 1000.0) / 1000.0;
    signal_line[i] = std::round(signal_line[i] * 1000.0) / 1000.0;
  }
  
  std::vector<double> histogram(n);
  for (size_t i = 0; i < n; ++i) {
    histogram[i] = macd_line[i] - signal_line[i];
  }
  
  // Create the Zero line
  std::vector<double> zero_line(n, 0.0);
  
  // Return a DataFrame directly:
  return Rcpp::DataFrame::create(
    Rcpp::Named("MACD") = Rcpp::wrap(macd_line),
    Rcpp::Named("Signal") = Rcpp::wrap(signal_line),
    Rcpp::Named("Histogram") = Rcpp::wrap(histogram),
    Rcpp::Named("Zero") = Rcpp::wrap(zero_line) // Add the Zero line to the output
  );
}