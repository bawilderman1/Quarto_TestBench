#ifndef MACD_H  // Include guard (prevents multiple inclusions)
#define MACD_H

#include <vector>
#include <Rcpp.h>

// Function declarations:
std::vector<double> ema(const std::vector<double>& data, int period);
Rcpp::List macd(const Rcpp::NumericVector& data, int short_period, int long_period, int signal_period);

#endif // MACD_H