#ifndef ULTIMATE_SMOOTHER_H
#define ULTIMATE_SMOOTHER_H

#include <Rcpp.h>

Rcpp::NumericVector ultimateSmoother(const Rcpp::NumericVector& price, int period, int displace);
Rcpp::DataFrame ultimateSmootherTbl(const Rcpp::NumericVector& price, int period, int displace);

#endif // ULTIMATE_SMOOTHER_H
