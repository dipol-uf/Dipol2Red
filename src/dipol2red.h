#pragma once
#include <Rcpp.h>
#include <Rinternals.h>

RcppExport SEXP d2r_do_work_sigma_2_ex(
	SEXP input, 
	SEXP date_col,
	SEXP obs_col,
	SEXP what,
	SEXP extra_vars);

void mag_2_px_py(
	Rcpp::NumericVector &arg,
	Rcpp::NumericVector &data,
	Rcpp::Vector<INTSXP>::const_iterator begin,
	Rcpp::Vector<INTSXP>::const_iterator end,
	std::vector<double> &px,
	std::vector<double> &py,
	std::vector<double> &time);