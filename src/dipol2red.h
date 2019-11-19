#pragma once
#include <Rcpp.h>
#include <Rinternals.h>

constexpr auto batch_size = 4;

RcppExport SEXP d2r_do_work_sigma_2_ex(
	SEXP input, 
	SEXP date_col,
	SEXP obs_col,
	SEXP what,
	SEXP extra_vars);

void mag_2_px_py(
	const Rcpp::NumericVector &arg,
	const Rcpp::NumericVector &data,
	const Rcpp::IntegerVector &range,
	std::vector<double> &px,
	std::vector<double> &py,
	std::vector<double> &time);

Rcpp::List extract_extra_cols(
	const std::vector<std::string> &cols,
	const Rcpp::DataFrame &data_frame,
	const Rcpp::IntegerVector &idx);

SEXP subset_generic(
	SEXP input,
	const std::vector<int> &idx);