#pragma once
#include "imports.h"
#include "vctrs_provider.h"
#include "math.h"

RcppExport SEXP d2r_do_work_sigma_2_ex(
	SEXP input, 
	SEXP date_col,
	SEXP obs_col,
	SEXP what,
	SEXP extra_vars);

void mag_2_px_py(
	const Rcpp::NumericVector &data,
	const Rcpp::IntegerVector &range,
	std::vector<double> &px,
	std::vector<double> &py);

Rcpp::List extract_extra_cols(
	const std::vector<std::string> &cols,
	const Rcpp::DataFrame &data_frame,
	const Rcpp::IntegerVector &idx);

double average_vector(const Rcpp::NumericVector &input);

Rcpp::List average_single(const std::vector<double> &px, const std::vector<double> &py);
Rcpp::List average_multiple(const std::vector<double> &px, const std::vector<double> &py);