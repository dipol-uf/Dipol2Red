#pragma once
#include "imports.h"
#include "provider.h"
#include "d2r_math.h"

constexpr auto batch_size = 4;

void mag_2_px_py(
	const Rcpp::NumericVector &data,
	const Rcpp::IntegerVector &range,
	std::vector<double> &px,
	std::vector<double> &py);

Rcpp::List extract_extra_cols(
	const std::vector<std::string> &cols,
	const Rcpp::DataFrame &data_frame,
	const Rcpp::List &idx);

Rcpp::NumericVector average_arg(
	const Rcpp::NumericVector &data,
	const Rcpp::List &idx);


void postprocess_pol(
	Rcpp::List &input,
	double px_corr,
	double py_corr,
	double angle_corr);

avg_result sigma_2(
	const Rcpp::NumericVector &data,
	const Rcpp::IntegerVector &range,
	double eps_val,
	int itt_max);

void arrange_data(
	Rcpp::List &input, 
	const Rcpp::NumericVector &arg);