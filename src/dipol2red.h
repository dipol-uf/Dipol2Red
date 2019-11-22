//   MIT License
//
//   Copyright(c) 2018 - 2019
//   Ilia Kosenkov[ilia.kosenkov.at.gm@gmail.com],
//
//   Permission is hereby granted, free of charge, to any person obtaining a copy
//   of this software and associated documentation files(the "Software"), to deal
//   in the Software without restriction, including without limitation the rights
//   to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
//   copies of the Software, and to permit persons to whom the Software is
//   furnished to do so, subject to the following conditions :
//
//   The above copyright notice and this permission
//   notice shall be included in all
//   copies or substantial portions of the Software.
//
//   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
//   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
//   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
//   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH
//   THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#pragma once
#include "imports.h"
// ReSharper disable CppUnusedIncludeDirective
#include "provider.h"
// ReSharper restore CppUnusedIncludeDirective
#include "d2r_math.h"

constexpr auto batch_size = 4;

RcppExport SEXP d2r_fsigma_2(
	SEXP input, 
	SEXP date_col,
	SEXP obs_col,
	SEXP what,
	SEXP extra_vars,
	SEXP eps,
	SEXP itt_max);


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