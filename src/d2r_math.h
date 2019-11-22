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
#include <vector>
// ReSharper disable CppUnusedIncludeDirective
#include <stdexcept>
#include <cmath>
// ReSharper restore CppUnusedIncludeDirective
#include <tuple>

typedef std::tuple<
	double,					// Px
	double,					// Py
	double,					// SG
	int,					// Itt
	int,					// N
	double,					// Ratio
	std::vector<double>>    // Q
	avg_result;

double average(const std::vector<double> &input);
double sum(const std::vector<double> &input);
double dot_prod(
	const std::vector<double> &lhs,
	const std::vector<double> &rhs);

void abs_diff(
	const std::vector<double> &lhs,
	double rhs,
	std::vector<double> &result);

double weighted_sg(
	const std::vector<double> &w,
	const std::vector<double> &x,
	double avg_x,
	double sum_w);

std::vector<double> make_cov(
	const std::vector<double> &x,
	const std::vector<double> &y,
	const std::vector<double> &w_x,
	const std::vector<double> &w_y);


avg_result average_single(
	const std::vector<double> &px,
	const std::vector<double> &py);

avg_result average_multiple(
	const std::vector<double> &px,
	const std::vector<double> &py,
	double eps,
	int itt_max);

void correct_pol(
	std::vector<double> &px,
	std::vector<double> &py,
	std::vector<double> &p,
	std::vector<double> &a,
	std::vector<double> &sg_a,
	const std::vector<double> &sg,
	double px_corr,
	double py_corr,
	double angle_corr);