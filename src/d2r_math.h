#pragma once
#include <vector>
#include <stdexcept>
#include <cmath>
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

void diff(
	const std::vector<double> &lhs,
	double rhs,
	std::vector<double> &result);

double weighted_sg(
	const std::vector<double> &w,
	const std::vector<double> &x,
	double avg_x,
	double sum_w);

std::vector<double> make_cov(
	const std::vector<double> &x0,
	const std::vector<double> &y0,
	const std::vector<double> &w_x,
	const std::vector<double> &w_y,
	double mean_px,
	double mean_py);


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