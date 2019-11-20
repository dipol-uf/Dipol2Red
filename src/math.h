#pragma once
#include <vector>
#include <stdexcept>
#include<cmath>

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
	const std::vector<double> &w_y,
	double avg_x,
	double avg_y);