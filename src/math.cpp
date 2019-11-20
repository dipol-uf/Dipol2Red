#include "math.h"

namespace {
	long double sum_(const std::vector<double> &input)
	{
		if (input.empty())
			return 0.0L;

		auto result = 0.0L;
		for (const auto &itt : input)
			result += itt;

		return result;
	}

	long double dot_prod_(
		const std::vector<double> &lhs,
		const std::vector<double> &rhs)
	{
		if (lhs.empty() || rhs.empty())
			return 0.0;
		if (lhs.size() != rhs.size())
			throw std::range_error("Input vectors are of different sizes");

		auto prod = 0.0L;

		for (size_t i = 0; i < lhs.size(); i++)
			prod += lhs[i] * rhs[i];

		return prod;
	}

	long double dot_prod_(
		const std::vector<double> &arg_1,
		const std::vector<double> &arg_2,
		const std::vector<double> &arg_3)
	{
		if (arg_1.empty() || arg_2.empty() || arg_3.empty())
			return 0.0;
		if (arg_1.size() != arg_2.size() || arg_1.size() != arg_3.size())
			throw std::range_error("Input vectors are of different sizes");

		auto prod = 0.0L;

		for (size_t i = 0; i < arg_1.size(); i++)
			prod += arg_1[i] * arg_2[i] * arg_3[i];

		return prod;
	}
}
double average(const std::vector<double> &input)
{
	if (input.empty())
		return nan("");
	return static_cast<double>(sum_(input) / input.size());
}

double sum(const std::vector<double> &input) 
{
	return static_cast<double>(sum_(input));
}

double dot_prod(
	const std::vector<double> &lhs,
	const std::vector<double> &rhs) 
{
	return static_cast<double>(dot_prod_(lhs, rhs));
}


void abs_diff(
	const std::vector<double> &lhs,
	double rhs,
	std::vector<double> &result) 
{
	if(lhs.size() != result.size())
		throw std::range_error("Input vectors are of different sizes");

	for (size_t i = 0; i < lhs.size(); i++)
		result[i] = fabs(lhs[i] - rhs);

	return;
}

double weighted_sg(
	const std::vector<double> &w,
	const std::vector<double> &x,
	double avg_x,
	double sum_w) 
{
	if (w.empty() || x.empty())
		return nan("");
	if(w.size() != x.size())
		throw std::range_error("Input vectors are of different sizes");

	auto prod = 0.0L;

	for (size_t i = 0; i < w.size(); i++) 
		prod += w[i] * pow(x[i] - avg_x, 2);

	return static_cast<double>(prod / sum_w);
}

std::vector<double> make_cov(
	const std::vector<double> &x,
	const std::vector<double> &y,
	const std::vector<double> &w_x,
	const std::vector<double> &w_y,
	double avg_x,
	double avg_y)
{
	if(x.empty() || y.empty() || w_x.empty() || w_y.empty())
	{
		return std::vector<double>(4, 0.0);
	}
	const auto size = x.size();
	if(size != y.size() || size != w_x.size() || size != w_y.size())
		throw std::range_error("Input vectors are of different sizes");

	const auto w_x_sum = sum_(w_x);
	const auto w_y_sum = sum_(w_y);

	std::vector<double> w_xy(size);
	for (size_t i = 0; i < size; i++)
		w_xy[i] = sqrt(0.5 * (pow(w_x[i], 2) + pow(w_y[i], 2)));
	
	auto w_xy_sum = 0.0L;
	for (size_t i = 0; i < size; i++)
		w_xy_sum += w_xy[i];

	const auto w_x_corr = pow(w_x_sum, 2) - dot_prod_(w_x, w_x);
	const auto w_y_corr = pow(w_y_sum, 2) - dot_prod(w_y, w_y);

	auto w_xy_corr = pow(w_xy_sum, 2);
	for (size_t i = 0; i < size; i++)
		w_xy_corr -= 0.5 * (pow(w_x[i], 2) + pow(w_y[i], 2));

	std::vector<double> result(4, 0.0);
	result[0] = static_cast<double>(w_x_sum * dot_prod_(w_x, x, x) / w_x_corr);
	result[1] = static_cast<double>(w_xy_sum * dot_prod_(w_xy, x, y) / w_xy_corr);
	result[2] = static_cast<double>(w_xy_sum * dot_prod_(w_xy, y, x) / w_xy_corr);
	result[3] = static_cast<double>(w_y_sum * dot_prod_(w_y, y, y) / w_y_corr);

	return result;
}