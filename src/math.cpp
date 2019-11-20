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
	if (lhs.empty() || rhs.empty())
		return 0.0;
	if (lhs.size() != rhs.size())
		throw std::range_error("Input vectors are of different sizes");

	auto prod = 0.0L;

	for (size_t i = 0; i < lhs.size(); i++)
		prod += lhs[i] * rhs[i];

	return static_cast<double>(prod / lhs.size());
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