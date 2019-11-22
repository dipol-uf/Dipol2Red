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


#include "d2r_math.h"

constexpr auto std_init = 1e-100;

namespace {
	const long double pi = 4 * atanl(1.0);
	
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
	const double rhs,
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
	const double avg_x,
	const double sum_w) 
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
	const std::vector<double> &w_y)
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

avg_result average_single(
	const std::vector<double> &px,
	const std::vector<double> &py)
{
	if (px.size() != 1 || py.size() != 1)
		throw std::range_error("Vector of length > 1 provided where single values was expected.");

	return std::make_tuple(px[0], py[0], 0.0, 1, 1, 0.0, std::vector<double>(4, 0));
}

avg_result average_multiple(
	const std::vector<double> &px,
	const std::vector<double> &py,
	const double eps,
	const int itt_max)
{
	auto std = std_init;

	if (px.size() != py.size())
		throw std::range_error("Input vectors are of different sizes");

	const auto size = px.size();
	std::vector<double> wx(size, 1);
	std::vector<double> wy(size, 1);

	std::vector<double> dx(size, 0);
	std::vector<double> dy(size, 0);


	auto mean_px = average(px);
	auto mean_py = average(py);
	auto sg = 0.0;
	auto n_w = 0;
	auto i = 0;

	for (; i < itt_max; i++)
	{
		abs_diff(px, mean_px, dx);
		abs_diff(py, mean_py, dy);

		n_w = 0.0;
		for (size_t j = 0; j < size; j++)
		{
			if (dx[j] > 3 * std)
			{
				wx[j] = 0.0;
				n_w++;
			}
			else if (dx[j] > 2 * std)
			{
				wx[j] = 1 / pow(2 * dx[j] / std - 3, 2);
				n_w++;
			}
			else
				wx[j] = 1.0;

			if (dy[j] > 3 * std)
			{
				wy[j] = 0.0;
				n_w++;
			}
			else if (dy[j] > 2 * std)
			{
				wy[j] = 1 / pow(2 * dy[j] / std - 3, 2);
				n_w++;
			}
			else
				wy[j] = 1.0;
		}

		const auto sum_wx = sum(wx);
		const auto sum_wy = sum(wy);
		const auto comp_px = dot_prod(wx, px) / sum_wx;
		const auto comp_py = dot_prod(wy, py) / sum_wy;
		const auto sg_x = weighted_sg(wx, px, comp_px, sum_wx);
		const auto sg_y = weighted_sg(wy, py, comp_py, sum_wy);
		sg = sqrt((sg_x + sg_y) / (sum_wx + sum_wy - 2));
		std = sg * sqrt((sum_wx + sum_wy) / 2);
		const auto delta = sqrt((pow(comp_px - mean_px, 2) + pow(comp_py - mean_py, 2)) / 2);
		mean_px = comp_px;
		mean_py = comp_py;
		if (delta <= eps)
			break;
	}

	return std::make_tuple(mean_px, mean_py, sg, i,
		static_cast<int>(size),
		0.5 * n_w / static_cast<int>(size),
		make_cov(px, py, wx, wy));
}

void correct_pol(
	std::vector<double> &px,
	std::vector<double> &py,
	std::vector<double> &p,
	std::vector<double> &a,
	std::vector<double> &sg_a,
	const std::vector<double> &sg,
	double px_corr,
	double py_corr,
	double angle_corr)
{
	const auto size = px.size();
	if (py.size() != size || sg.size() != size || p.size() != size || a.size() != size || sg_a.size() != size)
		throw std::range_error("Input size mismatch.");

	for(size_t i = 0; i < size; i++)
	{
		px[i] += px_corr;
		py[i] += py_corr;
		a[i] = fmod(90.0 / pi  * atan2(py[i], px[i]) + angle_corr, 180.0);
		p[i] = sqrt(pow(px[i], 2) + pow(py[i], 2));
		sg_a[i] = 90.0 / pi * atan2(sg[i], p[i]);
	}
}