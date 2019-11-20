#include "dipol2red.h"

using namespace Rcpp;

// [[Rcpp:export]]
SEXP d2r_do_work_sigma_2_ex(
	SEXP input,
	SEXP date_col,
	SEXP obs_col,
	SEXP what,
	SEXP extra_vars)
{
	if (Rf_inherits(input, "data.frame") != TRUE)
		forward_rcpp_exception_to_r(exception("`input` is of unsupported type."));
	const auto data_frame = as<DataFrame>(input);
	const auto x_col = as<std::string>(as<CharacterVector>(date_col)[0]);
	const auto y_col = as<std::string>(as<CharacterVector>(obs_col)[0]);
	const auto idx = as<IntegerVector>(what);
	const auto extra_cols = as<std::vector<std::string>>(extra_vars);

	if (idx.length() % batch_size != 0)
		forward_rcpp_exception_to_r(exception("`what` should be divisible by 4."));
	const size_t nrow = idx.length() / 4;
	
	std::vector<double> px(nrow);
	std::vector<double> py(nrow);

	const auto arg = as<NumericVector>(data_frame[x_col]);
	const auto data = as<NumericVector>(data_frame[y_col]);
	
	mag_2_px_py(arg, data, idx, px, py);
	auto preserved_cols = extract_extra_cols(extra_cols, data_frame, idx);

	const auto avg = nrow == 1
		? average_single(px, py)
		: average_multiple(px, py);

	preserved_cols.push_back(avg["Ratio"], "Ratio");
	preserved_cols.push_back(avg["Itt"], "Itt");
	preserved_cols.push_back(avg["SG"], "SG");
	preserved_cols.push_back(avg["N"], "N");
	preserved_cols.push_back(avg["Py"], "Py");
	preserved_cols.push_back(avg["Px"], "Px");
	
	preserved_cols.push_back(wrap(average_vector(arg)), "JD");
		
	return preserved_cols;
}

void mag_2_px_py(
	const NumericVector &arg,
	const NumericVector &data,
	const IntegerVector &range,
	std::vector<double> &px,
	std::vector<double> &py)
{
	auto i = 0;
	auto temp_px = 0.0;
	auto temp_py = 0.0;
	for (auto itt : range)
	{
		const auto rem = i % batch_size;
		switch (rem)
		{
		case 0:
			temp_px = pow(10.0, 0.4 * data[itt - 1]);
			break;
		case 1:
			temp_py = pow(10.0, 0.4 * data[itt - 1]);
			break;
		case 2:
			temp_px -= pow(10.0, 0.4 * data[itt - 1]);
			px[i / batch_size] = 100.0 * temp_px;
			break;
		default:
			temp_py -= pow(10.0, 0.4 * data[itt - 1]);
			py[i / batch_size] = 100.0 * temp_py;
			break;
		}
		++i;
	}
}


List extract_extra_cols(
	const std::vector<std::string> &cols,
	const DataFrame &data_frame,
	const IntegerVector &idx)
{
	if (cols.empty())
		return List::create();

	auto result = List::create();
	const auto vec_slice = vctrs_provider::vec_slice();
	
	for (const auto& col : cols)
		result.push_back(vec_slice(data_frame[col], idx[0]), col);

	return result;
}

double average_vector(const NumericVector &input)
{
	if (input.length() == 0)
		return nan("");

	long double sum = 0.0;
	for (const auto &itt : input)
		sum += itt;

	return static_cast<double>(sum / input.length());
}

List average_single(const std::vector<double> &px, const std::vector<double> &py)
{
	if (px.size() != 1 || py.size() != 1)
	{
		forward_rcpp_exception_to_r(exception("Vector of length > 1 provided where single values was expected."));
		return R_NilValue;
	}

	return List::create(
		_["Px"] = wrap(px[0]),
		_["Py"] = wrap(py[0]),
		_["SG"] = wrap(0.0),
		_["Itt"] = wrap(1),
		_["N"] = wrap(1),
		_["Ratio"] = wrap(0.0));
}

List average_multiple(
	const std::vector<double> &px, 
	const std::vector<double> &py)
{
	const size_t itt_max = 15;
	auto std = 1e100;
	auto delta = 1e100;
	const auto eps = 1e-6;
	
	if (px.size() !=  py.size() )
	{
		forward_exception_to_r(std::range_error("Input vectors are of different sizes"));
		return R_NilValue;
	}

	const auto size = px.size();
	std::vector<double> wx(size,1);
	std::vector<double> wy(size, 1);

	std::vector<double> dx(size, 0);
	std::vector<double> dy(size, 0);

	
	auto mean_px = average(px);
	auto mean_py = average(py);
	auto sg = 0.0;
	auto n_w = 0;
	size_t i = 0;
	for(; i < itt_max; i++)
	{
		abs_diff(px, mean_px, dx);
		abs_diff(py, mean_py, dy);

		n_w = 0.0;
		for(size_t j = 0; j < size; j++)
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
		delta = sqrt((pow(comp_px - mean_px, 2) + pow(comp_py - mean_py, 2)) / 2);
		mean_px = comp_px;
		mean_py = comp_py;

		if (delta <= eps)
			break;
	}
	
	return List::create(
		_["Px"] = wrap(mean_px),
		_["Py"] = wrap(mean_py),
		_["SG"] = wrap(sg),
		_["Itt"] = wrap(i),
		_["N"] = wrap(static_cast<int>(size)),
		_["Ratio"] = wrap(0.5 * n_w / static_cast<int>(size)));
}