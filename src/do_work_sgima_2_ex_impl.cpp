#include "dipol2red.h"

using namespace Rcpp;

template<typename T>
void debug_print(const std::vector<T> &input)
{
	Rcout << "[ ";
	for(size_t i = 0; i < input.size(); i++)
	{
		if (i != 0)
		{
			Rcout << ", ";
			if (i % 10 == 0)
				Rcout << std::endl;
		}
		Rcout << input[i];
	}

	Rcout << " ]" << std::endl << std::endl;
}

// [[Rcpp:export]]
SEXP d2r_do_work_sigma_2_ex(
	SEXP input,
	SEXP date_col,
	SEXP obs_col,
	SEXP what,
	SEXP extra_vars,
	SEXP eps,
	SEXP itt_max)
{
	if (Rf_inherits(input, "data.frame") != TRUE)
		forward_rcpp_exception_to_r(exception("`input` is of unsupported type."));
	const auto data_frame = as<DataFrame>(input);
	const auto x_col = as<std::string>(as<CharacterVector>(date_col)[0]);
	const auto y_col = as<std::string>(as<CharacterVector>(obs_col)[0]);
	const auto extra_cols = as<std::vector<std::string>>(extra_vars);
	const auto eps_val = as<double>(eps);
	const auto itt_max_val = as<int>(itt_max);

	const auto idx_t = as<List>(what);
	const auto idx = as <IntegerVector>(idx_t[0]);
	
	if (idx.length() % batch_size != 0)
		forward_rcpp_exception_to_r(exception("`what` should be divisible by 4."));
	const size_t nrow = idx.length() / 4;
	
	std::vector<double> px(nrow);
	std::vector<double> py(nrow);

	const auto arg = as<NumericVector>(data_frame[x_col]);
	const auto data = as<NumericVector>(data_frame[y_col]);
	
	mag_2_px_py(data, idx, px, py);
	auto out_list = List();//extract_extra_cols(extra_cols, data_frame, idx);

	const auto avg = nrow == 1
		? average_single(px, py)
		: average_multiple(px, py, eps_val, itt_max_val);

	const auto q_vec = as<NumericVector>(avg["Q"]);
	const NumericMatrix q_mat(2, 2, q_vec.cbegin());
	
	out_list.push_back(List::create(q_mat), "Q");
	out_list.push_back(avg["Ratio"], "Ratio");
	out_list.push_back(avg["Itt"], "Itt");
	out_list.push_back(avg["SG"], "SG");
	out_list.push_back(avg["N"], "N");
	out_list.push_back(avg["Py"], "Py");
	out_list.push_back(avg["Px"], "Px");
	
	out_list.push_back(wrap(average_vector(arg)), "JD");
	postprocess_pol(out_list);
	return out_list;
}

void mag_2_px_py(
	const NumericVector &data,
	const IntegerVector &range,
	std::vector<double> &px,
	std::vector<double> &py)
{
	auto i = 0;
	auto temp_px_1 = 0.0;
	auto temp_px_2 = 0.0;
	auto temp_py_1 = 0.0;
	for (auto itt : range)
	{
		const auto rem = i % batch_size;
		switch (rem)
		{
		case 0:
			temp_px_1 = pow(10.0, 0.4 * data[itt - 1]);
			break;
		case 1:
			temp_py_1 = pow(10.0, 0.4 * data[itt - 1]);
			break;
		case 2:
			temp_px_2 = pow(10.0, 0.4 * data[itt - 1]);
			break;
		default:
			const auto temp_py_2 = pow(10.0, 0.4 * data[itt - 1]);
			const auto sum = temp_px_1 + temp_px_2 + temp_py_1 + temp_py_2;
			px[i / batch_size] = 100.0 * (temp_px_1 - temp_px_2) / sum;
			py[i / batch_size] = 100.0 * (temp_py_1 - temp_py_2) / sum;
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
		_["Ratio"] = wrap(0.0),
		_["Q"] = NumericVector(4, 0.0));
}

List average_multiple(
	const std::vector<double> &px, 
	const std::vector<double> &py,
	const double eps,
	const int itt_max)
{
	auto std = std_init;
	
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
	auto i = 0;

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
		const auto delta = sqrt((pow(comp_px - mean_px, 2) + pow(comp_py - mean_py, 2)) / 2);
		mean_px = comp_px;
		mean_py = comp_py;
		if (delta <= eps)
			break;
	}

	const auto cov = make_cov(px, py, wx, wy, mean_px, mean_py);
	return List::create(
		_["Px"] = wrap(mean_px),
		_["Py"] = wrap(mean_py),
		_["SG"] = wrap(sg),
		_["Itt"] = wrap(i),
		_["N"] = wrap(static_cast<int>(size)),
		_["Q"] = wrap(cov),
		_["Ratio"] = wrap(0.5 * n_w / static_cast<int>(size)));
}

void postprocess_pol(List &input)
{
	const auto px = as<std::vector<double>>(input["Px"]);
	const auto py = as<std::vector<double>>(input["Py"]);
	const auto sg = as<std::vector<double>>(input["SG"]);

	std::vector<double> a(px.size());
	std::vector<double> p(px.size());
	std::vector<double> sg_a(px.size());

	for (size_t i = 0; i < px.size(); i++)
	{
		a[i] = fmod(90.0 / PI * atan2(py[i], px[i]), 180.0);
		p[i] = sqrt(pow(px[i], 2) + pow(py[i], 2));
		sg_a[i] = 90.0 / PI * atan2(sg[i], p[i]);
	}
	input["A"] = wrap(a);
	input["P"] = wrap(p);
	input["SG_A"] = wrap(sg_a);
}