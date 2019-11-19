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
	std::vector<double> time(nrow);

	const auto arg = as<NumericVector>(data_frame[x_col]);
	const auto data = as<NumericVector>(data_frame[y_col]);
	
	mag_2_px_py(arg, data, idx, px, py, time);
	const auto preserved_cols = extract_extra_cols(extra_cols, data_frame, idx);

	auto result = List::create(
		_["jd"] = NumericVector(time.cbegin(), time.cend()),
		_["px"] = NumericVector(px.cbegin(), px.cend()),
		_["py"] = NumericVector(py.cbegin(), py.cend()));

	const auto preserved_cols_names = as<std::vector<std::string>>(preserved_cols.names());
	
	for(auto i = 0; i < preserved_cols.length(); ++i)
		result.push_back(preserved_cols[i], preserved_cols_names[i]);

	for(const auto &attr_name : data_frame.attributeNames())
	{
		if (attr_name != "class" && attr_name != "row.names" && attr_name != "names")
			result.attr(attr_name) = data_frame.attr(attr_name);
		else if(attr_name == "row.names" && Rf_inherits(data_frame, "tbl_df"))
		{
			std::vector<int> row_names(nrow);
			for (size_t i = 0; i < nrow; ++i)
				row_names[i] = i + 1;

			result.attr(attr_name) = row_names;
		}
	}

	result.attr("class") = data_frame.attr("class");
	
	return result;
}

void mag_2_px_py(
	const NumericVector &arg,
	const NumericVector &data,
	const IntegerVector &range,
	std::vector<double> &px,
	std::vector<double> &py,
	std::vector<double> &time)
{
	auto i = 0;
	auto temp_px = 0.0;
	auto temp_py = 0.0;
	auto temp_time = 0.0;
	for (auto itt : range)
	{
		temp_time += arg[itt - 1];
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
			time[i / batch_size] = 0.25 * temp_time;
			temp_time = 0.0;
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

	std::vector<int> batch_idx(idx.length() / batch_size);
	for (size_t i = 0; i < batch_idx.size(); ++i)
		batch_idx[i] = idx[batch_size * i];

	auto result = List::create();

	
	for (const auto& col : cols)
		result.push_back(subset_generic(data_frame[col], batch_idx), col);

	return result;
}

SEXP subset_generic(
	SEXP input,
	const std::vector<int> &idx)
{
	const auto pkg = Environment::namespace_env("vctrs");
	const auto vec_slice = as<Function>(pkg["vec_slice"]);
	return vec_slice(input, IntegerVector(idx.cbegin(), idx.cend()));
}