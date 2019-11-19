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
	const auto extra_cols = as<CharacterVector>(extra_vars);
	
	if (idx.length() % 4 != 0)
		forward_rcpp_exception_to_r(exception("`what` should be divisible by 4."));
	const auto nrow = idx.length() / 4;
	
	std::vector<double> px(nrow);
	std::vector<double> py(nrow);
	std::vector<double> time(nrow);

	auto data = as<NumericVector>(data_frame[y_col]);
	auto arg = as<NumericVector>(data_frame[x_col]);
	
	mag_2_px_py(arg, data, idx.cbegin(), idx.cend(), px, py, time);
	
	return DataFrame::create(
		_["jd"] = NumericVector(time.cbegin(), time.cend()),
		_["px"] = NumericVector(px.cbegin(), px.cend()),
		_["py"] = NumericVector(py.cbegin(), py.cend()));
}

void mag_2_px_py(
	NumericVector &arg,
	NumericVector &data,
	const Rcpp::Vector<INTSXP>::const_iterator begin,
	const Rcpp::Vector<INTSXP>::const_iterator end,
	std::vector<double> &px,
	std::vector<double> &py,
	std::vector<double> &time)
{
	auto i = 0;
	auto temp_px = 0.0;
	auto temp_py = 0.0;
	auto temp_time = 0.0;
	for(auto itt = begin; itt != end; ++itt)
	{
		temp_time += arg[*itt - 1];
		const auto rem = i % 4;
		switch (rem)
		{
		case 0:
			temp_px = pow(10.0, 0.4 * data[*itt - 1]);
			break;
		case 1:
			temp_py = pow(10.0, 0.4 * data[*itt - 1]);
			break;
		case 2:
			temp_px -= pow(10.0, 0.4 * data[*itt - 1]);
			px[i / 4] = 100.0 * temp_px;
			break;
		default:
			temp_py -= pow(10.0, 0.4 * data[*itt - 1]);
			py[i / 4] = 100.0 * temp_py;
			time[i / 4] = 0.25 * temp_time;
			temp_time = 0.0;
			break;
		}
		++i;
	}
}