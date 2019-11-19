#include "dipol2red.h"
using namespace Rcpp;



// [[Rcpp:export]]
SEXP d2r_do_work_sigma_2_ex(
	SEXP input,
	SEXP date_col,
	SEXP obs_col,
	SEXP what)
{
	if (Rf_inherits(input, "data.frame") != TRUE)
		forward_rcpp_exception_to_r(exception("`input` is of unsupported type."));
	const auto data_frame = as<DataFrame>(input);
	const auto x_col = as<std::string>(as<CharacterVector>(date_col)[0]);
	const auto y_col = as<std::string>(as<CharacterVector>(obs_col)[0]);
	const auto idx = as<IntegerVector>(what);

	if (idx.length() % 4 != 0)
		forward_rcpp_exception_to_r(exception("`what` should be divisible by 4."));

	std::vector<double> px(idx.length() / 4);
	std::vector<double> py(idx.length() / 4);

	mag_2_px_py(as<NumericVector>(data_frame[y_col]), idx.begin(), idx.end(), px, py);
	
	return List::create(
		_["px"] = NumericVector(px.begin(), px.end()),
		_["py"] = NumericVector(py.begin(), py.end()));
}

void mag_2_px_py(
	const NumericVector data,
	const Vector<INTSXP>::const_iterator begin,
	const Vector<INTSXP>::const_iterator end,
	std::vector<double> &px,
	std::vector<double> &py)
{
	auto i = 0;
	auto temp_px = 0.0;
	auto temp_py = 0.0;
	for(auto itt = begin; itt != end; ++itt)
	{
		const auto rem = i++ % 4;
		switch (rem)
		{
		case 0:
			temp_px = 100.0 * pow(10.0, 0.5 * data[*itt - 1]);
			break;
		case 1:
			temp_py = 100.0 * pow(10.0, 0.5 * data[*itt - 1]);
			break;
		case 2:
			temp_px -= 100.0 * pow(10.0, 0.5 * data[*itt - 1]);
			px[i / 4] = temp_px;
			break;
		default:
			temp_py -= 100.0 * pow(10.0, 0.5 * data[*itt - 1]);
			py[i / 4] = temp_py;
			break;
		}
	}
}