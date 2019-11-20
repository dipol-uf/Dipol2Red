#include "dipol2red.h"
#include "vctrs_provider.h"

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
	preserved_cols.push_front(wrap(average_vector(arg)), "JD");
		
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