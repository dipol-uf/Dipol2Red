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
	const NumericMatrix q_mat(2, 2, std::get<6>(avg).cbegin());
	
	out_list.push_back(List::create(q_mat), "Q");
	out_list.push_back(wrap(std::get<5>(avg)), "Ratio");
	out_list.push_back(wrap(std::get<4>(avg)), "N");
	out_list.push_back(wrap(std::get<3>(avg)), "Itt");
	out_list.push_back(wrap(std::get<2>(avg)), "SG");
	out_list.push_back(wrap(std::get<1>(avg)), "Py");
	out_list.push_back(wrap(std::get<0>(avg)), "Px");
	
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