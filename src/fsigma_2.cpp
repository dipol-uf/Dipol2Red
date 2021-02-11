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

SEXP d2r_fsigma_2(
	SEXP input,
	SEXP date_col,
	SEXP obs_col,
	SEXP what,
	SEXP extra_vars,
	SEXP eps,
	SEXP itt_max)
{
	if (Rf_inherits(input, "data.frame") != TRUE)
		Rf_error("Input should be of type `data.frame` or compatible.");

	try 
	{
		const auto data_frame = as<DataFrame>(input);
		const auto x_col = as<std::string>(as<CharacterVector>(date_col)[0]);
		const auto y_col = as<std::string>(as<CharacterVector>(obs_col)[0]);
		const auto extra_cols = as<std::vector<std::string>>(extra_vars);
		const auto eps_val = as<double>(eps);
		const auto itt_max_val = as<int>(itt_max);

		auto idx_t = as<List>(what);
		const auto n_groups = idx_t.length();


		const auto arg = as<NumericVector>(data_frame[x_col]);
		const auto data = as<NumericVector>(data_frame[y_col]);

		arrange_data(idx_t, arg);
		
		std::vector<double> px(n_groups);
		std::vector<double> py(n_groups);
		std::vector<double> sg(n_groups);
		std::vector<int> itt(n_groups);
		std::vector<int> n(n_groups);
		std::vector<double> ratio(n_groups);
		List q(n_groups);

		for (auto i = 0; i < n_groups; i++)
		{
			const auto avg = sigma_2(data, as<IntegerVector>(idx_t[i]), eps_val, itt_max_val);
			px[i] = std::get<0>(avg);
			py[i] = std::get<1>(avg);
			sg[i] = std::get<2>(avg);
			itt[i] = std::get<3>(avg);
			n[i] = std::get<4>(avg);
			ratio[i] = std::get<5>(avg);

			q[i] = NumericMatrix(2, 2, std::get<6>(avg).cbegin());
		}

		const auto cols = extract_extra_cols(extra_cols, data_frame, idx_t);


		std::vector<std::string> names{ x_col, "Px", "Py", "P", "SG", "A", "SG_A", "Itt", "N", "Ratio", "Q" };
		List result(names.size() + extra_cols.size());
		names.reserve(result.length());

		for (const auto &it : extra_cols)
			names.push_back(it);
		result.names() = names;

		result[x_col] = average_arg(arg, idx_t);
		result["Px"] = wrap(px);
		result["Py"] = wrap(py);
		result["SG"] = wrap(sg);
		result["Itt"] = wrap(itt);
		result["N"] = wrap(n);
		result["Ratio"] = wrap(ratio);
		result["Q"] = q;


		for (const auto &it : extra_cols)
			result[it] = cols[it];

		postprocess_pol(result, 0.0, 0.0, 0.0);

		return result;
	}
	catch(exception &r_ex)
	{
		forward_rcpp_exception_to_r(r_ex);
	}
	catch(std::exception &ex)
	{
		forward_exception_to_r(ex);
	}
	catch(...)
	{
		Rf_error("Unknown error");
	}
	return R_NilValue;
}


SEXP d2r_correct_pol(
	SEXP data,
	SEXP px_corr,
	SEXP py_corr,
	SEXP angle_corr)
{
	if (Rf_inherits(data, "data.frame") != TRUE
		&& Rf_inherits(data, "list") != TRUE)
		Rf_error("Input should be of type `data.frame`, `list`, or compatible.");
	
	try
	{
		auto list_rep = as<List>(data);
		const auto px = as<double>(px_corr);
		const auto py = as<double>(py_corr);
		const auto angle = as<double>(angle_corr);

		postprocess_pol(list_rep, px, py, angle);

		return list_rep;
	}
	catch (exception &r_ex)
	{
		forward_rcpp_exception_to_r(r_ex);
	}
	catch (std::exception &ex)
	{
		forward_exception_to_r(ex);
	}
	catch (...)
	{
		Rf_error("Unknown error");
	}
	return R_NilValue;
}

avg_result sigma_2(
	const NumericVector &data,
	const IntegerVector &range,
	const double eps_val,
	const int itt_max)
{
	if (range.length() % batch_size != 0)
		throw std::logic_error("`what` should be divisible by 4.");

	const size_t nrow = range.length() / 4;

	std::vector<double> px(nrow);
	std::vector<double> py(nrow);

	mag_2_px_py(data, range, px, py);

	return nrow == 1
		? average_single(px, py)
		: average_multiple(px, py, eps_val, itt_max);
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
	const List &idx)
{
	if (cols.empty())
		return List::create();

	IntegerVector id_v(idx.length());

	for (auto i = 0; i < id_v.length(); i++)
		id_v[i] = as<IntegerVector>(idx[i])[0];
	
	List result(cols.size());
	const auto vec_slice = vctrs_provider::vec_slice();
	
	for (size_t i = 0; i < cols.size(); i++)
		result[i] = vec_slice(data_frame[cols[i]], id_v);
	result.names() = cols;
	
	return result;
}

void postprocess_pol(
	List &input,
	const double px_corr,
	const double py_corr,
	const double angle_corr)
{
	auto px = as<std::vector<double>>(input["Px"]);
	auto py = as<std::vector<double>>(input["Py"]);
	const auto sg = as<std::vector<double>>(input["SG"]);

	std::vector<double> a(px.size());
	std::vector<double> p(px.size());
	std::vector<double> sg_a(px.size());

	correct_pol(px, py, p, a, sg_a, sg, px_corr, py_corr, angle_corr);
	
	input["Px"] = wrap(px);
	input["Py"] = wrap(py);
	input["A"] = wrap(a);
	input["P"] = wrap(p);
	input["SG_A"] = wrap(sg_a);
}

NumericVector average_arg(
	const NumericVector &data,
	const List &idx)
{
	NumericVector result(idx.length());
	for(auto i = 0; i < result.length(); i++)
	{
		auto avg = 0.0;
		const auto vec = as<NumericVector>(idx[i]);
		for(const auto &it : vec)
			avg += data[it - 1];

		result[i] = avg / vec.size();
	}

	return result;	
}

void arrange_data(
	List &input, 
	const NumericVector &arg)
{
	const auto order = base_provider::order();
	const auto vec_slice = vctrs_provider::vec_slice();
	
	for(auto &writt : input)
	{
		const auto data = vec_slice(arg, as<IntegerVector>(writt));

		const auto order_id = as<IntegerVector>(order(data));

		writt = vec_slice(as<IntegerVector>(writt), order_id);
	}
}
