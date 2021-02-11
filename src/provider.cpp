#include "provider.h"

using namespace Rcpp;

namespace vctrs_provider
{
	namespace
	{
		Environment vctrs_package_env = Environment::namespace_env("vctrs");
		Function vec_slice_impl = as<Function>(vctrs_package_env["vec_slice"]);
	}
	
	Function vec_slice()
	{
		return vec_slice_impl;
	}
}

namespace base_provider
{
	namespace
	{
		Environment base_package_env = Environment::base_env();
		Function order_impl = as<Function>(base_package_env["order"]);
	}

	Function order()
	{
		return order_impl;
	}
}