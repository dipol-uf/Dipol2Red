#' Generate weighted variance.
#' @description Generates covariance matrix from Dipol data
#' @param p_x Vector of stokes parameters
#' @param p_y Vector of stoke parameters
#' @param w_x Vector of weights
#' @param w_y Vector of weights
#' @param p_x_mean Weighted average stokes parameter
#' @param p_y_mean Weighted average stokes parameter
#'
#' @return Covariance matrix
#' @export
generate_Q <- function(p_x, p_y, w_x, w_y, p_x_mean, p_y_mean) {

  p_x <- vec_cast(p_x, double(), x_arg = "p_x")
  p_y <- vec_cast(p_y, double(), x_arg = "p_y")
  w_x <- vec_cast(w_x, double(), x_arg = "w_x")
  w_x <- vec_cast(w_y, double(), x_arg = "w_y")

  p_x_mean <- vec_cast(
    vec_assert(p_x_mean, size = 1L),
    double(),
    x_arg = "p_x_mean"
  )
  p_y_mean <- vec_cast(
    vec_assert(p_y_mean, size = 1L),
    double(),
    x_arg = "p_y_mean"
  )

  x <- p_x - p_x_mean
  y <- p_y - p_y_mean

  w_x_sum <- sum(w_x)
  w_y_sum <- sum(w_y)
  w_xy <- sqrt(w_x ^ 2 + w_y ^ 2)
  w_xy_sum <- sum(w_xy)

  w_x_corr <- w_x_sum ^ 2 - sum(w_x ^ 2)
  w_y_corr <- w_y_sum ^ 2 - sum(w_y ^ 2)
  w_xy_corr <- w_xy_sum ^ 2 - sum(w_xy ^ 2)

  matrix(
    vec_c(
      w_x_sum * dot_prod(w_x, (x * x)) / w_x_corr,
      w_xy_sum * dot_prod(w_xy, (y * x)) / w_xy_corr,
      w_xy_sum * dot_prod(w_xy, (x * y)) / w_xy_corr,
      w_y_sum * dot_prod(w_y, (y * y)) / w_y_corr
    ),
    ncol = 2L
  )
}