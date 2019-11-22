#   MIT License
#
#   Copyright(c) 2019
#   Ilia Kosenkov [ilia.kosenkov.at.gm@gmail.com],
#
#   Permission is hereby granted, free of charge, to any person obtaining a copy
#   of this software and associated documentation files(the "Software"), to deal
#   in the Software without restriction, including without limitation the rights
#   to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
#   copies of the Software, and to permit persons to whom the Software is
#   furnished to do so, subject to the following conditions:
#
#   The above copyright notice and this permission
#   notice shall be included in all
#   copies or substantial portions of the Software.
#
#   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
#   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
#   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
#   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH
#   THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#' @title generate_Q
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

    assert_that(is_numeric(p_x))
    assert_that(is_numeric(p_y))
    assert_that(is_numeric(w_x))
    assert_that(is_numeric(w_y))

    assert_that(is.number(p_x_mean))
    assert_that(is.number(p_y_mean))

    x <- p_x - p_x_mean
    y <- p_y - p_y_mean

    w_x_sum <- sum(w_x)
    w_y_sum <- sum(w_y)
    w_xy_sum <- sum(sqrt(0.5 * (w_x ^ 2 + w_y ^ 2)))

    w_x_corr <- w_x_sum ^ 2 - sum(w_x ^ 2)
    w_y_corr <- w_y_sum ^ 2 - sum(w_y ^ 2)
    w_xy_corr <- w_xy_sum ^ 2 - 0.5 * sum(w_x ^ 2 + w_y ^ 2)

    mtrx <- matrix(vec_c(
            w_x_sum * dot_prod(w_x, (x * x)) / w_x_corr,
            w_xy_sum * dot_prod(sqrt(0.5 * (w_y ^ 2 + w_x ^ 2)), (y * x)) / w_xy_corr,
            w_xy_sum * dot_prod(sqrt(0.5 * (w_x ^ 2 + w_y ^ 2)), (x * y)) / w_xy_corr,
            w_y_sum * dot_prod(w_y, (y * y)) / w_y_corr
        ), ncol = 2L)


    return(mtrx)
}

is_numeric <- function(x) is_double(x) || is_integer(x)