#   MIT License
#
#   Copyright(c) 2018-2019
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

#' @title fsigma_2
#'
#' @param data Input (possibly grouped) tibble/data.frame in long format.
#' @param date_col Date column name (which is quasiquoted), to uniquely identify and order measurements.
#' @param obs_col Column containing magnitude difference between two rays (quasiquoted)
#' @param itt_max \code{integer}, limits number of iterations to perform. Typically, \code{50} is enough.
#' @param eps \code{double}, controls the convergence.
#' @param ... Names of the extra columns (tidyselected) to include to the output.
#' Values are taken as the first element in the group.
#' @description Performs the same task as \code{Dipol2Red::sigma_2} using \code{C++} implementation.
#' Much faster but requires calibration of data.
#' @return Computed polarization parameters in form of a \code{tibble}
#' @export
fsigma_2 <- function(data,
                        date_col = JD,
                        obs_col = Obs,
                        ...,
                        itt_max = 500L,
                        eps = 1e-16) {

    assert_that(is_tibble(data) || is.data.frame(data))
    assert_that(is.count(itt_max))
    assert_that(is.number(eps), eps > 0)

    extra_vars <-
        union(
            group_vars(data),
            tidyselect::vars_select(dplyr::tbl_vars(data), !!!enquos(...)))


    indices <- group_indices(data)
    unique <- sort(unique(indices))

    idx <- map(unique, ~ which(indices == .x))

    fsigma_2_(
        data,
        as_name(ensym(date_col)),
        as_name(ensym(obs_col)),
        idx,
        extra_vars,
        eps,
        itt_max)
}

fsigma_2_ <- function(data, date_col, obs_col,
                            what,
                            extra_vars = NULL,
                            eps,
                            itt_max) {
    as_tibble(
        .Call(
            "d2r_fsigma_2",
            data, date_col, obs_col,
            what,  extra_vars,
            eps, itt_max))
}