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
#' @param date_col Date column name (which is quasiquoted),
#'   to uniquely identify and order measurements.
#' @param obs_col Column containing magnitude difference between two rays.
#' @param itt_max \code{integer}, limits number of iterations to perform.
#'   Typically, \code{50} is enough.
#' @param eps \code{double}, controls the convergence.
#' @param ... Names of the extra columns inlude in the output.
#' Values are taken as the first element in the group.
#' @description Performs the same task as \code{Dipol2Red::sigma_2} using
#' \code{C++} implementation.
#' Much faster but requires calibration of data.
#' @return Computed polarization parameters in form of a \code{tibble}
#' @export
fsigma_2 <- function(data,
                      date_col = JD,
                      obs_col = Obs,
                      ...,
                      itt_max = 500L,
                      eps = 1e-16) {

  if (!is_tibble(data) && !is.data.frame(data)) {
    abort(
      glue(
        err_invalid_arg(),
        glue(
          err_cross(),
          "`data` has unsupported type of `{vec_ptype_abbr(data)}`.",
          .sep = " "
        ),
        glue(
          err_info(),
          "Allowed types are",
          "`{vec_ptype_abbr(tibble())}` and",
          "`{vec_ptype_abbr(data.frame())}`.",
          .sep = " "
        ),
        .sep = "\n ",
        .trim = FALSE
      ),
      class = "d2r_invalid_arg"
    )
  }

  itt_max <- vec_assert(
    vec_cast(itt_max, integer(), x_arg = "itt_max"),
    size = 1L,
    arg = "itt_max"
  )
  eps <- vec_assert(
    vec_cast(eps, double(), x_arg = "eps"),
    size = 1L,
    arg = "eps"
  )

  if (itt_max < 1L) {
    abort(
      glue(
        err_invalid_arg(),
        paste(
          err_cross(),
          "`itt_max` should be `>= 1`."
        )
      )
    )
  }
   if (eps <= 0) {
    abort(
      paste(
        err_invalid_arg(),
        paste(
          err_cross(),
          "`eps` should be `> 0`."
        ),
        sep = "\n "
      )
    )
  }

  extra_vars <-
    union(
      group_vars(data),
      names2(
        tidyselect::eval_select(
          rlang::expr(c(...)),
          data
        )
      )
    )
  indices <- group_indices(data)
  unique <- sort(unique(indices))

  idx <- map(unique, ~ which(indices == .x))

  mutate(
    fsigma_2_(
        data,
        date_col = as_name(ensym(date_col)),
        obs_col = as_name(ensym(obs_col)),
        what = idx,
        extra_vars = extra_vars,
        eps = eps,
        itt_max = itt_max),
      Q = as_list_of(Q))
}

#' correct_pol
#'
#' @param data Input \code{tibble} in \code{sigma_2}/\code{fsigma_2}-compatible format.
#' @param px Correction to \code{Px}.
#' @param py Correction to \code{Py}.
#' @param angle Correction to angle.
#'
#' @return Updated table.
#' @export
correct_pol <- function(data, px = 0, py = 0, angle = 0) {
    assert_that(is_tibble(data) || is.data.frame(data))
    assert_that(is.number(px))
    assert_that(is.number(py))
    assert_that(is.number(angle))
    correct_pol_(data, px, py, angle)
}

fsigma_2_ <- function(data, date_col, obs_col,
                            what,
                            extra_vars = NULL,
                            eps,
                            itt_max) {
  as_tibble(
    d2r_fsigma_2(
      data, date_col, obs_col,
      what,  extra_vars,
      eps, itt_max
    )
  )
}


correct_pol_ <- function(data, px = 0, py = 0, angle = 0) {
  as_tibble(
    d2r_correct_pol(
            data, px, py, angle
    )
  )
}
