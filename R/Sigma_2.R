#   MIT License
#
#   Copyright(c) 2018-2019
#   Ilia Kosenkov [ilia.kosenkov.at.gm@gmail.com],
#   Vilppu Piirola
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

utils::globalVariables(vctrs::vec_c(
                       "Obs", "Id", "JD", "Q", "sQ", "PX", "PY",
                       "mPX", "mPY", ".", "dX", "WX", "dY", "WY",
                       "mJD", "Px", "Py", "SGx", "SGy", "SG", "P",
                       "NW", "A", "STD", "SG_A", "Cov", "N", "Ratio",
                       "Itt", "Angle", ".x", "BandInfo"))
#' @title sigma_2
#' @rdname sigma_2
#' @param data Input data in form of a \code{tibble}.
#' @param filter Filter name.
#' @param band_info A \code{tibble} containing
#' filter descriptions.
#' @param ... Extra variables that should be preserved. Method preserves first element of the column
#' @param eqtrialCorrFactor \emph{Deprecated since 0.4.1 and no longer used}.
#' @param itt_max Maximum iterations to perform.
#' @param eps Tolerance. If average difference between previous and current step
#' is smaller than \code{eps}, execution stops.
#' @param date Name of the date column. Supports \code{rlang}
#' quasiquotiong.
#' @param obs Name of the observations column. Supports \code{rlang}
#' quasiquotiong.
#' @param ittMax \emph{Deprecated since 0.4.1 in favor of \code{itt_max}}.
#' @param bandInfo \emph{Deprecated since 0.4.1 in favor of \code{band_info}}.
#' @export
#' @aliases sigma_2
sigma_2 <- function(data,
                        filter = "B",
                        band_info = NULL,
                        ...,
                        itt_max = 500L,
                        eps = 1e-16,
                        date = JD,
                        obs = Obs,
                        eqtrialCorrFactor = lifecycle::deprecated(),
                        bandInfo = lifecycle::deprecated(),
                        ittMax = lifecycle::deprecated()) {

    if(!is_missing(ittMax))
        lifecycle::deprecate_soft("0.4.1", "Dipol2Red::sigma_2(eqtrialCorrFactor =)",
                              details = "Parameter is no longer used")
    if (!is_missing(ittMax)) {
        lifecycle::deprecate_warn("0.4.1", "Dipol2Red::sigma_2(ittMax =)", "Dipol2Red::sigma_2(itt_max =)")
        itt_max <- ittMax
    }
    if (!is_missing(bandInfo)) {
        lifecycle::deprecate_warn("0.4.1", "Dipol2Red::sigma_2(bandInfo =)", "Dipol2Red::sigma_2(band_info =)")
        band_info <- bandInfo
    }

    date <- enquo(date)
    obs <- enquo(obs)

    extra_vars <- tidyselect::vars_select(dplyr::tbl_vars(data), !!!enquos(...))

    assert_that(is_tibble(data))
    assert_that(is.string(filter))
    assert_that(is.count(itt_max))
    assert_that(is.number(eps), eps > 0)

    if (rlang::is_null(band_info)) {
        if (!exists("BandInfo", envir = .GlobalEnv))
            data("BandInfo", package = "Dipol2Red", envir = .GlobalEnv)
        band_info <- BandInfo
    }

    assert_that(is_tibble(band_info))

    band_info %<>% filter(Filter == filter)
    p0 <- band_info %>% extract(1L, vec_c("Px", "Py")) %>% flatten_dbl
    a0 <- band_info %>% slice(1) %>% pull(Angle)

    if (is_grouped_df(data))
        result <- data %>%
            group_map(
                ~do_work_sigma_2(.x, !!date, !!obs, p0, a0, itt_max, eps, extra_vars = extra_vars) %>%
                    select(JD, Px, Py, P, SG, A, SG_A, Q, N, Ratio, Itt, one_of(extra_vars)) %>%
                    bind_cols(.y)) %>%
            bind_rows
    else
        result <- do_work_sigma_2(data, !!date, !!obs, p0, a0, itt_max, eps, extra_vars = extra_vars) %>%
               select(JD, Px, Py, P, SG, A, SG_A, Q, N, Ratio, Itt, one_of(extra_vars))

    return(result)
}

#' @rdname sigma_2
#' @export
Sigma_2 <- sigma_2

dot_prod <- function(x, y) {
    result <- vec_recycle_common(!!!vec_cast_common(x, y))
    sum(result[[1]] * result[[2]])
}

do_work_sigma_2 <- function(data, date, obs, p0, a0,
                            itt_max, eps,
                            get_px = ~100.0 * (.x[1] - .x[3]),
                            get_py = ~100.0 * (.x[2] - .x[4]),
                            extra_vars = NULL) {
    date <- enquo(date)
    obs <- enquo(obs)

    get_px <- as_function(get_px)
    get_py <- as_function(get_py)

    # `Magical` parameter.
    # Every 4 observations give 1 polarization measurement.
    n_obs_per_measure <- 4L
    err_msg <- paste("Input table should have a multiple of", n_obs_per_measure, "rows")
    assert_that(vec_size(data) %% n_obs_per_measure == 0L, msg = err_msg)

    # Store mean polarizations between iterations
    px_mean <- vec_recycle(0L, vec_size(data) / n_obs_per_measure)
    py_mean <- vec_recycle(0L, vec_size(data) / n_obs_per_measure)

    std <- 0
    delta <- 1e100

    summ_expr <- list(mJD = quo(mean(JD)), sQ = quo(sum(Q)), PX = quo(get_px(Q) / sQ), PY = quo(get_py(Q) / sQ))
    if (!is_empty(extra_vars) && is_character(extra_vars)) {
        preserve_vars <- extra_vars %>% set_names(.) %>% map(~quo((!!sym(.x))[1]))
    }
    else
        preserve_vars <- NULL

    transformed_data <- data %>%
        mutate(JD = !!date, Q = 10 ^ (0.4 * !!obs)) %>%
        arrange(JD) %>%
        mutate(Id = (1L:n() - 1L) %/% n_obs_per_measure + 1L) %>%
        group_by(Id)

    prepared_data <- transformed_data %>%
        summarise(!!!summ_expr, !!!preserve_vars) %>%
        mutate(PX = PX - p0[1], PY = PY - p0[2])

    if (vec_size(prepared_data) == 1L) {
        result <- prepared_data %>%
            rename(JD = mJD, Px = PX, Py = PY) %>%
            mutate(SG = 0, SG_A = 0, Cov = 0, N = 1L, Ratio = 0, Itt = 1L) %>%
            mutate(
                P = sqrt(Px ^ 2L + Py ^ 2L),
                A = (90 / pi * atan2(Py, Px) + a0) %% 180,
                Q = list(matrix(vec_recycle(NA_real_, 4L), ncol = 2L))) 
    }

    else
        for (i in seq_len(itt_max)) {
            result <- prepared_data %>%
                mutate(WX = 1, WY = 1) %>%
                mutate(mPX = px_mean, mPY = py_mean) %>%
                mutate(dX = abs(PX - mPX), dY = abs(PY - mPY)) %>% {
                    if (i > 1L) {
                        mutate(., WX = if_else(dX > 2 * std,
                            1 / (2 * dX / std - 3) ^ 2, WX)) %>%
                        mutate(WX = if_else(WX < 0.11, 0, WX)) %>%
                        mutate(WY = if_else(dY > 2 * std,
                            1 / (2 * dY / std - 3) ^ 2, WY)) %>%
                        mutate(WY = if_else(WY < 0.11, 0, WY))
                    }
                    else
                        .
                } %>%
                summarise(
                    JD = mean(mJD),
                    NW = sum(sum(WX < 1), sum(WY < 1)),
                    Px = dot_prod(WX, PX) / sum(WX),
                    Py = dot_prod(WY, PY) / sum(WY),
                    P = sqrt(Px ^ 2 + Py ^ 2),
                    A = (90 / pi * atan2(Py, Px) + a0) %% 180,
                    SGx = dot_prod(WX, c(Px - PX) ^ 2) / sum(WX),
                    SGy = dot_prod(WY, c(Py - PY) ^ 2) / sum(WY),
                    SG = sqrt((SGx + SGy) / (sum(WX) + sum(WY) - 2L)),
                    SG_A = 90 / pi * atan2(SG, P),
                    Q = list(generate_Q(PX, PY, WX, WY, Px, Py)),
                    STD = SG * sqrt((sum(WX) + sum(WY)) / 2),
                    Ratio = 0.5 * NW / nrow(.),
                    N = n(), Itt = i,
                    !!!preserve_vars)

            delta <- sqrt(
                        ((mean(px_mean) - result$Px) ^ 2 +
                        (mean(py_mean) - result$Py) ^ 2) /
                        (vec_size(px_mean) + vec_size(py_mean)))

            px_mean <- vec_recycle(result$Px, vec_size(px_mean))
            py_mean <- vec_recycle(result$Py, vec_size(py_mean))

            std <- result %>% pull(STD)

            if (delta <= eps)
                break

        }

    result
}