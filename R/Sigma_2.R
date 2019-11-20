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
#' @param bandInfo A \code{tibble} containing
#' filter descriptions.
#' @param ... Extra variables that should be preserved. Method preserves first element of the column
#' @param eqtrialCorrFactor Equatorial correction for the angle.
#' @param ittMax Maximum iterations to perform.
#' @param eps Tolerance. If average difference between previous and current step
#' is smaller than \code{eps}, execution stops.
#' @param date Name of the date column. Supports \code{rlang}
#' quasiquotiong.
#' @param obs Name of the observations column. Supports \code{rlang}
#' quasiquotiong.
#' @export
#' @aliases sigma_2
#' @importFrom dplyr %>% pull mutate group_by summarise if_else n select tbl_vars
#' @importFrom dplyr transmute group_map is_grouped_df bind_cols bind_rows one_of
#' @importFrom magrittr %<>% extract extract2 subtract
#' @importFrom rlang enquo !! is_null flatten_dbl as_function enquos is_empty is_character syms quo
#' @importFrom assertthat assert_that on_failure is.string is.number is.count on_failure<-
#' @importFrom vctrs vec_c vec_cast_common vec_size vec_recycle
#' @importFrom tidyselect vars_select
sigma_2 <- function(data,
                        filter = "B",
                        bandInfo = NULL,
                        ...,
                        eqtrialCorrFactor = 0.034907,
                        ittMax = 500L,
                        eps = 1e-16,
                        date = JD,
                        obs = Obs) {
    date <- enquo(date)
    obs <- enquo(obs)

    extra_vars <- tidyselect::vars_select(dplyr::tbl_vars(data), !!!enquos(...))
    

    assert_that(is_tibble(data))
    assert_that(is.string(filter))
    assert_that(is.number(eqtrialCorrFactor))
    assert_that(is.count(ittMax))
    assert_that(is.number(eps), eps > 0)

    if (rlang::is_null(bandInfo)) {
        if(!exists("BandInfo", envir = .GlobalEnv))
            data("BandInfo", package = "Dipol2Red", envir = .GlobalEnv)
        bandInfo <- BandInfo
    }

    assert_that(is_tibble(bandInfo))

    bandInfo %<>% filter(Filter == filter)

    p0 <- bandInfo %>% extract(1L, vec_c("Px", "Py")) %>% flatten_dbl
    a0 <- bandInfo %>% slice(1) %>% pull(Angle)

    if (is_grouped_df(data))
        result <- data %>%
            group_map(
                ~do_work_sigma_2(.x, !!date, !!obs, p0, a0, eqtrialCorrFactor, ittMax, eps, extra_vars = extra_vars) %>%
                    select(JD, Px, Py, P, SG, A, SG_A, Q, N, Ratio, Itt, one_of(extra_vars)) %>%
                    bind_cols(.y)) %>%
            bind_rows
    else
        result <- do_work_sigma_2(data, !!date, !!obs, p0, a0, eqtrialCorrFactor, ittMax, eps, extra_vars = extra_vars) %>%
               select(JD, Px, Py, P, SG, A, SG_A, Q, N, Ratio, Itt, one_of(extra_vars))

    return (result)
}

#' @rdname sigma_2
#' @export
Sigma_2 <- sigma_2

assertthat::on_failure(is_tibble) <- function(call, env) paste0("`", deparse(call[[2]]), "` is not a tibble")

dot_prod <- function(x, y) {
    vec_cast_common(x, y)
    sum(x * y)
}


do_work_sigma_2 <- function(data, date, obs, p0, a0,
                            eqtrialCorrFactor,
                            ittMax, eps,
                            get_px = ~100.0 * (.x[1] - .x[3]),
                            get_py = ~100.0 * (.x[2] - .x[4]),
                            extra_vars = NULL) {

    date <- enquo(date)
    obs <- enquo(obs)
    
    get_px <- as_function(get_px)
    get_py <- as_function(get_py)

    # `Magical` parameter.
    # Every 4 observations give 1 polarization measurement.
    nObsPerMes <- 4L
    err_msg <- paste("Input table should have a multiple of", nObsPerMes, "rows")
    assert_that(vec_size(data) %% nObsPerMes == 0L, msg = err_msg)

    # Store mean polarizations between iterations
    pxMean <- vec_recycle(0L, vec_size(data) / nObsPerMes)
    pyMean <- vec_recycle(0L, vec_size(data) / nObsPerMes)

    std <- 0
    delta <- 1e100

    summ_expr <- list(mJD = quo(mean(JD)), sQ = quo(sum(Q)), PX = quo(get_px(Q) / sQ), PY = quo(get_py(Q) / sQ))
    if (!is_empty(extra_vars) && is_character(extra_vars)) {
        preserve_vars <- extra_vars %>% set_names(.) %>% map(~quo((!!sym(.x))[1]))
    }
    else
        preserve_vars <- NULL

    trnsfData <- data %>%
        mutate(JD = !!date, Q = 10 ^ (0.4 * !!obs)) %>%
        mutate(Id = (1L:n() - 1L) %/% nObsPerMes + 1L) %>%
        group_by(Id)

    prepData <- trnsfData %>%
        summarise(!!!summ_expr, !!!preserve_vars) %>%
        mutate(PX = PX - p0[1], PY = PY - p0[2])


    if (vec_size(prepData) == 1L) {
        result <- prepData %>%
            rename(JD = mJD, Px = PX, Py = PY) %>%
            mutate(SG = 0, SG_A = 0, Cov = 0, N = 1L, Ratio = 0, Itt = 1L) %>%
            mutate(
                P = sqrt(Px ^ 2L + Py ^ 2L),
                A = (90 / pi * atan2(Py, Px) + a0) %% 180) %>%
            mutate(
                A = 90 / pi * A * eqtrialCorrFactor,
                Px = P * cos(pi / 90 * A),
                Py = P * sin(pi / 90 * A),
                Q = list(matrix(vec_recycle(NA_real_, 4L), ncol = 2L)))
    }

    else
        for (i in seq_len(ittMax)) {
            result <- prepData %>%
                mutate(WX = 1, WY = 1) %>%
                mutate(mPX = pxMean, mPY = pyMean) %>%
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
                }  %>%
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
                        ((mean(pxMean) - result$Px) ^ 2 +
                        (mean(pyMean) - result$Py) ^ 2) /
                        (vec_size(pxMean) + vec_size(pyMean)))
            

            pxMean <- vec_recycle(result$Px, vec_size(pxMean))
            pyMean <- vec_recycle(result$Py, vec_size(pyMean))

            result %<>%
                mutate(
                    A = 90 / pi * A * eqtrialCorrFactor,
                    Px = P * cos(pi / 90 * A),
                    Py = P * sin(pi / 90 * A))


            std <- result %>% pull(STD)

            if (delta <= eps)
                break

        }

    result
}