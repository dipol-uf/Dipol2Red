#   MIT License
#
#   Copyright(c) 2018
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

utils::globalVariables(c(
                       "Obs", "Id", "JD", "Q", "sQ", "PX", "PY",
                       "mPX", "mPY", ".", "dX", "WX", "dY", "WY",
                       "mJD", "Px", "Py", "SGx", "SGy", "SG", "P",
                       "NW", "A", "STD", "SG_A", "Cov", "N", "Ratio",
                       "Itt"))
#' @title Sigma_2
#' @param data Input data in form of a \code{tibble}.
#' @param bandInfo A row from a \code{tibble} containing
#' filter descriptions.
#' @param eqtrialCorrFactor Equatorial correction for the angle.
#' @param ittMax Maximum iterations to perform.
#' @param eps Tolerance. If average difference between previous and current step
#' is smaller than \code{eps}, execution stops.
#' @param date Name of the date column. Supports \code{rlang}
#' quasiquotiong.
#' @param obs Name of the observations column. Supports \code{rlang}
#' quasiquotiong.
#' @export
#' @importFrom dplyr %>% pull mutate group_by summarise if_else n select
#' @importFrom dplyr transmute
#' @importFrom magrittr %<>% extract extract2 subtract
#' @importFrom rlang enquo !!
#' @importFrom assertthat assert_that on_failure is.number is.count on_failure<-
Sigma_2 <- function(data,
                        bandInfo = NULL,
                        eqtrialCorrFactor = 0.034907,
                        ittMax = 500L,
                        eps = 1e-16,
                        date = JD,
                        obs = Obs) {
    date <- enquo(date)
    obs <- enquo(obs)

    on_failure(is_tibble) <- function(call, env) paste0("`", deparse(call[[2]]), "` is not a tibble")
    assert_that(is_tibble(data))
    assert_that(is_tibble(bandInfo))

    assert_that(is.number(eqtrialCorrFactor))
    assert_that(is.count(ittMax))
    assert_that(is.number(eps), eps > 0)

    # `Magical` parameter.
    # Every 4 observations give 1 polarization measurement.
    nObsPerMes <- 4
    err_msg <- paste("Input table should have a multiple of", nObsPerMes, "rows")
    assert_that(nrow(data) %% nObsPerMes == 0, msg = err_msg)

    GetPX <- function(x)
        100.0 * (x[1] - x[3])

    GetPY <- function(x)
        100.0 * (x[2] - x[4])

    p0 <- bandInfo %>% extract(1, c("Px", "Py")) %>% as.numeric 
    a0 <- bandInfo %>% pull("Angle")
    # Store mean polarizations between iterations
    pxMean <- rep(0, nrow(data) / nObsPerMes)
    pyMean <- rep(0, nrow(data) / nObsPerMes)

    std <- 0
    delta <- 1e100

    trnsfData <- data %>%
        transmute(JD = !!date, Q = 10 ^ (0.4 * !!obs)) %>%
        mutate(Id = (1:n() - 1) %/% nObsPerMes) %>%
        mutate(Id = as.integer(Id) + 1L) %>%
        group_by(Id)

    prepData <- trnsfData %>%
        summarise(mJD = mean(JD), sQ = sum(Q),
                  PX = GetPX(Q) / sQ,
                  PY = GetPY(Q) / sQ) %>%
        mutate(PX = PX - p0[1], PY = PY - p0[2])

    if (nrow(prepData) == 1L) {
        result <- prepData %>%
            transmute(JD = mJD, Px = PX, Py = PY) %>%
            mutate(SG = 0, SG_A = 0, Cov = 0, N = 1L, Ratio = 0, Itt = 1L) %>%
            mutate(
                P = sqrt(Px ^ 2 + Py ^ 2),
                A = (90 / pi * atan2(Py, Px) + a0) %% 180) %>%
            mutate(
                A = 90 / pi * A * eqtrialCorrFactor,
                Px = P * cos(pi / 90 * A),
                Py = P * sin(pi / 90 * A))
    }

    else
        for (i in seq(1, ittMax, by = 1)) {
            result <- prepData %>%
                mutate(WX = 1, WY = 1) %>%
                mutate(mPX = pxMean, mPY = pyMean) %>%
                mutate(dX = abs(PX - mPX), dY = abs(PY - mPY)) %>% {
                    if (i > 1) {
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
                    Px = as.numeric(WX %*% PX / sum(WX)),
                    Py = as.numeric(WY %*% PY / sum(WY)),
                    P = sqrt(Px ^ 2 + Py ^ 2),
                    A = (90 / pi * atan2(Py, Px) + a0) %% 180,
                    SGx = as.numeric(WX %*% c(Px - PX) ^ 2 / sum(WX)),
                    SGy = as.numeric(WY %*% c(Py - PY) ^ 2 / sum(WY)),
                    SG = sqrt((SGx + SGy) / (sum(WX) + sum(WY) - 2L)),
                    SG_A = 90 / pi * atan2(SG, P),
                    #Cov = as.numeric(sqrt(WX * WY) %*%
                        #(c(Px - PX) * c(Py - PY)) /
                            #sqrt(sum(WX) * sum(WY))),
                    Q = list(generate_Q(PX, PY, WX, WY, Px, Py)),
                    STD = SG * sqrt((sum(WX) + sum(WY)) / 2),
                    Ratio = 0.5 * NW / nrow(.),
                    N = n(),
                    Itt = i)

            delta <- sqrt(
                        ((mean(pxMean) - result$Px) ^ 2 +
                        (mean(pyMean) - result$Py) ^ 2) /
                        (length(pxMean) + length(pyMean)))

            pxMean <- rep(result$Px, length(pxMean))
            pyMean <- rep(result$Py, length(pyMean))

            result %<>%
                mutate(
                    A = 90 / pi * A * eqtrialCorrFactor,
                    Px = P * cos(pi / 90 * A),
                    Py = P * sin(pi / 90 * A))


            std <- result %>% pull(STD)

            if (delta <= eps)
                break

        }


    return(result %>%
              #select(JD, Px, Py, P, SG, A, SG_A, Cov, Q, N, SGx, SGy))

               select(JD, Px, Py, P, SG, A, SG_A, Q, N, Ratio, Itt))
}