#   MIT License
#
#   Copyright(c) 2019
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

utils::globalVariables(c("n1", "n2"))

#' @title h_test
#' @description Performs Hotelling T^2 test on the Dipol-2 averages
#' @param data Two-row input tibble
#' @param p_x Px polarization column name
#' @param p_y Py polarization column name
#' @param sg Sigma column name
#' @param cov Covariance column name
#' @param n Number of observations column name
#' @importFrom assertthat assert_that
#' @importFrom rlang ensym !! flatten_dbl as_double
#' @importFrom vctrs vec_c %<-% vec_size
#' @importFrom dplyr %>% select slice 
#' @importFrom tibble tibble is_tibble
#' @importFrom stats pf
#' @importFrom magrittr extract2
#' @export

h_test <- function(data, p_x = Px, p_y = Py, sg = SG, cov = Q, n = N) {

    assert_that(is_tibble(data))
    assert_that(vec_size(data) == 2L)

    p_x <- ensym(p_x)
    p_y <- ensym(p_y)
    sg <- ensym(sg)
    cov <- ensym(cov)
    n <- ensym(n)

    mean1 <- data %>% select(!!p_x, !!p_y) %>% slice(1L) %>% flatten_dbl
    mean2 <- data %>% select(!!p_x, !!p_y) %>% slice(2L) %>% flatten_dbl

    sigma1 <- data %>% extract2(1, cov)
    sigma2 <- data %>% extract2(2, cov)

    c(n1, n2) %<-% (data %>% pull(!!n))

    nu1 <- n1 - 1L
    nu2 <- n2 - 1L
    k <- 2L
    S <- 1.0 / (nu1 + nu2) * (nu1 * sigma1 + nu2 * sigma2)

    I_S <- solve(S)

    XY <- mean1 - mean2

    T2 <- n1 * n2 * dot_prod(t(XY), I_S %*% (XY)) / (n1 + n2)
    f <- (n1 + n2 - 1 - k) * T2 / ((n1 + n2 - 2) * k)
    d1 <- k
    d2 <- n1 + n2 - 1L - k

    p <- pf(f, d1, d2)
    p_inv <- pf(f, d1, d2, lower.tail = FALSE)
    lgp <- pf(f, d1, d2, log.p = TRUE) / log(10)
    lgp_inv <- pf(f, d1, d2, lower.tail = FALSE, log.p = TRUE) / log(10)
    return(tibble("T^2" = T2, "f" = f,
        "p" = p, "1-p" = p_inv,
        "lg(p)" = lgp,
        "lg(1-p)" = lgp_inv,
        "d1" = d1, "d2" = d2, "n" = n1 + n2))
}

