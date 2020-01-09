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

utils::globalVariables(c("temp_n[1]", "temp_n[2]"))

#' @title h_test
#' @description Performs Hotelling T^2 test on the Dipol-2 averages
#' @param data Two-row input tibble
#' @param p_x Px polarization column name
#' @param p_y Py polarization column name
#' @param sg Sigma column name
#' @param cov Covariance column name
#' @param n Number of observations column name
#' @export

h_test <- function(data, p_x = Px, p_y = Py, sg = SG, cov = Q, n = N) {

    assert_that(is_tibble(data))
    assert_that(vec_size(data) == 2L)

    mean1 <- data %>% slice(1L) %>% select({{ p_x }}, {{ p_y }}) %>% flatten_dbl
    mean2 <- data %>% slice(2L) %>% select({{ p_x }}, {{ p_y }}) %>% flatten_dbl

    sigma1 <- data %>% slice(1L) %>% pull({{ cov }})
    sigma2 <- data %>% slice(2L) %>% pull({{ cov }})

    temp_n <- pull(data, {{ n }})
    nu1 <- temp_n[1] - 1L
    nu2 <- temp_n[2] - 1L
    k <- 2L
    S <- 1.0 / (nu1 + nu2) * (nu1 * sigma1[[1]] + nu2 * sigma2[[1]])

    I_S <- solve(S)

    XY <- mean1 - mean2

    T2 <- temp_n[1] * temp_n[2] * as.numeric(t(XY) %*% I_S %*% (XY)) / (temp_n[1] + temp_n[2])
    f <- (temp_n[1] + temp_n[2] - 1 - k) * T2 / ((temp_n[1] + temp_n[2] - 2) * k)
    d1 <- k
    d2 <- temp_n[1] + temp_n[2] - 1L - k

    p <- pf(f, d1, d2)
    p_inv <- pf(f, d1, d2, lower.tail = FALSE)
    lgp <- pf(f, d1, d2, log.p = TRUE) / log(10)
    lgp_inv <- pf(f, d1, d2, lower.tail = FALSE, log.p = TRUE) / log(10)
    return(tibble("T^2" = T2, "f" = f,
        "p" = p, "1-p" = p_inv,
        "lg(p)" = lgp,
        "lg(1-p)" = lgp_inv,
        "d1" = d1, "d2" = d2, "n" = temp_n[1] + temp_n[2]))
}

utils::globalVariables(c(
    "1-p", "T^2", "d1", "d2",
    "lg(1-p)", "lg(p)",
     "n1", "n2", "q1", "q2", "x1", "x2"))

#' @title h_test2
#'
#' @param left,right Equal-sized \code{data.frame}s for pairwise testing
#' @param ... Additional columns to include to the output.
#' Value from the \code{left} \code{data.frame} is included.
#' @param id Id column to match entries. If absent, row numbers are used
#' (\code{tibble::rowid_to_column}).
#' @param px,py Column that contains value of Stokes \code{q} and \code{u}, present in both tables/
#' @param n Column that contains number of data points used to average.
#' @param q Column that contains the specific covariance matrix (as \code{list} of \code{matrix})
#'
#' @return A \code{data.frame} with test results
#' @export
h_test2 <- function(left, right, ..., id, px = Px, py = Py, n = N, q = Q) {
    assert_that(is_tibble(left))
    assert_that(is_tibble(right))
    assert_that(vec_size(left) == vec_size(right))

    extra_cols <- ensyms(...)

    if (is_missing(ensym(id))) {
        # No `id` column for joining, using row numbers
        select(left, {{ px }}, {{ py }}, {{ n }}, {{ q }}, !!!extra_cols) -> left
        select(right, {{ px }}, {{ py }}, {{ n }}, {{ q }}, !!!extra_cols) -> right
        if (vec_in("rowid", names(left)))
            abort(
                "`rowid` column is already present, provide custom joining `id`",
                "dipol2red_invalid_argument")

        left <- rowid_to_column(left)
        right <- rowid_to_column(right)
        id <- sym("rowid")
    }
    else {
        select(left, {{ id }}, {{ px }}, {{ py }}, {{ n }}, {{ q }}, !!!extra_cols) -> left
        select(right, {{ id }}, {{ px }}, {{ py }}, {{ n }}, {{ q }}, !!!extra_cols) -> right
    }

    bind_rows(left, right) %>%
        group_split({{ id }}) -> grouped_data

    if (some(grouped_data, ~ vec_size(.x) != 2L)) {
        # Ids are not unique, pairwise joining failed
        abort(
            "Ids should be unique and identify one row in each table",
            "dipol2red_invalid_argument")
    }

    acc_gen <- function(input)
        compose(~slice(input, .x), ~ select(.x, {{ px }}, {{ py }}), flatten_dbl, .dir = "forward")

    map_dfr(grouped_data, function(dt) {
        pull(dt, {{ n }}) %->% c(n1, n2)
        pull(dt, {{ q }}) %->% c(q1, q2)

        acc <- acc_gen(dt)
        map(1:2, acc) %->% c(x1, x2)

        q <- ((n1 - 1L) * q1 + (n2 - 1L) * q2) / (n1 + n2 - 2L)
        s <- solve(q)
        df <- x1 - x2

        t2 <- as.numeric(t(df) %*% s %*% df) * n1 * n2 / (n1 + n2)

        df1 <- 2L
        df2 <- n1 + n2 - df1 - 1L

        f <- df2 * t2 / (n1 + n2 - 2L) / df1

        p <- pf(f, df1, df2)
        p_inv <- pf(f, df1, df2, lower.tail = FALSE)
        lgp <- pf(f, df1, df2, log.p = TRUE) / log(10)
        lgp_inv <- pf(f, df1, df2, lower.tail = FALSE, log.p = TRUE) / log(10)

        slice(dt, 1L) %>%
            transmute({{ id }},
                "T^2" = t2, "f" = f,
                "p" = p, "1-p" = p_inv,
                "lg(p)" = lgp,
                "lg(1-p)" = lgp_inv,
                "d1" = df1, "d2" = df2, "n" = n1 + n2,
                 !!!extra_cols) %>%
        # Temprorary solution to the column order issue present 
        # in `transmute` of the dev version of {dplyr}
        # https://github.com/tidyverse/dplyr/issues/4717
            select({{ id }},
                   `T^2`, `f`, `p`, `1-p`, `lg(p)`, `lg(1-p)`, `d1`, `d2`, `n`,
                   !!!extra_cols)
    })
}


`%->%` <- function(lhs, rhs) {
    deconstructor(lhs, {{ rhs }})
}

`%<-%` <- function(lhs, rhs) {
    deconstructor(rhs, {{lhs}})
}

deconstructor <- function(what, into) {
    q <- enquo(into)
    env <- quo_get_env(q)
    expr <- as.list(quo_get_expr(q))

    assert_that(expr[[1]] == sym("c"), msg = "Only `c` can be used to combine names")
    names <- expr[-1]

    assert_that(vec_size(what) == vec_size(names), msg = "LHS and RHS should have equal length")

    walk2(what, names, ~assign(as.character(.y), .x, envir = env))
}