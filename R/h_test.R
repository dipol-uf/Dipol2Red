#' H-test
#' @description Performs Hotelling T^2 test on the Dipol-2 averages
#' @param data Two-row input tibble
#' @param p_x Px polarization column name
#' @param p_y Py polarization column name
#' @param sg Sigma column name
#' @param cov Covariance column name
#' @param n Number of observations column name
#' @export

h_test <- function(data, p_x = "Px", p_y = "Py", sg = "SG", cov = "Q", n = "N") {
  if (!tibble::is_tibble(data) && !is.data.frame(data)) {
    rlang::abort(
      glue::glue(
        err_invalid_arg(),
        glue::glue(
          err_cross(),
          "`data` has unsupported type of `{vec_ptype_abbr(data)}`.",
          .sep = " "
        ),
        glue::glue(
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
  vctrs::vec_assert(data, size = 2L, arg = "data")


  mean1 <- data %>%
    dplyr::select({{ p_x }}, {{ p_y }}) %>%
    dplyr::slice(1L) %>%
    rlang::flatten_dbl()
  mean2 <- data %>%
    dplyr::select({{ p_x }}, {{ p_y }}) %>%
    dplyr::slice(2L) %>%
    rlang::flatten_dbl()

  sigma1 <- data %>% dplyr::slice(1L) %>% dplyr::pull({{ cov }})
  sigma2 <- data %>% dplyr::slice(2L) %>% dplyr::pull({{ cov }})

  temp_n <- dplyr::pull(data, {{ n }})
  nu1 <- temp_n[1] - 1L
  nu2 <- temp_n[2] - 1L
  k <- 2L
  S <- 1.0 / (nu1 + nu2) * (nu1 * sigma1[[1]] + nu2 * sigma2[[1]])

  I_S <- solve(S)

  XY <- mean1 - mean2

  T2 <- temp_n[1] * temp_n[2] *
    as.numeric(t(XY) %*% I_S %*% (XY)) / (temp_n[1] + temp_n[2])
  f <- (temp_n[1] + temp_n[2] - 1 - k) * T2 /
    ((temp_n[1] + temp_n[2] - 2) * k)
  d1 <- k
  d2 <- temp_n[1] + temp_n[2] - 1L - k

  p <- stats::pf(f, d1, d2)
  p_inv <- stats::pf(f, d1, d2, lower.tail = FALSE)
  lgp <- stats::pf(f, d1, d2, log.p = TRUE) / log(10)
  lgp_inv <- stats::pf(f, d1, d2, lower.tail = FALSE, log.p = TRUE) / log(10)
  tibble::tibble(
    "T^2" = T2,
    "f" = f,
    "p" = p,
    "1-p" = p_inv,
    "lg(p)" = lgp,
    "lg(1-p)" = lgp_inv,
    "d1" = d1,
    "d2" = d2,
    "n" = temp_n[1] + temp_n[2]
  )
}

#' @title h_test2
#'
#' @param left,right Equal-sized \code{data.frame}s for pairwise testing
#' @param ... Additional columns to include to the output.
#' Value from the \code{left} \code{data.frame} is included.
#' @param id Id column to match entries. If absent, row numbers are used
#' (\code{tibble::rowid_to_column}).
#' @param px,py Column that contains value of Stokes \code{q} and \code{u},
#'  present in both tables/
#' @param n Column that contains number of data points used to average.
#' @param q Column that contains the specific covariance matrix 
#'  (as \code{list} of \code{matrix})
#'
#' @return A \code{data.frame} with test results
#' @export
h_test2 <- function(left, right, ..., id,
                    px = "Px", py = "Py", n = "N", q = "Q") {
  if (!tibble::is_tibble(left) && !is.data.frame(left)) {
    rlang::abort(
      glue::glue(
        err_invalid_arg(),
        glue::glue(
          err_cross(),
          "`left` has unsupported type of `{vec_ptype_abbr(left)}`.",
          .sep = " "
        ),
        glue::glue(
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

   if (!tibble::is_tibble(right) && !is.data.frame(right)) {
    rlang::abort(
      glue::glue(
        err_invalid_arg(),
        glue::glue(
          err_cross(),
          "`right` has unsupported type of `{vec_ptype_abbr(right)}`.",
          .sep = " "
        ),
        glue::glue(
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
  if (vctrs::vec_size(left) != vctrs::vec_size(right)) {
    rlang::abort(
      glue::glue(
        "Incompatible function arguments.",
        paste(
          err_cross(),
          "`left` and `right` should have equal sizes."
        ),
        "{err_info()} Size of `left` is `{vec_size(left)}`.",
        "{err_info()} Size of `right` is `{vec_size(right)}`.",
        .sep = "\n ",
        .trim = FALSE
      )
    )
  }

  extra_cols <- tidyselect::eval_select(rlang::expr(c(...)), left) %>%
    rlang::names2()

  if (rlang::is_missing(rlang::ensym(id))) {
    # No `id` column for joining, using row numbers
    dplyr::select(left, {{ px }}, {{ py }}, {{ n }}, {{ q }}, !!!extra_cols) -> left
    dplyr::select(right, {{ px }}, {{ py }}, {{ n }}, {{ q }}, !!!extra_cols) -> right
    if (vctrs::vec_in("rowid", rlang::names2(left)))
      rlang::abort(
        glue::glue(
          err_invalid_arg(),
          "{err_cross()} `rowid` column is already present.",
          "{err_info()} provide custom joining `id`.",
          .sep = "\n ",
          .trim = FALSE
        ),
        "dipol2red_invalid_argument"
      )

    left <- tibble::rowid_to_column(left)
    right <- tibble::rowid_to_column(right)
    id <- rlang::sym("rowid")
  } else {
    dplyr::select(
      left, {{ id }}, {{ px }}, {{ py }}, {{ n }}, {{ q }}, 
      !!!extra_cols) -> left
    dplyr::select(
      right, {{ id }}, {{ px }}, {{ py }}, {{ n }}, {{ q }},
      !!!extra_cols) -> right
  }

  dplyr::bind_rows(left, right) %>%
    dplyr::group_split({{ id }}) -> grouped_data

  if (purrr::some(grouped_data, ~ vctrs::vec_size(.x) != 2L)) {
    # Ids are not unique, pairwise joining failed
    rlang::abort(
      glue::glue(
        err_invalid_arg(),
        "{err_cross()} IDs are not unique.",
        "{err_info()} IDs should identify one row in each table.",
        .sep = "\n ",
        .trim = FALSE
      ),
      "dipol2red_invalid_argument"
    )
  }

  acc_gen <- function(input) {
    purrr::compose(
      ~ dplyr::slice(input, .x),
      ~ dplyr::select(.x, {{ px }}, {{ py }}),
      rlang::flatten_dbl,
      .dir = "forward"
    )
  }
  purrr::map_dfr(
    grouped_data,
    function(dt) {
      n_col <- dplyr::pull(dt, {{ n }})
      q_col <- dplyr::pull(dt, {{ q }})
      acc <- acc_gen(dt)
      x_col <- purrr::map(1:2, acc)

      q <- ((n_col[1] - 1L) * q_col[[1]] + (n_col[2] - 1L) * q_col[[2]]) / (n_col[1] + n_col[2] - 2L)
      s <- solve(q)
      df <- x_col[[1]] - x_col[[2]]

      t2 <- as.numeric(t(df) %*% s %*% df) * n_col[1] * n_col[2] / (n_col[1] + n_col[2])

      df1 <- 2L
      df2 <- n_col[1] + n_col[2] - df1 - 1L

      f <- df2 * t2 / (n_col[1] + n_col[2] - 2L) / df1

      p <- stats::pf(f, df1, df2)
      p_inv <- stats::pf(f, df1, df2, lower.tail = FALSE)
      lgp <- stats::pf(f, df1, df2, log.p = TRUE) / log(10)
      lgp_inv <- stats::pf(
        f, df1, df2,
        lower.tail = FALSE,
        log.p = TRUE
      ) / log(10)

      dplyr::transmute(
        dplyr::slice(dt, 1L),
        {{ id }},
        "T^2" = t2,
        "f" = f,
        "p" = p,
        "1-p" = p_inv,
        "lg(p)" = lgp,
        "lg(1-p)" = lgp_inv,
        "d1" = df1,
        "d2" = df2,
        "n" = n_col[1] + n_col[2],
        !!! extra_cols
      )
    })
}
