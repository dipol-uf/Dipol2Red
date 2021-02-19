utils::globalVariables(
  c(
    "temp_n[1]", "temp_n[2]", "Px", "Py",
    "SG", "N"
  )
)

#' H-test
#' @description Performs Hotelling T^2 test on the Dipol-2 averages
#' @param data Two-row input tibble
#' @param p_x Px polarization column name
#' @param p_y Py polarization column name
#' @param sg Sigma column name
#' @param cov Covariance column name
#' @param n Number of observations column name
#' @export

h_test <- function(data, p_x = Px, p_y = Py, sg = SG, cov = Q, n = N) {

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
  vec_assert(data, size = 2L, arg = "data")


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
    tibble(
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
h_test2 <- function(left, right, ..., id, px = Px, py = Py, n = N, q = Q) {
  if (!is_tibble(left) && !is.data.frame(left)) {
    abort(
      glue(
        err_invalid_arg(),
        glue(
          err_cross(),
          "`left` has unsupported type of `{vec_ptype_abbr(left)}`.",
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

   if (!is_tibble(right) && !is.data.frame(right)) {
    abort(
      glue(
        err_invalid_arg(),
        glue(
          err_cross(),
          "`right` has unsupported type of `{vec_ptype_abbr(right)}`.",
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
  if (vec_size(left) != vec_size(right)) {
    abort(
      glue(
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

  extra_cols <- tidyselect::eval_select(expr(c(...)), left) %>% names2

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
  } else {
    select(
      left, {{ id }}, {{ px }}, {{ py }}, {{ n }}, {{ q }}, 
      !!!extra_cols) -> left
    select(
      right, {{ id }}, {{ px }}, {{ py }}, {{ n }}, {{ q }},
      !!!extra_cols) -> right
  }

  bind_rows(left, right) %>% group_split({{ id }}) -> grouped_data

  if (some(grouped_data, ~ vec_size(.x) != 2L)) {
    # Ids are not unique, pairwise joining failed
    abort(
        "Ids should be unique and identify one row in each table",
        "dipol2red_invalid_argument")
  }

  acc_gen <- function(input) {
    compose(
      ~ slice(input, .x),
      ~ select(.x, {{ px }}, {{ py }}),
      flatten_dbl,
      .dir = "forward"
    )
  }
  map_dfr(
    grouped_data,
    function(dt) {
      n_col <- pull(dt, {{ n }})
      q_col <- pull(dt, {{ q }})
      acc <- acc_gen(dt)
      x_col <- map(1:2, acc)

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

      transmute(
        slice(dt, 1L),
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
