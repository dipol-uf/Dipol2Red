#' @useDynLib Dipol2Red
#' @importFrom Rcpp       sourceCpp
#'
#' @importFrom dplyr      %>%
#'
#' @importFrom rlang      .data .env
#'
#'
NULL

has_crayon <- function() {
  requireNamespace("crayon", quietly = TRUE) &&
    crayon::has_color()
}

err_invalid_arg <- function() "Invalid function argument."

err_cross <- function() {
  if (has_crayon()) {
    crayon::red("x")
  } else {
    "x"
  }
}

err_info <- function() {
  if (has_crayon()) {
    crayon::yellow("i")
  } else {
    "i"
  }
}

err_ok <- function() {
  if (has_crayon()) {
    crayon::green("v")
  } else {
    "v"
  }
}

err_idx <- function(idx, msg = "Invalid values at position(s):", max = 5L) {
  len <- vctrs::vec_size(idx)
  if (len > 0) {
    n <- min(len, vctrs::vec_cast(max, integer()))
    result <- glue::glue_collapse(
      glue::glue("`{vctrs::vec_slice(idx, seq_len(n))}`"),
      sep = ", "
    )

    if (n < len) {
      result <- glue::glue("{result}, etc")
    }

    glue::glue("{msg} {result}.")
  } else {
    glue::glue("")
  }

}