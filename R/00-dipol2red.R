#' @useDynLib Dipol2Red
#' @importFrom Rcpp       sourceCpp
#'
#' @importFrom stringr    str_detect regex str_subset str_split str_match
#' @importFrom stringr    str_replace str_extract
#'
#' @importFrom dplyr      %>% case_when group_vars group_indices arrange ungroup
#' @importFrom dplyr      pull mutate group_by summarise if_else n select
#' @importFrom dplyr      transmute group_map is_grouped_df bind_cols bind_rows
#' @importFrom dplyr      across one_of tbl_vars slice rename group_split filter
#' @importFrom dplyr      slice_sample
#'
#' @importFrom rlang      as_name enquo !! !!! is_null as_function enquos
#' @importFrom rlang      is_empty is_character syms quo is_missing ensym
#' @importFrom rlang      ensyms quo_get_env quo_get_expr set_names sym abort
#' @importFrom rlang      is_double is_integer list2 names2 expr .data .env
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