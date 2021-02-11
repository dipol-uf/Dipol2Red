#' @useDynLib Dipol2Red
#' @importFrom Rcpp       sourceCpp
#'
#' @importFrom vctrs      vec_c vec_in vec_rbind vec_is_empty vec_cast_common vec_size
#' @importFrom vctrs      vec_recycle vec_recycle_common vec_slice vec_cbind list_of as_list_of
#'
#' @importFrom purrr      map imap map_chr keep reduce flatten_chr flatten_dbl discard map_if
#' @importFrom purrr      walk2 map2_chr map_at some map_dfr compose
#'
#' @importFrom stringr    str_detect regex str_subset str_split str_match str_extract
#' @importFrom stringr    str_replace
#'
#' @importFrom dplyr      %>% case_when group_vars group_indices arrange group_split
#' @importFrom dplyr      pull mutate group_by summarise if_else n select tbl_vars slice rename
#' @importFrom dplyr      transmute group_map is_grouped_df bind_cols bind_rows one_of
#'
#' @importFrom forcats    as_factor
#'
#' @importFrom rlang      as_name enquo !! !!! is_null as_function enquos
#' @importFrom rlang      is_empty is_character syms quo is_missing ensym
#' @importFrom rlang      ensyms quo_get_env quo_get_expr set_names sym abort
#' @importFrom rlang      is_double is_integer
#'
#' @importFrom utils      glob2rx
#'
#' @importFrom magrittr   %<>% extract extract2 subtract
#'
#' @importFrom assertthat assert_that is.string is.number is.count is.readable
#'
#' @importFrom tidyselect vars_select
#'
#' @importFrom tibble     tibble is_tibble as_tibble rowid_to_column
#'
#' @importFrom stats      pf
#'
#' @importFrom readr      read_csv cols read_lines parse_integer
#'
#' @importFrom utils      tail
#'
#' @importFrom glue       glue glue_collapse
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