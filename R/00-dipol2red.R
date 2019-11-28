#' @useDynLib Dipol2Red
#' @importFrom Rcpp       sourceCpp
#' @importFrom vctrs      vec_c vec_in vec_rbind vec_is_empty vec_cast_common vec_size vec_recycle vec_recycle_common vec_slice vec_cbind
#' @importFrom purrr      map imap map_chr keep reduce flatten_chr flatten_dbl discard map_if walk2 map2_chr map_at
#' @importFrom stringr    str_detect regex str_subset str_split str_match str_extract str_replace
#' @importFrom dplyr      %>% case_when group_vars group_indices arrange group_split
#' @importFrom dplyr      pull mutate group_by summarise if_else n select tbl_vars slice rename
#' @importFrom dplyr      transmute group_map is_grouped_df bind_cols bind_rows one_of
#' @importFrom forcats    as_factor
#' @importFrom rlang      as_name enquo !! !!! is_null as_function enquos is_double set_names
#' @importFrom rlang      is_empty is_character syms quo is_missing ensym is_integer sym
#' @importFrom utils      glob2rx
#' @importFrom magrittr   %<>% extract extract2 subtract
#' @importFrom assertthat assert_that is.string is.number is.count is.readable
#' @importFrom tidyselect vars_select
#' @importFrom tibble     tibble is_tibble as_tibble
#' @importFrom stats      pf
#' @importFrom readr      read_csv cols read_lines parse_integer
#' @importFrom utils tail


NULL