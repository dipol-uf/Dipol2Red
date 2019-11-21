#' @useDynLib Dipol2Red
#' @importFrom Rcpp       sourceCpp
#' @importFrom vctrs      vec_c vec_rbind vec_is_empty vec_cast_common vec_size vec_recycle vec_recycle_common vec_slice
#' @importFrom purrr      map imap map_chr keep reduce
#' @importFrom stringr    str_detect regex str_subset str_split
#' @importFrom dplyr      %>% case_when group_vars group_indices arrange group_split
#' @importFrom dplyr      pull mutate group_by summarise if_else n select tbl_vars
#' @importFrom dplyr      transmute group_map is_grouped_df bind_cols bind_rows one_of
#' @importFrom forcats    as_factor
#' @importFrom rlang      as_name enquo !! !!! is_null flatten_dbl as_function enquos 
#' @importFrom rlang      is_empty is_character syms quo is_missing
#' @importFrom utils      glob2rx
#' @importFrom magrittr   %<>% extract subtract
#' @importFrom assertthat assert_that is.string is.number is.count
#' @importFrom tidyselect vars_select
#' @importFrom tibble     as_tibble

NULL