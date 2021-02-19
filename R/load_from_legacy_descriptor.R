#' load_from_legacy_descriptor
#' @param desc Descriptors as output from \code{read_legacy_descriptor}.
#' @param root Root for the files in the descriptor.
#' @export
load_from_legacy_descriptor <- function(desc, root = ".") {
  data <-
    desc %>%
    dplyr::mutate(
      Data = purrr::map(
        fs::path(root, .data$File),
        readr::read_csv,
        col_types = readr::cols()
      ),
      Data = purrr::map(
        .data$Data,
        ~rlang::set_names(.x, fix_names(rlang::names2(.x)))
      ) %>%
      vctrs::as_list_of()
    )

  dplyr::summarise(
    data,
    invalid = which(
      purrr::map2_lgl(
        .data$Count,
        .data$Data,
        ~.x != vctrs::vec_size(.y)
      )
    )
  ) %>%
  dplyr::pull("invalid") -> invalid


  if (vctrs::vec_size(invalid) > 0L) {
    msg <- err_idx(invalid, "Data mismatch found at position(s):")
    rlang::abort(
      glue::glue(
        "Read data does not match its descriptor.",
        "{err_cross()} {msg}",
        "{err_info()} Make sure descriptor contains up to date information.",
        .sep = "\n ",
        .trim = FALSE
      ),
      class = "d2r_io_failed"
    )
  }

  data
}
