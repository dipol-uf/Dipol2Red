#' load_from_legacy_descriptor
#' @param desc Descriptors as output from \code{read_legacy_descriptor}.
#' @param root Root for the files in the descriptor.
#' @export
load_from_legacy_descriptor <- function(desc, root = ".") {
  data <-
    desc %>%
    mutate(
      Data = map(
        fs::path(root, File),
        read_csv,
        col_types = cols()
      ),
      Data = map(
        Data,
        ~set_names(.x, fix_names(names2(.x)))
      ) %>%
      as_list_of
    )

  with(
    data,
    map2_lgl(
        Count,
        Data,
        ~.x != vec_size(.y)
    )
  ) %>%
  which -> invalid

  if (vec_size(invalid) > 0L) {
    msg <- err_idx(invalid, "Data mismatch found at position(s):")
    abort(
      paste(
        "Read data does not match its descriptor.",
        paste(
          err_cross(),
          msg
        ),
        paste(
          err_info(),
          "Make sure descriptor contains up to date information."
        ),
        sep = "\n "
      ),
      class = "d2r_io_failed"
    )
  }

  data
}
