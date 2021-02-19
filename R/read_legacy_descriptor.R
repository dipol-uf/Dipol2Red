#' Read legacy descriptor
#' @param path Path to the descriptor file (e.g. \code{Vin.txt})
#' @export
read_legacy_descriptor <- function(path) {
  if (
    !fs::file_access(
      vctrs::vec_cast(path, to = fs::path(), x_arg = "path"),
      vctrs::vec_c("exists", "readable")
    )
  ) {
    rlang::abort(
      glue::glue(
        err_invalid_arg(),
        "{err_cross()} `path` should point to a readable file.",
        glue::glue_collapse(
          err_info(),
          "Make sure that file exists and is not blocked",
          "by another process.",
          sep = " "
        ),
        .sep = "\n ",
        .trim = FALSE
      ),
      class = "d2r_invalid_arg"
    )
  }

  lines <- readr::read_lines(path)

  if ((vctrs::vec_size(lines) - 1L) %% 2L != 0L) {
    rlang::abort(
      glue::glue(
        "Invalid file contents.",
        "{err_cross()} File is incorrectly formatted.",
        "{err_info()} Expected odd number of lines, got `{vec_size(lines)}`.",
        .sep = "\n ",
        .trim = FALSE
      ),
      class = "d2r_io_failed"
    )
  }

  seq.int(0L, (vctrs::vec_size(lines) - 1L) / 2L - 1L, by = 1L) %>%
    purrr::map(~ lines[2L * .x + 1L:2L]) %>%
    purrr::map_dfr(
      ~ rlang::set_names(
        rlang::list2(
          .x[1L],
          !!!stringr::str_split(.x[2L], "\\s+")[[1]]
        ),
        vctrs::vec_c("File", "Start", "Count", "Object", "Filter")
      )
    ) %>%
    dplyr::mutate(
      dplyr::across(
        c(.data$Start, .data$Count, .data$Filter),
        readr::parse_integer
      )
    )
}
