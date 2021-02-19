utils::globalVariables(c("Start", "Count"))

#' Read legacy descriptor
#' @param path Path to the descriptor file (e.g. \code{Vin.txt})
#' @export
read_legacy_descriptor <- function(path) {
  if (
    !fs::file_access(
      vec_cast(path, to = fs::path(), x_arg = "path"),
      c("exists", "readable")
    )
  ) {
    abort(
      paste(
        err_invalid_arg(),
        paste(
          err_cross(),
          "`path` should point to a readable file."
        ),
        paste(
          err_info(),
          "Make sure that file exists and is not blocked",
          "by another process."
        ),
        sep = "\n "
      ),
      class = "d2r_invalid_arg"
    )
  }

  lines <- read_lines(path)

  if ((vec_size(lines) - 1L) %% 2L != 0L) {
    abort(
      paste(
        "Invalid file contents.",
        paste(
          err_cross(),
          "File is incorrectly formatted."
        ),
        glue(
          err_info(),
          " Expected odd number of lines, got `{vec_size(lines)}`.",
        ),
        sep = "\n "
      ),
      class = "d2r_io_failed"
    )
  }

  seq.int(0L, (vec_size(lines) - 1L) / 2L - 1L, by = 1L) %>%
    map(~ lines[2L * .x + 1L:2L]) %>%
    map_dfr(
      ~ set_names(
        list2(
          .x[1L],
          !!!str_split(.x[2L], "\\s+")[[1]]
        ),
        c("File", "Start", "Count", "Object", "Filter")
      )
    ) %>%
    mutate(across(c(Start, Count, Filter), parse_integer))
}
