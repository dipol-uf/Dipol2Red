#' @title process_files
#'
#' @param path Path to the files. Supports globbing
#' @param by Average by how many
#'
#' @return A list of tibbles
#' @export
process_files <- function(path, by = 4L) {
  files <- Sys.glob(path)
  files <- rlang::set_names(files, fs::path_file(files))

  proc <- function(loc_path) {
    # Currently read only csv
    loc_path %>%
      readr::read_csv(col_types = readr::cols(), comment = "#") %>%
      fix_names() -> data

    nms <- rlang::names2(data)
    ref_names <- stringr::str_subset(nms, "^Ref_\\d+$")

    ref_names %>%
      purrr::keep(
        ~ sqrt(sum(data[[.x]]^2) / vctrs::vec_size(data)) > .Machine$double.eps
      ) %>%
      purrr::map(
        ~ readr::parse_integer(stringr::str_extract(.x, "(?<=_)\\d+$"))
      ) -> non_empty_ref_id

    if (!vctrs::vec_is_empty(non_empty_ref_id)) {
      q <- non_empty_ref_id %>%
        rlang::set_names(glue::glue("Obj_{.}")) %>%
        purrr::map(
          ~ rlang::quo(
            !!rlang::sym(glue::glue("Obj_{.x}")) -
              !!rlang::sym(glue::glue("Ref_{.x}"))
          )
        )

      dplyr::mutate(data, !!!q) -> data
    }

    if (vctrs::vec_in("DeltaMag", rlang::names2(data))) {
      nm <- rlang::sym("DeltaMag")
    } else {
      nm <- rlang::sym("Obj_1")
    }

    data %>%
      fsigma_2(date_col = "JD", obs_col = !!nm) %>%
      dplyr::select(
        .data$JD, .data$Px, .data$Py, .data$P,
        .data$SG, .data$A, .data$SG_A, .data$N, .data$Ratio
      ) -> result_full

    data %>%
      dplyr::mutate(Id = factor((1:n() - 1L) %/% (4L * by))) %>%
      dplyr::group_by(.data$Id) %>%
      fsigma_2(date_col = "JD", obs_col = !!nm) %>%
      dplyr::select(
        .data$Id, .data$JD, .data$Px, .data$Py,
        .data$P, .data$SG, .data$A, .data$SG_A,
        .data$N, .data$Ratio
      ) -> result_individual

    list(
      Full = result_full,
      Individual = result_individual
    ) %>%
      purrr::map(dplyr::mutate, JDF = .data$JD - floor(.data$JD))
  }

  files %>%
    purrr::map(proc)
}
