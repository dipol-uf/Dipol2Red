#' @title fix_names
#' @rdname fix_names
#' @param x Names of the CSV file to replace
#'
#' @return R-compatible names suitable for further work
#' @export
fix_names <- function(x) {
  UseMethod("fix_names")
}

#' @rdname fix_names
#' @export
fix_names.character <- function(x) {
  x %>%
    stringr::str_match(
      stringr::regex(
        "(jd|mjd|date|filter|ref|obj)(\\d+)?(?:\\s*:\\s*(.*)$)?",
        ignore_case = TRUE
      )
    ) %>%
    tibble::as_tibble(.name_repair = "minimal") %>%
    rlang::set_names(vctrs::vec_c("Match", "Name", "Id", "Desc")) %>%
    dplyr::mutate(
      Desc = dplyr::if_else(
        stringr::str_detect(
          .data$Desc,
          "[Mm]agnitude(?:\\s*\\([Cc]entroid\\))?"
        ) |
          is.na(.data$Desc),
        glue::glue(""),
        glue::glue("_({stringr::str_replace(.data$Desc, \"\\\\s+\", \"_\")})")
      ),
      Id = dplyr::if_else(
        nzchar(.data$Id) & !is.na(.data$Id),
        glue::glue("_{.data$Id}"),
        glue::glue("")
      ),
      Name = tolower(.data$Name),
      Name = dplyr::case_when(
        vctrs::vec_in(.data$Name, vctrs::vec_c("jd", "date", "mjd")) ~ "JD",
        Name == "obj" ~ "Obj",
        Name == "ref" ~ "Ref",
        Name == "filter" ~ "Filter",
        TRUE ~ x
      ),
      Result = glue::glue("{.data$Name}{.data$Id}{.data$Desc}")
    ) %>%
    dplyr::pull("Result")
}

#' @rdname fix_names
#' @export
fix_names.tbl_df <- function(x) {
  rlang::set_names(x, fix_names(rlang::names2(x)))
}

#' @rdname fix_names
#' @export
fix_names.data.frame <- function(x) {
  rlang::set_names(x, fix_names(rlang::names2(x)))
}

fix_names.default <- function(x) {
  rlang::abort(
    glue::glue(
      err_invalid_arg(),
      glue::glue(
        err_cross(),
        "Cannot fix names of object of type",
        "`{vctrs::vec_ptype_abbr(x)}`.",
        .sep = " ",
        .trim = FALSE
      ),
      glue::glue(
        err_info(),
        "Supported types are",
        "`{vctrs::vec_ptype_abbr(character())}`,",
        "`{vctrs::vec_ptype_abbr(tibble::tibble())}`, ",
        "`{vctrs::vec_ptype_abbr(data.frame())}`, ",
        .sep = " ",
        .trim = FALSE
      ),
      .sep = "\n ",
      .trim = FALSE
    ),
    class = "d2r_invalid_arg"
  )
}
