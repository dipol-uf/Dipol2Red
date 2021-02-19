utils::globalVariables(vctrs::vec_c("Obj", "Name", "Src", "Result", "Desc", "Id"))

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
    str_match(
      regex(
        "(jd|mjd|date|filter|ref|obj)(\\d+)?(?:\\s*:\\s*(.*)$)?",
        ignore_case = TRUE
      )
    ) %>%
    as_tibble(.name_repair = "minimal") %>%
    set_names(c("Match", "Name", "Id", "Desc")) %>%
    mutate(
      Desc = if_else(
        str_detect(
          Desc,
          "[Mm]agnitude(?:\\s*\\([Cc]entroid\\))?"
        ) |
          is.na(Desc),
        "",
        paste0("_(", str_replace(Desc, "\\s+", "_"), ")")
      ),
      Id = if_else(
        nzchar(Id) & !is.na(Id),
        paste0("_", Id),
        ""
      ),
      Name = tolower(Name),
      Name = case_when(
        vec_in(Name, vec_c("jd", "date", "mjd")) ~ "JD",
        Name == "obj" ~ "Obj",
        Name == "ref" ~ "Ref",
        Name == "filter" ~ "Filter",
        TRUE ~ x
      ),
      Result = paste0(Name, Id, Desc)
    ) %>%
    pull(Result)
}

#' @rdname fix_names
#' @export
fix_names.tbl_df <- function(x) {
  set_names(x, fix_names(names(x)))
}

#' @rdname fix_names
#' @export
fix_names.data.frame <- function(x) {
  set_names(x, fix_names(names(x)))
}

fix_names.default <- function(x) {
  abort(
    paste(
      err_invalid_arg(),
      glue(
        err_cross(),
        "Cannot fix names of object of type",
        "`{vec_ptype_abbr(x)}`.",
        .sep = " "
      ),
      glue(
        err_info(),
        "Supported types are",
        "`{vec_ptype_abbr(character())}`,",
        "`{vec_ptype_abbr(tibble())}`, ",
        "`{vec_ptype_abbr(data.frame())}`, ",
        .sep = " "
      ),
      sep = "\n "
    ),
    class = "d2r_invalid_arg"
  )
}
