#   MIT License
#
#   Copyright(c) 2019
#   Ilia Kosenkov [ilia.kosenkov.at.gm@gmail.com],
#
#   Permission is hereby granted, free of charge, to any person obtaining a copy
#   of this software and associated documentation files(the "Software"), to deal
#   in the Software without restriction, including without limitation the rights
#   to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
#   copies of the Software, and to permit persons to whom the Software is
#   furnished to do so, subject to the following conditions:
#
#   The above copyright notice and this permission
#   notice shall be included in all
#   copies or substantial portions of the Software.
#
#   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
#   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
#   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
#   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH
#   THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

utils::globalVariables(vctrs::vec_c("Obj", "Name", "Src", "Result", "Desc"))

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
