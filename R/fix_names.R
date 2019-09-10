#   MIT License
#
#   Copyright(c) 2019
#   Ilia Kosenkov [ilia.kosenkov.at.gm@gmail.com],
#   Vilppu Piirola
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

utils::globalVariables(vctrs::vec_c("Obj", "Name", "Src", "Result"))

#' @title fix_names
#'
#' @param text Names of the CSV file to replace
#'
#' @return R-compatible names suitable for further work
#' @export 
#' @importFrom dplyr %>% as_tibble mutate if_else pull
#' @importFrom glue glue
#' @importFrom stringr str_match
#' @importFrom vctrs vec_c
#' @importFrom purrr map2_chr
fix_names <- function(text) {
    text %>%
        str_match("(JD|Ref|Obj)(\\d*)") %>%
        as_tibble(.name_repair = ~vec_c("Match", "Name", "Id")) %>%
        mutate(
            Src = text,
            Id = if_else(nzchar(Id) & !is.na(Id), glue("_{Id}"), glue("")),
            Name = if_else(is.na(Name), Src, Name)) %>%
        mutate(Result = map2_chr(Name, Id, paste0)) %>%
        pull(Result)
}
