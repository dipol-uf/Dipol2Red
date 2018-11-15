#   MIT License
#
#   Copyright(c) 2018 Ilia Kosenkov [ilia.kosenkov.at.gm@gmail.com]
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

#' @title ReadLegacyDescriptor
#' @param path Path to the descriptor file (e.g. \code{Vin.txt})
#' @export
#' @importFrom glue glue
#' @importFrom readr read_lines
#' @importFrom purrr map map_at
#' @importFrom utils tail
#' @importFrom stringr str_match
#' @importFrom rlang set_names
#' @importFrom purrr %>%
ReadLegacyDescriptor <- function(path) {
    if (!file.exists(path))
        stop(glue("File `{path}` is not found."))

    lines <- read_lines(path)

    if ((length(lines) - 1) %% 2 != 0)
        stop("Incorrect number of lines in the input file.")

    profiles <- seq(1L, (length(lines) - 1L) / 2L, by = 1L) %>%
        subtract(1) %>%
        map(~lines[2L * .x + 1:2]) %>%
        map(~append(list(File = .x[1]),
            str_match(.x[2],
                "^([0-9]+)\\ +([0-9]+)\\ +(.*)\\ +([0-9]+)$") %>%
                as.character %>%
                tail(-1) %>%
                as.list %>%
                map_at(c(1, 2, 4), as.integer)  %>%
                set_names(c("Start", "Count", "Object", "Filter"))
            ))

    return(profiles)
}