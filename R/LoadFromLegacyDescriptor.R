#   MIT License
#
#   Copyright(c) 2018
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

#' @title LoadFromLegacyDescriptor
#' @param desc Descriptors as output from \code{ReadLegacyDescriptor}.
#' @param root Root for the files in the descriptor.
#' @export
#' @importFrom purrr %>% map map_if walk2
#' @importFrom rlang set_names
#' @importFrom readr read_csv cols
LoadFromLegacyDescriptor <- function(desc, root = ".") {
    data <- desc %>%
        map(extract2, "File") %>%
        map_if(~nzchar(root), ~ file.path(root, .x)) %>%
        map(~read_csv(.x, col_types = cols())) %>%
        map(set_names, c("JD", "Ref", "Obs"))

    walk2(data, desc, function(obs, des) {
            if (nrow(obs) != des$Count)
                warning(glue("Read number of observations ({nrow(obs)}) ",
                    "does not match expected number ({des$Count})."))
            })

    return(data)
}