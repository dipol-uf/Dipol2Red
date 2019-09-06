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

if (interactive()) {
    #roxygen2::roxygenise()
    # Uncomment this to check the package from within interactive session
    #devtools::check()
    library(tidyverse)
    library(magrittr)
    library(rlang)
    
} else {

    message("Running `roxygen2::roxygenize`...")
    roxygen2::roxygenize()

    isWin <- grepl("win(dows)?", Sys.info()["sysname"])
    if (is.na(isWin))
        stop("Unable to detect system. Run `R CMD build` manually.")
    sfx <- ifelse(isWin, ".exe", "")
    cmd_1 <- sprintf("R%s CMD build .", sfx)

    message(paste("Executing:", cmd_1))
    if (isWin)
        shell(cmd_1, mustWork = TRUE)
    else
        system(cmd_1)

    pckgs <- base::dir(".", "*.gz")

    `%>%` <- dplyr::`%>%`
    stringr::str_match_all(pckgs, "Dipol2Red_(([0-9]\\.){3}).*gz") %>%
        purrr::map(dplyr::as_tibble) %>%
        purrr::reduce(dplyr::bind_rows) %>%
        dplyr::select(1:2) %>%
        stats::setNames(nm = c("File", "Version")) %>%
        dplyr::arrange(desc(Version)) %>%
        dplyr::slice(1) %>%
        dplyr::pull(File) -> latestPckg



    cmd_2 <- sprintf("R%s CMD check %s", sfx, latestPckg)

    message(paste("Executing:", cmd_2))
    if (isWin)
        shell(cmd_2, mustWork = TRUE)
    else
        system(cmd_2)
}