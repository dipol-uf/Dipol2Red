#   MIT License
#
#   Copyright(c) 2018-2019
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

if (interactive()) {
    library(tidyverse)
    library(magrittr)
    library(rlang)
    library(vctrs)
    library(assertthat)

    if (!exists("compile_src"))
        compile_src <<- function() {
            cmds <- vctrs::vec_c(
                "rm src/*dll",
                "rm src/*o",
                "mv src/Makevars.win src/Makevars.win.cache",
                "mv src/Makevars.win.dbg src/Makevars.win",
                "cd src && RCMD.exe SHLIB *cpp -o dipol_2_red.dll",
                "mv src/Makevars.win src/Makevars.win.dbg",
                "mv src/Makevars.win.cache src/Makevars.win")

            purrr::map_int(cmds, shell)
            if (getLoadedDLLs() %>% names %>% stringr::str_detect("dipol_2_red") %>% any)
                dyn.unload("src/dipol_2_red.dll")

            dyn.load("src/dipol_2_red.dll", local = FALSE)
        }

    compile_src()
    purrr::walk(fs::dir_ls("R", glob = "*R"), source)
} else {

    message("Running `roxygen2::roxygenize`...")
    roxygen2::roxygenize(".")
    message("Finished `roxygen2::roxygenize`...")

    is_win <- grepl("[Ww]in(dows)?", Sys.info()["sysname"])
    if (is.na(is_win))
        stop("Unable to detect system. Run `R CMD build` manually.")
    sfx <- ifelse(is_win, ".exe", "")
    cmd_1 <- sprintf("R%s CMD build .", sfx)

    message(paste("Executing:", cmd_1))
    if (is_win)
        shell(cmd_1, mustWork = TRUE)
    else
        system(cmd_1)

    pckgs <- fs::dir_ls(".", glob = "*.gz")

    `%>%` <- dplyr::`%>%`

    stringr::str_match(pckgs, "^.*_((?:[0-9]+?\\.?){3})\\.tar\\.gz$") %>%
        dplyr::as_tibble(.name_repair = ~c("File", "Version")) %>%
        dplyr::mutate(
            VersionNum = stringr::str_split(Version, "\\."),
            Major = purrr::map_int(VersionNum, ~ readr::parse_integer(.x[1])),
            Minor = purrr::map_int(VersionNum, ~ readr::parse_integer(.x[2])),
            Patch = purrr::map_int(VersionNum, ~ readr::parse_integer(.x[3]))) %>%
        dplyr::arrange(desc(Major), desc(Minor), desc(Patch)) %>%
        dplyr::slice(1) %>%
        dplyr::pull(File) -> latest_pckg


    cmd_2 <- sprintf("R%s CMD check %s", sfx, latest_pckg)
    message(paste("Executing:", cmd_2))
    if (is_win)
        shell(cmd_2, mustWork = TRUE)
    else
        system(cmd_2)
}
