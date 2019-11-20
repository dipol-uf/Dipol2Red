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

sigma_2_ex <- function(data,
                        date_col = JD,
                        obs_col = Obs,
                        ...,
                        eqtrialCorrFactor = 0.034907,
                        ittMax = 500L,
                        eps = 1e-16) {

    extra_vars <- tidyselect::vars_select(dplyr::tbl_vars(data), !!!enquos(...))

    #assert_that(is_tibble(data))
    #assert_that(is.number(eqtrialCorrFactor))
    #assert_that(is.count(ittMax))
    #assert_that(is.number(eps), eps > 0)

    data %>%
        group_split %>%
        map(do_work_sigma_2_ex, as.character(ensym(date_col)), as_name(ensym(obs_col)), eqtrialCorrFactor, ittMax, eps, extra_vars = extra_vars) #%>% { vec_rbind(!!!.) }
        
                    #select(JD, Px, Py, P, SG, A, SG_A, Q, N, Ratio, Itt, one_of(extra_vars))) %>%
                        #bind_rows
    #else
        #result <- do_work_sigma_2(data, !!date, !!obs, p0, a0, eqtrialCorrFactor, ittMax, eps, extra_vars = extra_vars) %>%
               #select(JD, Px, Py, P, SG, A, SG_A, Q, N, Ratio, Itt, one_of(extra_vars))

    #return(result)
}


do_work_sigma_2_ex <- function(data, date_col, obs_col,
                            eqtrialCorrFactor,
                            ittMax, eps,
                            extra_vars = NULL) {
    .Call("d2r_do_work_sigma_2_ex", data, date_col, obs_col, what = 1L:4L, extra_vars = extra_vars)
}


compile_src <- function() {
    cmds <- vec_c(
                  "rm src/*dll",
                  "rm src/*o",
                  "cd src && RCMD.exe SHLIB *cpp -o dipol_2_red.dll")

    map_int(cmds, shell)
    if(getLoadedDLLs() %>% names %>% str_detect("dipol_2_red") %>% any)
        dyn.unload("src/dipol_2_red.dll")

    dyn.load("src/dipol_2_red.dll", local = FALSE)
}

if (isNamespaceLoaded("rlang")) {
    pth <- system.file("tests", "legacy_descriptor.dat",
                    package = "Dipol2Red", mustWork = TRUE)
    desc <- read_legacy_descriptor(pth)

    data <- desc %>%
        load_from_legacy_descriptor(
            root = system.file(
                "tests",
                package = "Dipol2Red",
                mustWork = TRUE)) %>%
                imap(~mutate(.x, Test = 1:n() - 1L, Type = as_factor(.y))) %>%
                RLibs::vec_rbind_uq %>%
                group_by(Type)

    compile_src()
    sigma_2_ex(data, JD, Obj_1, Test, Type) %>% print
}