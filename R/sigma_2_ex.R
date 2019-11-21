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
                        ittMax = 500L,
                        eps = 1e-16) {

    assert_that(is_tibble(data) || is.data.frame(data))
    assert_that(is.count(ittMax))
    assert_that(is.number(eps), eps > 0)

    extra_vars <- union(
        group_vars(data),
        tidyselect::vars_select(dplyr::tbl_vars(data), !!!enquos(...)))


    indices <- group_indices(data)
    unique <- sort(unique(indices))

    idx <- map(unique, ~ which(indices == .x))
    do_work_sigma_2_ex(data,
                       as_name(ensym(date_col)),
                       as_name(ensym(obs_col)),
                       idx,
                       extra_vars,
                       eps,
                       ittMax)
}


do_work_sigma_2_ex <- function(data, date_col, obs_col,
                            what,
                            extra_vars = NULL,
                            eps,
                            ittMax) {
    as_tibble(.Call("d2r_do_work_sigma_2_ex", data, date_col, obs_col,
        what,
        extra_vars,
        eps,
        ittMax))
}


#compile_src <- function() {
    #cmds <- vec_c(
                  #"rm src/*dll",
                  #"rm src/*o",
                  #"cd src && RCMD.exe SHLIB *cpp -o dipol_2_red.dll")

    #map_int(cmds, shell)
    #if(getLoadedDLLs() %>% names %>% str_detect("dipol_2_red") %>% any)
        #dyn.unload("src/dipol_2_red.dll")

    #dyn.load("src/dipol_2_red.dll", local = FALSE)
#}

##if (isNamespaceLoaded("rlang")) {
    #pth <- system.file("tests", "legacy_descriptor.dat",
                    #package = "Dipol2Red", mustWork = TRUE)
    #desc <- read_legacy_descriptor(pth)

    #data <- desc %>%
        #load_from_legacy_descriptor(
            #root = system.file(
                #"tests",
                #package = "Dipol2Red",
                #mustWork = TRUE)) %>%
                #imap(~mutate(.x, Test = 1:n() - 1L, Type = as_factor(.y))) %>%
                #RLibs::vec_rbind_uq %>%
                #group_by(Type)

    ##compile_src()
    ##sigma_2_ex(data, JD, Obj_1, Test) %>% print
    ##sigma_2(data, filter = "B", bandInfo = NULL, obs = Obj_1) %>% print
##}

#microbenchmark::microbenchmark(
    #cpp = sigma_2_ex(data, JD, Obj_1, Test),
    #r = sigma_2(data, filter = "B", bandInfo = BandInfo, obs = Obj_1, Test),
    #times = 20L) %>% print