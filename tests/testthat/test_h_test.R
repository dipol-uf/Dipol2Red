#   MIT License
#
#   Copyright(c) 2018-2020
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

provide_test_data <- function() {
    pth_1 <- system.file("tests", "p1.rds",
                    package = "Dipol2Red", mustWork = TRUE)
    pth_2 <- system.file("tests", "p2.rds",
                    package = "Dipol2Red", mustWork = TRUE)

    return(list(P1 = readRDS(pth_1), P2 = readRDS(pth_2)))
}

context("[h_test] and [h_test2]")
test_that("[h_test] produces correct results", {
    provide_test_data() %>%
        imap_dfr(~mutate(.x, Group = as_factor(.y))) %>%
        group_split(Filter) %>%
        set_names(map_chr(., ~ as.character(.x$Filter)[1])) %>%
        map(h_test) %>%
        imap_dfr(~mutate(.x, Filter = as_factor(.y))) -> test_results

    # Preliminary incorrect results
    # TODO : Update with reevaluated comparison values
    expect_equal(
        c(-1.59658, -0.09215, -0.03641),
        round(pull(test_results, `lg(p)`), 5))
})