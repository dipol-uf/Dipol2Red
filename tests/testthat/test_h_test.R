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
    res <- list(P1 = readRDS(pth_1), P2 = readRDS(pth_2))
    return(map(res, mutate_at, vars(Q), as_list_of))
}

context("[h_test] and [h_test2]")
test_that("[h_test] produces correct results", {
    provide_test_data() %>%
        imap_dfr(~mutate(.x, Group = as_factor(.y))) %>%
        group_split(Filter) %>%
        set_names(map_chr(., ~ vec_slice(.x$Filter, 1L))) %>%
        map(h_test) %>%
        imap_dfr(~mutate(.x, Filter = as_factor(.y))) -> test_results

    expect_equal(
        c(-0.00003, -0.15496, -0.02110),
        pull(test_results, `lg(p)`),
        tolerance = 1e-5)
})

test_that("Id-less [h_test2] produces correct results", {
    provide_test_data() %->% c(p1, p2)

    q <- sym("Q")
    bind_rows(p1, p2) %>%
        group_split(Filter) %>%
        map(function(d) {
            pull(d, {{ q }}) %->% c(q1, q2)
        })

    h_test2(p1, p2, Filter) -> test_results

    expect_equal(
        c(-0.00003, -0.15496, -0.02110),
        pull(test_results, `lg(p)`),
        tolerance = 1e-5)
})

test_that("Custom id [h_test2] produces correct results", {
    provide_test_data() %->% c(p1, p2)

    mutate(p1, ID = as_factor(letters[1:n()])) %>% slice_sample(n = vec_size(.)) -> p1
    mutate(p2, ID = as_factor(letters[1:n()])) %>% slice_sample(n = vec_size(.)) -> p2

    h_test2(p1, p2, Filter, id = ID) -> test_results

    expect_equal(
        c(-0.00003, -0.15496, -0.02110),
        pull(test_results, `lg(p)`),
        tolerance = 1e-5)
})

test_that("[h_test] and [h_test2] produce same results", {
    provide_test_data() %>%
        imap_dfr(~mutate(.x, Group = as_factor(.y))) %>%
        group_split(Filter) %>%
        set_names(map_chr(., ~ vec_slice(.x$Filter, 1L))) %>%
        map(h_test) %>%
        imap_dfr(~mutate(.x, Filter = as_factor(.y))) -> test_results_1

    provide_test_data() %->% c(p1, p2)

    h_test2(p1, p2, Filter) -> test_results_2

    expect_equal(pull(test_results_1, `lg(p)`), pull(test_results_2, `lg(p)`))
})