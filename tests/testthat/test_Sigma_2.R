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


context("[Sigma_2] tests.")
test_that("Executing [Sigma_2] on the test data", {
    pth <- system.file("tests", "legacy_descriptor.dat",
                    package = "Dipol2Red", mustWork = TRUE)

    desc <- read_legacy_descriptor(pth)

    data <- desc %>%
        load_from_legacy_descriptor(
            root = system.file(
                "tests",
                package = "Dipol2Red",
                mustWork = TRUE)) %>%
                map(~mutate(.x, Test = 1:n()))

    band_info <- get(data("BandInfo", package = "Dipol2Red"))

    result <- map2_dfr(data, desc,
        ~ sigma_2(
            data = .x,
            filter = dplyr::filter(band_info, ID == .y$Filter)$Filter,
            band_info = band_info, obs = Obj_1))

    expect_equal(nrow(result), 2)
    expect_equal(result$JD, c(2458196.12, 2458222.10), tolerance = 1e-2)
    expect_equal(result$P, c(0.59770, 0.67309), tolerance = 1e-5)
    expect_equal(result$A, c(63.7010, 64.8771), tolerance = 1e-4)
    expect_equal(result$SG, c(0.0561595, 0.0658703), tolerance = 1e-7)
    expect_equal(result$SG_A, c(2.6839, 2.7947), tolerance = 1e-4)
    expect_equal(2 * result$N * result$Ratio, c(16, 3))
})

test_that("[Sigma_2] handles column names", {
     pth <- system.file("tests", "test1v.csv",
                    package = "Dipol2Red", mustWork = TRUE)

    data1 <- read_csv(pth)
    data2 <- data1 %>% set_names(c("NotJD", "Smth", "Obs1234"))
    band_info <- get(data("BandInfo", package = "Dipol2Red")) %>%
        filter(Filter == "V")

    expect_error(sigma_2(data2, "V", band_info), "object 'JD' not found")

   walk2(sigma_2(data2, "V", band_info, date = NotJD, obs = Obs1234),
        sigma_2(data2, "V", band_info, date = !!sym("NotJD"), obs = !!sym("Obs1234")),
        expect_equal)


})

test_that("[sigma_2] and [sigma_2_ex] do the same", {
    pth <- system.file("tests", "legacy_descriptor.dat",
                    package = "Dipol2Red", mustWork = TRUE)

    desc <- read_legacy_descriptor(pth)

    data <- desc %>%
        load_from_legacy_descriptor(
            root = system.file(
                "tests",
                package = "Dipol2Red",
                mustWork = TRUE)) %>%
                map(~mutate(.x, Test = 1:n()))

    band_info <- get(data("BandInfo", package = "Dipol2Red")) %>%
        mutate(Angle = 0.0, Px = 0.0, Py = 0.0)

    result <- map2_dfr(data, desc,
        ~ sigma_2(
            data = .x,
            filter = dplyr::filter(band_info, ID == .y$Filter)$Filter,
            band_info = band_info,
            obs = Obj_1))

    result_2 <- map2_dfr(data, desc,
        ~ fsigma_2(
            data = .x,
            obs = Obj_1))

    # Exact equality is achieved
    expect_equal(result$Px, result_2$Px)
    expect_equal(result$Py, result_2$Py)
    expect_equal(result$SG, result_2$SG)
    expect_equal(result$JD, result_2$JD)
    expect_equal(result$A, result_2$A)
})