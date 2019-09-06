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

library(dplyr)
library(purrr)
library(magrittr)
library(rlang)
library(readr)
context("[Sigma_2] tests.")
test_that("Executing [Sigma_2] on the test data", {
    pth <- system.file("tests", "legacy_descriptor.dat",
                    package = "Dipol2Red", mustWork = TRUE)

    desc <- ReadLegacyDescriptor(pth)

    data <- desc %>%
        LoadFromLegacyDescriptor(
            root = system.file(
                "tests",
                package = "Dipol2Red",
                mustWork = TRUE))

    bandInfo <- get(data("BandInfo", package = "Dipol2Red"))

    result <- map2_dfr(data, desc,
        ~ Sigma_2(
            data = .x,
            filter = dplyr::filter(bandInfo, ID == .y$Filter)$Filter,
            bandInfo = bandInfo))

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
    bandInfo <- get(data("BandInfo", package = "Dipol2Red")) %>%
        filter(Filter == "V")

    expect_error(Sigma_2(data2, "V", bandInfo), "object 'JD' not found")

   walk2(Sigma_2(data2, "V", bandInfo, date = NotJD, obs = Obs1234),
        Sigma_2(data2, "V", bandInfo, date = !!sym("NotJD"), obs = !!sym("Obs1234")),
        expect_equal)


})