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

library(purrr)
context("[LoadFromLegacyDescriptor] tests.")
test_that("Reading data using legacy descriptor", {

    pth <- system.file("tests", "legacy_descriptor.dat",
                    package = "Dipol2Red", mustWork = TRUE)

    desc <- ReadLegacyDescriptor(pth)

    data <- desc %>%
        LoadFromLegacyDescriptor(
            root = system.file(
                "tests",
                package = "Dipol2Red",
                mustWork = TRUE))

    expect_equal(length(data), 2)
    expect_equal(nrow(data[[1]]), 192)
    expect_equal(nrow(data[[2]]), 152)
})