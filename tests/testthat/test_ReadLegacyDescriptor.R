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

context("[ReadLegacyDescriptor] tests.")
test_that("Reading file", {
    pth <- system.file("tests", "legacy_descriptor.dat",
                    package = "Dipol2Red", mustWork = TRUE)

    desc <- ReadLegacyDescriptor(pth)

    expect_equal(length(desc), 2)

    expect_equal(desc[[1]]$File, "test1v.csv")
    expect_equal(desc[[2]]$File, "test2v.csv")

    expect_equal(desc[[1]]$Start, 1)
    expect_equal(desc[[2]]$Start, 1)

    expect_equal(desc[[1]]$Count, 192)
    expect_equal(desc[[2]]$Count, 152)

    expect_equal(desc[[1]]$Object, "601")
    expect_equal(desc[[2]]$Object, "601")

    expect_equal(desc[[1]]$Filter, 3)
    expect_equal(desc[[2]]$Filter, 3)
})