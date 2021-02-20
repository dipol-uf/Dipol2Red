
context("[read_legacy_descriptor] tests.")
test_that("Reading file", {
  pth <- system.file(
    "tests",
    "legacy_descriptor.dat",
    package = "Dipol2Red",
    mustWork = TRUE
  )

  desc <- read_legacy_descriptor(pth)

  expect_equal(vctrs::vec_size(desc), 2)

  expect_equal(desc[["File"]], c("test1v.csv", "test2v.csv"))
  expect_equal(desc[["Start"]], c(1L, 1L))
  expect_equal(desc[["Count"]], c(192L, 152L))
  expect_equal(desc[["Object"]], c("601", "601"))
  expect_equal(desc[["Filter"]], c(3L, 3L))
})
