context("[load_from_legacy_descriptor] tests.")
test_that("Reading data using legacy descriptor", {

  pth <- system.file(
    "tests",
    "legacy_descriptor.dat",
    package = "Dipol2Red",
    mustWork = TRUE
  )

  desc <- read_legacy_descriptor(pth)

  data <- desc %>%
    load_from_legacy_descriptor(
      root = system.file(
        "tests",
        package = "Dipol2Red",
        mustWork = TRUE
      )
    )

  expect_equal(vctrs::vec_size(data), 2)
  expect_equal(vctrs::vec_size(data[["Data"]][[1]]), 192)
  expect_equal(vctrs::vec_size(data[["Data"]][[2]]), 152)
})