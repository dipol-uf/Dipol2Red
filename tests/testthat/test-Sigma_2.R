provide_test_data <- function() {
  pth <- system.file(
    "tests",
    "legacy_descriptor.dat",
    package = "Dipol2Red",
    mustWork = TRUE
  )

  desc <- read_legacy_descriptor(pth)

  data <-
    desc %>%
    load_from_legacy_descriptor(
      root = system.file(
        "tests",
        package = "Dipol2Red",
        mustWork = TRUE
      )
    ) %>%
    map(mutate, Test = 1:n()) %>%
    map(arrange, sample(1:n()))
}

context("[fsigma_2] tests.")
test_that("Executing [fsigma_2] on the test data", {
  data <- provide_test_data()

  result <- map_dfr(
    data,
    ~ fsigma_2(
      data = .x,
      obs = Obj_1
    )
  )

  expect_equal(vec_size(result), 2L)
  expect_equal(result$JD, c(2458196.11954982, 2458222.0997718))
  expect_equal(result$P, c(0.596071323698895, 0.67155885306917))
  expect_equal(result$A, c(31.6085930431542, 32.7752723359075))
  expect_equal(result$SG, c(0.0561595176098245, 0.0658703217015173))
  expect_equal(result$SG_A, c(2.69114849595875, 2.80098872566289))
  expect_equal(2L * result$N * result$Ratio, c(16L, 3L))
})

test_that("[fsigma_2] handles column names", {
  pth <- system.file(
    "tests",
    "test1v.csv",
    package = "Dipol2Red",
    mustWork = TRUE
  )

  data1 <- read_csv(pth)
  data2 <- data1 %>% set_names(c("NotJD", "Smth", "Obs1234"))

  expect_error(fsigma_2(data2), "Index out of bounds")

  walk2(
    fsigma_2(data2, date = NotJD, obs = Obs1234),
    fsigma_2(data2, date = !!sym("NotJD"), obs = !!sym("Obs1234")),
    expect_equal
  )
})

test_that("[fsigma_2] grouped data", {
  data <- provide_test_data()
  data %>%
    imap_dfr(
      ~ mutate(
        .x,
        Group = factor(.y),
        Type = letters[.y]
      )
    ) %>%
    group_by(Group) -> data

  result <- fsigma_2(data = data, date_col = JD, obs_col = Obj_1, Type)

  expect_equal(result$Px, c(0.268595571558824, 0.277951722030802))
  expect_equal(result$Py, c(0.532125400516778, 0.61133798618742))
  expect_equal(result$SG, c(0.0561595176098245, 0.0658703217015173))
  expect_equal(result$JD, c(2458196.11954982, 2458222.0997718))
  expect_equal(result$A, c(31.6085930431542, 32.7752723359075))
  expect_equal(result$Group, factor(c("1", "2")))
  expect_equal(result$Type, c("a", "b"))
})
