provide_test_data <- function() {
  pth_1 <- system.file("tests", "p1.rds",
    package = "Dipol2Red", mustWork = TRUE
  )
  pth_2 <- system.file("tests", "p2.rds",
    package = "Dipol2Red", mustWork = TRUE
  )

  tibble(
    Set = factor(c("P1", "P2")),
    Path = c(pth_1, pth_2)
  ) %>%
    mutate(Temp = map(Path, readRDS)) %>%
    unnest(Temp) %>%
    select(-Path) %>%
    mutate(across(Q, as_list_of), .after = last_col())
}

context("[h_test] and [h_test2]")
test_that("[h_test] produces correct results", {
  provide_test_data() %>%
    group_split(Filter) %>%
    set_names(map_chr(., ~ vec_slice(.x$Filter, 1L))) %>%
    map(h_test) %>%
    imap_dfr(~ mutate(.x, Filter = factor(.y))) -> test_results

  expect_equal(
    c(-0.00003, -0.15496, -0.02110),
    pull(test_results, `lg(p)`),
    tolerance = 1e-5
  )
})

test_that("Id-less [h_test2] produces correct results", {
  provide_test_data() -> tmp
  p1 <- filter(tmp, Set == "P1")
  p2 <- filter(tmp, Set == "P2")

  h_test2(p1, p2, Filter) -> test_results

  expect_equal(
    c(-0.00003, -0.15496, -0.02110),
    pull(test_results, `lg(p)`),
    tolerance = 1e-5
  )
})

test_that("Custom id [h_test2] produces correct results", {
  provide_test_data() -> tmp
  p1 <- filter(tmp, Set == "P1")
  p2 <- filter(tmp, Set == "P2")

  mutate(
    p1,
    ID = factor(letters[1:n()])
  ) %>%
    slice_sample(n = vec_size(.)) -> p1
  mutate(
    p2,
    ID = factor(letters[1:n()])
  ) %>%
    slice_sample(n = vec_size(.)) -> p2

  h_test2(p1, p2, Filter, id = ID) -> test_results

  expect_equal(
    c(-0.00003, -0.15496, -0.02110),
    pull(test_results, `lg(p)`),
    tolerance = 1e-5
  )
})

test_that("[h_test] and [h_test2] produce same results", {
  provide_test_data() %>%
    group_split(Filter) %>%
    set_names(map_chr(., ~ vec_slice(.x$Filter, 1L))) %>%
    map(h_test) %>%
    imap_dfr(~ mutate(.x, Filter = factor(.y))) -> test_results_1

  provide_test_data() -> tmp
  p1 <- filter(tmp, Set == "P1")
  p2 <- filter(tmp, Set == "P2")

  h_test2(p1, p2, Filter) -> test_results_2

  expect_equal(pull(test_results_1, `lg(p)`), pull(test_results_2, `lg(p)`))
})
