provide_test_data <- function() {
  pth_1 <- system.file("tests", "p1.rds",
    package = "Dipol2Red", mustWork = TRUE
  )
  pth_2 <- system.file("tests", "p2.rds",
    package = "Dipol2Red", mustWork = TRUE
  )

  tibble::tibble(
    Set = factor(c("P1", "P2")),
    Path = c(pth_1, pth_2)
  ) %>%
    dplyr::mutate(Temp = purrr::map(Path, readRDS)) %>%
    tidyr::unnest(Temp) %>%
    dplyr::select(-Path) %>%
    dplyr::mutate(
      dplyr::across(Q, vctrs::as_list_of),
      .after = tidyselect::last_col()
    )
}

context("[h_test] and [h_test2]")
test_that("[h_test] produces correct results", {
  provide_test_data() %>%
    dplyr::group_split(Filter) %>%
    rlang::set_names(
      purrr::map_chr(., ~ vctrs::vec_slice(.x$Filter, 1L))
    ) %>%
    purrr::map(h_test) %>%
    purrr::imap_dfr(~ dplyr::mutate(.x, Filter = factor(.y))) -> test_results

  expect_equal(
    c(-0.00003, -0.15496, -0.02110),
    dplyr::pull(test_results, `lg(p)`),
    tolerance = 1e-5
  )
})

test_that("Id-less [h_test2] produces correct results", {
  provide_test_data() -> tmp
  p1 <- dplyr::filter(tmp, Set == "P1")
  p2 <- dplyr::filter(tmp, Set == "P2")

  h_test2(p1, p2, Filter) -> test_results

  expect_equal(
    c(-0.00003, -0.15496, -0.02110),
    dplyr::pull(test_results, `lg(p)`),
    tolerance = 1e-5
  )
})

test_that("Custom id [h_test2] produces correct results", {
  provide_test_data() -> tmp
  p1 <- dplyr::filter(tmp, Set == "P1")
  p2 <- dplyr::filter(tmp, Set == "P2")

  dplyr::mutate(
    p1,
    ID = factor(letters[1:dplyr::n()])
  ) %>%
    dplyr::slice_sample(n = vctrs::vec_size(.)) -> p1
  dplyr::mutate(
    p2,
    ID = factor(letters[1:dplyr::n()])
  ) %>%
    dplyr::slice_sample(n = vctrs::vec_size(.)) -> p2

  h_test2(p1, p2, Filter, id = ID) -> test_results

  expect_equal(
    c(-0.00003, -0.15496, -0.02110),
    dplyr::pull(test_results, `lg(p)`),
    tolerance = 1e-5
  )
})

test_that("[h_test] and [h_test2] produce same results", {
  provide_test_data() %>%
    dplyr::group_split(Filter) %>%
    rlang::set_names(
      purrr::map_chr(., ~ vctrs::vec_slice(.x$Filter, 1L))
    ) %>%
    purrr::map(h_test) %>%
    purrr::imap_dfr(~ dplyr::mutate(.x, Filter = factor(.y))) -> test_results_1

  provide_test_data() -> tmp
  p1 <- dplyr::filter(tmp, Set == "P1")
  p2 <- dplyr::filter(tmp, Set == "P2")

  h_test2(p1, p2, Filter) -> test_results_2

  expect_equal(
    dplyr::pull(test_results_1, `lg(p)`),
    dplyr::pull(test_results_2, `lg(p)`)
  )
})
