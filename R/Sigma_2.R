#' @title sigma_2
#' @rdname sigma_2
#' @param ... Unused.
#' @export
#' @aliases sigma_2
sigma_2 <- function(...) {
  lifecycle::deprecate_stop(
    when = "0.8.0",
    what = "Dipol2Red::sigma_2()",
    with = "Dipol2Red::fsigma_2()"
  )
}
