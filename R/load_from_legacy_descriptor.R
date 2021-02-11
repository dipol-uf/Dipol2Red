#' load_from_legacy_descriptor
#' @param desc Descriptors as output from \code{read_legacy_descriptor}.
#' @param root Root for the files in the descriptor.
#' @export
load_from_legacy_descriptor <- function(desc, root = ".") {
  # data <- desc %>%
  #     map(extract2, "File") %>%
  #     map_if(~nzchar(root), ~ fs::path(root, .x)) %>%
  #     map(read_csv, col_types = cols()) %>%
  #     map(~set_names(.x, fix_names(names(.x))))

  data <-
    desc %>%
    mutate(
      Data = map(
        fs::path(root, File),
        read_csv,
        col_types = cols()
      ),
      Data = map(
        Data,
        ~set_names(.x, fix_names(names2(.x)))
      ) %>%
      as_list_of
    )
    

  # walk2(data, desc, function(obs, des) {
  #         if (vec_size(obs) != des$Count)
  #             warning(paste0("Read number of observations (", nrow(obs), ") ",
  #                 "does not match expected number (", des$Count, ")."))
  #         })

  # return(data)
}
