
## TODO: copy to {RLibs}
process_path <- function(path)
{
    path %>%
        fs::path_norm %>%
        str_split("/", simplify = TRUE) %>%
        as.vector %>%
        map_chr(glob2rx) -> paths

    proc_dir <- function(pth, template) {
        if (!fs::is_dir(pth))
            return(NULL)
        paths <- fs::dir_ls(pth)
        rels <- fs::path_rel(paths, pth) %>%
            str_subset(template)

        fs::path(pth, rels)
    }

    c(".", paths) %>%
        reduce(~map(.x, proc_dir, .y) %>% discard(~is.null(.x)) %>% flatten_chr)
}

#' @title process_files
#'
#' @param path
#' @param default_filter
#' @param by
#'
#' @return
#' @export
#'
#' @examples
process_files <- function(path, default_filter = "B", by = 4L)
{
    files <- process_path(path)
    files <- set_names(files, fs::path_file(files))

    proc <- function(loc_path) {
        # Currently read only csv
        #fs::path_ext(pth) -> ext
        loc_path %>%
            read_csv(col_types = cols()) %>%
            fix_names -> data

        nms <- names(data)
        obj_names <- str_subset(nms, "Obj_\\d+")
        ref_names <- str_subset(nms, "Ref_\\d+")

        ref_names %>%
            keep(~sqrt(sum(data[[.x]] ^ 2) / vec_size(data)) > .Machine$double.eps) %>%
            map(~parse_integer(str_extract(.x, "(?<=_)\\d+$"))) -> non_empty_ref_id

        if (!vec_is_empty(non_empty_ref_id)) {

            q <- non_empty_ref_id %>%
                set_names(paste0("Obj_", .)) %>%
                map(~quo(!!sym(paste0("Obj_", .x)) - !!sym(paste0("Ref_", .x))))

            mutate(data, !!!q) -> data
        }

        if (!any(str_detect(names(data), regex("filter", ignore_case = TRUE)))) {

            rlang::warn(glue::glue("`Filter` column is missing in {fs::path_file(loc_path)}. Assuming default filter of `{default_filter}`."))
            mutate(data, Filter = as_factor(default_filter)) -> data
        }
        else {
            mutate(data, Filter = as_factor(Filter)) -> data
        }

        data %>%
            arrange(JD) %>%
            group_split(Filter) %>%
            set_names(map_chr(., ~ levels(.x$Filter)[.x$Filter[1]])) %>%
            imap(~sigma_2(.x, filter = .y, bandInfo = NULL, date = JD, obs = Obj_1, Filter)) %>%
            map(select, Filter, JD, Px, Py, P, SG, A, SG_A, N, Ratio) %>% { vec_rbind(!!!.) } -> result_full

        data %>%
            arrange(JD) %>%
            group_split(Filter) %>%
            map(mutate, Id = (1:n() - 1L) %/% (4L * by)) %>%
            map(group_by, Id) %>%
            set_names(map_chr(., ~ levels(.x$Filter)[.x$Filter[1]])) %>%
            imap(~sigma_2(.x, filter = .y, bandInfo = NULL, date = JD, obs = Obj_1, Filter)) %>%
            map(select, Filter, JD, Px, Py, P, SG, A, SG_A, N, Ratio) %>% { vec_rbind(!!!.) } -> result_individual

        return(list(Full = result_full, Individual = result_individual))

    }

    files %>%
        #head(3) %>%
        map(proc) %>%
        print_processed_files
}


#print_processed_files <- function(inputs) {
    #sigfig <- options("pillar.sigfig")
    #n_max <- options("tibble.print_max")

    #options(pillar.sigfig = 9, tibble.print_max = Inf)

    #print(inputs)
    ##map(inputs, map, ~ print(.x, n = vec_size(.x), width = 115))

    #options(pillar.sigfig = sigfig, tibble.print_max = n_max)

    #return(invisible(inputs))
#}
