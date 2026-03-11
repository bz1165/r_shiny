suppressPackageStartupMessages({
  library(yaml)
})

`%||%` <- function(x, y) if (!is.null(x)) x else y

load_ex_index <- function(app_root) {
  yaml::read_yaml(file.path(app_root, "exercises", "index.yml"))
}

make_exercise_choices <- function(ex_index) {
  items <- list()
  for (topic in names(ex_index)) {
    for (it in ex_index[[topic]]) {
      items[[it$id]] <- paste0("[", topic, "] ", it$title)
    }
  }
  unlist(items, use.names = TRUE)
}

load_exercise <- function(app_root, ex_index, ex_id) {
  for (topic in names(ex_index)) {
    for (it in ex_index[[topic]]) {
      if (identical(it$id, ex_id)) {
        yml_path <- file.path(app_root, it$yml)
        ex <- yaml::read_yaml(yml_path)

        base_dir <- dirname(yml_path)
        ex$id <- ex$id %||% ex_id
        ex$title <- ex$title %||% it$title
        ex$description <- ex$description %||% ""
        ex$timeout_sec <- ex$timeout_sec %||% 5

        ex$starter_file <- normalizePath(file.path(base_dir, ex$starter_file), winslash = "/", mustWork = TRUE)
        ex$reference_file <- normalizePath(file.path(base_dir, ex$reference_file), winslash = "/", mustWork = TRUE)

        return(ex)
      }
    }
  }
  stop("Exercise not found: ", ex_id)
}
