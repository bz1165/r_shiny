suppressPackageStartupMessages({
  library(yaml)
})

`%||%` <- function(x, y) if (!is.null(x)) x else y

load_ex_index <- function(app_root) {
  yaml::read_yaml(file.path(app_root, "exercises", "index.yml"))
}

make_exercise_choices <- function(ex_index) {
  # Shiny selectInput expects names as labels and values as submitted values.
  # We submit exercise id and display topic+title.
  labels <- c()
  values <- c()

  for (topic in names(ex_index)) {
    for (it in ex_index[[topic]]) {
      labels <- c(labels, paste0("[", topic, "] ", it$title))
      values <- c(values, it$id)
    }
  }

  stats::setNames(values, labels)
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
  known_ids <- unlist(lapply(ex_index, function(topic_items) vapply(topic_items, function(x) x$id, character(1))))
  stop("Exercise not found: ", ex_id, " (known ids: ", paste(known_ids, collapse = ", "), ")")
}
