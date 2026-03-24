suppressPackageStartupMessages({
  library(yaml)
})

`%||%` <- function(x, y) if (!is.null(x)) x else y

load_helper_catalog <- function(app_root) {
  path <- file.path(app_root, "util", "helpers", "catalog.yml")

  if (!file.exists(path)) {
    return(data.frame(
      key = character(),
      title = character(),
      tags = character(),
      file = character(),
      stringsAsFactors = FALSE
    ))
  }

  raw <- yaml::read_yaml(path)
  items <- raw$helpers %||% list()

  if (length(items) == 0) {
    return(data.frame(
      key = character(),
      title = character(),
      tags = character(),
      file = character(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(
    rbind,
    lapply(items, function(x) {
      data.frame(
        key = x$key %||% "",
        title = x$title %||% (x$key %||% ""),
        tags = paste(x$tags %||% character(), collapse = ", "),
        file = x$file %||% "",
        stringsAsFactors = FALSE
      )
    })
  )
}

filter_helper_catalog <- function(catalog, query = "") {
  if (nrow(catalog) == 0) return(catalog)

  q <- trimws(query %||%)
  if (!nzchar(q)) return(catalog)

  keep <- grepl(q, catalog$key, ignore.case = TRUE) |
    grepl(q, catalog$title, ignore.case = TRUE) |
    grepl(q, catalog$tags, ignore.case = TRUE)

  catalog[keep, , drop = FALSE]
}

helper_choices <- function(catalog) {
  if (nrow(catalog) == 0) return(character())
  stats::setNames(catalog$key, catalog$title)
}

read_helper_doc <- function(app_root, catalog, key) {
  if (nrow(catalog) == 0 || is.null(key) || !nzchar(key)) {
    return("No helper documentation available.")
  }

  row <- catalog[catalog$key == key, , drop = FALSE]
  if (nrow(row) == 0) {
    return("No helper documentation available.")
  }

  path <- file.path(app_root, row$file[1])
  if (!file.exists(path)) {
    return(paste0("Helper file not found:\n", path))
  }

  paste(readLines(path, warn = FALSE), collapse = "\n")
}
