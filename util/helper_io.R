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

  out <- do.call(
    rbind,
    lapply(items, function(x) {
      data.frame(
        key = as.character(x$key %||% ""),
        title = as.character(x$title %||% (x$key %||% "")),
        tags = paste(as.character(x$tags %||% character()), collapse = ", "),
        file = as.character(x$file %||% ""),
        stringsAsFactors = FALSE
      )
    })
  )

  rownames(out) <- NULL
  out
}

filter_helper_catalog <- function(catalog, query = "") {
  if (is.null(catalog) || nrow(catalog) == 0) return(catalog)

  if (is.null(query)) query <- ""
  q <- trimws(query)

  # IMPORTANT: when search box is empty, show ALL helpers
  if (!nzchar(q)) return(catalog)

  keep <- grepl(q, catalog$key, ignore.case = TRUE) |
    grepl(q, catalog$title, ignore.case = TRUE) |
    grepl(q, catalog$tags, ignore.case = TRUE)

  catalog[keep, , drop = FALSE]
}

helper_choices <- function(catalog) {
  if (is.null(catalog) || nrow(catalog) == 0) return(character())
  stats::setNames(catalog$key, catalog$title)
}

read_helper_doc <- function(app_root, catalog, key) {
  if (is.null(catalog) || nrow(catalog) == 0) {
    return("No helper documentation available.")
  }

  if (is.null(key) || !nzchar(key)) {
    return("Select a helper to preview its documentation.")
  }

  row <- catalog[catalog$key == key, , drop = FALSE]
  if (nrow(row) == 0) {
    return("Selected helper not found in catalog.")
  }

  path <- file.path(app_root, row$file[1])
  if (!file.exists(path)) {
    return(paste0("Helper file not found:\n", path))
  }

  paste(readLines(path, warn = FALSE), collapse = "\n")
}
