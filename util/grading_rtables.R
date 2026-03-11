suppressPackageStartupMessages({
  library(rtables)
})

tbl_to_matrix <- function(tbl) {
  m <- as.matrix(tbl)
  apply(m, c(1, 2), function(x) {
    x <- as.character(x)
    x <- gsub("[ \t]+", " ", x)
    trimws(x)
  })
}

grade_rtables_matrix <- function(user_tbl, ref_tbl, max_diffs = 15) {
  if (!inherits(user_tbl, "TableTree")) {
    return(list(pass = FALSE, msg = "Your code must return an rtables TableTree object (build_table output)."))
  }

  mu <- tbl_to_matrix(user_tbl)
  mr <- tbl_to_matrix(ref_tbl)

  if (!identical(dim(mu), dim(mr))) {
    return(list(
      pass = FALSE,
      msg = sprintf(
        "Table dimension mismatch: yours %s vs reference %s.",
        paste(dim(mu), collapse = "x"),
        paste(dim(mr), collapse = "x")
      )
    ))
  }

  diffs <- which(mu != mr, arr.ind = TRUE)
  if (nrow(diffs) == 0) {
    return(list(pass = TRUE, msg = "✅ Correct! Output matches reference."))
  }

  diffs <- diffs[seq_len(min(nrow(diffs), max_diffs)), , drop = FALSE]
  lines <- apply(diffs, 1, function(rc) {
    r <- rc[[1]]
    c <- rc[[2]]
    sprintf("Row %d, Col %d: expected [%s] but got [%s]", r, c, mr[r, c], mu[r, c])
  })

  list(pass = FALSE, msg = paste0("❌ Not yet. First differences:\n", paste(lines, collapse = "\n")))
}
