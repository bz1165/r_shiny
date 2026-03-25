is_rtables_table <- function(x) {
  inherits(x, c("TableTree", "VTableTree"))
}

tbl_to_lines <- function(tbl) {
  txt <- capture.output(print(tbl))
  txt <- gsub("[ \t]+", " ", txt)
  txt <- trimws(txt, which = "right")
  txt
}

grade_rtables_text <- function(user_tbl, ref_tbl, max_diffs = 20) {
  if (!is_rtables_table(user_tbl)) {
    return(list(
      pass = FALSE,
      msg = "Your code must return an rtables table object from build_table()."
    ))
  }

  if (!is_rtables_table(ref_tbl)) {
    return(list(
      pass = FALSE,
      msg = "Reference output is not an rtables table object."
    ))
  }

  u <- tbl_to_lines(user_tbl)
  r <- tbl_to_lines(ref_tbl)

  n <- max(length(u), length(r))
  if (length(u) < n) u <- c(u, rep("", n - length(u)))
  if (length(r) < n) r <- c(r, rep("", n - length(r)))

  diff_idx <- which(u != r)

  if (length(diff_idx) == 0) {
    return(list(pass = TRUE, msg = "✅ Correct! Output matches reference."))
  }

  diff_idx <- diff_idx[seq_len(min(length(diff_idx), max_diffs))]
  lines <- vapply(
    diff_idx,
    function(i) sprintf("Line %d:\n  expected: %s\n  got:      %s", i, r[i], u[i]),
    character(1)
  )

  list(
    pass = FALSE,
    msg = paste0("❌ Not yet. First differences:\n", paste(lines, collapse = "\n"))
  )
}

# Optional matrix-based fallback for future use
tbl_to_matrix <- function(tbl) {
  m <- as.matrix(tbl)
  apply(m, c(1, 2), function(x) {
    x <- as.character(x)
    x <- gsub("[ \t]+", " ", x)
    trimws(x)
  })
}

grade_rtables_matrix <- function(user_tbl, ref_tbl, max_diffs = 15) {
  if (!is_rtables_table(user_tbl)) {
    return(list(
      pass = FALSE,
      msg = "Your code must return an rtables table object from build_table()."
    ))
  }

  if (!is_rtables_table(ref_tbl)) {
    return(list(
      pass = FALSE,
      msg = "Reference output is not an rtables table object."
    ))
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

  list(
    pass = FALSE,
    msg = paste0("❌ Not yet. First differences:\n", paste(lines, collapse = "\n"))
  )
}
