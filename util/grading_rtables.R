normalize_tbl_lines <- function(tbl) {
  lines <- capture.output(print(tbl))

  # normalize whitespace
  lines <- gsub("[ \t]+", " ", lines)
  lines <- gsub("\\s+$", "", lines)

  # remove leading/trailing blank lines
  while (length(lines) > 0 && trimws(lines[1]) == "") {
    lines <- lines[-1]
  }
  while (length(lines) > 0 && trimws(lines[length(lines)]) == "") {
    lines <- lines[-length(lines)]
  }

  lines
}

grade_rtables_matrix <- function(user_tbl, ref_tbl, max_diffs = 15) {
  user_lines <- normalize_tbl_lines(user_tbl)
  ref_lines  <- normalize_tbl_lines(ref_tbl)

  max_len <- max(length(user_lines), length(ref_lines))
  length(user_lines) <- max_len
  length(ref_lines)  <- max_len

  user_lines[is.na(user_lines)] <- "<missing line>"
  ref_lines[is.na(ref_lines)]   <- "<missing line>"

  diffs <- which(user_lines != ref_lines)

  if (length(diffs) == 0) {
    return(list(pass = TRUE, msg = "✅ Correct! Output matches reference."))
  }

  diffs <- head(diffs, max_diffs)

  msg_lines <- vapply(
    diffs,
    function(i) {
      paste0(
        "Line ", i, ":\n",
        "  expected: ", ref_lines[i], "\n",
        "  got     : ", user_lines[i]
      )
    },
    character(1)
  )

  list(
    pass = FALSE,
    msg = paste0("❌ Not yet. First differences:\n\n", paste(msg_lines, collapse = "\n\n"))
  )
}
