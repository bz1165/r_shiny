save_submission <- function(save_dir, exercise_id, code) {
  dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

  prefix <- tolower(sub("^(ex[0-9]+).*", "\\1", exercise_id))
  if (!nzchar(prefix)) prefix <- "exercise"

  pattern <- paste0("^", prefix, "_submission([0-9]+)\\.R$")
  old_files <- list.files(save_dir, pattern = pattern)

  next_n <- if (length(old_files) == 0) {
    1L
  } else {
    nums <- as.integer(sub(pattern, "\\1", old_files))
    max(nums, na.rm = TRUE) + 1L
  }

  out_file <- file.path(save_dir, paste0(prefix, "_submission", next_n, ".R"))
  writeLines(code, out_file, useBytes = TRUE)

  normalizePath(out_file, winslash = "/", mustWork = FALSE)
}
