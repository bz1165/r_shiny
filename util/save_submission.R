save_submission <- function(save_root, user_id, exercise_id, code) {
  dir <- file.path(save_root, user_id, exercise_id)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  out <- file.path(dir, paste0("submission_", ts, ".R"))
  writeLines(code, out, useBytes = TRUE)

  normalizePath(out, winslash = "/", mustWork = FALSE)
}
