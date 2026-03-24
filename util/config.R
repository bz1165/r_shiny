CONFIG <- list()

CONFIG$APP_ROOT <- normalizePath(".", winslash = "/", mustWork = TRUE)

# Fixed VOB location parts (same for all users)
CONFIG$VOB_PARTS <- c("CSRTRAIN", "CSRTRAINCN", "csr_25")

resolve_user_id <- function(session_user = NULL) {
  u <- session_user
  if (is.null(u) || !nzchar(u)) u <- Sys.getenv("USER")
  if (is.null(u) || !nzchar(u)) u <- Sys.info()[["user"]]
  if (is.null(u) || !nzchar(u)) stop("Cannot determine current user.")
  u
}

build_training_ra_root <- function(user_id) {
  file.path(
    "/view",
    paste0(user_id, "_view"),
    "vob",
    CONFIG$VOB_PARTS[1],
    CONFIG$VOB_PARTS[2],
    CONFIG$VOB_PARTS[3]
  )
}

build_user_paths <- function(user_id) {
  ra_root <- build_training_ra_root(user_id)

  list(
    user_id   = user_id,
    ra_root   = ra_root,
    docs_dir  = file.path(ra_root, "util", "documentation"),
    save_dir  = file.path(ra_root, "pgm", "saf"),
    pgm_file  = file.path(ra_root, "pgm", "shiny_runner.R")
  )
}
