CONFIG <- list()

CONFIG$APP_ROOT <- normalizePath(".", winslash = "/", mustWork = TRUE)
CONFIG$TRAINING_RA_ROOT <- Sys.getenv(
  "TRAINING_RA_ROOT",
  unset = "/view/zhaibe1_view/vob/CSRTRAIN/CSRTRAINCN/csr_25"
)
CONFIG$SAVE_ROOT <- file.path(CONFIG$TRAINING_RA_ROOT, "submissions")
