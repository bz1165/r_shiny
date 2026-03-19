suppressPackageStartupMessages({
  library(rtables)
})

source("util/config.R")

make_training_wd <- function() {
  file.path(CONFIG$TRAINING_RA_ROOT, "pgm", "shiny_runner.R")
}

eval_in_training_env <- function(code, timeout_sec = 5, setup_timeout_sec = 30) {
  # Use baseenv() so core language/base functions are available when sourcing
  # company setup scripts (e.g., `if`, `library`, `getOption`).
  env <- new.env(parent = baseenv())
  env$wd <- make_training_wd()

  block <- function(...) stop("Blocked in training sandbox.")
  env$system <- block
  env$system2 <- block
  env$unlink <- block
  env$file.remove <- block

  # Compatibility fallback for environments where conflicted::conflict_prefer
  # is unavailable but referenced in company setup scripts.
  if (!exists("conflict_prefer", mode = "function", inherits = TRUE)) {
    env$conflict_prefer <- function(...) invisible(NULL)
  }

  setup_path <- file.path(CONFIG$TRAINING_RA_ROOT, "util", "_setup.R")

  setup_res <- tryCatch({
    setTimeLimit(elapsed = setup_timeout_sec, transient = TRUE)
    on.exit(setTimeLimit(elapsed = Inf, transient = FALSE), add = TRUE)
    sys.source(setup_path, envir = env)
    NULL
  }, error = function(e) {
    conditionMessage(e)
  })

  if (!is.null(setup_res)) {
    return(list(ok = FALSE, value = NULL, env = env, error = paste0("Setup failed: ", setup_res)))
  }

  tryCatch({
    setTimeLimit(elapsed = timeout_sec, transient = TRUE)
    on.exit(setTimeLimit(elapsed = Inf, transient = FALSE), add = TRUE)

    val <- eval(parse(text = code), envir = env)
    list(ok = TRUE, value = val, env = env, error = NULL)
  }, error = function(e) {
    list(ok = FALSE, value = NULL, env = env, error = conditionMessage(e))
  })
}

extract_table <- function(res) {
  if (!res$ok) return(NULL)

  if (inherits(res$value, "TableTree")) return(res$value)

  if (exists("tbl", envir = res$env, inherits = FALSE)) {
    t <- get("tbl", envir = res$env, inherits = FALSE)
    if (inherits(t, "TableTree")) return(t)
  }

  NULL
}

run_user_code <- function(code, timeout_sec = 5) {
  res <- eval_in_training_env(code, timeout_sec)
  if (!res$ok) return(list(ok = FALSE, tbl = NULL, msg = res$error))

  tbl <- extract_table(res)
  if (is.null(tbl)) {
    return(list(
      ok = FALSE,
      tbl = NULL,
      msg = "Code ran but no rtables TableTree was returned (return tbl or build_table output)."
    ))
  }

  list(ok = TRUE, tbl = tbl, msg = "OK")
}

run_reference_code <- function(reference_file, timeout_sec = 5) {
  code <- paste(readLines(reference_file, warn = FALSE), collapse = "\n")
  run_user_code(code, timeout_sec)
}
