suppressPackageStartupMessages({
  library(rtables)
})

source("util/config.R")

.RUNNER_SETUP_CACHE <- new.env(parent = emptyenv())

inject_compat_shims <- function(target_env) {
  # Provide commonly used stringr helpers for company setup/utils that may run
  # before tidyverse attaches. Includes typo-compatible alias str_dect.
  if (requireNamespace("stringr", quietly = TRUE)) {
    stringr_fns <- c(
      "regex", "str_detect", "str_match", "str_extract", "str_replace",
      "str_replace_all", "str_split_fixed", "str_subset", "str_to_lower",
      "str_to_upper", "str_trim", "str_count"
    )
    for (fn in stringr_fns) {
      assign(fn, getExportedValue("stringr", fn), envir = target_env)
    }
    # Compatibility alias for typo sometimes present in legacy scripts.
    assign("str_dect", getExportedValue("stringr", "str_detect"), envir = target_env)
  }

  if (requireNamespace("magrittr", quietly = TRUE)) {
    assign("%>%", getExportedValue("magrittr", "%>%"), envir = target_env)
  }
}

make_training_wd <- function() {
  file.path(CONFIG$TRAINING_RA_ROOT, "pgm", "shiny_runner.R")
}

first_existing_path <- function(paths) {
  hit <- paths[file.exists(paths)][1]
  if (is.na(hit) || !nzchar(hit)) return(NULL)
  hit
}

load_setup_env <- function(setup_timeout_sec = 30, force_reload = FALSE) {
  cache_key <- gsub("[^A-Za-z0-9_]", "_", CONFIG$TRAINING_RA_ROOT)
  if (!force_reload && exists(cache_key, envir = .RUNNER_SETUP_CACHE, inherits = FALSE)) {
    return(get(cache_key, envir = .RUNNER_SETUP_CACHE, inherits = FALSE))
  }

  compat_env <- new.env(parent = baseenv())
  compat_env$conflict_prefer <- function(...) invisible(NULL)
  inject_compat_shims(compat_env)

  setup_env <- new.env(parent = compat_env)
  setup_env$wd <- make_training_wd()

  # Make conflict_prefer visible for setup scripts that resolve in .GlobalEnv.
  had_global_conflict_prefer <- exists("conflict_prefer", envir = .GlobalEnv, inherits = FALSE)
  if (!had_global_conflict_prefer) {
    assign("conflict_prefer", function(...) invisible(NULL), envir = .GlobalEnv)
    on.exit(rm("conflict_prefer", envir = .GlobalEnv), add = TRUE)
  }

  funcs_general_path <- first_existing_path(c(
    file.path(CONFIG$TRAINING_RA_ROOT, "util", "funcs_general.R"),
    file.path(CONFIG$TRAINING_RA_ROOT, "utils", "funcs_general.R"),
    file.path(CONFIG$TRAINING_RA_ROOT, "util", "funcs.R"),
    file.path(CONFIG$TRAINING_RA_ROOT, "utils", "funcs.R")
  ))

  setup_path <- first_existing_path(c(
    file.path(CONFIG$TRAINING_RA_ROOT, "util", "_setup.R"),
    file.path(CONFIG$TRAINING_RA_ROOT, "utils", "_setup.R")
  ))

  if (is.null(setup_path)) {
    stop("Setup failed: cannot find _setup.R under util/ or utils/ in TRAINING_RA_ROOT.")
  }

  setup_res <- tryCatch({
    setTimeLimit(elapsed = setup_timeout_sec, transient = TRUE)
    on.exit(setTimeLimit(elapsed = Inf, transient = FALSE), add = TRUE)

    # Preload general funcs once (if present) so set_paths/read_df helpers exist.
    if (!is.null(funcs_general_path)) {
      sys.source(funcs_general_path, envir = setup_env)
    }

    # Then load project setup.
    sys.source(setup_path, envir = setup_env)
    NULL
  }, error = function(e) {
    conditionMessage(e)
  })

  if (!is.null(setup_res)) {
    stop(paste0("Setup failed: ", setup_res))
  }

  assign(cache_key, setup_env, envir = .RUNNER_SETUP_CACHE)
  setup_env
}

eval_in_training_env <- function(code, timeout_sec = 5, setup_timeout_sec = 60) {
  setup_env <- tryCatch({
    load_setup_env(setup_timeout_sec = setup_timeout_sec)
  }, error = function(e) {
    return(list(ok = FALSE, value = NULL, env = NULL, error = conditionMessage(e)))
  })

  if (is.list(setup_env) && identical(setup_env$ok, FALSE)) {
    return(setup_env)
  }

  env <- new.env(parent = setup_env)
  env$wd <- make_training_wd()

  block <- function(...) stop("Blocked in training sandbox.")
  env$system <- block
  env$system2 <- block
  env$unlink <- block
  env$file.remove <- block

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

  if (!is.null(res$env) && exists("tbl", envir = res$env, inherits = FALSE)) {
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
