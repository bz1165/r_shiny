suppressPackageStartupMessages({
  library(rtables)
})

source("util/config.R")

.RUNNER_SETUP_CACHE <- new.env(parent = emptyenv())

make_training_wd <- function(training_ra_root) {
  # Must be a file path because company _setup.R / set_paths() expects a program path
  file.path(training_ra_root, "pgm", "shiny_runner.R")
}

first_existing_path <- function(paths) {
  hit <- paths[file.exists(paths)][1]
  if (is.na(hit) || !nzchar(hit)) return(NULL)
  hit
}

load_setup_env <- function(
  training_ra_root = CONFIG$TRAINING_RA_ROOT,
  setup_timeout_sec = 90,
  force_reload = FALSE
) {
  cache_key <- gsub("[^A-Za-z0-9_]", "_", training_ra_root)

  if (!force_reload && exists(cache_key, envir = .RUNNER_SETUP_CACHE, inherits = FALSE)) {
    return(get(cache_key, envir = .RUNNER_SETUP_CACHE, inherits = FALSE))
  }

  setup_path <- first_existing_path(c(
    file.path(training_ra_root, "util", "_setup.R"),
    file.path(training_ra_root, "utils", "_setup.R")
  ))

  if (is.null(setup_path)) {
    stop("Setup failed: cannot find _setup.R under util/ or utils/ in TRAINING_RA_ROOT.")
  }

  # Use globalenv() as parent so package search path behaves like normal interactive use
  setup_env <- new.env(parent = globalenv())
  setup_env$wd <- make_training_wd(training_ra_root)

  # Soft fallback only if company setup references conflict_prefer before attaching conflicted
  if (!exists("conflict_prefer", envir = setup_env, inherits = TRUE)) {
    setup_env$conflict_prefer <- function(...) invisible(NULL)
  }

  setup_res <- tryCatch({
    setTimeLimit(elapsed = setup_timeout_sec, transient = TRUE)
    on.exit(setTimeLimit(elapsed = Inf, transient = FALSE), add = TRUE)

    sys.source(setup_path, envir = setup_env)
    NULL
  }, error = function(e) conditionMessage(e))

  if (!is.null(setup_res)) {
    stop(paste0("Setup failed: ", setup_res))
  }

  if (!exists("st", envir = setup_env, inherits = TRUE)) {
    stop("Setup failed: object `st` not created. Check set_paths(wd) in _setup.R.")
  }

  st0 <- get("st", envir = setup_env, inherits = TRUE)
  if (is.null(st0$analysis) || is.null(st0$derived)) {
    stop("Setup loaded but st$analysis/st$derived are missing.")
  }

  assign(cache_key, setup_env, envir = .RUNNER_SETUP_CACHE)
  setup_env
}

# Evaluate line-by-line so runtime errors can be annotated with source line number
eval_code_with_lines <- function(code, env, timeout_sec = 10) {
  exprs <- parse(text = code, keep.source = TRUE)
  srcrefs <- attr(exprs, "srcref")

  last_value <- NULL

  setTimeLimit(elapsed = timeout_sec, transient = TRUE)
  on.exit(setTimeLimit(elapsed = Inf, transient = FALSE), add = TRUE)

  for (i in seq_along(exprs)) {
    line_no <- i
    if (!is.null(srcrefs) && length(srcrefs) >= i && !is.null(srcrefs[[i]])) {
      line_no <- srcrefs[[i]][1]
    }

    last_value <- tryCatch(
      eval(exprs[[i]], envir = env),
      error = function(e) {
        stop(sprintf("Line %s: %s", line_no, conditionMessage(e)), call. = FALSE)
      }
    )
  }

  last_value
}

is_rtables_table <- function(x) {
  inherits(x, c("TableTree", "VTableTree"))
}

eval_in_training_env <- function(
  code,
  training_ra_root = CONFIG$TRAINING_RA_ROOT,
  timeout_sec = 10,
  setup_timeout_sec = 90
) {
  setup_env <- tryCatch(
    load_setup_env(
      training_ra_root = training_ra_root,
      setup_timeout_sec = setup_timeout_sec
    ),
    error = function(e) {
      return(list(ok = FALSE, value = NULL, env = NULL, error = conditionMessage(e)))
    }
  )

  if (is.list(setup_env) && identical(setup_env$ok, FALSE)) {
    return(setup_env)
  }

  env <- new.env(parent = setup_env)
  env$wd <- make_training_wd(training_ra_root)

  # Minimal safety blocks
  block <- function(...) stop("Blocked in training sandbox.")
  env$system <- block
  env$system2 <- block
  env$unlink <- block
  env$file.remove <- block

  tryCatch({
    val <- eval_code_with_lines(code = code, env = env, timeout_sec = timeout_sec)
    list(ok = TRUE, value = val, env = env, error = NULL)
  }, error = function(e) {
    list(ok = FALSE, value = NULL, env = env, error = conditionMessage(e))
  })
}

extract_table <- function(res) {
  if (!res$ok) return(NULL)

  if (is_rtables_table(res$value)) return(res$value)

  if (!is.null(res$env) && exists("tbl", envir = res$env, inherits = TRUE)) {
    t <- get("tbl", envir = res$env, inherits = TRUE)
    if (is_rtables_table(t)) return(t)
  }

  NULL
}

run_user_code <- function(
  code,
  training_ra_root = CONFIG$TRAINING_RA_ROOT,
  timeout_sec = 10
) {
  res <- eval_in_training_env(
    code = code,
    training_ra_root = training_ra_root,
    timeout_sec = timeout_sec
  )

  if (!res$ok) {
    return(list(ok = FALSE, tbl = NULL, env = res$env, msg = res$error))
  }

  tbl <- extract_table(res)
  if (is.null(tbl)) {
    return(list(
      ok = FALSE,
      tbl = NULL,
      env = res$env,
      msg = "Code ran but no rtables table object was returned. Please ensure your final object is `tbl` (build_table output)."
    ))
  }

  list(ok = TRUE, tbl = tbl, env = res$env, msg = "OK")
}

run_reference_code <- function(
  reference_file,
  training_ra_root = CONFIG$TRAINING_RA_ROOT,
  timeout_sec = 10
) {
  code <- paste(readLines(reference_file, warn = FALSE), collapse = "\n")
  run_user_code(
    code = code,
    training_ra_root = training_ra_root,
    timeout_sec = timeout_sec
  )
}
