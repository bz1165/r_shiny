suppressPackageStartupMessages({
  library(rtables)
})

.RUNNER_SETUP_CACHE <- new.env(parent = emptyenv())

first_existing_path <- function(paths) {
  hit <- paths[file.exists(paths)][1]
  if (is.na(hit) || !nzchar(hit)) return(NULL)
  hit
}

make_training_wd <- function(training_ra_root) {
  # MUST be a FILE path (company set_paths expects program path)
  file.path(training_ra_root, "pgm", "shiny_runner.R")
}

strip_setup_snippet <- function(code) {
  if (is.null(code) || !nzchar(code)) return(code)

  pattern <- paste0(
    "(?s)",
    "^\\s*#\\s*Setup RA Environment.*?",
    "wd\\s*<-\\s*ifelse\\s*\\(.*?\\)\\s*",
    "source\\s*\\(\\s*paste0\\s*\\(.*?\\)\\s*\\)\\s*"
  )

  out <- sub(pattern, "", code, perl = TRUE)
  out <- sub("^\\s+", "", out, perl = TRUE)
  out
}

load_setup_env <- function(training_ra_root, setup_timeout_sec = 90, force_reload = FALSE) {
  cache_key <- gsub("[^A-Za-z0-9_]", "_", training_ra_root)

  if (!force_reload && exists(cache_key, envir = .RUNNER_SETUP_CACHE, inherits = FALSE)) {
    return(get(cache_key, envir = .RUNNER_SETUP_CACHE, inherits = FALSE))
  }

  setup_path <- first_existing_path(c(
    file.path(training_ra_root, "util", "_setup.R"),
    file.path(training_ra_root, "utils", "_setup.R")
  ))

  if (is.null(setup_path)) {
    stop("Setup failed: cannot find _setup.R under util/ or utils/ in training RA root.")
  }

  setup_env <- new.env(parent = globalenv())
  setup_env$wd <- make_training_wd(training_ra_root)

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
    stop("Setup loaded but st$analysis/st$derived missing. Please set them in _setup.R.")
  }

  assign(cache_key, setup_env, envir = .RUNNER_SETUP_CACHE)
  setup_env
}

eval_in_training_env <- function(code, training_ra_root, timeout_sec = 10, setup_timeout_sec = 90) {
  setup_env <- tryCatch(
    load_setup_env(training_ra_root = training_ra_root, setup_timeout_sec = setup_timeout_sec),
    error = function(e) {
      return(list(ok = FALSE, value = NULL, env = NULL, error = conditionMessage(e)))
    }
  )

  if (is.list(setup_env) && identical(setup_env$ok, FALSE)) {
    return(setup_env)
  }

  env <- new.env(parent = setup_env)
  env$wd <- make_training_wd(training_ra_root)

  block <- function(...) stop("Blocked in training sandbox.")
  env$system <- block
  env$system2 <- block
  env$unlink <- block
  env$file.remove <- block

  tryCatch({
    setTimeLimit(elapsed = timeout_sec, transient = TRUE)
    on.exit(setTimeLimit(elapsed = Inf, transient = FALSE), add = TRUE)

    code2 <- strip_setup_snippet(code)

    val <- eval(parse(text = code2), envir = env)
    list(ok = TRUE, value = val, env = env, error = NULL)
  }, error = function(e) {
    list(ok = FALSE, value = NULL, env = env, error = paste0("Code execution failed: ", conditionMessage(e)))
  })
}

extract_table <- function(res) {
  if (!res$ok) return(NULL)

  if (inherits(res$value, "TableTree")) return(res$value)

  if (!is.null(res$env) && exists("tbl", envir = res$env, inherits = TRUE)) {
    t <- get("tbl", envir = res$env, inherits = TRUE)
    if (inherits(t, "TableTree")) return(t)
  }

  NULL
}

run_user_code <- function(code, training_ra_root, timeout_sec = 10) {
  res <- eval_in_training_env(
    code = code,
    training_ra_root = training_ra_root,
    timeout_sec = timeout_sec
  )

  if (!res$ok) {
    return(list(ok = FALSE, tbl = NULL, msg = res$error))
  }

  tbl <- extract_table(res)
  if (is.null(tbl)) {
    return(list(
      ok = FALSE,
      tbl = NULL,
      msg = paste(
        "Code ran but no rtables TableTree was returned.",
        "Please ensure your final object is `tbl` (build_table output)."
      )
    ))
  }

  list(ok = TRUE, tbl = tbl, msg = "OK")
}

run_reference_code <- function(reference_file, training_ra_root, timeout_sec = 10) {
  code <- paste(readLines(reference_file, warn = FALSE), collapse = "\n")
  run_user_code(
    code = code,
    training_ra_root = training_ra_root,
    timeout_sec = timeout_sec
  )
}
