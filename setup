#==============================================================================================================
# FILENAME    : _setup.R
# DESCRIPTION : Minimal setup for training app usage (analysis/derived only)
# NOTES       : Place this file under <RA_ROOT>/util/_setup.R (or utils/_setup.R)
#==============================================================================================================

cat(paste(c("The library dictionaries within which packages are looked for:", .libPaths(), "\n"), collapse = "\n"))

suppressPackageStartupMessages({
  library(conflicted)
  library(data.table)
  library(tidyverse)
  library(haven)
  library(reporter)
  library(arsenal)
})

# conflict policy
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("select", "dplyr")

# wd must exist (runner injects it). Keep fallback for local interactive run.
if (!exists("wd", inherits = FALSE)) {
  wd <- ifelse(
    interactive(),
    rstudioapi::getSourceEditorContext()$path,
    system("echo $PWD", intern = TRUE)
  )
}

# Load company general functions (support both util/ and utils/ layouts)
ra_root <- gsub(r"((^.*/vob(/\w+){3}).*$)", r"(\1)", wd)
funcs_general_path <- c(
  file.path(ra_root, "util", "funcs_general.R"),
  file.path(ra_root, "utils", "funcs_general.R"),
  file.path(ra_root, "util", "funcs.R"),
  file.path(ra_root, "utils", "funcs.R")
)
funcs_general_path <- funcs_general_path[file.exists(funcs_general_path)][1]
if (is.na(funcs_general_path) || !nzchar(funcs_general_path)) {
  stop("Cannot find funcs_general.R/funcs.R under util/ or utils/.")
}
source(funcs_general_path)

# Set absolute study/report paths
st <- set_paths(wd)

# Optional helpers: provide read_sas2(x) for objects like analysis.adsl / derived.adae
read_sas2 <- function(lib_data_symbol) {
  sym <- deparse(substitute(lib_data_symbol))
  parts <- strsplit(sym, "\\.")[[1]]
  if (length(parts) != 2) stop("Use syntax like read_sas2(analysis.adsl)")
  lib <- parts[1]
  dat <- parts[2]
  if (!lib %in% names(st)) stop(sprintf("Library '%s' not found in st", lib))

  candidates <- c(
    file.path(st[[lib]], paste0(dat, ".sas7bdat")),
    file.path(st[[lib]], paste0(dat, ".xpt")),
    file.path(st[[lib]], paste0(dat, ".rds"))
  )
  hit <- candidates[file.exists(candidates)][1]
  if (is.na(hit) || !nzchar(hit)) stop(sprintf("No file found for %s in %s", sym, st[[lib]]))

  ext <- tolower(tools::file_ext(hit))
  if (ext == "sas7bdat") return(haven::read_sas(hit))
  if (ext == "xpt") return(haven::read_xpt(hit))
  readRDS(hit)
}

cat("_setup.R executing done -------------------------------------------------\n")
