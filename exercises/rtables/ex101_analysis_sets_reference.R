# Setup RA Environment ---------------------------------------------------------
wd <- ifelse(
  interactive(),
  rstudioapi::getSourceEditorContext()$path,
  system("echo $PWD", intern = TRUE)
)
source(paste0(gsub(r"((^.*/vob(/\w+){3}).*$)", r"(\1)", wd), "/util/_setup.R"))

library(dplyr)
library(rtables)

# Load + prepare data ----------------------------------------------------------
adsl_raw <- read_df(analysis.adsl)

aset_df <- adsl_raw %>%
  transmute(
    USUBJID = as.character(usubjid),
    ARM     = as.character(trt01p),
    RANDFL  = toupper(as.character(randfl)),
    FASFL   = toupper(as.character(fasfl)),
    SAFFL   = toupper(as.character(saffl)),
    OLSFL   = toupper(as.character(olsfl))
  ) %>%
  distinct()

aset_long <- bind_rows(
  aset_df %>%
    filter(RANDFL == "Y") %>%
    transmute(USUBJID, ARM, SET = "Randomized set"),

  aset_df %>%
    filter(FASFL == "Y") %>%
    transmute(USUBJID, ARM, SET = "Full analysis set"),

  aset_df %>%
    filter(SAFFL == "Y") %>%
    transmute(USUBJID, ARM, SET = "Safety set"),

  aset_df %>%
    filter(OLSFL == "Y") %>%
    transmute(USUBJID, ARM, SET = "Open-label set")
) %>%
  mutate(
    SET = factor(
      SET,
      levels = c("Randomized set", "Full analysis set", "Safety set", "Open-label set")
    )
  )

# Reusable helper functions ----------------------------------------------------
a_npct_subj <- function(x, labelstr, .N_col) {
  n <- length(unique(x[!is.na(x)]))

  cell <- if (n == 0L) {
    rcell(0, format = "xx")
  } else {
    rcell(c(n, n / .N_col * 100), format = "xx (xx.x)")
  }

  in_rows(" " = cell, .labels = labelstr)
}

split_fun <- drop_split_levels

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ARM", split_fun = split_fun) %>%
  add_overall_col("Total") %>%
  split_rows_by(
    "SET",
    split_label = "Analysis set",
    split_fun = split_fun,
    label_pos = "visible",
    nested = FALSE
  ) %>%
  summarize_row_groups("USUBJID", cfun = a_npct_subj)

tbl <- build_table(lyt, aset_long)

tbl
