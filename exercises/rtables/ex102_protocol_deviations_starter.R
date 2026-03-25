# Setup RA Environment ---------------------------------------------------------
wd <- ifelse(
  interactive(),
  rstudioapi::getSourceEditorContext()$path,
  system("echo $PWD", intern = TRUE)
)
source(paste0(gsub(r"((^.*/vob(/\\w+){3}).*$)", r"(\\1)", wd), "/util/_setup.R"))

library(dplyr)
library(stringr)
library(rtables)

# Helper reused from training --------------------------------------------------
a_npct_subj <- function(x, labelstr, .N_col) {
  n <- length(unique(x[!is.na(x)]))

  cell <- if (n == 0L) {
    rcell(0, format = "xx")
  } else {
    rcell(c(n, n / .N_col * 100), format = "xx (xx.x)")
  }

  in_rows(" " = cell, .labels = labelstr)
}

a_any_pd <- function(x, .N_col) {
  n <- length(unique(x[!is.na(x)]))

  cell <- if (n == 0L) {
    rcell(0, format = "xx")
  } else {
    rcell(c(n, n / .N_col * 100), format = "xx (xx.x)")
  }

  in_rows("Any protocol deviation" = cell)
}

# Data prep --------------------------------------------------------------------
adsl_raw <- read_df(analysis.adsl)
dv_raw   <- read_df(derived.dv)

pick_first_chr <- function(df, candidates, default = NA_character_) {
  hit <- candidates[candidates %in% names(df)][1]
  if (is.na(hit) || !nzchar(hit)) return(rep(default, nrow(df)))
  as.character(df[[hit]])
}

ol_denom <- adsl_raw %>%
  transmute(
    USUBJID = as.character(usubjid),
    OPENFL  = toupper(pick_first_chr(adsl_raw, c("olbfl", "olsfl", "olfl"))),
    TRTSEQP = str_replace_all(
      str_squish(pick_first_chr(adsl_raw, c("trtseqp", "trt01p", "arm"))),
      "\\s*-\\s*",
      "-"
    )
  ) %>%
  filter(OPENFL == "Y") %>%
  mutate(
    ARM = case_when(
      TRTSEQP == "Inclisiran-Inclisiran" ~ "Inclisiran-Inclisiran",
      TRTSEQP == "Placebo-Inclisiran" ~ "Placebo-Inclisiran",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(ARM)) %>%
  mutate(
    ARM = factor(
      ARM,
      levels = c("Inclisiran-Inclisiran", "Placebo-Inclisiran")
    )
  ) %>%
  distinct(USUBJID, ARM)

pd_long <- dv_raw %>%
  transmute(
    USUBJID   = as.character(usubjid),
    CATEGORY  = str_squish(pick_first_chr(dv_raw, c("dvcat", "dvscat"), "Other")),
    CRIT      = str_squish(pick_first_chr(dv_raw, c("dvterm", "dvdecod", "dvobj"), "Protocol deviation"))
  ) %>%
  left_join(ol_denom, by = "USUBJID") %>%
  filter(!is.na(ARM)) %>%
  mutate(
    CATEGORY = if_else(is.na(CATEGORY) | CATEGORY == "", "Other", CATEGORY),
    CRIT = if_else(is.na(CRIT) | CRIT == "", "Protocol deviation", CRIT)
  ) %>%
  distinct(USUBJID, ARM, CATEGORY, CRIT)

split_fun <- drop_split_levels

# Exercise ---------------------------------------------------------------------
# Goal:
# 1) split columns by ARM
# 2) add overall column Total
# 3) add top row Any protocol deviation
# 4) split rows by CATEGORY, then summarize with a_npct_subj
# 5) split rows by CRIT underneath CATEGORY, then summarize with a_npct_subj
# 6) use alt_counts_df = ol_denom
# 7) add top-left text c("Category", "  Protocol deviation")

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx"
)

# TODO:
# lyt <- lyt %>%
#   split_cols_by("ARM", split_fun = split_fun) %>%
#   add_overall_col("Total") %>%
#   analyze("USUBJID", afun = a_any_pd, show_labels = "hidden", section_div = " ") %>%
#   split_rows_by("CATEGORY", split_fun = split_fun, label_pos = "visible", nested = FALSE) %>%
#   summarize_row_groups("USUBJID", cfun = a_npct_subj) %>%
#   split_rows_by("CRIT", split_fun = split_fun, nested = TRUE, indent_mod = 1L) %>%
#   summarize_row_groups("USUBJID", cfun = a_npct_subj) %>%
#   append_topleft(c("Category", "  Protocol deviation"))

tbl <- build_table(lyt, pd_long)

tbl
