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


lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) %>%
  split_cols_by("ARM", split_fun = split_fun) %>%
  analyze("USUBJID", afun = a_any_teae, show_labels = "hidden", section_div = " ") %>%
  split_rows_by("PT", split_fun = split_fun, label_pos = "visible", nested = FALSE) %>%
  summarize_row_groups("USUBJID", cfun = a_npct_subj) %>%
  append_topleft("Preferred term")

tbl <- build_table(lyt, pt_long, alt_counts_df = adsl_olb)

tbl
