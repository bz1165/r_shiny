# Setup RA Environment ---------------------------------------------------------
wd <- ifelse(
  interactive(),
  rstudioapi::getSourceEditorContext()$path,
  system("echo $PWD", intern = TRUE)
)
source(paste0(gsub(r"((^.*/vob(/\w+){3}).*$)", r"(\1)", wd), "/util/_setup.R"))

# Reference solution -----------------------------------------------------------
library(rtables)
adsl <- read_sas2(analysis.adsl)

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ARM") %>%
  add_overall_col(label = "Overall") %>%
  split_rows_by("SEX", label = "Sex – n (%)") %>%
  summarize_row_groups(
    var = "USUBJID",
    label_fstr = "N",
    format = "xx"
  ) %>%
  analyze("USUBJID", afun = rtables::count_occurrences, format = "xx (xx.x%)")

tbl <- build_table(lyt, adsl)

tbl
