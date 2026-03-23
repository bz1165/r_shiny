# Setup RA Environment ---------------------------------------------------------
wd <- ifelse(
  interactive(),
  rstudioapi::getSourceEditorContext()$path,
  system("echo $PWD", intern = TRUE)
)


# Exercise ---------------------------------------------------------------------
# Goal:
# 1) split columns by ARM + Overall
# 2) add row group label "Sex – n (%)"
# 3) add an N row under the row group label
# 4) show SEX counts as xx (xx.x%)

library(rtables)
adsl <- read_sas2(analysis.adsl)

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ARM") %>%
  add_overall_col(label = "Overall") %>%
  split_rows_by("SEX", label = "Sex – n (%)") %>%
  analyze("USUBJID", afun = rtables::count_occurrences, format = "xx (xx.x%)")

tbl <- build_table(lyt, adsl)

tbl
