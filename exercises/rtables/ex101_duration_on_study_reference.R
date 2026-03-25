library(rtables)

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) %>%
  split_cols_by("ARM", split_fun = split_fun) %>%
  add_overall_col("Total") %>%
  analyze("DURSTDY", afun = a_num_shell, show_labels = "hidden") %>%
  append_topleft("Duration on study (days)")

tbl <- build_table(lyt, adsl_fas)

tbl
