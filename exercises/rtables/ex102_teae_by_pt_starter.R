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


# Complete the layout.
# Requirements:
# - split columns by ARM
# - top row: Number of subjects with at least one event
# - split rows by PT
# - subject-level n (%) using a_npct_subj
# - add top-left text: Preferred term
# - use alt_counts_df = adsl_olb

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx"
)

# TODO

tbl <- build_table(lyt, pt_long, alt_counts_df = adsl_olb)

tbl
