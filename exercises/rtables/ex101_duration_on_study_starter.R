# Setup RA Environment ---------------------------------------------------------
wd <- ifelse(
  interactive(),
  rstudioapi::getSourceEditorContext()$path,
  system("echo $PWD", intern = TRUE)
)
source(paste0(gsub(r"((^.*/vob(/\w+){3}).*$)", r"(\1)", wd), "/util/_setup.R"))

library(dplyr)
library(rtables)


# Complete the layout.
# Requirements:
# - split columns by ARM
# - add overall column Total
# - analyze DURSTDY using a_num_shell
# - add top-left text: Duration on study (days)

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx"
)

# TODO

tbl <- build_table(lyt, adsl_fas)

tbl
