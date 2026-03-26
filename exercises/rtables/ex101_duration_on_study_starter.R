# Setup RA Environment ---------------------------------------------------------
wd <- ifelse(
  interactive(),
  rstudioapi::getSourceEditorContext()$path,
  system("echo $PWD", intern = TRUE)
)
source(paste0(gsub(r"((^.*/vob(/\w+){3}).*$)", r"(\1)", wd), "/util/_setup.R"))

library(dplyr)
library(rtables)


# TODO
# 1) read the required dataset(s)
# 2) prep data
# 3) build the rtables layout
# 4) return final table object as `tbl`
