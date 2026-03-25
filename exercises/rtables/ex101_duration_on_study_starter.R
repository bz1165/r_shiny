# Setup RA Environment ---------------------------------------------------------
wd <- ifelse(
  interactive(),
  rstudioapi::getSourceEditorContext()$path,
  system("echo $PWD", intern = TRUE)
)
source(paste0(gsub(r"((^.*/vob(/\w+){3}).*$)", r"(\1)", wd), "/util/_setup.R"))

library(dplyr)
library(rtables)

# Helper reused from training --------------------------------------------------
a_num_shell <- function(x) {
  x <- x[!is.na(x)]
  qs <- quantile(x, probs = c(0.25, 0.75), names = FALSE, type = 2)

  in_rows(
    "N"         = length(x),
    "Mean (SD)" = rcell(c(mean(x), sd(x)), format = "xx.x (xx.xx)"),
    "Median"    = rcell(median(x), format = "xx.x"),
    "Q1-Q3"     = rcell(qs, format = "xx.x - xx.x"),
    "Min-Max"   = rcell(range(x), format = "xx - xx")
  )
}

# Data prep --------------------------------------------------------------------
adsl_raw <- read_df(analysis.adsl)

adsl_fas <- adsl_raw %>%
  transmute(
    USUBJID  = as.character(usubjid),
    FASFL    = toupper(as.character(fasfl)),
    TRTSEQP  = as.character(trtseqp),
    TRTSDT   = trtsdt,
    LASTSVDT = lastsvdt,
    EOSDT    = eosdt
  ) %>%
  filter(FASFL == "Y") %>%
  mutate(
    ARM = case_when(
      TRTSEQP == "Inclisiran-Inclisiran" ~ "Inclisiran-Inclisiran",
      TRTSEQP == "Placebo-Inclisiran" ~ "Placebo-Inclisiran",
      TRUE ~ NA_character_
    ),
    LASTDT = coalesce(LASTSVDT, EOSDT),
    DURSTDY = as.integer(LASTDT - TRTSDT + 1)
  ) %>%
  filter(!is.na(ARM), !is.na(TRTSDT), !is.na(LASTDT), !is.na(DURSTDY)) %>%
  mutate(
    ARM = factor(
      ARM,
      levels = c("Inclisiran-Inclisiran", "Placebo-Inclisiran")
    )
  ) %>%
  distinct(USUBJID, ARM, DURSTDY)

split_fun <- drop_split_levels

# Exercise ---------------------------------------------------------------------
# Goal:
# 1) split columns by ARM
# 2) add overall column Total
# 3) analyze DURSTDY using a_num_shell
# 4) add top-left text "Duration on study (days)"

lyt <- basic_table(
  show_colcounts = TRUE,
  colcount_format = "N=xx"
)

# TODO:
# lyt <- lyt %>%
#   split_cols_by("ARM", split_fun = split_fun) %>%
#   add_overall_col("Total") %>%
#   analyze("DURSTDY", afun = a_num_shell, show_labels = "hidden") %>%
#   append_topleft("Duration on study (days)")

tbl <- build_table(lyt, adsl_fas)

tbl
