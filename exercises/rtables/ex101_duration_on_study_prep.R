library(dplyr)
library(rtables)

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

adsl_raw <- read_df(analysis.adsl)

adsl_fas <- adsl_raw %>%
  transmute(
    USUBJID = as.character(usubjid),
    FASFL   = toupper(trimws(as.character(fasfl))),
    ARM_RAW = gsub("\\s*-\\s*", "-", trimws(as.character(trtseqa))),
    TRTSDT  = trtsdt,
    EOSDT   = eosdt
  ) %>%
  filter(FASFL == "Y") %>%
  mutate(
    ARM = case_when(
      ARM_RAW == "Inclisiran-Inclisiran" ~ "Inclisiran-Inclisiran",
      ARM_RAW == "Placebo-Inclisiran" ~ "Placebo-Inclisiran",
      TRUE ~ NA_character_
    ),
    DURSTDY = as.integer(EOSDT - TRTSDT + 1)
  ) %>%
  filter(!is.na(ARM), !is.na(DURSTDY)) %>%
  transmute(
    USUBJID,
    ARM = factor(
      ARM,
      levels = c("Inclisiran-Inclisiran", "Placebo-Inclisiran")
    ),
    DURSTDY
  ) %>%
  distinct()

split_fun <- drop_split_levels
