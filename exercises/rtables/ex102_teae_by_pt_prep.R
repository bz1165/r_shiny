library(dplyr)
library(rtables)

a_npct_subj <- function(x, labelstr, .N_col) {
  n <- length(unique(x[!is.na(x)]))

  cell <- if (n == 0L) {
    rcell(0, format = "xx")
  } else {
    rcell(c(n, n / .N_col * 100), format = "xx (xx.x)")
  }

  in_rows(" " = cell, .labels = labelstr)
}

a_any_teae <- function(x, .N_col) {
  n <- length(unique(x[!is.na(x)]))

  cell <- if (n == 0L) {
    rcell(0, format = "xx")
  } else {
    rcell(c(n, n / .N_col * 100), format = "xx (xx.x)")
  }

  in_rows("Number of subjects with at least one event" = cell)
}

adsl_raw <- read_df(analysis.adsl)
adae_raw <- read_df(analysis.adae)

adsl_olb <- adsl_raw %>%
  transmute(
    USUBJID = as.character(usubjid),
    OLBFL   = toupper(trimws(as.character(olbfl))),
    ARM_RAW = gsub("\\s*-\\s*", "-", trimws(as.character(trtseqa)))
  ) %>%
  filter(OLBFL == "Y") %>%
  mutate(
    ARM = case_when(
      ARM_RAW == "Inclisiran-Inclisiran" ~ "Inclisiran-Inclisiran",
      ARM_RAW == "Placebo-Inclisiran" ~ "Placebo-Inclisiran",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(ARM)) %>%
  transmute(
    USUBJID,
    ARM = factor(
      ARM,
      levels = c("Inclisiran-Inclisiran", "Placebo-Inclisiran")
    )
  ) %>%
  distinct()

adae_teae <- adae_raw %>%
  transmute(
    USUBJID = as.character(usubjid),
    TRTEMFL = toupper(trimws(as.character(trtemfl))),
    AEDECOD = trimws(as.character(aedecod))
  ) %>%
  filter(TRTEMFL == "Y") %>%
  left_join(adsl_olb, by = "USUBJID") %>%
  filter(!is.na(ARM), !is.na(AEDECOD), AEDECOD != "") %>%
  distinct(USUBJID, ARM, AEDECOD)

pt_order_ii <- adae_teae %>%
  filter(ARM == "Inclisiran-Inclisiran") %>%
  count(AEDECOD, sort = TRUE) %>%
  pull(AEDECOD)

pt_all <- sort(unique(adae_teae$AEDECOD))
pt_levels <- c(pt_order_ii, setdiff(pt_all, pt_order_ii))

pt_long <- adae_teae %>%
  transmute(
    USUBJID,
    ARM,
    PT = factor(AEDECOD, levels = pt_levels)
  ) %>%
  distinct()

split_fun <- drop_split_levels
