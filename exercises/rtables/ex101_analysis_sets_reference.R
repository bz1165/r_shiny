# Setup RA Environment ---------------------------------------------------------
wd <- ifelse(
  interactive(),
  rstudioapi::getSourceEditorContext()$path,
  system("echo $PWD", intern = TRUE)
)
source(paste0(gsub(r"((^.*/vob(/\w+){3}).*$)", r"(\1)", wd), "/util/_setup.R"))

library(dplyr)
library(stringr)
library(rtables)

# Load + prepare data ----------------------------------------------------------
adsl_raw <- read_df(analysis.adsl)

pick_first_chr <- function(df, candidates, default = NA_character_) {
  hit <- candidates[candidates %in% names(df)][1]
  if (is.na(hit) || !nzchar(hit)) {
    return(rep(default, nrow(df)))
  }
  as.character(df[[hit]])
}

subject_base <- tibble(
  USUBJID = as.character(adsl_raw$usubjid),
  SCREENFL = toupper(pick_first_chr(adsl_raw, c("scrnfl", "enrlfl"))),
  RANDFL   = toupper(pick_first_chr(adsl_raw, c("randfl"))),
  FASFL    = toupper(pick_first_chr(adsl_raw, c("fasfl"))),
  SAFFL    = toupper(pick_first_chr(adsl_raw, c("saffl"))),
  OPENFL   = toupper(pick_first_chr(adsl_raw, c("olbfl", "olsfl", "olfl"))),
  TRTSEQP  = str_replace_all(
    str_squish(pick_first_chr(adsl_raw, c("trtseqp", "arm", "trt01p"))),
    "\\s*-\\s*",
    "-"
  )
) %>%
  distinct() %>%
  mutate(
    COL_RAW = case_when(
      RANDFL != "Y" | is.na(TRTSEQP) | TRTSEQP == "" ~ "SF",
      TRTSEQP == "Inclisiran-Inclisiran" ~ "II",
      TRTSEQP == "Placebo-Inclisiran" ~ "PI",
      TRUE ~ "SF"
    )
  )

n_ii <- subject_base %>%
  filter(RANDFL == "Y", COL_RAW == "II") %>%
  summarise(n = n_distinct(USUBJID)) %>%
  pull(n)

n_pi <- subject_base %>%
  filter(RANDFL == "Y", COL_RAW == "PI") %>%
  summarise(n = n_distinct(USUBJID)) %>%
  pull(n)

n_total <- n_ii + n_pi

col_levels <- c("SF", "II", "PI", "TOT")
col_labels <- c(
  "Screen Failures/Not Assigned",
  paste0("Inclisiran-Inclisiran\nN=", n_ii, " n (%)"),
  paste0("Placebo-Inclisiran\nN=", n_pi, " n (%)"),
  paste0("Total\nN=", n_total, " n (%)")
)

make_set_rows <- function(df, keep_flag, set_label, include_screen = FALSE) {
  d <- df %>% filter(.data[[keep_flag]] == "Y")

  if (!include_screen) {
    d <- d %>% filter(COL_RAW %in% c("II", "PI"))
  }

  bind_rows(
    d %>% transmute(USUBJID, SET = set_label, COL = COL_RAW),
    d %>% transmute(USUBJID, SET = set_label, COL = "TOT")
  )
}

aset_long <- bind_rows(
  make_set_rows(subject_base, "SCREENFL", "Screened set", include_screen = TRUE),
  make_set_rows(subject_base, "RANDFL", "Randomized set", include_screen = FALSE),
  make_set_rows(subject_base, "FASFL",  "Full analysis set", include_screen = FALSE),
  make_set_rows(subject_base, "SAFFL",  "Safety set", include_screen = FALSE),
  make_set_rows(subject_base, "OPENFL", "Open-label set", include_screen = FALSE)
) %>%
  distinct() %>%
  mutate(
    SET = factor(
      SET,
      levels = c(
        "Screened set",
        "Randomized set",
        "Full analysis set",
        "Safety set",
        "Open-label set"
      )
    ),
    COL = factor(COL, levels = col_levels, labels = col_labels)
  )

aset_counts <- bind_rows(
  subject_base %>% filter(COL_RAW == "SF") %>% transmute(USUBJID, COL = "SF"),
  subject_base %>% filter(RANDFL == "Y", COL_RAW == "II") %>% transmute(USUBJID, COL = "II"),
  subject_base %>% filter(RANDFL == "Y", COL_RAW == "PI") %>% transmute(USUBJID, COL = "PI"),
  subject_base %>% filter(RANDFL == "Y", COL_RAW %in% c("II", "PI")) %>% transmute(USUBJID, COL = "TOT")
) %>%
  distinct() %>%
  mutate(
    COL = factor(COL, levels = col_levels, labels = col_labels)
  )

split_fun <- drop_split_levels

lyt <- basic_table(show_colcounts = FALSE) %>%
  split_cols_by("COL", split_fun = split_fun) %>%
  split_rows_by(
    "SET",
    split_label = "Analysis set",
    split_fun = split_fun,
    label_pos = "visible",
    nested = FALSE
  ) %>%
  summarize_row_groups("USUBJID", cfun = a_set_row_cell) %>%
  append_topleft("Analysis set")

tbl <- build_table(lyt, aset_long, alt_counts_df = aset_counts)

tbl

# Reusable helper functions ----------------------------------------------------
a_set_row_cell <- function(x, labelstr, .N_col) {
  n <- length(unique(x[!is.na(x)]))

  cell <- if (identical(labelstr, "Screened set")) {
    if (n == 0L) rcell("") else rcell(n, format = "xx")
  } else {
    if (n == 0L) {
      rcell("")
    } else {
      rcell(c(n, n / .N_col * 100), format = "xx (xx.x)")
    }
  }

  in_rows(" " = cell, .labels = labelstr)
}
