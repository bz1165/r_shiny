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
library(lubridate)

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

pick_first_chr <- function(df, candidates, default = NA_character_) {
  hit <- candidates[candidates %in% names(df)][1]
  if (is.na(hit) || !nzchar(hit)) return(rep(default, nrow(df)))
  as.character(df[[hit]])
}

pick_first_date <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)][1]
  if (is.na(hit) || !nzchar(hit)) return(rep(as.Date(NA), nrow(df)))

  x <- df[[hit]]

  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))

  x_chr <- as.character(x)
  x_chr[x_chr %in% c("", "NA", ".")] <- NA_character_

  out <- suppressWarnings(as.Date(x_chr))
  bad <- is.na(out) & !is.na(x_chr)
  if (any(bad)) {
    out[bad] <- suppressWarnings(as.Date(parse_date_time(
      x_chr[bad],
      orders = c("Ymd", "d b Y", "dB Y", "dmy", "dmy HMS", "Ymd HMS")
    )))
  }
  out
}

adsl_fas <- adsl_raw %>%
  transmute(
    USUBJID = as.character(usubjid),
    FASFL   = toupper(pick_first_chr(adsl_raw, c("fasfl"))),
    TRTSEQP = str_replace_all(
      str_squish(pick_first_chr(adsl_raw, c("trtseqp", "trt01p", "arm"))),
      "\\s*-\\s*",
      "-"
    ),
    FIRSTDT = pick_first_date(adsl_raw, c("rfstdtc", "rfstdt", "trtsdt", "trtstdt")),
    LASTDT  = pick_first_date(adsl_raw, c("lastsvdt", "eosdt", "eot02dt", "acutdt"))
  ) %>%
  filter(FASFL == "Y") %>%
  mutate(
    ARM = case_when(
      TRTSEQP == "Inclisiran-Inclisiran" ~ "Inclisiran-Inclisiran",
      TRTSEQP == "Placebo-Inclisiran" ~ "Placebo-Inclisiran",
      TRUE ~ NA_character_
    ),
    DURSTDY = as.integer(LASTDT - FIRSTDT + 1)
  ) %>%
  filter(!is.na(ARM), !is.na(DURSTDY)) %>%
  mutate(
    ARM = factor(
      ARM,
      levels = c("Inclisiran-Inclisiran", "Placebo-Inclisiran")
    )
  ) %>%
  distinct(USUBJID, ARM, DURSTDY)

split_fun <- drop_split_levels

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
