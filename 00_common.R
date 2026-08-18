# 00_common.R -- shared paths, constants and helpers
# Austin baby names 2017-2024.

suppressPackageStartupMessages({
  library(readr)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(stringi)
  library(purrr)
})

# ---- paths -------------------------------------------------------------
# The raw data directory is READ-ONLY. Nothing in this project writes to it.
PROJ_DIR <- if (dir.exists("../data")) ".." else "/Users/gv6699/Documents/Python"
DATA_DIR <- file.path(PROJ_DIR, "data")
OUT_DIR  <- file.path(PROJ_DIR, "babbies", "out")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

YEARS      <- 2017:2024
PERIOD_LAB <- "2017–2024"

# The 2017 file is a pre-aggregated CSV; 2018-2024 are Excel microdata.
csv_2017 <- file.path(DATA_DIR, "APH_Final_NRN_C320620-061826_2017.csv")
# Filenames are BUILT from the year, never globbed. "APH_..._2021 1.xlsx" is a
# byte-identical duplicate of "APH_..._2021.xlsx" (md5 f40ba983...); globbing
# *.xlsx would silently double-count 2021.
xlsx_for <- function(y) file.path(DATA_DIR, sprintf("APH_Final_NRN_C320620-061826_%d.xlsx", y))
texas_xlsx <- file.path(DATA_DIR, "Texas_baby_names.xlsx")

# ---- canonicalization --------------------------------------------------
# Order matters. See notes on each step.
canon <- function(x) {
  # 1. curly/modifier apostrophes -> straight. MUST precede transliteration:
  #    stri_trans_general("Latin-ASCII") leaves U+2019 untouched.
  x <- gsub("[‘’ʼ´`]", "'", x)
  # 2. strip diacritics. NOT iconv(): on macOS iconv//TRANSLIT yields JOS'E,
  #    ZO"E, 'ALVARO. stringi is correct -> JOSE, ZOE, ALVARO.
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  # 3. squeeze all whitespace (incl. NBSP) to single spaces
  x <- gsub("[[:space:] ]+", " ", x)
  # 4. trim + upper
  x <- toupper(trimws(x))
  # 5. drop a trailing generational suffix as its own token. Anchored at the
  #    end so the real name IV'AYHA is untouched.
  sub("\\s+(JR|SR|II|III|IV)\\.?$", "", x)
}

# Placeholder records. EXACT match only -- a prefix regex such as ^(BABY|NA)
# would wrongly eat the real names NA'RIYAH, NA'IM, NA'ILAH.
DENY <- c(
  "INFANT", "BABY", "BABY BOY", "BABY GIRL", "BABY A", "BABY B",
  "INFANT BABY", "INFANT BABY BOY", "INFANT BABY GIRL",
  "INFANT BOY", "INFANT GIRL", "INFANT A", "INFANT B",
  "INFANT MALE", "INFANT FEMALE", "BABY BOY DARLISSA",
  "JR", "SR", "II", "III", "IV", "MR.LOVING",
  "UNKNOWN", "NO NAME", "NONE", "TEST"
)
# Real 2-letter names exist (TY 19, BO 18, OM 7, CY, JO, AN), so the length
# guard only removes bare single characters.
MIN_NCHAR <- 2L

drop_reason <- function(nm) {
  dplyr::case_when(
    is.na(nm) | nm == ""      ~ "blank",
    nm %in% DENY              ~ "placeholder",
    nchar(nm) < MIN_NCHAR     ~ "single character",
    TRUE                      ~ NA_character_
  )
}

# Title Case for display, capitalising after space, hyphen and apostrophe.
title_case <- function(x) {
  gsub("(^|[ '\\-])([a-z])", "\\1\\U\\2", tolower(x), perl = TRUE)
}

# Search key used by the HTML: lowercase, accent-free, punctuation-free.
search_key <- function(x) {
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  gsub("[^a-z0-9 ]", "", tolower(x))
}

msg <- function(...) cat(sprintf(...), "\n", sep = "")
