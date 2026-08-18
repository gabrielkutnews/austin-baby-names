# 01_clean.R -- ingest the 9 Austin sources + the Texas reference, canonicalize,
# drop placeholders, and write auditable evidence of every decision.

source("00_common.R")

# ---- 1. ingest ---------------------------------------------------------

# 2017: pre-aggregated CSV. Sex is GIRL/BOY here and FEMALE/MALE everywhere else.
a2017 <- read_csv(csv_2017, col_types = "ccc") %>%
  setNames(c("sex", "name_raw", "n")) %>%
  mutate(
    year = 2017L,
    n    = as.integer(n),
    sex  = recode(sex, GIRL = "FEMALE", BOY = "MALE")
  ) %>%
  select(year, sex, name_raw, n)

# 2018-2024: row-level microdata. Sheet is selected BY NAME -- sheet 1 of each
# workbook ("<YEAR> Counts") is a flattened, broken pivot table and must not be
# read. col_types="text" stops Excel serial coercion.
read_micro <- function(y) {
  read_excel(xlsx_for(y), sheet = as.character(y), col_types = "text") %>%
    setNames(c("name_raw", "sex")) %>%
    mutate(year = as.integer(y)) %>%
    count(year, sex, name_raw, name = "n")
}
a_micro <- map_dfr(2018:2024, read_micro)

austin_raw <- bind_rows(a2017, a_micro) %>%
  mutate(sex = if_else(sex %in% c("FEMALE", "MALE"), sex, "UNKNOWN"))

msg("ingested %s rows, %s births, %d years",
    format(nrow(austin_raw), big.mark = ","),
    format(sum(austin_raw$n), big.mark = ","),
    n_distinct(austin_raw$year))

# ---- 2. canonicalize ---------------------------------------------------

austin_c <- austin_raw %>%
  mutate(
    name_canon = canon(name_raw),
    reason     = drop_reason(name_canon)
  )

# ---- 3. audit: dropped records ----------------------------------------

dropped <- austin_c %>%
  filter(!is.na(reason)) %>%
  count(year, sex, name_raw, name_canon, reason, wt = n, name = "births") %>%
  arrange(desc(births), name_canon)

write_csv(dropped, file.path(OUT_DIR, "audit_dropped.csv"))
msg("dropped %d records / %d births -> audit_dropped.csv",
    nrow(dropped), sum(dropped$births))

# ---- 4. audit: suffix strips ------------------------------------------

suffixed <- austin_c %>%
  filter(is.na(reason), toupper(trimws(name_raw)) != name_canon,
         grepl("\\s+(JR|SR|II|III|IV)\\.?$", toupper(trimws(name_raw)))) %>%
  count(year, name_raw, name_canon, wt = n, name = "births") %>%
  arrange(name_canon)

write_csv(suffixed, file.path(OUT_DIR, "audit_suffix.csv"))
msg("stripped a generational suffix from %d records -> audit_suffix.csv", nrow(suffixed))

# ---- 5. keep + collapse ------------------------------------------------

austin <- austin_c %>%
  filter(is.na(reason)) %>%
  group_by(year, sex, name_canon) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  mutate(name_display = title_case(name_canon))

write_csv(austin, file.path(OUT_DIR, "austin_clean.csv"))

# ---- 6. audit: fold groups (spellings that merged) --------------------
# This is the payoff of the diacritic fix: 2017-18 were stripped upstream,
# 2019-24 were not, so one name was split across up to five spellings.

folds <- austin_c %>%
  filter(is.na(reason)) %>%
  group_by(name_canon) %>%
  filter(n_distinct(name_raw) > 1) %>%
  summarise(
    n_variants = n_distinct(name_raw),
    variants   = paste(sort(unique(name_raw)), collapse = " | "),
    births     = sum(n),
    .groups    = "drop"
  ) %>%
  arrange(desc(births))

write_csv(folds, file.path(OUT_DIR, "audit_folds.csv"))
msg("%d fold-groups merged >1 raw spelling -> audit_folds.csv", nrow(folds))

# Per-name variant list, consumed by the HTML detail panel.
variant_map <- austin_c %>%
  filter(is.na(reason)) %>%
  count(name_canon, name_raw, wt = n, name = "births") %>%
  arrange(name_canon, desc(births))
write_csv(variant_map, file.path(OUT_DIR, "name_variants.csv"))

# ---- 7. audit: totals reconciliation ----------------------------------

totals <- austin_raw %>%
  group_by(year) %>% summarise(births_raw = sum(n), .groups = "drop") %>%
  left_join(austin %>% group_by(year) %>% summarise(births_clean = sum(n), .groups = "drop"),
            by = "year") %>%
  mutate(dropped = births_raw - births_clean)

totals <- bind_rows(totals, summarise(totals,
  year = NA_integer_, births_raw = sum(births_raw),
  births_clean = sum(births_clean), dropped = sum(dropped)))

write_csv(totals, file.path(OUT_DIR, "audit_totals.csv"))
print(as.data.frame(totals))

stopifnot(sum(austin_raw$n) == 169366L)   # guard: the 2021 duplicate is excluded

# ---- 8. Texas reference ------------------------------------------------
# 8 sheets, one per year, wide with duplicated headers:
#   Rank | Name...2 | males | Name...4 | females      -- TOP 100 ONLY per sex.

read_tx <- function(s) {
  t <- read_excel(texas_xlsx, sheet = s, col_types = "text")
  bind_rows(
    tibble(sex = "MALE",   tx_rank = as.integer(t[[1]]), nm = t[[2]], tx_n = as.integer(t[[3]])),
    tibble(sex = "FEMALE", tx_rank = as.integer(t[[1]]), nm = t[[4]], tx_n = as.integer(t[[5]]))
  ) %>% mutate(year = as.integer(s))
}
texas <- map_dfr(as.character(YEARS), read_tx) %>%
  transmute(year, sex, tx_rank, name_canon = canon(nm), tx_n) %>%
  filter(!is.na(name_canon), name_canon != "")

write_csv(texas, file.path(OUT_DIR, "texas_clean.csv"))
msg("texas: %d rows, ranks %d-%d, %d years",
    nrow(texas), min(texas$tx_rank), max(texas$tx_rank), n_distinct(texas$year))

msg("01_clean.R done.")
