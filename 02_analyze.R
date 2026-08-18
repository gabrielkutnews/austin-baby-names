# 02_analyze.R -- rankings by year, over 2017-2024, and Austin vs Texas.

source("00_common.R")

austin <- read_csv(file.path(OUT_DIR, "austin_clean.csv"), col_types = cols(
  year = col_integer(), sex = col_character(),
  name_canon = col_character(), n = col_integer(), name_display = col_character()
))
texas <- read_csv(file.path(OUT_DIR, "texas_clean.csv"), col_types = cols(
  year = col_integer(), sex = col_character(), tx_rank = col_integer(),
  name_canon = col_character(), tx_n = col_integer()
))

# ---- ranking helper ----------------------------------------------------
# Rank on rate per 1,000 births so years are comparable: Austin grew from
# 19,896 births in 2017 to 23,248 in 2024. Within a single year+sex the rate is
# a monotonic transform of the count, so the rank is the same either way -- the
# rate is what makes 2017 and 2024 comparable to each other.
# Tie-break: rate desc -> count desc -> name A-Z. min_rank() = competition
# ranking (1, 2, 2, 4).
rank_block <- function(df, denom) {
  df %>%
    mutate(rate = 1000 * n / denom) %>%
    arrange(desc(rate), desc(n), name_canon) %>%
    mutate(rank = min_rank(desc(rate))) %>%
    arrange(rank, name_canon)
}

# "ALL" = both sexes pooled (denominator is every birth that year, including
# the 4 records whose sex was not recorded).
with_all <- function(df, by_year = TRUE) {
  grp <- if (by_year) c("year", "name_canon") else "name_canon"
  bind_rows(
    df %>% filter(sex %in% c("FEMALE", "MALE")),
    df %>% group_by(across(all_of(grp))) %>%
      summarise(n = sum(n), name_display = first(name_display), .groups = "drop") %>%
      mutate(sex = "ALL")
  )
}

# ---- 1. by year --------------------------------------------------------

by_year <- with_all(austin) %>%
  group_by(year, sex) %>%
  group_modify(~ rank_block(.x, denom = sum(.x$n))) %>%
  ungroup() %>%
  select(year, sex, rank, name = name_display, name_canon, n, rate)

write_csv(by_year, file.path(OUT_DIR, "austin_by_year.csv"))

# ---- 2. whole period 2017-2024 ----------------------------------------

period <- austin %>%
  group_by(sex, name_canon) %>%
  summarise(n = sum(n), name_display = first(name_display), .groups = "drop") %>%
  with_all(by_year = FALSE) %>%
  group_by(sex) %>%
  group_modify(~ rank_block(.x, denom = sum(.x$n))) %>%
  ungroup() %>%
  select(sex, rank, name = name_display, name_canon, n, rate)

write_csv(period, file.path(OUT_DIR, "austin_period.csv"))

msg("\n=== %s  TOP 10 ===", PERIOD_LAB)
print(period %>% filter(sex != "ALL") %>% group_by(sex) %>% slice_head(n = 10) %>%
        select(sex, rank, name, n) %>% as.data.frame())

# ---- 3. Austin vs Texas ------------------------------------------------
# The Texas file is TOP 100 ONLY per sex per year and carries no statewide
# birth total. So: no Texas rate can be computed, and a name missing from the
# list has an UNKNOWN rank -- never 101. Comparison is rank-based only.

vs_tx <- by_year %>%
  filter(sex %in% c("FEMALE", "MALE"), rank <= 100) %>%
  left_join(texas, by = c("year", "sex", "name_canon")) %>%
  mutate(
    in_tx_top100 = !is.na(tx_rank),
    # positive = Austin ranks it higher than Texas does
    rank_delta   = if_else(in_tx_top100, tx_rank - rank, NA_integer_)
  ) %>%
  select(year, sex, name, name_canon, au_rank = rank, au_n = n, au_rate = rate,
         tx_rank, tx_n, in_tx_top100, rank_delta)

write_csv(vs_tx, file.path(OUT_DIR, "austin_vs_texas.csv"))

# Overlap: how much of Austin's top 100 also makes the Texas top 100.
# Competition ranking means ties at the boundary can push the Austin set past
# exactly 100 names, so the denominator is reported rather than assumed.
overlap <- vs_tx %>%
  group_by(year, sex) %>%
  summarise(austin_at_rank_le_100 = n(),
            also_in_tx_top100     = sum(in_tx_top100),
            pct                   = round(100 * mean(in_tx_top100), 1),
            .groups = "drop")

write_csv(overlap, file.path(OUT_DIR, "austin_texas_overlap.csv"))
msg("\n=== TOP-100 OVERLAP WITH TEXAS ===")
print(as.data.frame(overlap))

# ---- 4. distinctively Austin ------------------------------------------
# Restricted to names in BOTH top-100s (the only place a rank delta is
# defined), with a >=30 Austin births floor to suppress small-count noise.

MIN_BIRTHS <- 30L

distinctive <- vs_tx %>%
  filter(in_tx_top100, au_n >= MIN_BIRTHS) %>%
  arrange(desc(rank_delta)) %>%
  select(year, sex, name, au_rank, tx_rank, rank_delta, au_n)

write_csv(distinctive, file.path(OUT_DIR, "austin_distinctive.csv"))

# Austin top-100 names that Texas's top 100 does not contain at all. Their
# Texas rank is unknown, not 101 -- flagged, not ranked.
only_austin <- vs_tx %>%
  filter(!in_tx_top100, au_n >= MIN_BIRTHS) %>%
  count(sex, name, name_canon, wt = au_n, name = "au_births_total", sort = TRUE) %>%
  mutate(tx_rank = "outside TX top 100")

write_csv(only_austin, file.path(OUT_DIR, "austin_only.csv"))

msg("\n=== MOST DISTINCTIVELY AUSTIN (2024, in both top 100s, >=%d births) ===", MIN_BIRTHS)
print(distinctive %>% filter(year == 2024) %>% head(10) %>% as.data.frame())

msg("\n=== IN AUSTIN'S TOP 100 BUT NOT TEXAS'S (any year, >=%d births) ===", MIN_BIRTHS)
print(only_austin %>% head(10) %>% as.data.frame())

msg("\n02_analyze.R done.")
