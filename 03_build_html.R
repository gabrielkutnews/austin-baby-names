# 03_build_html.R -- precompute layouts and write three self-contained Grove
# embeds. No pandoc, no CDN, no build step.

source("00_common.R")
suppressPackageStartupMessages({ library(jsonlite); library(packcircles) })
source("html_shared.R")
source("embed_bubbles.R")
source("embed_texas.R")
source("embed_table.R")

# Only the pooled 2017-2024 period is published, so the two layouts differ by
# SCREEN SIZE rather than year. 140 bubbles are unreadable on a phone.
# Published site root, served by GitHub Pages -- same layout as
# food-inspection-scores/docs.
DOCS_DIR <- file.path(PROJ_DIR, "babbies", "docs")
dir.create(DOCS_DIR, showWarnings = FALSE, recursive = TRUE)

TOP_D <- 140L
TOP_M <- 55L
# The pack is roughly circular, so its size is governed by the SHORTER viewBox
# side. A squarer box therefore buys bigger bubbles without dropping names.
VB_W  <- 1000
VB_H  <- 830

austin <- read_csv(file.path(OUT_DIR, "austin_clean.csv"), col_types = cols(
  year = col_integer(), sex = col_character(),
  name_canon = col_character(), n = col_integer(), name_display = col_character()))
by_year <- read_csv(file.path(OUT_DIR, "austin_by_year.csv"), col_types = cols(
  year = col_integer(), sex = col_character(), rank = col_integer(),
  name = col_character(), name_canon = col_character(),
  n = col_integer(), rate = col_double()))
period <- read_csv(file.path(OUT_DIR, "austin_period.csv"), col_types = cols(
  sex = col_character(), rank = col_integer(), name = col_character(),
  name_canon = col_character(), n = col_integer(), rate = col_double()))
texas <- read_csv(file.path(OUT_DIR, "texas_clean.csv"), col_types = cols(
  year = col_integer(), sex = col_character(), tx_rank = col_integer(),
  name_canon = col_character(), tx_n = col_integer()))
variants <- read_csv(file.path(OUT_DIR, "name_variants.csv"), col_types = cols(
  name_canon = col_character(), name_raw = col_character(), births = col_integer()))

# ---- name index --------------------------------------------------------
# Union of Austin and Texas names: a Texas top-100 name with no Austin births
# would still need a label in the comparison.
idx_tbl <- bind_rows(
    austin %>% group_by(name_canon) %>%
      summarise(name_display = first(name_display), .groups = "drop"),
    tibble(name_canon = setdiff(texas$name_canon, austin$name_canon)) %>%
      mutate(name_display = title_case(name_canon))
  ) %>%
  arrange(name_canon) %>%
  mutate(i = row_number() - 1L)
IDX <- setNames(idx_tbl$i, idx_tbl$name_canon)
msg("%d distinct names indexed", nrow(idx_tbl))

# ---- colour-mode inputs ------------------------------------------------
# Trend over the period: change in share of births, late half vs early half.
trend_tbl <- by_year %>%
  mutate(half = if_else(year <= 2020, "early", "late")) %>%
  group_by(sex, name_canon, half) %>% summarise(r = mean(rate), .groups = "drop") %>%
  pivot_wider(names_from = half, values_from = r, values_fill = 0) %>%
  mutate(trend = if_else(early > 0, (late - early) / early, NA_real_)) %>%
  select(sex, name_canon, trend)

ms_tbl <- austin %>% filter(sex %in% c("FEMALE", "MALE")) %>%
  group_by(name_canon) %>%
  summarise(ms = sum(n[sex == "MALE"]) / sum(n), .groups = "drop")

tx_delta <- by_year %>% filter(sex %in% c("FEMALE", "MALE")) %>%
  inner_join(texas, by = c("year", "sex", "name_canon")) %>%
  transmute(name_canon, td = tx_rank - rank) %>%
  group_by(name_canon) %>% summarise(td = as.integer(round(mean(td))), .groups = "drop")

# ---- bubble layouts ----------------------------------------------------
# Bubble AREA is proportional to the count, the correct encoding for magnitude.
# circleProgressiveLayout packs largest-first around the origin.
build_view <- function(df, topn) {
  d <- df %>% arrange(rank, name_canon) %>% head(topn)
  lay <- circleProgressiveLayout(d$n, sizetype = "area")
  x <- lay$x; y <- lay$y; r <- lay$radius
  s  <- min((VB_W - 24) / (max(x + r) - min(x - r)),
            (VB_H - 24) / (max(y + r) - min(y - r)))
  cx <- (max(x + r) + min(x - r)) / 2
  cy <- (max(y + r) + min(y - r)) / 2
  list(i    = unname(IDX[d$name_canon]),
       x    = round((x - cx) * s + VB_W / 2, 1),
       y    = round((y - cy) * s + VB_H / 2, 1),
       r    = round(r * s, 1),
       n    = d$n, rate = round(d$rate, 3), rank = d$rank,
       ms   = round(d$ms, 3), tr = round(d$tr, 3), td = d$td)
}

views <- list()
for (sx in c("ALL", "FEMALE", "MALE")) {
  d <- period %>% filter(sex == sx) %>%
    left_join(ms_tbl, by = "name_canon") %>%
    left_join(trend_tbl %>% filter(sex == sx) %>% select(name_canon, tr = trend),
              by = "name_canon") %>%
    left_join(tx_delta, by = "name_canon")
  views[[paste0("d|", sx)]] <- build_view(d, TOP_D)
  views[[paste0("m|", sx)]] <- build_view(d, TOP_M)
}
msg("built %d bubble layouts (desktop %d / mobile %d)", length(views), TOP_D, TOP_M)

# ---- per-name series (search results, sparkline, rank history) --------
series <- by_year %>% filter(sex %in% c("FEMALE", "MALE")) %>%
  transmute(y = year - 2017L, s = if_else(sex == "FEMALE", 0L, 1L),
            i = unname(IDX[name_canon]), n = n, rk = rank) %>%
  arrange(i, y, s)

tx_ship <- texas %>% filter(name_canon %in% names(IDX)) %>%
  transmute(y = year - 2017L, s = if_else(sex == "FEMALE", 0L, 1L),
            i = unname(IDX[name_canon]), rk = tx_rank)

var_ship <- variants %>%
  group_by(name_canon) %>% filter(n() > 1) %>%
  summarise(v = paste(name_raw[order(-births)], collapse = " · "), .groups = "drop") %>%
  mutate(i = unname(IDX[name_canon]))
VAR <- setNames(as.list(var_ship$v), as.character(var_ship$i))

# ---- Austin vs Texas ---------------------------------------------------
TOPC <- 20L; MIN_CMP <- 25L; MIN_YRS <- 4L

build_cmp <- function(sx) {
  au <- period %>% filter(sex == sx) %>% select(name_canon, rank, n)
  # Pooling by summed COUNT would invent Texas ranks past 100 for names Texas
  # simply omitted in some years -- exactly the false precision the truncated
  # source forbids. Average the ranks Texas actually published instead, and
  # keep how many of the 8 lists the name appeared in.
  tx <- texas %>% filter(sex == sx) %>%
    group_by(name_canon) %>%
    summarise(tx_rank = as.integer(round(mean(tx_rank))), yrs = n(),
              tx_n = sum(tx_n), .groups = "drop") %>%
    arrange(tx_rank, desc(tx_n))

  j <- au %>% left_join(tx %>% select(name_canon, tx_rank, yrs), by = "name_canon")
  au_top <- j  %>% arrange(rank, name_canon) %>% head(TOPC)
  tx_top <- tx %>% arrange(tx_rank, desc(tx_n)) %>% head(TOPC) %>%
    left_join(au %>% select(name_canon, rank), by = "name_canon")

  # Require the name on at least half the Texas lists, so an averaged rank is
  # not built from one fluke year.
  gaps <- j %>% filter(!is.na(tx_rank), rank <= 100, n >= MIN_CMP, yrs >= MIN_YRS) %>%
    mutate(d = tx_rank - rank)
  fav <- function(df) list(i = unname(IDX[df$name_canon]), au = df$rank,
                           tx = df$tx_rank, d = df$d)
  m <- j %>% filter(is.na(tx_rank), rank <= 100, n >= MIN_CMP) %>% arrange(rank) %>% head(8)

  list(au = list(i = unname(IDX[au_top$name_canon]), rank = au_top$rank,
                 n = au_top$n, tx = au_top$tx_rank),
       tx = list(i = unname(IDX[tx_top$name_canon]), rank = tx_top$tx_rank,
                 n = tx_top$tx_n, au = tx_top$rank),
       inBoth = sum(!is.na(j$tx_rank[j$rank <= 100])),
       ofAu   = sum(j$rank <= 100),
       auFav  = fav(gaps %>% arrange(desc(d)) %>% head(8)),
       txFav  = fav(gaps %>% arrange(d) %>% head(8)),
       missing = list(i = unname(IDX[m$name_canon]), au = m$rank, n = m$n))
}
cmp <- lapply(c(FEMALE = "FEMALE", MALE = "MALE"), build_cmp)

# ---- write embed 1: bubbles -------------------------------------------
bubbles_data <- toJSON(list(
  meta   = list(years = YEARS, vbw = VB_W, vbh = VB_H,
                birthsTotal = sum(austin$n)),
  names  = idx_tbl$name_display,
  views  = views,
  series = as.list(series),
  tx     = as.list(tx_ship),
  vars   = VAR
), auto_unbox = TRUE, na = "null", digits = 4)

writeLines(embed_html("babynames-bubbles", "Austin baby names",
                      bubbles_body(), bubbles_js(), bubbles_data, bubbles_css()),
           file.path(DOCS_DIR, "embed-bubbles.html"), useBytes = TRUE)

# ---- write embed 2: Austin vs Texas -----------------------------------
# Ships only the names the comparison references -- a few hundred, not 25,000.
used <- sort(unique(unlist(lapply(cmp, function(c)
  c(c$au$i, c$tx$i, c$auFav$i, c$txFav$i, c$missing$i)))))
remap <- setNames(seq_along(used) - 1L, as.character(used))
reindex <- function(v) unname(remap[as.character(v)])
cmp_small <- lapply(cmp, function(c) {
  c$au$i <- reindex(c$au$i); c$tx$i <- reindex(c$tx$i)
  c$auFav$i <- reindex(c$auFav$i); c$txFav$i <- reindex(c$txFav$i)
  c$missing$i <- reindex(c$missing$i)
  c
})
texas_data <- toJSON(list(names = idx_tbl$name_display[used + 1L], cmp = cmp_small),
                     auto_unbox = TRUE, na = "null")

writeLines(embed_html("babynames-texas", "Austin baby names vs Texas",
                      texas_body(), texas_js(), texas_data, texas_css()),
           file.path(DOCS_DIR, "embed-texas.html"), useBytes = TRUE)

# ---- write embed 3: top-100 table -------------------------------------
# Static table: no chart JS. The shared height broadcast still runs.
writeLines(embed_html("babynames-table", "Austin's most common baby names",
                      table_body(period), "", NULL, table_css()),
           file.path(DOCS_DIR, "embed-table.html"), useBytes = TRUE)

for (f in c("embed-bubbles.html", "embed-texas.html", "embed-table.html"))
  msg("wrote docs/%-22s %6.0f KB", f, file.size(file.path(DOCS_DIR, f)) / 1024)

msg("03_build_html.R done.")
