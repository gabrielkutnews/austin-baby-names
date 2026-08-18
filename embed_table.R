# embed_table.R -- the top 100 names of 2017-2024 as a plain table.
# Static HTML, no JSON and no chart JS: it is the accessible, no-JavaScript
# rendering of the same ranking the bubble chart shows.
# Two 3-column tables (Girls, Boys) rather than one 6-column table, because six
# columns cannot fit a phone.

table_css <- function() r"---(
  .gv .tabgrid{display:grid;grid-template-columns:1fr 1fr;gap:16px;margin-top:14px}
  @media (max-width:640px){.gv .tabgrid{grid-template-columns:1fr;gap:14px}}
  .gv .tabcard{padding:12px 14px 14px}
  .gv .tabcard h2{font-size:14px;margin:0 0 8px;letter-spacing:.02em}
  .gv table.top{width:100%;border-collapse:collapse;font-size:13.5px;
    font-variant-numeric:tabular-nums}
  .gv table.top th{text-align:left;font-size:10.5px;letter-spacing:.06em;
    text-transform:uppercase;color:var(--text-muted);font-weight:600;
    padding:0 0 5px;border-bottom:1px solid var(--line)}
  .gv table.top th.num,.gv table.top td.num{text-align:right}
  .gv table.top td{padding:5px 0;border-top:1px solid var(--line)}
  .gv table.top td.rk{color:var(--text-muted);width:2.6em}
  .gv table.top tbody tr:hover{background:var(--surface-3)}
  .gv table.top td.nm{width:60%}
)---"

# Ranks tie, so the two sexes are paired by position and each keeps its own rank.
table_body <- function(period) {
  side <- function(sx, heading) {
    d <- period %>% filter(sex == sx) %>% arrange(rank, name) %>% head(100)
    rows <- paste0(
      "<tr><td class='rk'>", d$rank, "</td><td class='nm'>", d$name,
      "</td><td class='num'>", format(d$n, big.mark = ","), "</td></tr>",
      collapse = "\n")
    paste0(
      "<div class=\"card tabcard\">\n<h2>", heading, "</h2>\n",
      "<table class=\"top\">\n<thead><tr><th>#</th><th>Name</th>",
      "<th class=\"num\">Babies</th></tr></thead>\n<tbody>\n", rows,
      "\n</tbody>\n</table>\n</div>")
  }
  paste0(r"---(
<h1>Austin's 100 most common baby names</h1>
<p class="standfirst">)---", STANDFIRST, r"---(</p>
<div class="tabgrid">
)---", side("FEMALE", "Girls"), "\n", side("MALE", "Boys"), r"---(
</div>
<p class="credit">)---", CREDIT, r"---(</p>
)---")
}
