# run_all.R -- rebuild everything from the raw files.
#   cd /Users/gv6699/Documents/Python/babbies && Rscript run_all.R
# The raw data directory is only ever read from.

setwd(dirname(normalizePath(sub("^--file=", "",
  grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))))

for (s in c("01_clean.R", "02_analyze.R", "03_build_html.R")) {
  cat("\n========== ", s, " ==========\n", sep = "")
  source(s, local = new.env())
}
cat("\nAll done. Embeds written to docs/ (GitHub Pages root):",
    "\n  embed-bubbles.html   packed bubble chart, search, detail panel",
    "\n  embed-texas.html     Austin vs Texas",
    "\n  embed-table.html     top 100 of 2017-2024\n")
