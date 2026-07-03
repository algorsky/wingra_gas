# Runs all manuscript figure scripts end to end, each in its own clean R session
# (so scripts that reuse variable names like `data`/`df`/`crosswalk` can't leak into
# one another). Run from the project root: Rscript RunAllFigures.R

figure_scripts <- c(
  "src/Figure1.R",
  "src/Figure2.R",
  "src/Figure3_Flame.R",
  "src/Figure4.R",
  "src/Figure5.R",
  "src/Figure6.R",
  "src/Figure7.R"
)

for (script in figure_scripts) {
  cat("\n==== Running", script, "====\n")
  status <- system2("Rscript", args = shQuote(script))
  if (status != 0) {
    stop("Failed: ", script, call. = FALSE)
  }
}

cat("\nAll figures generated successfully.\n")
