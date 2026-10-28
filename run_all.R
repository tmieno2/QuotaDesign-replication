# Runs the analysis. Run from the repository root so that .Rprofile loads the
# packages and the functions in code/R/.
#
#   Rscript run_all.R           # all steps (steps 02 and 03 are slow)
#   Rscript run_all.R 05        # figures and tables only, from the included outputs
#   Rscript run_all.R 01 05     # any subset of steps, in the order given

steps <- c(
  "01" = "code/01-data-prep.qmd",
  "02" = "code/02-1-solve-DP.qmd",
  "03" = "code/03-1-simulate.qmd",
  "05" = "code/05-make-figures-tables.qmd"
)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) steps <- steps[args]

for (qmd in steps) {
  message("Running ", qmd)
  r_file <- tempfile(fileext = ".R")
  knitr::purl(qmd, output = r_file, quiet = TRUE)
  source(r_file)
}
