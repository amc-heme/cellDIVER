# Validate both the captured main revision and the constrained documentation.
results_directory <- "test-results/pilot"
dir.create(results_directory, recursive = TRUE, showWarnings = FALSE)
writeLines(
  capture.output(sessionInfo()),
  file.path(results_directory, "session-info.txt")
)
write.csv(
  as.data.frame(installed.packages())[, c("Package", "Version")],
  file.path(results_directory, "package-versions.csv"),
  row.names = FALSE
)
options(testthat.summary.max_reports = Inf)
testthat::set_max_fails(Inf)
reporter <- testthat::MultiReporter$new(list(
  testthat::SummaryReporter$new(),
  testthat::JunitReporter$new(
    file = normalizePath(results_directory, mustWork = TRUE) |>
      file.path("junit.xml")
  )
))
results <- devtools::test(reporter = reporter, stop_on_failure = FALSE)
results <- as.data.frame(results)
write.csv(
  results[, intersect(
    names(results),
    c("file", "context", "test", "nb", "failed", "skipped",
      "error", "warning", "passed")
  )],
  file.path(results_directory, "tests.csv"),
  row.names = FALSE
)
if (
  nrow(results) == 0L || sum(results$passed) == 0L ||
  any(results$failed > 0L | results$error | results$skipped)
) {
  stop("The complete test suite must pass without skips.")
}

# Browser tests already ran above; pkgdown below renders the proposed articles.
rcmdcheck::rcmdcheck(
  args = c("--no-manual", "--no-tests", "--no-vignettes"),
  build_args = c("--no-build-vignettes", "--no-manual"),
  error_on = "error",
  check_dir = file.path(results_directory, "check")
)
pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)
