suppressPackageStartupMessages(library(shiny))

dge_error_ui_test_environment <- new.env(parent = globalenv())
package_root <- if (dir.exists("R")) "." else file.path("..", "..")
sys.source(
  file.path(package_root, "R", "icon_notification_ui.R"),
  envir = dge_error_ui_test_environment
)
sys.source(
  file.path(package_root, "R", "module-dge_tab.R"),
  envir = dge_error_ui_test_environment
)
dge_edger_error_ui <- dge_error_ui_test_environment$dge_edger_error_ui

test_that("edgeR failures retain their actionable reason", {
  observed <- as.character(
    dge_edger_error_ui(
      simpleError(
        paste0(
          "edgeR requires at least 2 retained biological samples per group. ",
          "Insufficient groups: Treated (1)."
        )
      )
    )
  )

  expect_match(observed, "edgeR could not run", fixed = TRUE)
  expect_match(observed, "at least 2 retained biological samples", fixed = TRUE)
  expect_match(observed, "Treated (1)", fixed = TRUE)
})

test_that("edgeR error details are HTML escaped", {
  observed <- as.character(
    dge_edger_error_ui(simpleError("Missing sample <script>alert(1)</script>"))
  )

  expect_false(grepl("<script>", observed, fixed = TRUE))
  expect_match(observed, "&lt;script&gt;", fixed = TRUE)
})
