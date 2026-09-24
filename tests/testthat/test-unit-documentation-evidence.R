test_that("documentation evidence is disabled in normal test runs", {
  withr::local_envvar(CELLDIVER_DOCS_EVIDENCE = "")
  # A missing driver must be safe when capture has not been requested.
  expect_null(browser_documentation(NULL, "config", "Not captured"))
})

test_that("documentation capture rejects unexpected milestone paths", {
  directory <- withr::local_tempdir()
  withr::local_envvar(CELLDIVER_DOCS_EVIDENCE = directory)
  expect_error(
    browser_documentation(NULL, "../outside", "Not captured"),
    "Unknown documentation milestone"
  )
  expect_length(list.files(directory), 0)
})

test_that("documentation capture preserves evidence and fails on missing images", {
  directory <- withr::local_tempdir()
  withr::local_envvar(CELLDIVER_DOCS_EVIDENCE = directory)
  app <- list(
    wait_for_idle = function() NULL,
    wait_for_js = function(expression) {
      expect_match(
        expression,
        "document.fonts.status|bounds.bottom <= window.innerHeight"
      )
    },
    run_js = function(expression) {
      expect_match(expression, "window.scrollTo|scrollIntoView")
    },
    get_screenshot = function(file, delay, selector) {
      expect_identical(delay, 0)
      expect_identical(selector, "viewport")
      writeBin(as.raw(c(137, 80, 78, 71)), file)
    },
    get_js = function(expression) {
      expect_identical(expression, "document.body.innerText")
      "AML Reference Dataset"
    },
    get_logs = function() data.frame(message = "Preview rendered")
  )
  expect_null(browser_documentation(app, "config", "Verified preview"))
  expect_setequal(
    list.files(directory), c("config.png", "config.json", "config-logs.csv")
  )
  observations <- jsonlite::read_json(file.path(directory, "config.json"))
  expect_identical(observations$name, "config")
  expect_identical(observations$description, "Verified preview")
  expect_identical(observations$text, "AML Reference Dataset")
  expect_identical(observations$screenshot, "config.png")
  expect_equal(
    utils::read.csv(file.path(directory, "config-logs.csv"))$message,
    "Preview rendered"
  )

  # Reusing a milestone must not disguise a failed capture with an old image.
  app$get_screenshot <- function(file, delay, selector) NULL
  expect_error(
    browser_documentation(app, "config", "Missing preview"),
    "Documentation screenshot is missing or empty"
  )
  expect_false(file.exists(file.path(directory, "config.png")))
})
