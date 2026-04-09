# Smoke test: verify the scExploreR app starts without error
#
# This test uses shinytest2 to launch the app and confirm
# it initializes successfully. It does not interact with the
# app beyond verifying startup.

test_that("scExploreR app starts without error", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  app_dir <- system.file(
    "tests", "testthat", "apps", "scExploreR",
    package = "scExploreR"
  )

  # Fall back to local path if system.file doesn't resolve
  # (e.g. during devtools::test())
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    app_dir <- test_path("apps", "scExploreR")
  }

  # Verify the app directory and app.R exist
  expect_true(
    dir.exists(app_dir),
    info = paste("App directory not found:", app_dir)
  )
  expect_true(
    file.exists(file.path(app_dir, "app.R")),
    info = "app.R not found in app directory"
  )

  # Launch the app with shinytest2
  app <- shinytest2::AppDriver$new(
    app_dir = app_dir,
    name = "smoke-test",
    # Allow sufficient time for the app to load the
    # test dataset and initialize all modules
    timeout = 60000,
    # Use a fixed window size for reproducibility
    width = 1200,
    height = 900
  )

  # Ensure the app process is stopped when the test ends
  withr::defer(app$stop())

  # Verify the app launched and is responsive
  expect_type(app$get_url(), "character")
})
