test_that("configured credentials gate the app behind a login screen", {
  skip_if_not_installed("shinymanager")
  app <- browser_app("auth-login", fixture = "auth")
  present <- function(selector) {
    app$get_js(sprintf(
      "document.querySelector(%s) !== null",
      jsonlite::toJSON(selector, auto_unbox = TRUE)
    ))
  }

  # The login form is served and the app itself is not
  app$wait_for_js("document.querySelector('#auth-user_id') !== null")
  expect_true(present("#auth-user_pwd"))
  expect_false(present("a[data-value='plots']"))

  # bob is in the credentials file but not on the users allow-list
  app$set_inputs(`auth-user_id` = "bob", `auth-user_pwd` = "other", wait_ = FALSE)
  app$click("auth-go_auth", wait_ = FALSE)
  app$wait_for_idle(timeout = 20000)
  expect_true(present("#auth-user_id"))
  expect_false(present("a[data-value='plots']"))

  # alice logs in and reaches the app. shinymanager reloads the session on
  # success, so wait on the DOM rather than on Shiny being idle.
  app$set_inputs(`auth-user_id` = "alice", `auth-user_pwd` = "secret", wait_ = FALSE)
  app$click("auth-go_auth", wait_ = FALSE)
  app$wait_for_js("document.querySelector(\"a[data-value='plots']\") !== null")
  expect_false(present("#auth-user_id"))

  logs <- app$get_logs()
  server <- logs[logs$location == "shiny", "message"]
  expect_true(any(grepl("Authentication enabled (1 credential(s) loaded)", server, fixed = TRUE)))
})
