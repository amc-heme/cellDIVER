# Unit tests for error_data()

test_that("error_data returns a list with expected structure", {
  result <- scExploreR:::error_data(
    message = "Test error",
    notification_ui = shiny::tags$div("Error occurred"),
    notification_id = "test_error_id"
  )

  expect_type(result, "list")
  expect_equal(result$err_message, "Test error")
  expect_equal(result$notification_id, "test_error_id")
  expect_true(inherits(result$notification, "shiny.tag"))
})

test_that("error_data stores all three fields", {
  result <- scExploreR:::error_data(
    message = "Something went wrong",
    notification_ui = shiny::tags$p("Please try again"),
    notification_id = "err_001"
  )

  expect_named(
    result,
    c("err_message", "notification", "notification_id")
  )
})

test_that("error_data handles tagList as notification_ui", {
  ui <- shiny::tagList(
    shiny::tags$b("Error:"),
    shiny::tags$span("Details here")
  )

  result <- scExploreR:::error_data(
    message = "Complex error",
    notification_ui = ui,
    notification_id = "complex_err"
  )

  expect_equal(result$err_message, "Complex error")
  expect_equal(result$notification_id, "complex_err")
})
