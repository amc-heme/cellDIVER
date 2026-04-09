# Unit tests for UI helper functions:
# github_link(), icon_notification_ui(), collapsible_panel(),
# summary_tags(), two_column_layout()

# github_link ####

test_that("github_link returns a shiny.tag anchor element", {
  result <- scExploreR:::github_link("Report an issue")
  expect_s3_class(result, "shiny.tag")
  expect_equal(result$name, "a")
  expect_equal(
    result$attribs$href,
    "https://github.com/amc-heme/DataExploreShiny/issues"
  )
  expect_equal(result$attribs$target, "_blank")
  expect_equal(
    result$attribs$rel,
    "noopener noreferrer"
  )
})

test_that("github_link uses custom href", {
  result <- scExploreR:::github_link(
    "Custom link",
    href = "https://example.com"
  )
  expect_equal(result$attribs$href, "https://example.com")
})

# icon_notification_ui ####

test_that(
  "icon_notification_ui returns a div with icon and message",
  {
    result <- scExploreR:::icon_notification_ui(
      icon_name = "exclamation-triangle",
      "Warning message"
    )
    expect_s3_class(result, "shiny.tag")
    expect_equal(result$name, "div")
  }
)

test_that("icon_notification_ui applies custom style", {
  result <- scExploreR:::icon_notification_ui(
    icon_name = "info",
    "Info message",
    notification_style = "color: blue;"
  )
  expect_equal(result$attribs$style, "color: blue;")
})

# collapsible_panel ####

test_that("collapsible_panel returns a tagList", {
  result <- scExploreR:::collapsible_panel(
    shiny::tags$p("Content here"),
    inputId = "test_panel",
    label = "Test Panel"
  )
  expect_s3_class(result, "shiny.tag.list")
})

test_that("collapsible_panel errors on invalid size", {
  expect_error(
    scExploreR:::collapsible_panel(
      shiny::tags$p("Content"),
      inputId = "test_panel",
      label = "Test",
      size = "xl"
    ),
    "Invalid entry for `size`"
  )
})

test_that(
  "collapsible_panel creates active panel with display block",
  {
    result <- scExploreR:::collapsible_panel(
      shiny::tags$p("Content"),
      inputId = "active_panel",
      label = "Active Panel",
      active = TRUE
    )
    # The result is a tagList; the second element is
    # the content div
    content_div <- result[[2]]
    # The active panel should have display: block in style
    expect_true(
      any(grepl("display: block", content_div$attribs$style))
    )
  }
)

# summary_tags ####

test_that("summary_tags returns a tagList", {
  summary_data <- summary(c(1, 2, 3, 4, 5, 6))
  result <- scExploreR:::summary_tags(summary_data)
  expect_s3_class(result, "shiny.tag.list")
})

test_that("summary_tags produces six paragraphs", {
  summary_data <- summary(c(10, 20, 30, 40, 50, 60))
  result <- scExploreR:::summary_tags(summary_data)
  # The result is a tagList containing one lapply result
  # which should have 6 elements
  inner_list <- result[[1]]
  expect_length(inner_list, 6)
})

# two_column_layout ####

test_that("two_column_layout returns a div", {
  result <- scExploreR:::two_column_layout(
    left_colummn = shiny::tags$div("Left"),
    right_column = shiny::tags$div("Right")
  )
  expect_s3_class(result, "shiny.tag")
  expect_equal(result$name, "div")
  expect_true(
    grepl(
      "two-column-container",
      result$attribs$class
    )
  )
})
