test_that("categorical palettes render swatches and escape their label", {
  palette <- c("#000000", "#FFFFFF")
  palette_tag <- cellDIVER:::palette_html(
    palette, type = "categorical", n = 4, palette_name = "<Example>"
  )
  palette_html <- cellDIVER:::palette_html(
    palette, type = "categorical", n = 4, palette_name = "<Example>",
    output_html = TRUE
  )

  expect_s3_class(palette_tag, "shiny.tag")
  expect_identical(palette_tag$name, "div")
  expect_identical(palette_html, as.character(palette_tag))
  expect_length(
    regmatches(palette_html, gregexpr("background-color:", palette_html))[[1]],
    4
  )
  expect_match(palette_html, "background-color: #000000", fixed = TRUE)
  expect_match(palette_html, "background-color: #FFFFFF", fixed = TRUE)
  expect_match(palette_html, "&lt;Example&gt;", fixed = TRUE)
})

test_that("continuous palettes preserve all gradient stops regardless of n", {
  palette <- c("#000000", "#808080", "#FFFFFF")
  palette_tag <- cellDIVER:::palette_html(palette, type = "continuous", n = 1)
  palette_html <- cellDIVER:::palette_html(
    palette, type = "continuous", n = 1, output_html = TRUE
  )

  expect_identical(palette_tag$name, "span")
  expect_identical(palette_html, as.character(palette_tag))
  expect_match(
    palette_html,
    "linear-gradient(to right, #000000, #808080, #FFFFFF)",
    fixed = TRUE
  )
})

test_that("plot column defaults change at the intended panel boundaries", {
  feature_counts <- c(1, 3, 4, 9, 11, 12, 20)
  expected_defaults <- c(1, 3, 2, 3, 3, 4, 4)

  for (case_index in seq_along(feature_counts)) {
    feature_count <- feature_counts[[case_index]]
    expect_equal(
      cellDIVER:::ncol_settings(
        object = NULL, rule = "features",
        features_entered = paste0("Gene", seq_len(feature_count))
      ),
      c(min = 1, max = feature_count, default = expected_defaults[[case_index]])
    )
  }
  expect_error(
    cellDIVER:::ncol_settings(NULL, rule = "unsupported"),
    "must be set to either"
  )
  expect_error(
    cellDIVER:::ncol_settings(NULL, rule = "features"),
    "features_entered.*must be defined"
  )
  expect_error(
    cellDIVER:::ncol_settings(NULL, rule = "split_by"),
    "split_by.*must be defined"
  )
})

test_that("interactive coordinates map distribution endpoints and midpoint", {
  expect_equal(
    cellDIVER:::interactive_transform(
      x_coord = c(0.1, 0.5, 0.9),
      distribution_range = 20,
      distribution_minimum = -5,
      plot_min_coord = 0.1,
      plot_max_coord = 0.9
    ),
    c(-5, 5, 15)
  )
})

test_that("display utilities preserve boundary and signed values", {
  expect_identical(cellDIVER:::truncate_str("Alpha", 5), "Alpha")
  expect_identical(cellDIVER:::truncate_str("Alphabet", 5), "Alpha...")
  expect_identical(cellDIVER:::truncate_str("", 5), "")
  expect_identical(cellDIVER:::vector_to_text(NULL), "NULL")
  expect_identical(cellDIVER:::vector_to_text("Alpha"), "Alpha")
  expect_identical(
    cellDIVER:::vector_to_text(c("Alpha", "Beta")), "Alpha and Beta"
  )
  expect_identical(
    cellDIVER:::vector_to_text(c("Alpha", "Beta", "Gamma")),
    "Alpha, Beta, and Gamma"
  )
  expect_equal(cellDIVER:::to_log2(log(c(0.25, 1, 8))), c(-2, 0, 3))
  expect_identical(as.character(cellDIVER:::to_GB(1.2345e9)), "1.234 GB")
  expect_identical(as.character(cellDIVER:::to_GB(0)), "0 GB")
})

test_that("error information retains its notification and stable identifier", {
  notification <- shiny::tagList(
    shiny::tags$b("Error:"), shiny::tags$span("Choose another subset")
  )
  expect_identical(
    cellDIVER:::error_data("Empty subset", notification, "subset_error"),
    list(
      err_message = "Empty subset",
      notification = notification,
      notification_id = "subset_error"
    )
  )
})
