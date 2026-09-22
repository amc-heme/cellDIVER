test_that("manual plot dimensions are returned only while enabled", {
  shiny::testServer(
    cellDIVER:::manual_dimensions_server,
    {
      session$setInputs(manual_dim = FALSE, width = 8, height = 5, dpi = "300")
      expect_null(session$returned$width())
      expect_null(session$returned$height())
      expect_identical(session$returned$dpi(), 300)

      session$setInputs(manual_dim = TRUE)
      expect_identical(session$returned$width(), 8)
      expect_identical(session$returned$height(), 5)

      session$setInputs(width = 10, height = 6, dpi = "150")
      expect_identical(session$returned$width(), 10)
      expect_identical(session$returned$height(), 6)
      expect_identical(session$returned$dpi(), 150)

      session$setInputs(manual_dim = FALSE)
      expect_null(session$returned$width())
      expect_null(session$returned$height())
    }
  )
})

test_that("custom axis limits require apply and reset clears the override", {
  plot_data <- data.frame(position = c(1, 3, 5), value = c(2, 4, 6))
  original_plot <- ggplot2::ggplot(
    plot_data, ggplot2::aes(x = position, y = value)
  ) + ggplot2::geom_point()
  plot_reactive <- shiny::reactiveVal(original_plot)

  shiny::testServer(
    cellDIVER:::custom_xlim_server,
    args = list(plot = plot_reactive),
    {
      session$flushReact()
      expect_null(session$returned())
      expect_equal(current_xlim(), c(1, 5))

      session$setInputs(lower_xlim = "-1.5", upper_xlim = "8.5")
      expect_null(session$returned())
      session$setInputs(apply_xlim = 1)
      expect_identical(session$returned(), c(-1.5, 8.5))

      session$setInputs(lower_xlim = "0", upper_xlim = "10")
      expect_identical(session$returned(), c(-1.5, 8.5))
      session$setInputs(apply_xlim = 2)
      expect_identical(session$returned(), c(0, 10))

      plot_reactive(original_plot + ggplot2::coord_cartesian(xlim = c(0, 10)))
      session$flushReact()
      expect_equal(current_xlim(), c(0, 10))

      session$setInputs(restore_xlim = 1)
      expect_null(session$returned())
    }
  )
})

test_that("threshold picker behavior follows reactive mode changes", {
  threshold_mode <- shiny::reactiveVal(NULL)
  expect_identical(cellDIVER:::threshold_picker_behavior(NULL), "threshold")
  expect_identical(cellDIVER:::threshold_picker_behavior("range"), "threshold")
  expect_error(
    shiny::isolate(cellDIVER:::threshold_picker_behavior(threshold_mode)),
    class = "shiny.silent.error"
  )

  threshold_mode("range")
  expect_identical(
    shiny::isolate(cellDIVER:::threshold_picker_behavior(threshold_mode)),
    "range"
  )
  threshold_mode("greater_than")
  expect_identical(
    shiny::isolate(cellDIVER:::threshold_picker_behavior(threshold_mode)),
    "threshold"
  )
})
