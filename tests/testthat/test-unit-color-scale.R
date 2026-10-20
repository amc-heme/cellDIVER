test_that("color scales honor limits, clamp out-of-range values and keep names", {
  palette <- c("#2166AC", "#F7F7F7", "#B2182B")

  plain <- cellDIVER:::color_scale("colour", c("lightgrey", "blue"))
  expect_s3_class(plain, "ScaleContinuous")
  expect_identical(plain$aesthetics[[1]], "colour")
  expect_null(plain$limits)
  expect_s3_class(plain$name, "waiver")

  limited <- cellDIVER:::color_scale(
    "fill", c("lightgrey", "blue"), limits = c(0, 2), name = "Expression"
  )
  expect_identical(limited$aesthetics[[1]], "fill")
  expect_equal(limited$limits, c(0, 2))
  expect_identical(limited$name, "Expression")
  # Out-of-range values are squished to the ends rather than dropped
  expect_equal(limited$oob(c(-1, 1, 5), c(0, 2)), c(0, 1, 2))

  diverging <- cellDIVER:::color_scale(
    "colour", palette, limits = c(-2, 2), diverging = TRUE
  )
  expect_equal(diverging$limits, c(-2, 2))
  expect_identical(diverging$palette(0.5), "#F7F7F7")
  expect_equal(diverging$oob(c(-5, 0, 5), c(-2, 2)), c(-2, 0, 2))

  # Unusable limits fall back to the data range
  for (limits in list(NULL, c(1, 1), c(2, 1), c(NA_real_, 1), c(0, Inf), 1)) {
    expect_null(
      cellDIVER:::color_scale("fill", palette, limits, diverging = TRUE)$limits
    )
  }
})

test_that("plotted_range spans every panel of a ggplot or patchwork", {
  one <- ggplot2::ggplot(
    data.frame(x = 1:3, y = 1:3, value = c(-1, 0, 4)),
    ggplot2::aes(x, y, colour = value)
  ) +
    ggplot2::geom_point()
  two <- ggplot2::ggplot(
    data.frame(x = 1:3, y = 1:3, value = c(2, 9, NA)),
    ggplot2::aes(x, y, colour = value)
  ) +
    ggplot2::geom_point()

  expect_equal(cellDIVER:::plotted_range(one, "value"), c(-1, 4))
  expect_null(cellDIVER:::plotted_range(one, "missing"))

  combined <- patchwork::wrap_plots(one, two)
  expect_equal(cellDIVER:::plotted_range(combined, "value"), c(-1, 9))

  nested <- patchwork::wrap_plots(combined, one)
  expect_equal(cellDIVER:::plotted_range(nested, "value"), c(-1, 9))
})

test_that("plot functions report their plotted range and apply color limits", {
  object <- browser_object()
  reduction <- cellDIVER:::reduction_names(object)[[1]]
  palette <- c("#2166AC", "#F7F7F7", "#B2182B")

  # Dot plot: mean-centered scaled averages, clipped to +/- 2.5 by plot_dot()
  dot <- suppressWarnings(cellDIVER:::shiny_dot(
    object,
    features_entered = c("rna_CD34", "rna_CD38"),
    group_by = "condensed_cell_type"
  ))
  dot_range <- attr(dot, "plotted_range")
  expect_equal(dot_range, range(dot$data$avg_exp_scaled))
  expect_true(all(abs(dot_range) <= 2.5))
  expect_null(dot$scales$get_scales("colour")$limits)

  limited_dot <- suppressWarnings(cellDIVER:::shiny_dot(
    object,
    features_entered = c("rna_CD34", "rna_CD38"),
    group_by = "condensed_cell_type",
    diverging_palette = palette,
    color_limits = c(-1, 1)
  ))
  expect_equal(limited_dot$scales$get_scales("colour")$limits, c(-1, 1))

  # Hexbin feature plot: per-bin aggregates, so sums exceed means
  mean_hex <- cellDIVER:::shiny_hexbin_feature(
    object, features_entered = "rna_CD34", nbins = 30,
    reduction = reduction, split_by = "none", action = "mean"
  )
  sum_hex <- cellDIVER:::shiny_hexbin_feature(
    object, features_entered = "rna_CD34", nbins = 30,
    reduction = reduction, split_by = "none", action = "sum",
    color_limits = c(0, 1)
  )
  expect_equal(
    attr(mean_hex, "plotted_range"), range(mean_hex$data$feature_value)
  )
  expect_gte(
    attr(sum_hex, "plotted_range")[[2]], attr(mean_hex, "plotted_range")[[2]]
  )
  expect_equal(sum_hex$scales$get_scales("fill")$limits, c(0, 1))

  multi_hex <- cellDIVER:::shiny_hexbin_feature(
    object, features_entered = c("rna_CD34", "rna_CD38"), nbins = 30,
    reduction = reduction, split_by = "none"
  )
  expect_s3_class(multi_hex, "patchwork")
  expect_length(attr(multi_hex, "plotted_range"), 2)

  # Hexbin density plot: cell counts per bin
  density <- cellDIVER:::shiny_hexbin_density(
    object, nbins = 30, reduction = reduction, split_by = "none",
    color_limits = c(1, 5)
  )
  expect_equal(
    attr(density, "plotted_range"), range(density$data$number_of_cells)
  )
  expect_equal(density$scales$get_scales("fill")$limits, c(1, 5))

  # Feature plot: the legend title survives the final scale layer
  feature <- cellDIVER:::shiny_feature(
    object, features_entered = "rna_CD34", assay_config = NULL,
    split_by = "none", raw_feature_names = TRUE, legend_title = "feature",
    reduction = reduction, color_limits = c(0, 1)
  )
  feature_range <- attr(feature, "plotted_range")
  expression <- SCUBA::fetch_data(
    object, vars = "rna_CD34", layer = SCUBA::default_layer(object)
  )[, 1]
  expect_equal(feature_range, range(expression))
  feature_scale <- feature$scales$get_scales("colour")
  expect_equal(feature_scale$limits, c(0, 1))
  expect_identical(feature_scale$name, "rna_CD34")
})
