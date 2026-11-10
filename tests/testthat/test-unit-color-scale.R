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

test_that("resolve_color_limits converts quantiles against the given values", {
  values <- c(0, 1, 2, 3, 4, NA, Inf)

  expect_null(cellDIVER:::resolve_color_limits(NULL))
  expect_equal(cellDIVER:::resolve_color_limits(c(1, 3)), c(1, 3))
  expect_equal(
    cellDIVER:::resolve_color_limits(c(0.25, 0.75), quantile = TRUE, values),
    unname(quantile(0:4, c(0.25, 0.75)))
  )
  expect_null(
    cellDIVER:::resolve_color_limits(c(0.1, 0.9), quantile = TRUE, NA_real_)
  )
})

test_that("per-cell cutoffs act before binning and the cell range is reported", {
  object <- browser_object()
  reduction <- cellDIVER:::reduction_names(object)[[1]]
  cells <- SCUBA::fetch_data(
    object, vars = "rna_CD34", layer = SCUBA::default_layer(object)
  )[, 1]

  summed <- cellDIVER:::shiny_hexbin_feature(
    object, features_entered = "rna_CD34", nbins = 30,
    reduction = reduction, split_by = "none", action = "sum"
  )
  capped <- cellDIVER:::shiny_hexbin_feature(
    object, features_entered = "rna_CD34", nbins = 30,
    reduction = reduction, split_by = "none", action = "sum",
    data_cutoffs = c(NA, 1)
  )
  expect_equal(attr(summed, "cell_range"), range(cells))
  expect_equal(attr(capped, "cell_range"), range(cells))
  expect_lt(attr(capped, "plotted_range")[[2]], attr(summed, "plotted_range")[[2]])
  expect_true(all(capped$data$feature_value <= capped$data$number_of_cells + 1e-8))

  quantiled <- cellDIVER:::shiny_hexbin_feature(
    object, features_entered = "rna_CD34", nbins = 30,
    reduction = reduction, split_by = "none", action = "mean",
    data_cutoffs = c("q1", "q99")
  )
  expect_lte(attr(quantiled, "plotted_range")[[2]], unname(quantile(cells, 0.99)) + 1e-8)

  # Quantile color limits are evaluated against the plotted values
  dot <- suppressWarnings(cellDIVER:::shiny_dot(
    object, features_entered = c("rna_CD34", "rna_CD38"),
    group_by = "condensed_cell_type",
    color_limits = c(0.1, 0.9), color_limits_quantile = TRUE
  ))
  expect_equal(
    dot$scales$get_scales("colour")$limits,
    unname(quantile(dot$data$avg_exp_scaled, c(0.1, 0.9)))
  )

  # Feature plots clamp per-cell values and keep their legend title
  feature <- cellDIVER:::shiny_feature(
    object, features_entered = "rna_CD34", assay_config = NULL,
    split_by = "none", raw_feature_names = TRUE, legend_title = "feature",
    reduction = reduction, data_cutoffs = c(0, 1)
  )
  expect_equal(attr(feature, "cell_range"), range(cells))
  expect_true(all(attr(feature, "plotted_range") >= 0 - 1e-8))
  expect_true(all(attr(feature, "plotted_range") <= 1 + 1e-8))
  expect_identical(feature$scales$get_scales("colour")$name, "rna_CD34")
})

test_that("quantile cutoffs are resolved once over the pooled per-cell values", {
  values <- 1:100
  expect_equal(
    cellDIVER:::resolve_data_cutoffs(c("q10", "q90"), values),
    unname(stats::quantile(values, c(0.1, 0.9)))
  )
  expect_identical(cellDIVER:::resolve_data_cutoffs(c(NA, 2.5), values), c(NA, 2.5))
  expect_identical(cellDIVER:::resolve_data_cutoffs(c(1, 3)), c(1, 3))
  expect_null(cellDIVER:::resolve_data_cutoffs(NULL, values))
  # no values to take a quantile over: no cutoff rather than an error
  expect_true(all(is.na(cellDIVER:::resolve_data_cutoffs(c("q1", "q99"), numeric()))))

  # the same numeric cutoff reaches every split panel: both panels are
  # clamped at the pooled quantile, not at a per-group one
  object <- unit_pseudobulk_object()
  feature <- paste0("rna_", rownames(object)[1])
  configuration <- list(
    RNA = list(assay = "RNA", key = "rna_", suffix_human = "", dropdown_title = "Gene")
  )
  pooled <- unname(stats::quantile(
    cellDIVER:::feature_cell_values(object, feature), 0.9
  ))
  plot <- cellDIVER:::shiny_feature(
    object, features_entered = feature, split_by = "condition",
    group_by = "cell_type", reduction = "umap", assay_config = configuration,
    data_cutoffs = c(NA, "q90")
  )
  # every panel is clamped at the pooled quantile (a panel whose own values
  # stay below it is untouched), and the panel holding the largest values
  # reaches exactly that cutoff
  maxima <- vapply(
    seq_len(length(plot)), function(i) max(plot[[i]]$data[[feature]]), numeric(1)
  )
  expect_true(all(maxima <= pooled + 1e-8))
  expect_equal(max(maxima), pooled, tolerance = 1e-8)
})

test_that("split panels and shared scales keep pooled limits; separate scales keep their titles", {
  object <- unit_pseudobulk_object()
  features <- paste0("rna_", rownames(object)[1:2])
  configuration <- list(
    RNA = list(assay = "RNA", key = "rna_", suffix_human = "", dropdown_title = "Gene")
  )
  diverging <- c("blue", "white", "red")
  scale_of <- function(panel) panel$scales$get_scales("colour")

  # one feature split by group, diverging colors, no explicit limits: every
  # panel's scale spans the pooled range of the values shown
  split <- cellDIVER:::shiny_feature(
    object, features_entered = features[1], split_by = "condition",
    group_by = "cell_type", reduction = "umap", assay_config = configuration,
    diverging_palette = diverging, legend_title = "feature"
  )
  pooled <- cellDIVER:::plotted_range(split, features[1])
  for (i in seq_len(length(split))) {
    expect_equal(scale_of(split[[i]])$limits, pooled)
  }

  # several features on a shared scale: the same pooled limits on each panel
  shared <- cellDIVER:::shiny_feature(
    object, features_entered = features, split_by = "none",
    group_by = "cell_type", reduction = "umap", assay_config = configuration,
    diverging_palette = diverging, share_scale = TRUE, legend_title = "none"
  )
  pooled <- cellDIVER:::plotted_range(shared, features)
  for (i in seq_len(length(shared))) {
    expect_equal(scale_of(shared[[i]])$limits, pooled)
  }

  # separate scales titled by feature keep their titles when the scale is
  # replaced, and train on their own values
  separate <- cellDIVER:::shiny_feature(
    object, features_entered = features, split_by = "none",
    group_by = "cell_type", reduction = "umap", assay_config = configuration,
    diverging_palette = diverging, share_scale = FALSE, legend_title = "feature"
  )
  expect_identical(
    vapply(seq_len(length(separate)), function(i) scale_of(separate[[i]])$name, ""),
    rownames(object)[1:2]
  )
  expect_null(scale_of(separate[[1]])$limits)
})

test_that("symmetric color limits follow the handle that moved", {
  symmetric <- cellDIVER:::symmetric_color_limits
  # narrowing from either side
  expect_equal(symmetric(c(-5, 2), previous = c(-5, 5)), c(-2, 2))
  expect_equal(symmetric(c(-3, 5), previous = c(-5, 5)), c(-3, 3))
  # widening
  expect_equal(symmetric(c(-5, 7), previous = c(-5, 5)), c(-7, 7))
  # no previous value, or both handles changed: the larger magnitude
  expect_equal(symmetric(c(-5, 2)), c(-5, 5))
  expect_equal(symmetric(c(-1, 4), previous = c(-5, 5)), c(-4, 4))
  # non-numeric input is returned unchanged
  expect_identical(symmetric(NULL), NULL)
})

test_that("hexbin feature titles are removed when titles are switched off", {
  skip_if_not_installed("hextant")
  object <- unit_pseudobulk_object()
  features <- paste0("rna_", rownames(object)[1:2])
  single <- cellDIVER:::shiny_hexbin_feature(
    object, features_entered = features[1], reduction = "umap", nbins = 10,
    show_title = FALSE, custom_titles = "Custom"
  )
  expect_null(single$labels$title)
  multiple <- cellDIVER:::shiny_hexbin_feature(
    object, features_entered = features, reduction = "umap", nbins = 10,
    show_title = FALSE, custom_titles = c("A", "B")
  )
  for (i in seq_len(length(multiple))) expect_null(multiple[[i]]$labels$title)
  density <- cellDIVER:::shiny_hexbin_density(
    object, reduction = "umap", nbins = 10, show_title = FALSE, plot_title = NULL
  )
  expect_null(density$labels$title)
  # titles on: the custom title is used
  titled <- cellDIVER:::shiny_hexbin_feature(
    object, features_entered = features[1], reduction = "umap", nbins = 10,
    show_title = TRUE, custom_titles = "Custom"
  )
  expect_identical(titled$labels$title, "Custom")
})
