test_that("config preview and downloaded YAML survive a save/load roundtrip", {
  app <- browser_app("config-save", fixture = "config")
  browser_load_config(app)
  expect_equal(app$get_value(input = "dataset_label"), "AML Reference Dataset")
  initial_preview <- browser_plot(app, "preview_dimplot")

  app$set_inputs(
    dataset_label = "Browser roundtrip dataset",
    dataset_description = "Saved by the browser regression test.",
    `dimplot-group_by` = "condensed_cell_type",
    `dimplot-split_by` = "Batch",
    `dimplot-reduction` = "umap",
    `dimplot-label` = FALSE
  )
  app$wait_for_idle()
  browser_set(app, "dimplot-ncol", 2)
  preview <- browser_plot(app, "preview_dimplot")
  expect_false(identical(preview$src, initial_preview$src))
  browser_open_dropdown(app, "options", "export_selections")
  config_path <- browser_download(app, "export_selections", ".yaml")
  saved <- cellDIVER:::load_config(config_path)
  expect_equal(saved$label, "Browser roundtrip dataset")
  expect_equal(saved$description, "Saved by the browser regression test.")
  expect_equal(saved$preview$type, "dimplot")
  expect_equal(saved$preview$plot_settings$group_by, "condensed_cell_type")
  expect_equal(saved$preview$plot_settings$split_by, "Batch")
  expect_equal(saved$preview$plot_settings$reduction, "umap")
  expect_equal(saved$preview$plot_settings$ncol, 2)
  expect_false(saved$preview$plot_settings$label)
  expect_setequal(names(saved$assays), c("RNA", "AB"))
  expect_setequal(names(saved$metadata), c("condensed_cell_type", "Batch"))
  browser_stop(app)

  restored <- browser_app(
    "config-reload", fixture = "config", config_path = config_path
  )
  browser_load_config(restored)
  expect_equal(restored$get_value(input = "dataset_label"), saved$label)
  expect_equal(
    restored$get_value(input = "dataset_description"), saved$description
  )
  expect_equal(restored$get_value(input = "dimplot-split_by"), "Batch")
  expect_equal(restored$get_value(input = "dimplot-ncol"), 2)
  expect_false(restored$get_value(input = "dimplot-label"))
  browser_plot(restored, "preview_dimplot")
  browser_open_dropdown(restored, "options", "export_selections")
  second_path <- browser_download(restored, "export_selections", ".yaml")
  expect_equal(cellDIVER:::load_config(second_path), saved)
  browser_stop(restored)

  # The config written by the config app drives the main browser: it starts
  # and draws a real DimPlot from it.
  # TODO: nothing here asserts the dataset *label* reached the browser. The
  # previous assertion looked for it in the Plots tab body, but the label is
  # only ever rendered in the dataset-selection modal
  # (R/modals-data_Modal.R), which single-dataset deployments never open.
  browser <- browser_app("config-main-browser", config_path = config_path)
  browser_open_tab(browser, "plots", "object_plots-make_dimplot")
  browser_plot(browser, "object_plots-dimplot-plot")
})
