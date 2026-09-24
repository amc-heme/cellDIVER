test_that("metadata choices are naturally sorted within matching groups", {
  group_information <- list(
    list(
      group_name = "Samples",
      group_members = c("Sample 1", "Sample 2", "Sample 10")
    ),
    list(group_name = "Shared", group_members = c("Sample 2", "Control")),
    list(group_name = "Absent", group_members = "Unavailable")
  )

  expect_identical(
    cellDIVER:::group_metadata_choices(
      group_information,
      c("Sample 10", "Sample 2", "Sample 1", "Control", "Ungrouped")
    ),
    list(
      Samples = c("Sample 1", "Sample 2", "Sample 10"),
      Shared = c("Control", "Sample 2")
    )
  )
  expect_identical(
    cellDIVER:::group_metadata_choices(group_information, "Unmatched"),
    list()
  )
  expect_identical(
    cellDIVER:::group_metadata_choices(group_information, character()),
    list()
  )
})

test_that("choice processing reacts to grouping configuration changes", {
  metadata_configuration <- shiny::reactiveVal(list(cell_type = list()))
  choices <- c("Type 10", "Type 2", "Type 1")

  expect_identical(
    shiny::isolate(cellDIVER:::process_choices(
      metadata_configuration, "cell_type", choices
    )),
    c("Type 1", "Type 2", "Type 10")
  )

  metadata_configuration(list(
    cell_type = list(groups = list(
      list(group_name = "Selected", group_members = c("Type 10", "Type 2"))
    ))
  ))
  expect_identical(
    shiny::isolate(cellDIVER:::process_choices(
      metadata_configuration, "cell_type", choices
    )),
    list(Selected = c("Type 2", "Type 10"))
  )
  expect_identical(
    shiny::isolate(cellDIVER:::process_choices(
      metadata_configuration, "unconfigured_category", choices
    )),
    c("Type 1", "Type 2", "Type 10")
  )
})

test_that("categorical plot titles use labels and fall back to column names", {
  metadata_configuration <- list(cell_type = list(label = "Cell type"))

  for (plot_type in c("dimplot", "proportion", "pie")) {
    expect_identical(
      cellDIVER:::initial_title(
        plot_type, group_by = "cell_type",
        metadata_config = metadata_configuration
      ),
      "Cell type"
    )
    expect_identical(
      cellDIVER:::initial_title(
        plot_type, group_by = "condition",
        metadata_config = metadata_configuration
      ),
      "condition"
    )
    expect_error(
      cellDIVER:::initial_title(plot_type, group_by = "cell_type"),
      "metadata_config"
    )
    expect_error(
      cellDIVER:::initial_title(
        plot_type, metadata_config = metadata_configuration
      ),
      "group_by"
    )
  }
})

test_that("feature titles handle modality labels and multi-feature plots", {
  assay_configuration <- list(
    RNA = list(key = "rna_", suffix_human = ""),
    AB = list(key = "ab_", suffix_human = "Surface Protein")
  )

  for (plot_type in c("feature", "ridge")) {
    expect_equal(
      as.character(cellDIVER:::initial_title(
        plot_type, assay_config = assay_configuration,
        features_entered = "ab_GeneAlpha"
      )),
      "GeneAlpha (Surface Protein)"
    )
    expect_identical(
      cellDIVER:::initial_title(
        plot_type, assay_config = assay_configuration,
        features_entered = "ab_GeneAlpha", show_modality_key = TRUE
      ),
      "ab_GeneAlpha"
    )
    feature_cases <- list(
      NULL, character(), c("rna_GeneAlpha", "rna_GeneBeta")
    )
    for (features in feature_cases) {
      expect_identical(
        cellDIVER:::initial_title(
          plot_type, assay_config = assay_configuration,
          features_entered = features
        ),
        ""
      )
    }
    expect_error(cellDIVER:::initial_title(plot_type), "assay_config")
  }
  expect_error(cellDIVER:::initial_title("unsupported"), "plot_type")
})

test_that("human readable feature names respect suffix and metadata settings", {
  assay_configuration <- list(
    RNA = list(key = "rna_", suffix_human = ""),
    AB = list(key = "ab_", suffix_human = "Surface Protein")
  )

  expect_identical(
    cellDIVER:::hr_name("rna_GeneAlpha", assay_configuration),
    "GeneAlpha"
  )
  expect_identical(
    cellDIVER:::hr_name(
      "ab_GeneAlpha", assay_configuration, use_suffix = FALSE
    ),
    "GeneAlpha"
  )
  expect_identical(
    cellDIVER:::hr_name("quality_score", assay_configuration),
    "quality_score"
  )
  expect_error(
    cellDIVER:::hr_name(
      c("rna_GeneAlpha", "rna_GeneBeta"), assay_configuration
    ),
    "more than one machine-readable name"
  )
})
