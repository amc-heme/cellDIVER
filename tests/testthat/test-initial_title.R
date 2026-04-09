# Unit tests for initial_title()

test_that(
  "initial_title returns label for dimplot with config label",
  {
    metadata_config <- list(
      condensed_cell_type = list(
        label = "Cell Type"
      )
    )

    result <- scExploreR:::initial_title(
      plot_type = "dimplot",
      group_by = "condensed_cell_type",
      metadata_config = metadata_config
    )
    expect_equal(result, "Cell Type")
  }
)

test_that(
  "initial_title returns group_by name when no label defined",
  {
    metadata_config <- list(
      Batch = list(
        label = NULL
      )
    )

    result <- scExploreR:::initial_title(
      plot_type = "dimplot",
      group_by = "Batch",
      metadata_config = metadata_config
    )
    expect_equal(result, "Batch")
  }
)

test_that(
  "initial_title returns HR name for single feature plot",
  {
    assay_config <- list(
      RNA = list(
        key = "rna_",
        suffix_human = ""
      )
    )

    result <- scExploreR:::initial_title(
      plot_type = "feature",
      assay_config = assay_config,
      features_entered = "rna_TP53"
    )
    expect_equal(as.character(result), "TP53")
  }
)

test_that(
  "initial_title returns empty string for multi-feature plot",
  {
    assay_config <- list(
      RNA = list(
        key = "rna_",
        suffix_human = ""
      )
    )

    result <- scExploreR:::initial_title(
      plot_type = "feature",
      assay_config = assay_config,
      features_entered = c("rna_TP53", "rna_GAPDH")
    )
    expect_equal(result, "")
  }
)

test_that("initial_title errors on invalid plot_type", {
  expect_error(
    scExploreR:::initial_title(plot_type = "invalid"),
    "plot_type"
  )
})

test_that(
  "initial_title errors when metadata_config is missing",
  {
    expect_error(
      scExploreR:::initial_title(
        plot_type = "dimplot",
        group_by = "Batch"
      ),
      "metadata_config"
    )
  }
)

test_that(
  "initial_title errors when group_by is missing for dimplot",
  {
    expect_error(
      scExploreR:::initial_title(
        plot_type = "dimplot",
        metadata_config = list()
      ),
      "group_by"
    )
  }
)

test_that(
  "initial_title errors when assay_config is missing",
  {
    expect_error(
      scExploreR:::initial_title(
        plot_type = "feature"
      ),
      "assay_config"
    )
  }
)

test_that("initial_title works for proportion plot_type", {
  metadata_config <- list(
    Batch = list(label = "Sample Batch")
  )

  result <- scExploreR:::initial_title(
    plot_type = "proportion",
    group_by = "Batch",
    metadata_config = metadata_config
  )
  expect_equal(result, "Sample Batch")
})

test_that("initial_title works for pie plot_type", {
  metadata_config <- list(
    Batch = list(label = "Sample Batch")
  )

  result <- scExploreR:::initial_title(
    plot_type = "pie",
    group_by = "Batch",
    metadata_config = metadata_config
  )
  expect_equal(result, "Sample Batch")
})

test_that("initial_title works for ridge plot_type", {
  assay_config <- list(
    AB = list(
      key = "ab_",
      suffix_human = "Surface Protein"
    )
  )

  result <- scExploreR:::initial_title(
    plot_type = "ridge",
    assay_config = assay_config,
    features_entered = "ab_CD34"
  )
  expect_equal(
    as.character(result),
    "CD34 (Surface Protein)"
  )
})
