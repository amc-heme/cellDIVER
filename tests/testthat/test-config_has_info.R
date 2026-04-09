# Unit tests for browser_config_has_info() and
# dataset_config_has_info()

test_that(
  "browser_config_has_info returns TRUE when all info present",
  {
    datasets <- list(
      dataset1 = list(
        label = "Dataset 1",
        description = "Test dataset",
        plot = "dimplot"
      ),
      dataset2 = list(
        label = "Dataset 2",
        description = "Another dataset",
        plot = "featureplot"
      )
    )

    result <- scExploreR:::browser_config_has_info(datasets)
    expect_true(result)
  }
)

test_that(
  "browser_config_has_info returns FALSE when info is missing",
  {
    datasets <- list(
      dataset1 = list(
        label = "Dataset 1",
        description = "Test dataset"
        # Missing "plot"
      )
    )

    result <- scExploreR:::browser_config_has_info(datasets)
    expect_false(result)
  }
)

test_that(
  "browser_config_has_info returns FALSE when incomplete",
  {
    datasets <- list(
      dataset1 = list(
        label = "Dataset 1",
        description = "Test dataset",
        plot = "dimplot"
      ),
      dataset2 = list(
        label = "Dataset 2"
        # Missing "description" and "plot"
      )
    )

    result <- scExploreR:::browser_config_has_info(datasets)
    expect_false(result)
  }
)

test_that(
  "dataset_config_has_info returns TRUE when all info present",
  {
    datasets <- list(
      dataset1 = list(
        config = list(
          label = "Dataset 1",
          description = "A test dataset",
          preview = list(type = "dimplot")
        )
      ),
      dataset2 = list(
        config = list(
          label = "Dataset 2",
          description = "Another test dataset",
          preview = list(type = "featureplot")
        )
      )
    )

    result <- scExploreR:::dataset_config_has_info(datasets)
    expect_true(result)
  }
)

test_that(
  "dataset_config_has_info returns FALSE when info missing",
  {
    datasets <- list(
      dataset1 = list(
        config = list(
          label = "Dataset 1"
          # Missing "description" and "preview"
        )
      )
    )

    result <- scExploreR:::dataset_config_has_info(datasets)
    expect_false(result)
  }
)
