# Unit tests for ncol_settings()
#
# ncol_settings depends on a single-cell object (through
# n_unique / SCUBA). Tests with objects are skipped if
# the test dataset is not available. Pure logic tests
# for the "features" rule do not require an object.

test_that("ncol_settings errors on invalid rule", {
  expect_error(
    scExploreR:::ncol_settings(
      object = NULL,
      rule = "invalid"
    ),
    'must be set to either "split_by" or "features"'
  )
})

test_that(
  "ncol_settings errors when split_by is missing",
  {
    expect_error(
      scExploreR:::ncol_settings(
        object = NULL,
        rule = "split_by"
      ),
      "split_by.*must be defined"
    )
  }
)

test_that(
  "ncol_settings errors when features_entered is missing",
  {
    expect_error(
      scExploreR:::ncol_settings(
        object = NULL,
        rule = "features"
      ),
      "features_entered.*must be defined"
    )
  }
)

test_that(
  "ncol_settings returns correct values for 1-3 features",
  {
    # 2 features: default should be 2 (side-by-side)
    result <- scExploreR:::ncol_settings(
      object = NULL,
      rule = "features",
      features_entered = c("gene1", "gene2")
    )
    expect_named(result, c("min", "max", "default"))
    expect_equal(result[["min"]], 1)
    expect_equal(result[["max"]], 2)
    expect_equal(result[["default"]], 2)
  }
)

test_that(
  "ncol_settings returns floor(sqrt(n)) for 4-11 features",
  {
    # 9 features: floor(sqrt(9)) = 3
    result <- scExploreR:::ncol_settings(
      object = NULL,
      rule = "features",
      features_entered = paste0("gene", 1:9)
    )
    expect_equal(result[["min"]], 1)
    expect_equal(result[["max"]], 9)
    expect_equal(result[["default"]], 3)

    # 4 features: floor(sqrt(4)) = 2
    result_4 <- scExploreR:::ncol_settings(
      object = NULL,
      rule = "features",
      features_entered = paste0("gene", 1:4)
    )
    expect_equal(result_4[["default"]], 2)
  }
)

test_that(
  "ncol_settings returns 4 for 12+ features",
  {
    result <- scExploreR:::ncol_settings(
      object = NULL,
      rule = "features",
      features_entered = paste0("gene", 1:20)
    )
    expect_equal(result[["min"]], 1)
    expect_equal(result[["max"]], 20)
    expect_equal(result[["default"]], 4)
  }
)

test_that(
  "ncol_settings returns 1 for single feature",
  {
    result <- scExploreR:::ncol_settings(
      object = NULL,
      rule = "features",
      features_entered = "gene1"
    )
    expect_equal(result[["min"]], 1)
    expect_equal(result[["max"]], 1)
    expect_equal(result[["default"]], 1)
  }
)
