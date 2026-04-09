# Unit tests for group_metadata_choices()

test_that("group_metadata_choices sorts choices into groups", {
  group_info <- list(
    list(
      group_name = "Immune",
      group_members = c("T Cells", "B Cells", "NK Cells")
    ),
    list(
      group_name = "Stem",
      group_members = c("HSC", "MPP")
    )
  )

  choices <- c("T Cells", "B Cells", "HSC", "NK Cells", "MPP")

  result <- scExploreR:::group_metadata_choices(
    group_info, choices
  )

  expect_type(result, "list")
  expect_named(result, c("Immune", "Stem"))
  expect_equal(
    result$Immune,
    c("B Cells", "NK Cells", "T Cells")
  )
  expect_equal(result$Stem, c("HSC", "MPP"))
})

test_that(
  "group_metadata_choices handles choices not in any group",
  {
    group_info <- list(
      list(
        group_name = "Group A",
        group_members = c("A1", "A2")
      )
    )

    choices <- c("A1", "A2", "B1")

    result <- scExploreR:::group_metadata_choices(
      group_info, choices
    )

    # "B1" is not in any group, should not appear
    expect_equal(result$`Group A`, c("A1", "A2"))
    expect_null(result$`Group B`)
  }
)

test_that(
  "group_metadata_choices returns empty list for no matches",
  {
    group_info <- list(
      list(
        group_name = "Group A",
        group_members = c("X", "Y")
      )
    )

    choices <- c("A", "B", "C")

    result <- scExploreR:::group_metadata_choices(
      group_info, choices
    )

    expect_length(result, 0)
  }
)

test_that(
  "group_metadata_choices sorts entries numerically",
  {
    group_info <- list(
      list(
        group_name = "Numbers",
        group_members = c(
          "Sample 1", "Sample 2", "Sample 10", "Sample 3"
        )
      )
    )

    choices <- c(
      "Sample 10", "Sample 3", "Sample 1", "Sample 2"
    )

    result <- scExploreR:::group_metadata_choices(
      group_info, choices
    )

    # str_sort with numeric = TRUE should sort naturally
    expect_equal(
      result$Numbers,
      c("Sample 1", "Sample 2", "Sample 3", "Sample 10")
    )
  }
)
