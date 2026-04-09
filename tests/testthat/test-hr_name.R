# Unit tests for hr_name()

test_that("hr_name removes assay key and adds suffix", {
  assay_config <- list(
    RNA = list(
      key = "rna_",
      suffix_human = ""
    ),
    AB = list(
      key = "ab_",
      suffix_human = "Surface Protein"
    )
  )

  result <- scExploreR:::hr_name(
    machine_readable_name = "ab_CD34",
    assay_config = assay_config,
    use_suffix = TRUE
  )
  expect_equal(as.character(result), "CD34 (Surface Protein)")
})

test_that("hr_name removes key without suffix", {
  assay_config <- list(
    RNA = list(
      key = "rna_",
      suffix_human = ""
    )
  )

  result <- scExploreR:::hr_name(
    machine_readable_name = "rna_TP53",
    assay_config = assay_config,
    use_suffix = TRUE
  )
  # RNA has empty suffix, so no parenthetical
  expect_equal(as.character(result), "TP53")
})

test_that("hr_name respects use_suffix = FALSE", {
  assay_config <- list(
    AB = list(
      key = "ab_",
      suffix_human = "Surface Protein"
    )
  )

  result <- scExploreR:::hr_name(
    machine_readable_name = "ab_CD34",
    assay_config = assay_config,
    use_suffix = FALSE
  )
  expect_equal(as.character(result), "CD34")
})

test_that(
  "hr_name returns machine name for unmatched features",
  {
    assay_config <- list(
      RNA = list(
        key = "rna_",
        suffix_human = ""
      )
    )

    # Numeric metadata won't match any assay key
    result <- scExploreR:::hr_name(
      machine_readable_name = "nCount_RNA",
      assay_config = assay_config,
      use_suffix = TRUE
    )
    expect_equal(result, "nCount_RNA")
  }
)

test_that("hr_name errors on multi-element vector", {
  assay_config <- list(
    RNA = list(
      key = "rna_",
      suffix_human = ""
    )
  )

  expect_error(
    scExploreR:::hr_name(
      machine_readable_name = c("rna_TP53", "rna_GAPDH"),
      assay_config = assay_config
    ),
    "more than one machine-readable name"
  )
})
