library(testthat)

test_that("validate_dataset_names accepts valid names", {
  # Valid dataset names (no spaces)
  valid_datasets <- list(
    dataset_1 = list(object = "path1.rds", config = "config1.yaml"),
    my_dataset = list(object = "path2.rds", config = "config2.yaml"),
    AML_samples = list(object = "path3.rds", config = "config3.yaml")
  )
  
  # Should not throw an error
  expect_silent(validate_dataset_names(valid_datasets))
})

test_that("validate_dataset_names rejects names with whitespace", {
  # Invalid dataset names (contain whitespace)
  invalid_datasets <- list(
    `Seurat object` = list(object = "path1.rds", config = "config1.yaml"),
    `my dataset` = list(object = "path2.rds", config = "config2.yaml")
  )
  
  # Should throw an error
  expect_error(
    validate_dataset_names(invalid_datasets),
    "Invalid dataset names found in browser config file"
  )
  
  # Error message should mention the invalid names
  expect_error(
    validate_dataset_names(invalid_datasets),
    "Seurat object"
  )
  
  expect_error(
    validate_dataset_names(invalid_datasets),
    "my dataset"
  )
})

test_that("validate_dataset_names handles single invalid name", {
  # One invalid dataset name
  datasets <- list(
    `Invalid Name` = list(object = "path1.rds", config = "config1.yaml")
  )
  
  # Should throw an error
  expect_error(
    validate_dataset_names(datasets),
    "Invalid Name"
  )
})

test_that("validate_dataset_names handles mixed valid and invalid names", {
  # Mix of valid and invalid names
  mixed_datasets <- list(
    valid_dataset = list(object = "path1.rds", config = "config1.yaml"),
    `invalid dataset` = list(object = "path2.rds", config = "config2.yaml"),
    another_valid = list(object = "path3.rds", config = "config3.yaml")
  )
  
  # Should throw an error
  expect_error(
    validate_dataset_names(mixed_datasets),
    "Invalid dataset names found"
  )
  
  # Should mention only the invalid name
  expect_error(
    validate_dataset_names(mixed_datasets),
    "invalid dataset"
  )
})

test_that("validate_dataset_names handles empty list", {
  # Empty dataset list
  empty_datasets <- list()
  
  # Should not throw an error
  expect_silent(validate_dataset_names(empty_datasets))
})

test_that("validate_dataset_names provides helpful error message", {
  # Invalid dataset
  invalid_datasets <- list(
    `bad name` = list(object = "path.rds", config = "config.yaml")
  )
  
  # Check that error message contains helpful guidance
  expect_error(
    validate_dataset_names(invalid_datasets),
    "Replace whitespace in dataset names with underscores"
  )
  
  expect_error(
    validate_dataset_names(invalid_datasets),
    "Example of a valid browser config file"
  )
})
