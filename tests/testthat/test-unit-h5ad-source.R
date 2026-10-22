# File-backed .h5ad datasets served by SCUBA's Julia engine (class scuba_source).
skip_if_not_installed("SCUBA")
skip_if_not(exists("open_h5ad", asNamespace("SCUBA")), "SCUBA without open_h5ad()")
skip_if_not(isTRUE(tryCatch(SCUBA::scuba_julia_available(quiet = TRUE), error = function(e) FALSE)), "Julia engine not available")

h5ad_object <- function() {
  withr::local_options(list(SCUBA.engine = "julia"))
  SCUBA::open_h5ad(system.file("extdata", "AML_h5ad.h5ad", package = "SCUBA", mustWork = TRUE))
}

test_that("use_julia_h5ad() honours the option", {
  withr::with_options(list(cellDIVER.h5ad_engine = "reticulate"), expect_false(use_julia_h5ad()))
  withr::with_options(list(cellDIVER.h5ad_engine = "auto", SCUBA.engine = "julia"), expect_true(use_julia_h5ad()))
  withr::with_options(list(cellDIVER.h5ad_engine = "julia", SCUBA.engine = "julia"), expect_true(use_julia_h5ad()))
  withr::with_options(list(cellDIVER.h5ad_engine = "julia", SCUBA.engine = "r"), expect_error(use_julia_h5ad(), "not available"))
  withr::with_options(list(cellDIVER.h5ad_engine = "bogus"), expect_error(use_julia_h5ad(), "auto"))
})

test_that("dispatch helpers work for scuba_source objects", {
  withr::local_options(list(SCUBA.engine = "julia"))
  obj <- h5ad_object()
  expect_equal(make_key(obj, "protein"), "protein_")
  expect_equal(assay_names(obj), c("X", "protein"))          # no uns$cellDIVER_assays in the fixture: fall back to modalities
  expect_equal(reduction_names(obj), c("X_pca", "X_umap"))  # no uns$cellDIVER_reductions: numeric obsm arrays
  expect_equal(relabel_axis(obj, "X_umap_1"), "Embeddings Value")
  expect_null(relabel_axis(obj, "X_GAPDH"))
  expect_equal(relabel_axis(obj, "Batch"), "")
  expect_null(check_dataset(obj))
  # uns lists win when present
  obj2 <- obj; obj2$info$uns_assays <- "protein"; obj2$info$uns_reductions <- "X_umap"
  expect_equal(assay_names(obj2), c("X", "protein"))
  expect_equal(reduction_names(obj2), "X_umap")
})

test_that("update_object_metadata is copy-on-modify for scuba_source objects", {
  withr::local_options(list(SCUBA.engine = "julia"))
  obj <- h5ad_object()
  meta <- SCUBA::fetch_metadata(obj, full_table = TRUE)
  meta$flag <- meta$Batch == "BM_200AB"
  obj2 <- update_object_metadata(obj, meta)
  expect_true("flag" %in% SCUBA::meta_varnames(obj2))
  expect_false("flag" %in% SCUBA::meta_varnames(obj))
  expect_equal(SCUBA::fetch_data(obj2, c("GAPDH", "flag"))$flag, meta$flag)
})

test_that("make_subset filters scuba_source objects by metadata and expression", {
  withr::local_options(list(SCUBA.engine = "julia"))
  obj <- h5ad_object()
  meta <- SCUBA::fetch_metadata(obj, full_table = TRUE)
  sub <- make_subset(obj, list(list(var = "Batch", type = "categorical", value = "BM_200AB")))
  expect_s3_class(sub, "scuba_source")
  expect_setequal(SCUBA::get_all_cells(sub), rownames(meta)[meta$Batch == "BM_200AB"])
  expr <- SCUBA::fetch_data(obj, "X_GAPDH")
  sub2 <- make_subset(obj, list(list(var = "X_GAPDH", type = "numeric", mode = "greater_than", value = 3)))
  expect_setequal(SCUBA::get_all_cells(sub2), rownames(expr)[expr$X_GAPDH >= 3])
  both <- make_subset(obj, list(list(var = "Batch", type = "categorical", value = "BM_200AB"),
                                list(var = "X_GAPDH", type = "numeric", mode = "range", value = c(2, 3))))
  expect_setequal(SCUBA::get_all_cells(both), intersect(rownames(meta)[meta$Batch == "BM_200AB"], rownames(expr)[expr$X_GAPDH >= 2 & expr$X_GAPDH <= 3]))
  expect_identical(make_subset(obj, list()), obj)
  # the subset still serves plots' data needs
  expect_equal(nrow(SCUBA::fetch_data(sub, c("X_GAPDH", "Batch"))), length(SCUBA::get_all_cells(sub)))
})
