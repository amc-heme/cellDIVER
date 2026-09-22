test_that("rounded slider endpoints do not exclude extreme DGE results", {
  differential_results <- shiny::reactiveVal(data.frame(
    group = c("Type 1", "Type 1", "Type 2"),
    feature = c("GeneAlpha", "GeneBeta", "GeneGamma"),
    avgExpr = c(0.123456, 2, 9.87654),
    log2FC = c(-2.34567, 0, 3.45678),
    auc = c(0.3, 0.5, 0.7),
    pct_in = c(10, 20, 90),
    pct_out = c(20, 25, 15)
  ))

  shiny::testServer(
    cellDIVER:::dge_table_filtering_server,
    args = list(dge_table = differential_results),
    {
      session$setInputs(
        expr = c(0.1235, 9.877), lfc = c(-2.346, 3.457),
        expr_manual_enable = FALSE, lfc_manual_enable = FALSE,
        group = "Type 1", feature = "GeneAlpha", pval_adj = "0.01",
        auc = c(0.2, 0.8), pct_in = c(5, 95), pct_out = c(0, 30)
      )
      expect_identical(
        session$returned$expression(), list(min = NULL, max = NULL)
      )
      expect_identical(session$returned$lfc(), list(min = NULL, max = NULL))
      expect_identical(session$returned$group(), "Type 1")
      expect_identical(session$returned$feature(), "GeneAlpha")
      expect_identical(session$returned$pval_adj(), 0.01)
      expect_identical(session$returned$auc(), c(0.2, 0.8))
      expect_identical(session$returned$pct_in(), c(5, 95))
      expect_identical(session$returned$pct_out(), c(0, 30))

      session$setInputs(expr = c(1, 8), lfc = c(-1, 2))
      expect_identical(session$returned$expression(), list(min = 1, max = 8))
      expect_identical(session$returned$lfc(), list(min = -1, max = 2))

      session$setInputs(expr = c(0.1235, 8), lfc = c(-1, 3.457))
      expect_identical(session$returned$expression(), list(min = NULL, max = 8))
      expect_identical(session$returned$lfc(), list(min = -1, max = NULL))
    }
  )
})

test_that("manual DGE bounds distinguish zero from an unspecified endpoint", {
  differential_results <- shiny::reactiveVal(data.frame(
    avgExpr = c(0, 10),
    log2FC = c(-3, 3)
  ))

  shiny::testServer(
    cellDIVER:::dge_table_filtering_server,
    args = list(dge_table = differential_results),
    {
      session$setInputs(
        expr_manual_enable = TRUE, lfc_manual_enable = TRUE,
        expr_min_text = "0", expr_max_text = "",
        lfc_min_text = "", lfc_max_text = "0",
        expr = c(2, 8), lfc = c(-2, 2)
      )
      expect_identical(session$returned$expression(), list(min = 0, max = NULL))
      expect_identical(session$returned$lfc(), list(min = NULL, max = 0))

      session$setInputs(
        expr_min_text = "1.25", expr_max_text = "7.5",
        lfc_min_text = "-1.5", lfc_max_text = "2.25"
      )
      expect_identical(
        session$returned$expression(), list(min = 1.25, max = 7.5)
      )
      expect_identical(session$returned$lfc(), list(min = -1.5, max = 2.25))

      session$setInputs(expr_manual_enable = FALSE, lfc_manual_enable = FALSE)
      expect_identical(session$returned$expression(), list(min = 2, max = 8))
      expect_identical(session$returned$lfc(), list(min = -2, max = 2))
    }
  )
})

test_that("changing DGE result schemas disables obsolete filter values", {
  differential_results <- shiny::reactiveVal(data.frame(
    avgExpr = c(0, 10),
    log2FC = c(-3, 3),
    auc = c(0.2, 0.8),
    pct_in = c(10, 90),
    pct_out = c(5, 20)
  ))

  shiny::testServer(
    cellDIVER:::dge_table_filtering_server,
    args = list(dge_table = differential_results),
    {
      session$setInputs(
        expr = c(2, 8), lfc = c(-2, 2),
        auc = c(0.3, 0.7), pct_in = c(20, 80), pct_out = c(5, 10)
      )
      expect_identical(session$returned$expression(), list(min = 2, max = 8))
      expect_identical(session$returned$lfc(), list(min = -2, max = 2))
      expect_identical(session$returned$auc(), c(0.3, 0.7))

      differential_results(data.frame(statistic = c(1, 2)))
      session$flushReact()
      expect_identical(
        session$returned$expression(), list(min = NULL, max = NULL)
      )
      expect_identical(session$returned$lfc(), list(min = NULL, max = NULL))
      expect_null(session$returned$auc())
      expect_null(session$returned$pct_in())
      expect_null(session$returned$pct_out())

      differential_results(data.frame(
        auc = c(0.1, 0.9), pct_in = c(5, 95), pct_out = c(1, 30)
      ))
      session$flushReact()
      expect_identical(session$returned$auc(), c(0.3, 0.7))
      expect_identical(session$returned$pct_in(), c(20, 80))
      expect_identical(session$returned$pct_out(), c(5, 10))
    }
  )
})
