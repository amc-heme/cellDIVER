# chromote waits 10 seconds by default for Chrome's debugging port. A loaded CI
# runner can exceed that, and a failed launch surfaces as a skip - which the
# workflow treats as a failure. Give the browser room to start.
options(chromote.timeout = 120)

#' Start an isolated browser fixture
#'
#' Launches the current package in shinytest2's child R process. Missing
#' dependencies, browser executables, and fixtures deliberately fail outside
#' CRAN.
#'
#' @param name Unique test scenario name.
#' @param fixture App directory under `tests/testthat/apps`.
#' @param config_path Optional saved configuration to load.
#' @param cleanup_environment Environment whose exit stops the app.
#' @return A running `shinytest2::AppDriver`.
browser_app <- function(name, fixture = "cellDIVER", config_path = NULL,
                        cleanup_environment = parent.frame()) {
  testthat::skip_on_cran()
  app_options <- list()
  if (!is.null(config_path)) {
    app_options$cellDIVER.test_config <- normalizePath(
      config_path, mustWork = TRUE
    )
  }
  artifact_directory <- file.path(
    normalizePath(testthat::test_path("apps", fixture), mustWork = TRUE),
    "tests", "artifacts", name
  )
  app <- tryCatch(
    shinytest2::AppDriver$new(
      app_dir = normalizePath(
        testthat::test_path("apps", fixture), mustWork = TRUE
      ),
      name = name,
      seed = 325,
      width = 1440,
      height = 1000,
      load_timeout = 120000,
      timeout = 60000,
      options = app_options
    ),
    error = function(condition) {
      # shinytest2 attaches a partially initialized driver to startup errors.
      if (!is.null(condition$app)) {
        tryCatch(
          browser_artifacts(condition$app, artifact_directory),
          finally = condition$app$stop()
        )
      }
      stop(condition)
    }
  )
  stopped <- FALSE
  attr(app, "browser_cleanup") <- function() {
    if (stopped) return(invisible(NULL))
    on.exit({
      stopped <<- TRUE
      app$stop()
    }, add = TRUE)
    browser_artifacts(app, artifact_directory)
  }
  withr::defer(browser_stop(app), envir = cleanup_environment)
  app
}

#' Preserve browser diagnostics before shutting down
#'
#' Captures every scenario, including assertion failures and startup failures,
#' without relying on testthat reporter internals. The CI artifact upload
#' includes these fixture-local diagnostic directories.
#'
#' @param app Running or partially initialized browser driver.
#' @param directory Directory for logs and a final viewport screenshot.
#' @return No return value; writes diagnostic files.
browser_artifacts <- function(app, directory) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  for (artifact in c("logs.csv", "screenshot.png")) {
    path <- file.path(directory, artifact)
    unlink(path)
    tryCatch(
      {
        if (artifact == "logs.csv") {
          utils::write.csv(app$get_logs(), path, row.names = FALSE)
        } else {
          app$get_screenshot(file = path, delay = 0, selector = "viewport")
        }
      },
      error = function(condition) {
        # A crashed browser may prevent capture. Report that failure without
        # replacing the original assertion/error or preventing app cleanup.
        warning(
          "Could not capture browser artifact ", path, ": ",
          conditionMessage(condition), call. = FALSE
        )
      }
    )
  }
  invisible(NULL)
}

#' Save diagnostics and stop a browser exactly once
#'
#' @param app Driver returned by `browser_app()`.
#' @return No return value; releases browser and child R processes.
browser_stop <- function(app) {
  attr(app, "browser_cleanup")()
}

#' Read the shipped single-cell browser fixture
#'
#' @return The unmodified Seurat object used by the browser.
browser_object <- function() {
  readRDS(system.file(
    "extdata", "test_dataset.rds", package = "cellDIVER", mustWork = TRUE
  ))
}

#' Open a tab and wait for its dynamically rendered body
#'
#' The Plots and DGE tab bodies are `renderUI` outputs driven by an
#' `eventReactive` on the object and config. `wait_for_idle()` alone returns
#' before that UI arrives, so inputs read immediately after a tab click come
#' back NULL and `set_inputs()` silently targets elements that are not in the
#' DOM yet. Wait for a known input of the tab to exist *and* be bound.
#'
#' @param app Running browser driver.
#' @param tab Value of the tab's `data-value` attribute.
#' @param anchor Fully namespaced ID of an input the tab body must contain.
#' @return The driver, invisibly, once the tab body is interactive.
browser_open_tab <- function(app, tab, anchor) {
  app$click(selector = sprintf("a[data-value='%s']", tab))
  app$wait_for_js(
    sprintf("document.querySelector('#%s.shiny-bound-input') !== null", anchor),
    timeout = 60000
  )
  app$wait_for_idle()
  invisible(app)
}

#' Choose the differential expression method and wait for the server to apply it
#'
#' The method picker's choices are re-sent by the server once the object's
#' metadata is known, with the selection the server last saw. On a slow
#' runner that update can arrive after the test has already chosen edgeR
#' and silently put the picker back to Wilcoxon. Select, let Shiny settle,
#' confirm the value, retry if it was overwritten, and finally wait for the
#' edgeR options panel, which the server shows only once it has applied
#' the choice.
#'
#' @param app Running browser driver.
#' @param value `"edger"` or `"wilcoxon"`.
#' @return The driver, invisibly.
browser_select_method <- function(app, value) {
  input <- "object_dge-test_selections-method"
  for (attempt in 1:3) {
    browser_set(app, input, value)
    app$wait_for_idle(duration = 1000)
    if (identical(app$get_value(input = input), value)) break
  }
  testthat::expect_identical(app$get_value(input = input), value)
  if (identical(value, "edger")) {
    app$wait_for_js(
      paste0(
        "getComputedStyle(document.getElementById(",
        "'object_dge-test_selections-edger_options')).display !== 'none'"
      ),
      timeout = 30000
    )
  }
  invisible(app)
}

#' Set a dynamically named bound browser input
#'
#' Fails when the target is absent: `set_inputs()` only logs "Unable to find
#' input binding" and carries on, which turns a missing element into a
#' misleading timeout much later in the test.
#'
#' Also confirms the value the server holds afterwards. Many pickers are
#' refreshed by the server when another input changes (the group pickers after
#' `group_by`, the filter values after the filter variable, the method picker
#' after the object loads), and that refresh selects a default. On a slow
#' runner it can arrive after the test's own value and silently overwrite it,
#' which then surfaces as a wrong result or a timeout much later. Let Shiny
#' settle, read the value back, and set it again if it was overwritten.
#'
#' @param app Running browser driver.
#' @param input Fully namespaced input ID.
#' @param value Value to send through the real input binding.
#' @param verify Confirm the value from the server and retry. `FALSE` for
#'   one-shot menus the server resets on purpose after reading them (the
#'   subset filter type picker).
#' @return The driver, invisibly, after reactive updates settle.
browser_set <- function(app, input, value, verify = TRUE) {
  testthat::expect_true(
    app$get_js(sprintf("document.getElementById('%s') !== null", input)),
    label = paste0("input '", input, "' exists in the DOM")
  )
  for (attempt in 1:3) {
    do.call(app$set_inputs, c(
      stats::setNames(list(value), input), list(wait_ = FALSE)
    ))
    app$wait_for_idle(duration = 500)
    if (!verify || browser_value_matches(app$get_value(input = input), value)) break
  }
  if (!verify) return(invisible(app))
  testthat::expect_true(
    browser_value_matches(app$get_value(input = input), value),
    label = paste0(
      "input '", input, "' holds ", paste(format(value), collapse = ", "),
      " after being set"
    )
  )
  invisible(app)
}

#' Compare a value read back from Shiny with the value that was set
#'
#' Read-back values come as character vectors (or lists for multi-selects)
#' and in widget order, so compare as unordered character sets; numbers and
#' logicals are compared through their character form.
#'
#' @param actual Value from `app$get_value(input = )`.
#' @param expected Value passed to `browser_set()`.
#' @return `TRUE` when both describe the same selection.
browser_value_matches <- function(actual, expected) {
  actual <- as.character(unlist(actual))
  expected <- as.character(unlist(expected))
  if (is.null(actual) || !length(actual)) return(!length(expected))
  setequal(actual, expected)
}

#' Choose a value in a server-side selectize input
#'
#' The feature pickers are built with `choices = NULL` and `create = FALSE`,
#' then filled by `updateSelectizeInput(server = TRUE)`, which fetches options
#' from the server as the user types. Until those options arrive the widget
#' does not know the value, and selectize discards it - silently, so
#' `set_inputs()` reports success and the failure only surfaces much later as a
#' plot that never renders. On a fast machine the options usually win the race,
#' which is why this passed locally and failed in CI.
#'
#' Register the option client-side first, then select it through selectize's
#' own API so the change event reaches Shiny exactly as a real selection would.
#'
#' @param app Running browser driver.
#' @param input Fully namespaced selectize input ID.
#' @param values One or more option values to select.
#' @return The driver, invisibly, once Shiny reports the new value.
browser_set_feature <- function(app, input, values) {
  values <- as.character(values)
  id <- jsonlite::toJSON(input, auto_unbox = TRUE)
  script <- sprintf(
    paste0(
      "(() => { const element = document.getElementById(%s);",
      " const control = element && element.selectize;",
      " if (!control) throw new Error('no selectize instance on ' + %s);",
      " const values = %s;",
      " values.forEach(value =>",
      " control.addOption({value: value, label: value}));",
      " control.setValue(values, false); })()"
    ),
    id, id, jsonlite::toJSON(values)
  )
  # A late `updateSelectizeInput()` from the server can clear the selection
  # (see browser_set()): confirm the value and select again if needed.
  for (attempt in 1:3) {
    app$run_js(script)
    app$wait_for_idle(duration = 500)
    if (browser_value_matches(app$get_value(input = input), values)) break
  }
  # Fail here rather than at a downstream timeout if the value did not stick.
  testthat::expect_setequal(app$get_value(input = input), values)
  invisible(app)
}

#' Click a bound action input and wait for reactive work
#'
#' Some buttons only send custom UI messages, not a new output value.
#'
#' @param app Running browser driver.
#' @param input Fully namespaced action input ID.
#' @return The driver, invisibly, after reactive updates settle.
browser_click <- function(app, input) {
  app$click(input, wait_ = FALSE)
  app$wait_for_idle()
  invisible(app)
}

#' Expand a collapsible panel so its outputs are served
#'
#' `collapsible_panel()` renders a header button followed by a content div that
#' is `display: none` until clicked. Shiny suspends outputs inside it, so a
#' `renderUI` such as the subset filter summary stays empty while collapsed.
#'
#' @param app Running browser driver.
#' @param panel Input ID of the collapsible panel header.
#' @return The driver, invisibly, once the panel content is displayed.
browser_expand_panel <- function(app, panel) {
  visible <- sprintf(
    paste0(
      "(() => { const header = document.getElementById('%s');",
      " if (header === null) return false;",
      " const content = header.nextElementSibling;",
      " return content !== null &&",
      " getComputedStyle(content).display !== 'none'; })()"
    ),
    panel
  )
  if (!isTRUE(app$get_js(visible))) {
    app$run_js(sprintf("document.getElementById('%s').click()", panel))
  }
  app$wait_for_js(visible, timeout = 30000)
  app$wait_for_idle()
  invisible(app)
}

#' Add a categorical filter through the current filter editor
#'
#' @param app Running browser driver.
#' @param namespace Namespace of the subset selections module.
#' @param variable Metadata column to filter.
#' @param values Metadata values to retain.
#' @param panel Optional collapsible panel that must be open for the filter
#'   summary output to be served.
#' @return The driver, invisibly, with the filter saved but not submitted.
browser_filter <- function(app, namespace, variable, values, panel = NULL) {
  if (!is.null(panel)) browser_expand_panel(app, panel)
  browser_click(app, paste0(namespace, "-add_filter"))
  # the type picker is reset to blank by the server once it has read it
  browser_set(app, paste0(namespace, "-filter_type"), "categorical", verify = FALSE)
  browser_set(app, paste0(namespace, "-categorical_var"), variable)
  browser_set(app, paste0(namespace, "-categorical_values"), values)
  browser_click(app, paste0(namespace, "-filter_confirm"))
  for (value in values) {
    testthat::expect_match(
      app$get_text(paste0("#", namespace, "-filters_applied")),
      value, fixed = TRUE
    )
  }
  invisible(app)
}

#' Assert an actual plot image has rendered in the browser
#'
#' Checks the decoded image, not the presence of its UI placeholder.
#'
#' @param app Running browser driver.
#' @param output Fully namespaced plot output ID.
#' @return The plot output value, including its image and coordinate map.
browser_plot <- function(app, output) {
  selector <- jsonlite::toJSON(paste0("#", output, " img"), auto_unbox = TRUE)
  app$wait_for_js(sprintf(
    paste0(
      "(() => { const image = document.querySelector(%s);",
      " return image && image.complete && image.naturalWidth > 20 &&",
      " image.naturalHeight > 20 && image.getClientRects().length > 0; })()"
    ),
    selector
  ))
  value <- app$get_value(output = output)
  testthat::expect_match(value$src, "^data:image/png;base64,")
  testthat::expect_gt(value$width, 20)
  testthat::expect_gt(value$height, 20)
  value
}

#' Open a collapsed dropdown so its download link is served
#'
#' Shiny suspends outputs whose element is hidden, so a download control inside
#' a `shinyWidgets::dropdownButton()` keeps an empty `href` and a `disabled`
#' class until the dropdown is opened. Downloading before then fails with no
#' server-side error, because the URL was never issued at all.
#'
#' @param app Running browser driver.
#' @param toggle Input ID of the dropdown button.
#' @param output Download output ID whose href must become live.
#' @return The driver, invisibly, once the download URL is available.
browser_open_dropdown <- function(app, toggle, output) {
  app$run_js(sprintf("document.getElementById('%s').click()", toggle))
  app$wait_for_js(
    sprintf(
      paste0(
        "(() => { const link = document.getElementById('%s');",
        " return link !== null && link.getAttribute('href') !== ''; })()"
      ),
      output
    ),
    timeout = 30000
  )
  app$wait_for_idle()
  invisible(app)
}

#' Download a browser result into the R session's temporary directory
#'
#' @param app Running browser driver.
#' @param output Download output ID.
#' @param extension File suffix, including the dot.
#' @param cleanup_environment Environment whose exit removes the download.
#' @return Absolute path to the downloaded file.
browser_download <- function(app, output, extension,
                             cleanup_environment = parent.frame()) {
  path <- tempfile(
    pattern = "browser-download-", fileext = extension
  )
  withr::defer(unlink(path), envir = cleanup_environment)
  app$get_download(output, filename = path)
  testthat::expect_gt(file.info(path)$size, 0)
  path
}

#' Read a plot's semantic SVG export
#'
#' @param app Running browser driver.
#' @param namespace Namespace of a plot module.
#' @return SVG markup from the production download handler.
browser_svg <- function(app, namespace) {
  browser_set(app, paste0(namespace, "-file_type"), "svg")
  path <- browser_download(app, paste0(namespace, "-confirm_download"), ".svg")
  svg <- paste(readLines(path, warn = FALSE), collapse = "\n")
  testthat::expect_match(svg, "<svg")
  svg
}

#' Count data points in a legend-free SVG scatterplot
#'
#' @param svg SVG markup exported with legend and labels disabled.
#' @return Number of circle geometries.
browser_point_count <- function(svg) {
  length(regmatches(svg, gregexpr("<circle\\b", svg))[[1]])
}

#' Compute an independent Pearson correlation reference
#'
#' Constant features have undefined correlations and are explicitly kept as NA.
#'
#' @param expression Cells-by-features numeric data frame from SCUBA.
#' @param feature Reference feature column name.
#' @return Named numeric vector of Pearson correlation coefficients.
browser_correlations <- function(expression, feature) {
  standard_deviations <- vapply(expression, stats::sd, numeric(1))
  result <- stats::setNames(rep(NA_real_, ncol(expression)), names(expression))
  varying <- standard_deviations > 0
  if (standard_deviations[[feature]] > 0) {
    result[varying] <- stats::cor(
      expression[[feature]], as.matrix(expression[, varying, drop = FALSE])
    )[1, ]
  }
  result
}

#' Load the fixture configuration through the overwrite confirmation
#'
#' @param app Running configuration app driver.
#' @return The driver, invisibly, once the loaded inputs settle.
browser_load_config <- function(app) {
  # Ensure the confirmation path is deterministic even if defaults change.
  browser_set(app, "dataset_label", "Unsaved configuration")
  browser_click(app, "load_config")
  app$wait_for_js("document.getElementById('load_confirm') !== null")
  browser_click(app, "load_confirm")
  invisible(app)
}

#' Assert browser DGE results against an independent scDE computation
#'
#' Preserves the old numeric regression intent without relying on column
#' positions or frozen sums from a different scDE version. The caller supplies
#' an independently selected object, never the browser's internal subset.
#'
#' @param app Running browser driver after submitting a DGE analysis.
#' @param object Independently constructed expected subset.
#' @param group_by Metadata column defining the expected comparison.
#' @param previous Previous result that must not be returned again.
#' @return The verified result table.
browser_expect_dge <- function(app, object, group_by, previous = NULL) {
  actual <- app$wait_for_value(
    export = "object_dge-dge_table", ignore = list(NULL, previous)
  )
  expected <- scDE::run_dge(
    object, group_by = group_by, seurat_assay = "RNA",
    positive_only = TRUE, lfc_format = "log2", remove_raw_pval = FALSE
  )
  testthat::expect_s3_class(actual, "data.frame")
  testthat::expect_gt(nrow(actual), 0)
  columns <- c(
    "group", "feature", "avgExpr", "log2FC", "auc",
    "pval", "pval_adj", "pct_in", "pct_out"
  )
  testthat::expect_true(all(columns %in% names(actual)))
  actual_order <- order(actual$group, actual$feature)
  expected_order <- order(expected$group, expected$feature)
  testthat::expect_equal(
    as.data.frame(actual[actual_order, columns]),
    as.data.frame(expected[expected_order, columns]),
    tolerance = 1e-6, ignore_attr = TRUE
  )
  testthat::expect_true(all(actual$log2FC > 0))
  app$wait_for_value(output = "object_dge-subset_stats-n_cells")
  testthat::expect_equal(
    as.integer(app$get_value(output = "object_dge-subset_stats-n_cells")),
    length(SCUBA::get_all_cells(object))
  )
  browser_plot(app, "object_dge-umap")
  actual
}

#' Click inside a rendered plot using real browser pointer events
#'
#' @param app Running browser driver.
#' @param output Fully namespaced plot output ID.
#' @param fraction Horizontal fraction of the plot image at which to click.
#' @return The driver, invisibly, after the pointer event is processed.
browser_click_plot <- function(app, output, fraction = 0.4) {
  selector <- jsonlite::toJSON(paste0("#", output, " img"), auto_unbox = TRUE)
  position <- app$get_js(sprintf(
    paste0(
      "(() => { const image = document.querySelector(%s);",
      " image.scrollIntoView({block: 'center'});",
      " const bounds = image.getBoundingClientRect();",
      " return {x: bounds.left + bounds.width * %f,",
      " y: bounds.top + bounds.height / 2}; })()"
    ),
    selector, fraction
  ))
  session <- app$get_chromote_session()
  session$Input$dispatchMouseEvent(
    type = "mousePressed", x = position$x, y = position$y,
    button = "left", clickCount = 1
  )
  session$Input$dispatchMouseEvent(
    type = "mouseReleased", x = position$x, y = position$y,
    button = "left", clickCount = 1
  )
  app$wait_for_idle()
  invisible(app)
}

#' Read the replicated pseudobulk browser fixture
#'
#' Sources the same file as the `cellDIVER_pseudobulk` fixture app.
#'
#' @return The Seurat object served by that app.
browser_pseudobulk_object <- function() {
  fixture_environment <- new.env(parent = globalenv())
  sys.source(
    testthat::test_path("apps", "cellDIVER_pseudobulk", "pseudobulk_fixture.R"),
    envir = fixture_environment
  )
  fixture_environment$pseudobulk_browser_object()
}

#' Assert the browser's edgeR result equals a direct scDE run
#'
#' @param app Running browser driver.
#' @param expected The scDE result the app's table must match.
#' @param previous Previous exported table to wait past.
#' @return The actual table exported by the app.
browser_expect_pseudobulk <- function(app, expected, previous = NULL) {
  actual <- app$wait_for_value(
    export = "object_dge-dge_table", ignore = list(NULL, previous)
  )
  testthat::expect_s3_class(actual, "data.frame")
  testthat::expect_gt(nrow(actual), 0)
  testthat::expect_identical(attr(actual, "dge_method"), "edger")
  columns <- intersect(
    c("contrast", "method", "group", "feature", "avgExpr", "log2FC", "pval", "pval_adj"),
    names(expected)
  )
  testthat::expect_true(all(columns %in% names(actual)))
  key <- function(x) paste(x$group, x$feature, if ("contrast" %in% names(x)) x$contrast else "")
  actual_order <- order(key(actual))
  expected_order <- order(key(expected))
  testthat::expect_equal(
    as.data.frame(actual[actual_order, columns]),
    as.data.frame(expected[expected_order, columns]),
    tolerance = 1e-6, ignore_attr = TRUE
  )
  actual
}
