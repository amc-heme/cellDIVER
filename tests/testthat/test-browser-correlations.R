# No browser test for the correlations tab.
#
# TODO: a browser test is needed here once the correlations tab is reachable
# again. It was removed rather than fixed for three reasons:
#
#   1. The tab is commented out of the product. `run_cellDIVER()` does not
#      build `corr_tab_ui_dynamic` or `output$corr_dynamic_ui`
#      (R/run_cellDIVER.R, "3.1.3. Correlations tab UI"), so no user can reach
#      this module. The test drove a bespoke fixture app that mounted
#      `corr_tab_server()` by hand.
#
#   2. That fixture never completed a run. Submitting logs "Corr tab: Submit
#      button pressed" and "Displaying spinners", then stalls: the chain is
#      `submit -> is_subset() -> corr_table_content() -> corr_DT_content() ->
#      output$corr_table -> remove_spinners$trigger() -> show main_panel_ui`,
#      and `is_subset()` never emits under the fixture, so
#      "Corr tab: Begin correlation computations" is never reached.
#
#   3. There is also a latent suspension deadlock in the module itself, which
#      will bite whenever the tab is re-enabled: `main_panel_ui` is rendered
#      inside `shinyjs::hidden()` and is only revealed once `remove_spinners`
#      fires, but that trigger lives inside `corr_DT_content()`, which only
#      runs when `output$corr_table` renders - and Shiny suspends outputs whose
#      element is hidden. The table waits on the panel and the panel waits on
#      the table. `outputOptions(output, "corr_table", suspendWhenHidden =
#      FALSE)` breaks that cycle, but it is unverified while the tab is off.
#
# The fixture app is kept at tests/testthat/apps/correlations/ so the test can
# be restored without rebuilding it.
