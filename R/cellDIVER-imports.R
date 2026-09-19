#' Namespace imports
#'
#' `run_cellDIVER()` and `run_config_app()` call `library()` on their UI
#' dependencies so the running app can resolve them from the search path. That
#' covers the app but leaves the package namespace empty, so any internal helper
#' called outside a running app - from a test, from `cellDIVER:::fn()`, or from
#' another package - failed with `could not find function`.
#'
#' The directives below declare every function cellDIVER calls unqualified, so
#' the namespace resolves them on its own. They name individual functions rather
#' than whole packages: the set is derived from the calls that actually appear
#' in `R/`, which keeps masking between Seurat, rlang, shiny and the tidyverse
#' out of the namespace.
#'
#' @importFrom colourpicker colourInput
#' @importFrom dplyr "%>%" any_of arrange case_when desc group_by left_join n
#' @importFrom dplyr pull summarise
#' @importFrom DT datatable DTOutput formatSignif renderDT
#' @importFrom ggplot2 aes coord_cartesian coord_polar geom_bar geom_col
#' @importFrom ggplot2 geom_text geom_vline ggsave layer_scales position_stack
#' @importFrom ggplot2 scale_x_discrete theme_void
#' @importFrom ggsci pal_d3 pal_jco pal_lancet pal_locuszoom pal_rickandmorty
#' @importFrom ggsci pal_startrek
#' @importFrom glue glue
#' @importFrom patchwork plot_annotation plot_layout
#' @importFrom rintrojs introjsUI
#' @importFrom rlog log_error log_info log_warn
#' @importFrom shiny a actionButton actionLink addResourcePath checkboxInput
#' @importFrom shiny clickOpts conditionalPanel div downloadButton
#' @importFrom shiny downloadHandler downloadLink eventReactive
#' @importFrom shiny exportTestValues fileInput fluidPage fluidRow
#' @importFrom shiny freezeReactiveValue hoverOpts HTML icon imageOutput
#' @importFrom shiny includeCSS includeMarkdown includeScript insertUI
#' @importFrom shiny is.reactive isolate isTruthy mainPanel modalButton
#' @importFrom shiny modalDialog moduleServer navbarPage need NS observe
#' @importFrom shiny observeEvent onFlush onSessionEnded outputOptions
#' @importFrom shiny plotOutput reactive reactiveVal reactiveValues
#' @importFrom shiny removeModal removeUI renderImage renderPlot renderPrint
#' @importFrom shiny renderText renderUI req restoreInput selectInput
#' @importFrom shiny selectizeInput shinyApp showModal showNotification
#' @importFrom shiny sidebarLayout sidebarPanel sliderInput span tabPanel
#' @importFrom shiny tagList tags textAreaInput textInput textOutput uiOutput
#' @importFrom shiny updateCheckboxInput updateSelectInput
#' @importFrom shiny updateSelectizeInput updateSliderInput
#' @importFrom shiny updateTextAreaInput updateTextInput validate
#' @importFrom shiny verbatimTextOutput
#' @importFrom shinyBS bsTooltip
#' @importFrom shinyjs addClass click disable disabled enable extendShinyjs
#' @importFrom shinyjs hidden hideElement js showElement useShinyjs
#' @importFrom shinyWidgets awesomeCheckbox dropdownButton materialSwitch
#' @importFrom shinyWidgets multiInput pickerInput pickerOptions
#' @importFrom shinyWidgets radioGroupButtons updateAwesomeCheckbox
#' @importFrom shinyWidgets updateMaterialSwitch updateMultiInput
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom SingleCellExperiment altExp altExpNames altExps mainExpName
#' @importFrom sortable add_rank_list bucket_list rank_list
#' @importFrom stringr str_sort
#' @importFrom tibble add_row as_tibble tibble
#' @importFrom viridisLite cividis inferno mako plasma rocket viridis
#' @importFrom waiter spin_loaders useWaiter Waiter
#' @importFrom yaml read_yaml write_yaml
#'
#' @noRd
NULL

# Column names that dplyr and ggplot2 resolve against the data rather than the
# namespace. Declaring them keeps them from reading as undefined globals.
utils::globalVariables(c(
  "auc", "avgExpr", "count", "group", "log2FC", "pct_in", "pct_out",
  "pval_adj", "size"
))
