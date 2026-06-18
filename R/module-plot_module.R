#' Plot Module
#'
#' Displays selections menus for an individual plot, builds the plot, and
#' displays it to the screen.
#'
#' This module has two UI components, both of which must be called with the
#' same ID for the module to display plots properly. The UI created with
#' \code{plot_module_ui(ui_component = "options")} will give a list of menus used
#' for selecting options that apply to the plot created by the module, and the
#' UI created by \code{plot_module_ui(ui_component = "plot")} will display the
#' plot itself.
#'
#' plot_module_ui
#'
#' The module UI has two components: "options", which builds the selection menus
#' for plot options, and "plot", which contains the output for the plot itself.
#'
#' The UI component needs to be called twice, once in the desired location for
#' the menus using ui_component="options", and again in the desired location for
#' the plot output with ui_component="plot". For the plot component, the only
#' arguments that need to be set are id and the ui_component; all other
#' arguments are used for the options tab.
#'
#' @param id ID to use for module elements. IDs for the options and plot UI
#' components, and the server component, must match.
#' @param plot_type An explicit definition of the plot type. In the near future,
#' the plot type defined here is envisioned to be used to determine the UI 
#' components present, rather than the large set of group_by, split_by, etc. 
#' parameters.
#' @param ui_component Determines which UI component is to be plotted. Use
#' "options" to create the input menus for plot options, and "plot" to create
#' the output container for the plot.
#' @param meta_choices A named vector generated in the main server function
#' giving the choices of metadata categories to split and group plots by.
#' @param plot_label A human readable name for the plot that will appear in the
#' menus. The name should make sense by itself (i.e. "Feature Plot" should be
#' entered instead of "Feature").
#' @param reductions A vector giving all the reductions used in the Seurat
#' object. Must be supplied if reductions_menu == TRUE
#' @param reductions_menu if TRUE, display a menu with reduction options to
#' choose for the plot
#' @param group_by If TRUE, display a menu with metadata categories to group by.
#' @param split_by If TRUE, display a menu with metadata categories to split by.
#' @param ncol_slider If TRUE, display a slider to specify the number of columns
#' to use for the plot. This only works for UMAP and violin plots.
#' @param order_checkbox if TRUE, display a checkbox to plot cells in order of
#' expression (for feature plots). This may be useful if a few cells with high
#' expression are being obscured by neighboring cells with low expression, but
#' it may over represent the percentage of cells expressing a feature.
#' @param label_checkbox If TRUE, display a checkbox for including labels on
#' the plot.
#' @param legend_checkbox If TRUE, display a checkbox for including a legend.
#' @param limits_checkbox If TRUE, display a checkbox for the use of original
#' axes. Limits when a subset is plotted. This only works for UMAP and
#' Feature plots.
#' @param custom_colors If TRUE, display UI for adding custom colors to
#' the current plot
#' @param manual_dimensions If TRUE, display an interface to specify manual
#' height and width parameters for the plot.
#' @param separate_features If TRUE, display a checkbox to enter separate
#' features for the plot. A text entry will be created if the checkbox
#' is selected.
#' @param download_button If TRUE, display a download button that will save a
#' .png image of the plot to disk.
#'
#' @noRd
plot_module_ui <- function(id,
                           plot_type,
                           # The plot_module UI consists of the options
                           # panels and the plot output, which exist in
                           # different places in the app. This argument will
                           # allow the module server to update components in
                           # different locations
                           # Plot UI component functions called here
                           #...
                           ui_component = c("options", "plot"),
                           meta_choices = NULL,
                           plot_label = "",
                           reductions = NULL,
                           patient_colname = NULL,
                           # UI Elements: Included when the arguments are TRUE
                           reductions_menu =                       FALSE,
                           scatterplot_ui =                        FALSE,
                           group_by =                              FALSE,
                           split_by =                              FALSE,
                           title_menu =                            FALSE,
                           legend_title_menu =                     FALSE,
                           ncol_slider =                           FALSE,
                           legend_options_menu =                   FALSE,
                           share_scale_checkbox =                  FALSE,
                           color_by_feature_checkbox =             FALSE,
                           super_title_menu =                      FALSE,
                           sort_groups_menu =                      FALSE,
                           dot_x_labels_menu =                     FALSE,
                           blend_checkbox =                        FALSE,
                           order_checkbox =                        FALSE,
                           label_checkbox =                        FALSE,
                           legend_checkbox =                       FALSE,
                           display_coeff =                         FALSE,
                           limits_checkbox =                       FALSE,
                           custom_colors =                         FALSE,
                           custom_x_axis_ui =                      FALSE,
                           manual_dimensions =                     FALSE,
                           separate_features =                     FALSE,
                           download_button =                       FALSE,
                           # Modifiers to elements above
                           # Default values for inputs
                           label_default =                         TRUE,
                           # Include "none" for group_by/split_by choices
                           group_by_include_none =                 FALSE,
                           split_by_include_none =                 TRUE,
                           # Alternate label for group_by/split_by menus
                           group_by_label =                        NULL,
                           split_by_label =                        NULL,
                           # Default values for group by and split by choices
                           group_by_default =                      NULL,
                           split_by_default =                      NULL,
                           # Remove column for patient/sample level metadata
                           # from group by choices
                           remove_patient_column =                 FALSE
                           ){
  # Namespace function: prevents conflicts with IDs defined in other modules
  ns <- NS(id)

  # Disable the "include legend" checkbox if legend_options_menu is TRUE
  if (legend_options_menu == TRUE){
    legend_checkbox <- FALSE
  }

  if (ui_component == "options"){
    # UI for plot options ####
    # Elements are added to tagList if specified when calling the module ui
    # Attempted to use ifelse() for this; ifelse() did not print Shiny tags
    # properly and was unable to process NULL
    tagList(
      # Add menus if their corresponding arguments are TRUE
      # Two-feature entry menu specific to scatterplot
      ## Scatterplot Features ####
      if (scatterplot_ui == TRUE){
        tagList(
          # First feature
          selectizeInput(
            inputId = ns("scatter_1"),
            label = "Feature for X-axis",
            choices = NULL,
            selected = NULL,
            # Add remove button to inputs
            options = list(
              'plugins' = list('remove_button'),
              # Do not allow user to input features not
              # in the list of options
              'create'= FALSE
              )
            ),

          # Second feature
          selectizeInput(
            inputId = ns("scatter_2"),
            label = "Feature for Y-axis",
            choices = NULL,
            selected = NULL,
            # Add remove button to inputs
            options = list(
              'plugins' = list('remove_button'),
              # Do not allow user to input features not
              # in the list of options
              'create'= FALSE
              )
            )
          )
      } else NULL,
      
      
      ## Group by menu ####
      if (group_by == TRUE){
        # Choices for group by selection: should exclude "none" unless
        # explicitly included using group_by_include_none == TRUE
        
        #html anchors pointing to relevant section in full documentation based on plot type
        plot_to_anchor <- 
          list(
            "DimPlot" = "DimPlots-group-by",
            "Dot Plot" = "dot-plots-group-by",
            "Scatterplot" = "scatterplots-group-by",
            "Ridge" = "ridge-plots-group-by",
            "Cell Proportion" = "cell-proportion-plot-proportion-metadata",
            "Violin Plot" = "violin-plots-group-by"
          )
        #assign anchor based on plot_label
        anchor <- plot_to_anchor[[plot_label]]
        group_by_choices <-
          if (group_by_include_none == FALSE){
            meta_choices()[!meta_choices() %in% "none"]
          } else {
            meta_choices()
          }

        # Remove the metadata category used for patient- or sample- level
        # metadata, if the plot employs patient/sample level metadata
        if (remove_patient_column == TRUE){
          group_by_choices <-
            group_by_choices[!group_by_choices %in% patient_colname()]
        }
       
        # If TRUE, add element
        selectInput(
          inputId = ns("group_by"), 
          label = tagList(
            if (!is.null(group_by_label)){
              group_by_label
            } else {
              "Metadata to Group By"
            },
            a(id = ns("group_by_info_icon"),
              icon("info-circle"),
              href=paste0("https://amc-heme.github.io/cellDIVER/articles/",
                          "full_documentation.html#", anchor),
              target="_blank")
            ),
          # Can select all options except "none"
          choices = group_by_choices,
          # First option selected by default, unless a default choice is
          # provided
          selected =
            if (!is.null(group_by_default)){
              group_by_default
            } else {
              group_by_choices[1]
            }
          )
        # Do not add element if FALSE
      } else NULL,
      bsTooltip(
        id = ns("plot_info_icon"), 
        title = "Select variable to group cells by.",
        placement = "top", 
        trigger = "hover",
        options = NULL
      ),

      ## Split by menu ####
      if (split_by == TRUE){
        #html anchors pointing to relevant section in full documentation based on plot type
        plot_to_anchor <-
          list(
            "Scatterplot" = "scatterplots-group-by",
            "DimPlot" = "DimPlots-split-by",
            "Feature Plot" = "Feature-plots-split-by",
            "Cell Proportion" = "cell-proportion-plot-comparison-metadata",
            "Violin Plot" = "violin-plots-split-by"
          )
        #assign anchor based on plot_label
        anchor <- plot_to_anchor[[plot_label]]
        # Define choices for split by menu
        # Exclude "none" if split_by_include_none == FALSE
        # Default behavior is not to exclude "none"
        split_by_choices <-
          if (split_by_include_none == FALSE){
            meta_choices()[!meta_choices() %in% "none"]
          } else {
            meta_choices()
          }

        selectInput(
          inputId = ns("split_by"), 
          label = tagList(
            if (!is.null(split_by_label)){
              split_by_label
            } else {
              "Metadata to Split By"
            },
            a(id = ns("split_by_info_icon"),
              icon("info-circle"),
              href=paste0("https://amc-heme.github.io/cellDIVER/articles/",
                          "full_documentation.html#", anchor),
              target="_blank")
          ),
          # Use vector of included metadata category names from the config file
          choices = split_by_choices,
          # Default value: use split by default if provided
          selected =
            if (!is.null(split_by_default)){
              split_by_default
            } else {
              # If not, use "none" unless it is excluded
              if (split_by_include_none == TRUE){
                "none"
              } else {
                split_by_choices[1]
              }
            }
         )
      } else NULL,
      
      bsTooltip(
        id = ns("split_by_info_icon"), 
        title = "Select variable to split cells into separate plots.",
        placement = "bottom", 
        trigger = "hover",
        options = NULL
      ),

      ## Reductions menu ####
      if (reductions_menu == TRUE){
        if (is.null(reductions)){
          # Error handling: return error if reductions is not specified when
          # reductions_menu is TRUE
          stop(
            "If reductions_menu is TRUE, the argument `reductions`
            must have a value."
            )
        } else {
          if(!is.reactive(reductions)){
            # Also, return an error if `reductions` is not a reactive variable
            stop("`reductions` must be a reactive variable.")
          }
        }

        selectInput(
          inputId = ns("reduction"),
          label = "Choose Projection",
          choices = reductions()
          )
      } else NULL,

      ## Title selection and custom title ####
      # User may select default title behavior, a custom title (if available)
      # or none.
      # Available options are updated server-side
      if (title_menu == TRUE){
        #html anchors pointing to relevant section in full documentation based on plot type
        plot_to_anchor <-
        list(
          "DimPlot" = "DimPlots-title-options",
          "Feature Plot" = "Feature-plots-title-options",
          "Cell Proportion" = "cell-proportion-plot-title-options",
          "Ridge" = "ridge-title-options"
        )
        #assign anchor based on plot_label
        anchor <- plot_to_anchor[[plot_label]]
        tagList(
          selectInput(
            inputId = ns("title_settings"),
            label = tagList(
              "Title options",
              a(id = ns("title_settings_info_icon"),
                icon("info-circle"),
                href=paste0("https://amc-heme.github.io/cellDIVER/articles/",
                            "full_documentation.html#", anchor),
                target="_blank")
            ),
              
            # A third choice, "custom", is added server-side if it is 
            # valid based on the plot type
            choices =
              c("Default" = "default",
                "None" = "none"),
            selected = "default"
          ),
          bsTooltip(
            id = ns("title_settings_info_icon"), 
            title = 
              paste0(
                'Select "custom" to set a custom title, ',
                'and "None" to remove the title.'
                ),
            placement = "top", 
            trigger = "hover",
            options = NULL
          ),
          # Input for a single custom title
          # UI is hidden with shinyjs and made visible when "custom" is selected
          hidden(
            div(
              id = ns("custom_title_div"),
              class = "compact-options-container",
              tags$b("Enter Custom Title"),
              textInput(
                inputId = ns("custom_title"),
                label = NULL,
                value = ""
                ),
              div(
                id = ns("buttons"),
                actionButton(
                  inputId = ns("custom_title_reset"),
                  label = "Reset",
                  icon = icon("redo-alt"),
                  class = "button-ghost compact-button"
                ),
                actionButton(
                  inputId = ns("custom_title_update"),
                  label = "Update",
                  icon = icon("sync"),
                  class = "button-primary compact-button"
                )
              )
            )
          ),
          # UI for input of multiple custom titles (applies when a feature
          # plot is created with multiple split by groups)
          hidden(
            div(
              id = ns("custom_title_multi_div"),
              multi_text_input_ui(
                id = ns("custom_title_multi"),
                label = "Enter Custom Titles for Each Panel:"
                )
              )
            )
          )
      },

      ## Legend Title (feature plots) ####
      if (legend_title_menu == TRUE){
        div(
          style = "margin-top: 10px;",
          id = ns("legend_title_div"),
          selectInput(
            inputId = ns("legend_title"),
            label = "Display Options for Legend Title",
            choices =
              c("Feature Name" = "feature",
                "Expression" = "assay_score",
                "No Title" = "none"),
            selected = "default"
            )
          )
        } else NULL,

      ## Group Sorting interface (dot, violin, ridge, proportion plots) ####
      if (sort_groups_menu == TRUE){
        # html anchors pointing to relevant section in full documentation based on plot type
        plot_to_anchor <- 
        list(
          "Violin Plot" = "violin-plots-group-order",
          "Dot Plot" = "dot-plots-group-order",
          "Cell Proportion" = "cell-proportion-plot-group-order"
        )
        #assign anchor based on plot_label
        anchor <- plot_to_anchor[[plot_label]]
        div(
          id = ns("sort_groups_menu"),
          selectInput(
            inputId = ns("sort_groups"),
            label = tagList(
              "Order of Groups on plot",
              a(id = ns("sort_groups_info_icon"),
                icon("info-circle"),
                href=paste0("https://amc-heme.github.io/cellDIVER/articles/",
                            "full_documentation.html#", anchor),
                target="_blank")
            ),
            # Can select all options except "none"
            choices = 
              if (plot_type == "proportion"){
                c("Ascending" = "ascending",
                  "Descending" = "descending",
                  "Proportion" = "proportion",
                  "Custom" = "custom")
              } else {
                c("Ascending" = "ascending",
                  "Descending" = "descending",
                  "Feature Expression" = "expression",
                  "Custom" = "custom")
              },
            ),
          # Menus for sorting groups depending on the options chosen above #
          # Custom refactoring sortable input (for "custom")
          hidden(
            uiOutput(
              outputId = ns("refactor_sortable")
              )
            ),
          # Feature expression sorting (for "expression")
          hidden(
            uiOutput(
              outputId = ns("expr_sort_menu")
              )
            ),
          # Proportion sorting (for "proportion")
          hidden(
            uiOutput(
              outputId = ns("prop_sort_menu")
              )
            )
          )
        } else NULL,

      bsTooltip(
        id = ns("sort_groups_info_icon"), 
        title = 
          paste0(
            'Use "Ascending" or "Descending" to sort by group name in ',
            'ascending or descending alphanumeric order, respectively. ',
            # Tooltip text depends on plot type
            if (plot_type == "proportion"){
              paste0(
                'Use "proportion" to sort proportion plots by the proportion ',
                'in a chosen group, and "custom" to define a custom order.'
              )
            } else if (plot_type %in% c("violin", "ridge", "dot")){
              paste0(
                # Violin, ridge, dot plots: expression sorting
                'Use "Feature Expression" to sort by average expression in ',
                'each group, and "custom" to define a custom order.'
              )
            } else {
              'Use "custom" to define a custom order.'
            }
            ),
        placement = "top", 
        trigger = "hover",
        options = NULL
      ),

      ## Dot plots: rename features on x-axis labels ####
      if (dot_x_labels_menu == TRUE){
        tagList(
          selectInput(
            inputId = ns("dot_x_labels"),
            label = "Appearance of Features on x-axis:",
            choices =
              c(
                "Truncated" = "truncated",
                "Full" = "full",
                "Custom" = "custom"
                )
            ),
          hidden(
            div(
              id = ns("dot_x_labels_div"),
              multi_text_input_ui(
                id = ns("rename_dot_x_labels"),
                label = "Choose new Display Names for Feature Labels:"
                )
              )
            )
          )
      } else NULL,

      ## Number of columns ####
      if (ncol_slider == TRUE){
        #Dynamic UI (appears when split_by != "none")
        uiOutput(outputId = ns("ncol_slider"))
      } else NULL,

      ## Legend options menu ####
      if (legend_options_menu == TRUE){
        #html anchors pointing to relevant section in full documentation based on plot type
        plot_to_anchor <-
        list(
          "DimPlot" = "choose-number-of-columns",
          "Feature Plot" = "feature-choose-ncol", 
          "Violin Plot" = "violin-choose-ncol"
        )
        #assign anchor based on plot_label
        anchor <- plot_to_anchor[[plot_label]]
        
        collapsible_panel(
          inputId = ns("legend_options_panel"),
          label = "Legend Options",
          active = TRUE,
          class = "collapsible-panel-secondary",
          checkboxInput(
            inputId = ns("legend"),
            label = "Include Legend",
            value = TRUE
          ),
          # Number of columns in legend
          tags$b("Number of Columns in Legend"),
          # If this check box is enabled, the default is used
          div(
            id = ns("default_legend_ncol_div"),
            checkboxInput(
              inputId = ns("default_legend_ncol"),
              label = 
                tagList(
                  "Use default",
                  a(
                    id = ns("default_legend_ncol_info_icon"),
                    icon("info-circle"),
                    href =
                      paste0("https://amc-heme.github.io/cellDIVER/articles/",
                             "full_documentation.html#", anchor
                             ),
                    target = "_blank"
                    )
                ),
              value = TRUE
            )
          ),
          bsTooltip(
            id = ns("default_legend_ncol_info_icon"), 
            title = "Uncheck this to set the number of columns in the legend.", 
            placement = "top", 
            trigger = "hover",
            options = NULL
          ),
          # Slider shows when *not* using the default
          hidden(
            sliderInput(
              inputId = ns("legend_ncol"),
              label = NULL,
              ticks = FALSE,
              value = 1,
              min = 1,
              max = 5
            )
          ),
          sliderInput(
            inputId = ns("legend_size"),
            label = "Adjust Size of Legend:",
            ticks = FALSE,
            value = 6,
            min = 1,
            max = 9,
            step = 1
          )
        )
      } else NULL,

      ## Display a title above multi-panel plots ####
      # (feature plots only)
      if (super_title_menu == TRUE){
        hidden(
          checkboxInput(
            inputId = ns("super_title"),
            label = "Display Feature Name Above Panels",
            value = TRUE
            )
          )
        } else NULL,

      ## Share Scales Checkbox ####
      # (feature plots with multiple features and no split.by groups)
      if (share_scale_checkbox == TRUE){
        hidden(
          checkboxInput(
            inputId = ns("share_scale"),
            label = "Share Scale Between Features",
            # Value is FALSE by default
            value = FALSE
            )
        )
      } else NULL,

      ## Color by Feature Checkbox ####
      if (color_by_feature_checkbox == TRUE){
        hidden(
          checkboxInput(
            inputId = ns("color_by_feature"),
            label = "Use Separate Colors for Features",
            # Value is FALSE by default
            value = FALSE
          )
        )
      } else NULL,

      ## Blend features (feature plots only, with two features selected) ####
      if (blend_checkbox == TRUE){
        hidden(
          tagList(
            # Checkbox is shown when exactly two features are selected
            checkboxInput(
              inputId = ns("blend"),
              label = "View Feature Co-Expression",
              value = FALSE
            ),
            # Container for blend options
            div(
              id = ns("blend_options_container"),
              # Layout of blended plot
              selectInput(
                inputId = ns("blend_layout"),
                label = "Choose Layout for Panels",
                choices = c("Default" = "2col", "Wide" = "4col"),
                selected = "2col"
                ),

              # Blend Palette
              pickerInput(
                inputId = ns("blend_palette"),
                label = "Choose Palette for Co-Expression:",
                # choices are populated server-side
                choices = NULL,
                selected = NULL
                ),


              # selectInput(
              #   inputId = ns("blend_palette"),
              #   label = "Choose palette for co-expression:",
              #   choices =
              #     list(
              #       "RdBu" = c("lightgrey", "#FF0000", "#0000FF"),
              #       "GnBu" = c("lightgrey", "#00FF00", "#0000FF"),
              #       "RdGn" = c("lightgrey", "#FF0000", "#0000FF"),
              #       # Blue and orange (co-expression is pink)
              #       "BuOr" = c("lightgrey", "#1003FF", "#F76A0D"),
              #       # Red-green palette, with co-expression visible at a lower
              #       # threshold for both features
              #       "RdGn_accent" = c("lightgrey", "#FF1A1A", "#1AFF1A"),
              #       # Dark purple and dark yellow make peach when blended
              #       "DkPuDkYl" = c("lightgrey", "#55035C", "#CFAB19"),
              #       # Dark putple + dark red-orange -> pink blend
              #       "DkPuDkRd" = c("lightgrey", "#7A2180", "#9E2525"),
              #       # Custom: uses custom color inputs
              #       "Custom" = "custom"
              #       ),
              #   selected = "RdBu"
              # ),

              # Custom blend colors
              div(
                id = ns("blend_palette_custom_colors"),
                tags$b("Choose Custom Palette for Co-Expression"),
                colourInput(
                  inputId = ns("blend_custom_low"),
                  label = "Low expression color",
                  value = "#E1E1E1"
                ),
                colourInput(
                  inputId = ns("blend_custom_1"),
                  label = "First feature color",
                  value = "#FF0000"
                ),
                colourInput(
                  inputId = ns("blend_custom_2"),
                  label = "Second feature color",
                  value = "#0000FF"
                )
              )#,
              # Blend Resolution (dev mode)
              # sliderInput(
              #   inputId = ns("blend_resolution"),
              #   label = "Blend resolution",
              #   min = 0.0,
              #   max = 1.0,
              #   value = 0.5,
              #   step = 0.01,
              #   ticks = FALSE
              #   )
              )
            )
          )
        } else NULL,

      ## Order cells by expression (feature plots only) ####
      div(
        id = ns("order_div"),
        if (order_checkbox == TRUE){
          checkboxInput(
            inputId = ns("order"),
            label = "Order Cells by Expression",
            # Order should be FALSE by default
            value = FALSE
          )
        } else NULL
      ),
      bsTooltip(
        id = ns("order_div"), 
        title = "Plot cells with higher expression first (this may distort results).", 
        placement = "top", 
        trigger = "hover",
        options = NULL
      ),
        
      ## Add/remove labels ####
      div( 
        id = ns("label_tooltip"),
        if (label_checkbox == TRUE){
          checkboxInput(
            inputId = ns("label"),
            label = "Label Groups",
            # Default value is TRUE
            value = label_default
          )
        } else NULL
      ),
      
      bsTooltip(
        id = ns("label_tooltip"),
        title =
          paste0("Label cells by metadata on plot. If checked, a menu will ",
                 "appear to select a variable for grouping cells. Labels will ",
                 "display in the center of each group in the chosen metadata ",
                 "variable."),
        placement = "top",
        trigger = "hover",
        options = NULL
      ),
      
      ## Add or remove Legend ####
      if (legend_checkbox == TRUE){
        checkboxInput(
          inputId = ns("legend"),
          label = "Include Legend",
          value = TRUE
          )
      } else NULL,

      ## Checkbox to remove title from plot ####
      div(
        id = ns("display_coeff_div"),
        if (display_coeff == TRUE){
          checkboxInput(
            inputId = ns("display_coeff"),
            label = "Show Pearson Coefficient",
            value = TRUE
            )
        } else NULL
      ),
      bsTooltip(
        id = ns("display_coeff_div"), 
        title = "Display correlation coefficient as the plot title.", 
        placement = "top", 
        trigger = "hover",
        options = NULL
      ),
      
      ## Checkbox to specify original axes limits ####
      if (limits_checkbox == TRUE){
        # Dynamic UI: displays when a subset is selected
        uiOutput(outputId = ns("limits_checkbox"))
      } else NULL,

      ## Checkbox for custom colors on Feature plot ####
      if (custom_colors == TRUE){
        tagList(
          # Checkbox to select custom colors
          checkboxInput(
            inputId = ns("custom_colors"),
            label = "Use Custom Colors",
            # FALSE by default
            value = FALSE
          ),
          conditionalPanel(
            condition = glue("input['{ns('custom_colors')}'] == true"),
            tagList(
              div(
                class = "two-color-input-left",
                colourInput(
                  inputId = ns("min_color"),
                  label = "Low Expression",
                  # palette = "limited",
                  # allowedCols = color_choices,
                  value = "#E1E1E1"
                  )
                ),
              div(
                class = "two-color-input-right",
                colourInput(
                  inputId = ns("max_color"),
                  label = "High Expression",
                  # palette = "limited",
                  # allowedCols = color_choices,
                  value = "#000000"
                  )
                )
              )
            )
          )
        } else NULL,

      ## Custom x-axis limits ####
      # Currently for ridge plots only
      if (custom_x_axis_ui == TRUE){
        hidden(
          div(
            id = ns("custom_xlim_checkbox"),
            checkboxInput(
              inputId = ns("use_custom_xlim"),
              label = "Define Custom X-axis Limits",
              value = FALSE
              )
             ),
          div(
            id = ns("custom_xlim_interface"),
            custom_xlim_ui(
              id = ns("custom_xlim"),
              compact = TRUE
              )
            )
          )
      } else NULL,

      ## UI for user control of plot dimensions ####
      if (manual_dimensions == TRUE){
        #If TRUE, call module for manual adjustment of plot dimensions
        manual_dimensions_ui(id = ns("manual_dim"))
      } else NULL,

      ## Separate Features UI ####
      # UI to request use of separate features for plot
      if (separate_features == TRUE){
        # Store namespaced ID of use separate features check box
        # for facilitated entry in condionalPanel element
        sep_id <- ns("use_separate_features")

        tagList(
          # Checkbox to use separate features
          checkboxInput(
            inputId = sep_id,
            label = glue("Use separate features for {tolower(plot_label)}"),
            value = FALSE
            ),

          # Use conditional panel (rendering UI will reset the text entry)
          conditionalPanel(
            condition = glue("input['{sep_id}'] == true"),
            # Text Entry Element
            tagList(
              # Label
              tags$p(
                HTML(
                  glue("<strong> Enter features <br>
                       (specific to {tolower(plot_label)}): </strong>")
                  )
                ),

              # Selectize input for separate features
              div(
                style =
                  "vertical-align: top; margin-bottom: 0px;",
                selectizeInput(
                  inputId = ns("separate_features"),
                  multiple = TRUE,
                  label = NULL,
                  choices = NULL,
                  selected = NULL,
                  # Add remove button to inputs
                  options =
                    list(
                      'plugins' = list('remove_button'),
                      'create' = FALSE
                    )
                  )
                )
              )
            )
          )
      } else NULL,

      ## UI for download button ####
      if (download_button == TRUE){
        div(
          class = "plot-download-button",
          dropdownButton(
            inputId = ns("download"),
            status = "info",
            up = TRUE,
            label = "",
            size = "sm",
            icon = icon("download"),
            # Dropdown content
            div(
              selectInput(
                inputId = ns("file_type"),
                label = "Select File Type",
                choices = c(".png" = "png", ".svg" = "svg"),
                selected = "png"
              ),
              downloadButton(
                outputId = ns("confirm_download"),
                label = "Download",
                class = "confirm-download",
                icon = NULL
              )
            )
          )
        )
      } else NULL,

      # Add additional UI components requested by the user
      #...
    )

  } else if (ui_component == "plot"){
    # UI for plot output
    # Conditional UI: displays if the "make *" switch for the plot is turned on
    # in the plots tab
    uiOutput(outputId = ns("plot_output_ui"))
  }
}

#' plot_module_server
#'
#' Server function to run alongside the corresponding UI function
#'
#' @param id The module ID used for namespacing. This should match the ID used
#' in the UI components for the plot module.
#' @param object The Seurat object to be used for plotting. It may be a subset
#' or the full object.
#' @param plot_switch Switch in the plots tab specifying whether the user wishes
#' to see the plot created in this server function
#' @param plot_label The name for this plot type. Will display as entered
#' in the app
#' @param n_cells_original The number of cells in the original object. This is
#' set in the main server function each time a new object is loaded.
#' @param raw_feature_names This is set in the plots_tab server. If TRUE, the 
#' display names of the features being plotted will be un-modified, and will 
#' contain the "key" of the modality of  each feature (i.e. "rna_HOXA10"). If 
#' FALSE (the default), a "human-readable" display name will be applied to each 
#' feature, which will remove the modality key, and will add a "suffix" defined 
#' using the "label" property for the modality (known as the assay) in the 
#' config file. For example, for a feature from an modality with "(surface 
#' protein)" as the label and "adt" as the modality key, the display name will 
#' be "CD4 (Surface Protein)". 
#' @param features_entered The features entered in the plots tab of the app.
#' This is currently defined in the plots tab module and passed to this module.
#' @param manual_dimensions Creates a server instance for specifying manual
#' dimensions if TRUE. This should be set to TRUE if manual_dimensions is also
#' TRUE in the UI
#' @param plot_type The type of plot to create from the selected options.
#' @param valid_features A list of the features that may be selected. Used for
#' plot types based on features.
#' @param lim_orig A list with the original x- and y- axes limits for each
#' reuction enabled for the current object. The list is generated at app
#' startup.
#' @param palette A palette of colors currently selected by the user, to be used
#' on the plot.
#' @param metadata_config The metadata section of the config file loaded in the
#' main server function, at startup and upon changing the object.
#' @param assay_config The assay section of the config file loaded in the main
#' server function, at startup and upon changing the object.
#' @param patient_colname The metadata column to use for computing patient- or
#' sample level metadata for pie charts.
#' @param separate_features_server A boolean giving whether server code to
#' process separate features (features specific to the plot created by this
#' module) should be ran. This should be TRUE for all plots where the user can
#' enter features that apply just to that plot.
#' @param blend_palettes special palettes used for blended feature plots (
#' this is the full list of palettes).
#' @param current_tab The id of the navbarPage in the main app. This is used to 
#' determine if the plots tab is active and get reactive expressions to respond 
#' to switching to the tab. Currently, this is only used in scatterplots.
#'
#' @noRd
plot_module_server <- function(id,
                               plot_type,
                               plot_label,
                               object,
                               plot_switch,
                               n_cells_original,
                               raw_feature_names,
                               plots_tab_spinner = NULL,
                               features_entered = NULL,
                               manual_dimensions = TRUE,
                               valid_features = NULL,
                               lim_orig = lim_orig,
                               palette = NULL,
                               metadata_config = NULL,
                               assay_config = NULL,
                               patient_colname = NULL,
                               separate_features_server = FALSE,
                               blend_palettes = NULL,
                               current_tab = NULL
                               ){
  moduleServer(id,
               function(input,output,session){
                 # Server namespace function: for dynamic UI
                 ns <- session$ns

                 # Reactive trigger to restore scroll position of plots tab
                 # when the plots tab interface is hidden and shown again
                 scroll_restore <- cellDIVER:::makeReactiveTrigger()

                 # Return error notification if the plot type is not in the list
                 # of supported types
                 if (!plot_type %in%
                     c("dimplot",
                       "feature",
                       "violin",
                       "dot",
                       "scatter",
                       "ridge",
                       "proportion",
                       "pie"
                       )
                 ){
                   showNotification(
                     ui =
                       icon_notification_ui(
                         icon = "skull-crossbones",
                         # Change to feature when other
                         # features are supported
                         glue("Development error: plot module {id} has an
                              unrecognized value for argument `plot_type`.
                              Please contact us about this issue.")
                       ),
                     #Show notification for 5 seconds
                     session = session
                   )
                 }

                 # 1. Manual Dimensions Module Server --------------------------
                 if (manual_dimensions == TRUE){
                   manual_dim <- manual_dimensions_server(id = "manual_dim")
                   }

                 # 2. Fill Feature Selection (Scatterplots Only) ---------------
                 if (plot_type == "scatter"){
                   if (!is.reactive(valid_features)){
                     warning("For scatterplots, valid_features must be specified as a reactive to plot_module_server.")
                   }

                   # Originally done on initialization of the module, but this
                   # caused issues when the "info" tab was added.
                   # The update code is now placed within an observer, but
                   # fixing the way modules for each tab are initialzed when 
                   # switching objects would be a better solution
                   
                   # Upon module initialization, set up feature inputs
                   # for the scatterplot
                   # Use for loop to avoid duplication of code
                   # for (input_id in c("scatter_1", "scatter_2")){
                   #   # Update selectize inputs for scatter_1 and scatter_2
                   #   updateSelectizeInput(
                   #     session,
                   #     # Do not namespace IDs in update* functions
                   #     inputId = input_id,
                   #     # Use list of valid features generated at startup
                   #     choices = valid_features(),
                   #     # Select none by default
                   #     selected = character(0),
                   #     server = TRUE
                   #     )
                   #   }
                   
                   observe({
                     req(valid_features())
                     if (!is.null(current_tab)){
                       req(current_tab())
                     }
                     
                     # Update only when in the plots tab
                     if (current_tab() == "plots"){
                       for (input_id in c("scatter_1", "scatter_2")){
                         # Update selectize inputs for scatter_1 and scatter_2
                         updateSelectizeInput(
                           session,
                           inputId = input_id,
                           # Use list of valid features generated at startup
                           choices = valid_features(),
                           # Select none by default
                           selected = character(0),
                           server = TRUE
                           )
                         }
                       }
                     })
                   }

                 # 3. Title Settings Menu --------------------------------------
                 # Title settings and custom titles are enabled for the plot
                 # types below
                 if (plot_type %in%
                     c("dimplot", "feature", "ridge", "proportion", "pie")){
                   # Reactive trigger for updating single custom title input
                   update_title_single <- makeReactiveTrigger()

                   ## 3.1. Allow/disallow custom title ####
                   # If the menu is present, add the "custom" option
                   # if it applies

                   # menu_type: stores whether a single- or multiple- entry
                   # menu may be used
                   menu_type = reactiveVal(NULL)

                   observe({
                     req(plot_switch())

                     # Adds reactive dependency on input$title_settings
                     if (isTruthy(input$title_settings)){
                       # Variable below is set to TRUE when custom titles
                       # are possible
                       enable_custom = FALSE
                       menu_type("none")

                       # Conditional tree to determine if
                       # custom titles are possible
                       if (plot_type %in% c(
                         "dimplot", "proportion", "pie")){
                         # DimPlots: custom titles are always possible
                         # Proportion stacked bar plots use same behavior
                         # as DimPlots
                         enable_custom = TRUE
                         # Plot uses a single custom title menu
                         menu_type("single")
                       } else if (plot_type == "feature"){
                         # Feature plots: custom titles can be used when one
                         # feature is entered, or when multiple features have
                         # been entered without a split_by selection
                         if (length(features_entered()) == 1){
                           enable_custom = TRUE
                           # Single- or multiple- entry menu: depends
                           # on selected split_by setting
                           if (plot_selections$split_by() == "none"){
                             menu_type("single")
                           } else {
                             # Multiple title entry menu is used when a split_by
                             # setting is specified
                             menu_type("multiple")
                           }
                         } else if (length(features_entered()) > 1){
                           # Multi-feature plots: custom titles are currently
                           # disallowed for blended feature plots
                           if (plot_selections$split_by() == "none" &
                               !isTruthy(plot_selections$blend())){
                             enable_custom = TRUE
                             menu_type("multiple")
                           }
                         }
                       } else if (plot_type == "ridge"){
                         # For ridge type, custom titles depend only on the
                         # number of features.
                         if (length(features_entered()) == 1){
                           enable_custom = TRUE
                           menu_type("single")
                         } else if (length(features_entered()) > 1){
                           enable_custom = TRUE
                           menu_type("multiple")
                         }
                       }

                       # Update input based on conditionals above
                       updateSelectInput(
                         session,
                         inputId = "title_settings",
                         choices =
                           if (enable_custom == TRUE){
                             c("Default" = "default",
                               "Custom" = "custom",
                               "None" = "none")
                           } else {
                             c("Default" = "default",
                               "None" = "none")
                           },
                         selected =
                           # Preserve selection as long as it is still valid
                           # ("custom" is the only choice that is potentially
                           # invalid)
                           if (
                             (enable_custom == TRUE) |
                             (enable_custom == FALSE &
                              input$title_settings != "custom")
                           )
                             input$title_settings
                       )
                     }
                   })

                   ## 3.2. Show/hide custom title input ####
                   # Depends on state of input$title_settings and the current
                   # group_by category (for DimPlots)
                   observe({
                     req(plot_switch())

                     if (isTruthy(input$title_settings)){
                       if (input$title_settings == "custom" &
                           menu_type() == "single"){
                         showElement(
                           id = "custom_title_div",
                           anim = TRUE
                          )
                       } else {
                         hideElement(
                           id = "custom_title_div",
                           anim = TRUE
                         )
                       }
                     }
                   })

                   observe({
                     req(plot_switch())

                     if (isTruthy(input$title_settings)){
                       if (input$title_settings == "custom" &
                           menu_type() == "multiple"){
                         showElement(
                           id = "custom_title_multi_div",
                           anim = TRUE
                           )
                       } else {
                         hideElement(
                           id = "custom_title_multi_div",
                           anim = TRUE
                           )
                       }
                     }
                   })

                   ## 3.3. Set default custom title input ####
                   # Triggers when the custom title UI is shown, and when
                   # selections that influence the default title change
                   observeEvent(
                     # EventExpr depends on plot type
                     if (plot_type %in% c("dimplot", "proportion", "pie")){
                       c(input$title_settings,
                         object(),
                         plot_selections$group_by(),
                         plot_switch()
                         )
                     } else if (plot_type %in% c("feature", "ridge")){
                       c(input$title_settings,
                         object(),
                         features_entered(),
                         plot_switch()
                       )
                     },
                     label = glue("{id}: 3.3 Set custom title menu"),
                     {
                       # Only runs when the plot is enabled
                       req(plot_switch())

                       if (isTruthy(input$title_settings)){
                         if (input$title_settings == "custom"){
                           # Compute initial value for custom title input
                           initial_value <-
                             initial_title(
                               # initial_title processes the plot type and
                               # currently accepts "dimplot", "feature",
                               # or "proportion".
                               plot_type = plot_type,
                               group_by = plot_selections$group_by(),
                               metadata_config = metadata_config(),
                               features_entered =
                                 if (plot_type %in% c("feature", "ridge")){
                                   features_entered()
                                 } else NULL,
                               assay_config = assay_config(),
                               show_modality_key = 
                                 # "Raw" feature names setting from plots tab
                                 # Apply value of checkbox if provided to the 
                                 # module. If not provided, this is always 
                                 # FALSE.
                                 if (!is.null(raw_feature_names)){
                                   raw_feature_names()
                                 } else FALSE
                             )

                           # Update input with initial value
                           updateTextInput(
                             session,
                             inputId = "custom_title",
                             value = initial_value
                           )

                           # Keeps downstream reactives from processing until
                           # updateTextInput has completed (update occurrs
                           # after all reactives have executed)
                           freezeReactiveValue(input, "custom_title")

                           # Trigger processing of custom title input (3.5.)
                           #update_title_single$trigger()
                           # Programmatically click update button
                           # to process value
                           shinyjs::click("custom_title_update")

                           # updateSearchInput(
                           #   session,
                           #   inputId = "custom_title",
                           #   value = initial_value,
                           #   trigger = TRUE
                           # )
                         }
                       }
                     })

                   # observe({
                   #   print("input$custom_title has changed. New value:")
                   #   print(input$custom_title)
                   # })

                   ## 3.4. Manual reset of custom title input ####
                   observeEvent(
                     input$custom_title_reset,
                     label = glue("{id}: 3.4. Custom Title Reset"),
                     {
                       # Determine default value to reset to
                       initial_value <-
                         initial_title(
                           plot_type = plot_type,
                           group_by = plot_selections$group_by(),
                           metadata_config = metadata_config(),
                           features_entered =
                             if (plot_type %in% c("feature", "ridge")){
                               features_entered()
                               } else NULL,
                           assay_config = assay_config(),
                           show_modality_key = 
                             # "Raw" feature names setting from plots tab
                             raw_feature_names()
                         )

                       # Update input
                       updateTextInput(
                         session,
                         inputId = "custom_title",
                         value = initial_value
                         )

                       # Programmatically click update button to process value
                       shinyjs::click("custom_title_update")

                       # updateSearchInput(
                       #   session,
                       #   inputId = "custom_title",
                       #   value = initial_value,
                       #   trigger = TRUE
                       # )
                     })

                   ## 3.5 Process Input of Single Custom Title ####
                   # Input is updated when the "update" button is pressed.
                   custom_title_single <-
                     eventReactive(
                       # Respond to programmatic trigger
                       c(input$custom_title_update,
                         update_title_single$depend()),

                         # Make title available before the update button
                         # is pressed, and update with the default value
                         # in response to changes in the group_by category
                         # or the object)
                         #object(),
                         #plot_selections$group_by()),
                       label =
                         glue("{plot_label}: process input of single custom title"),
                       ignoreNULL = FALSE,
                       ignoreInit = TRUE,
                       {
                         #print("3.5. Update custom_title_single eventReactive")

                         # Pass value of custom_title if defined, otherwise pass
                         # the default value
                         default_title <-
                            initial_title(
                              plot_type = plot_type,
                              group_by = plot_selections$group_by(),
                              metadata_config = metadata_config(),
                              features_entered = features_entered(),
                              assay_config = assay_config(),
                              show_modality_key = 
                                # "Raw" feature names setting from plots tab
                                raw_feature_names()
                              )

                         if (isTruthy(input$custom_title)){
                           return(input$custom_title)
                         } else {
                           return(default_title)
                         }

                         # ALTERNATE
                         # Require input$custom_title to be defined
                         #req(input$custom_title)
                         #input$custom_title
                       })

                   # observe({
                   #   print("Value of custom_title_single:")
                   #   print(custom_title_single())
                   # })

                   ## 3.6. Server for multiple custom title input ####
                   if (plot_type %in% c("ridge", "feature")){
                     ### 3.6.1. Define default title(s) ####
                     if (plot_type == "feature"){
                       default_titles <-
                         reactive(
                           label = glue("{plot_label}: define default title for custom title input"),
                           {
                           # Only runs when the plot is enabled
                           req(plot_switch())

                           if (!is.null(features_entered())){
                             if (
                               length(features_entered()) == 1 &
                               plot_selections$split_by() != "none"
                             ){
                               # Single-feature plots with a split_by selection:
                               # use split_by groups
                               req(plot_selections$split_by())

                               default_titles <-
                                 SCUBA::unique_values(
                                   object = object(),
                                   var = plot_selections$split_by()
                                   ) |>
                                 str_sort(numeric = TRUE)

                               return(default_titles)
                             } else if (
                               length(features_entered()) > 1 &
                               plot_selections$split_by() == "none"
                             ){
                               # Multi-feature plots with no split_by selection
                               
                               # Set display names for features according to 
                               # the information in the config file, unless 
                               # the user requests "raw" feature names with
                               # the assay key added
                               if (raw_feature_names() == FALSE){
                                 sapply(
                                   1:length(features_entered()),
                                   function(i){
                                     hr_name(
                                       machine_readable_name =
                                         features_entered()[i],
                                       assay_config = assay_config(),
                                       # Do use the assay label if provided in
                                       # config app
                                       use_suffix = TRUE
                                     )
                                   }
                                 )
                               } else {
                                 features_entered()
                               }
                             }
                           }
                         })
                     } else if (plot_type == "ridge"){
                       # Default titles: feature names with assay key removed
                       default_titles <-
                         reactive(
                           label = glue("{plot_label}: define default title for custom title input"),
                           {
                           # Only runs when the plot is enabled, and if features
                           # have been entered
                           req(plot_switch())
                           req(features_entered())

                           # Remove assay key from title, unless the user 
                           # requests it be added 
                           if (raw_feature_names() == FALSE){
                             sapply(
                               1:length(features_entered()),
                               function(i){
                                 hr_name(
                                   machine_readable_name =
                                     features_entered()[i],
                                   assay_config = assay_config(),
                                   # Do use the assay label if provided in
                                   # config app
                                   use_suffix = TRUE
                                   )
                                 }
                               )
                             } else {
                               # If the user requests assay keys be added,
                               # report the machine-readable features with 
                               # the assay key
                               features_entered()
                             }
                           })
                     }

                     ### 3.6.2 Pass default titles to multi-text input module ####
                     custom_vector <-
                       multi_text_input_server(
                         id = "custom_title_multi",
                         default_vector = default_titles
                         )
                   }

                   ## 3.7. Custom title value ####
                   # Used for dimplots, stacked bar plots, pie charts
                   if (plot_type %in% c("dimplot", "proportion", "pie")){
                     plot_title <-
                       reactive(
                         label = glue("{plot_label}: custom title value"),
                         {
                         # Only runs when the plot is enabled
                         req(plot_switch())

                         # Define default title to use if a custom title is not
                         # requested. Default is the label of the group by
                         # category, or the name itself if the label is not
                         # defined in the config file
                         group_by_label <-
                           metadata_config()[[plot_selections$group_by()]]$label

                         default_title <-
                           if (isTruthy(group_by_label)){
                             group_by_label
                           } else {
                             plot_selections$group_by()
                           }

                         # Return title based on selections in title settings
                         # menu
                         if (isTruthy(input$title_settings)){
                           if (input$title_settings == "default"){
                             default_title
                           } else if (input$title_settings == "custom"){
                             # Use the custom title entered by the
                             # user in this case
                             custom_title_single()
                           } else if (input$title_settings == "none"){
                             # NULL is passed to the title argument, removing it
                             NULL
                           }
                         } else{
                           # Use default title if selection menu does not exist
                           default_title
                         }
                       })
                   }

                   ## 3.8. Custom title value for feature, ridge plots ####
                   # Must pass either single or multiple custom title output
                   # depending on the current number of panels on the feature
                   # plot, or NULL
                   if (plot_type %in% c("feature", "ridge")){
                     variable_length_custom_title <-
                       reactive(
                         label = glue("{plot_label}: custom title value"),
                         {
                         # Only runs when the plot is enabled
                         req(plot_switch())

                         # Only continue if menu_type is not NULL (when custom
                         # titles is selected)
                         req(menu_type())
                         
                         if (input$title_settings == "custom"){
                           # Determine current menu structure
                           if (menu_type() == "single"){
                             # For single_panel plots (menu_type() == "single"),
                             # use the single text entry
                             custom_title_single()
                           } else if (menu_type() == "multiple"){
                             req(custom_vector())
                             # For multi-panel plots, use the vector of custom
                             # title inputs from the custom_title_multi module
                             custom_vector()
                           }
                         } else {
                           # If custom titles is FALSE, additional logic is 
                           # used to set a default title. If the title involves
                           # the names of features entered, additional 
                           # transformations are involved.
                           
                           # Determine if the default title involves the use
                           # of feature names. feature_based_title is used to 
                           # determine operations to peform based on the 
                           # results of the previous conditionals
                           if (plot_type == "feature"){
                             if (plot_selections$split_by() != "none"){
                               # When split_by is enabled, the title is based 
                               # on the split by groups, not the feature name
                               feature_based_title <- FALSE
                             } else {
                               # Otherwise, the title is based on the feature
                               # name
                               feature_based_title <- TRUE
                             }
                           } else {
                             feature_based_title <- TRUE
                           }
                           
                           # For defaults that involve the use of feature 
                           # names, the assay keys will be removed. In other
                           # cases, NULL is returned, which will prompt 
                           # plotting functions to use their defaults.
                           if (feature_based_title == TRUE){
                             if (raw_feature_names() == FALSE){
                               # For feature based default titles, the 
                               # feature names are modified to remove the 
                               # assay key, unless the "raw" feature names 
                               # checkbox is TRUE.
                               sapply(
                                 features_entered(),
                                 function(feature, assay_config){
                                   hr_name(
                                     machine_readable_name = feature,
                                     assay_config = assay_config,
                                     use_suffix = TRUE
                                   )
                                 },
                                 assay_config()
                               )
                               } else {
                               features_entered()
                               }
                           } else {
                             NULL
                           }
                           }
                       })
                   }

                   ## 3.9. Hide title settings menu for blended plots ####
                   # Stopgap solution; custom titles may be supported in the
                   # future.
                   if (plot_type == "feature"){
                     observe(
                       label = glue("{plot_label}: hide title settings menu for blended plots"),
                       {
                       target_id <- "title_settings"

                       if (isTruthy(plot_selections$blend())){
                         hideElement(
                           id = target_id
                         )
                       } else {
                         showElement(
                           id = target_id
                         )
                       }
                     })
                   }

                   ## 3.10. X-axis title for Cell Type Proportion Plots ####
                   # Aim is to eventually add a custom title input
                   if (plot_type == "proportion"){
                     proportion_x_axis_title <-
                       reactive(
                         label = glue("{plot_label}: compute x-axis title"),
                         {
                         # Only runs when the plot is enabled
                         req(plot_switch())

                         split_by_label <-
                           isolate({metadata_config()})[[
                             plot_selections$split_by()]]$label

                         default_title <-
                           if (isTruthy(split_by_label)){
                             split_by_label
                           } else {
                             plot_selections$split_by()
                           }
                       })
                   }
                 }

                 # 4. Show/hide...  --------------------------------------------
                 ## 4.1. Super Title Menu ####
                 if (plot_type == "feature"){
                   observe(
                     label = glue("{plot_label}: super title menu"),
                     {
                     # Only runs when the plot is enabled
                     req(plot_switch())

                     # Show is set to TRUE when conditionals below are satisfied
                     show <- FALSE

                     # Super-titles (titles above all panels) currently only
                     # possible for multi-panel feature plots
                     if (!is.null(features_entered()) &
                         !is.null(plot_selections$split_by())){
                       if (length(features_entered()) == 1 &
                           plot_selections$split_by() != "none"){
                         show <- TRUE
                       }
                     }

                     if (show == TRUE){
                       showElement(
                         id = "super_title"
                         )
                     } else {
                       hideElement(
                         id = "super_title"
                       )
                     }
                   })
                 }

                 ## 4.2. Legend Title Options ####
                 if (plot_type == "feature"){
                   observe(
                     label = glue("{plot_label}: legend title options"),
                     {
                     # Only runs when the plot is enabled
                     req(plot_switch())

                     # show is set to TRUE when conditions are met
                     show <- FALSE
                     # ID of legend title container
                     elem_id <- "legend_title_div"
                     # Default value of input: changed based on conditionals
                     default_value = "feature"

                     # Appears on single-feature plots, and multi-feature plots
                     # with no split by category
                     if (!is.null(features_entered()) &
                         !is.null(plot_selections$split_by())){
                       if (length(features_entered()) == 1){
                         show <- TRUE
                       } else if (length(features_entered()) > 1 &
                                  plot_selections$split_by() == "none"){
                         show <- TRUE
                         # In this case, set default value of input to "none"
                         default_value = "none"
                       }
                     }

                     if (show == TRUE){
                       showElement(
                         id = elem_id
                       )
                     } else {
                       hideElement(
                         id = elem_id
                       )
                     }

                     # Update input with default value
                     updateSelectInput(
                       session,
                       inputId = "legend_title",
                       selected = default_value
                     )
                   })
                 }

                 ## 4.3 Share Scale Between Features ####
                 # Show when conditions below are met
                 # Errors will result unless plot_type is restricted to feature
                 # (DimPlots don't process features_entered())
                 if (plot_type == "feature"){
                   observe(
                     label = glue("{plot_label}: share scale between features"),
                     {
                     # Only runs when the plot is enabled
                     req(plot_switch())

                     # show is set to TRUE when conditions below are met
                     show <- FALSE
                     elem_id <- "share_scale"

                     if (!is.null(features_entered()) &
                         !is.null(plot_selections$split_by())){
                       # Currently visible when multiple features are chosen,
                       # and no split by category is selected
                       if (length(features_entered()) > 1){
                         if (plot_selections$split_by() == "none"){
                           show <- TRUE
                         }
                       }
                     }

                     # Show/hide based on outcome above
                     if (show == TRUE){
                       showElement(
                         id = elem_id
                       )
                     } else {
                       hideElement(
                         id = elem_id
                       )
                     }
                   })
                 }

                 ### 4.3.1. Update legend_title based on share_scale ####
                 # If share_scale == TRUE, "feature" cannot be used for
                 # share_scale. This option must be removed in this case.
                 if (plot_type == "feature"){
                   observe(
                     label = glue("{plot_label}: update legend_title based on share_scale"),
                     {
                     # Only runs when the plot is enabled
                     req(plot_switch())

                     if (!is.null(plot_selections$share_scale())){
                       updateSelectInput(
                         session,
                         inputId = "legend_title",
                         # Choices depend on state of share_scale
                         choices =
                           if (plot_selections$share_scale() == TRUE){
                             c("Expression" = "assay_score",
                               "No Title" = "none")
                           } else {
                             c("Feature Name" = "feature",
                               "Expression" = "assay_score",
                               "No Title" = "none")
                           },
                         selected =
                           # Code preserves current selection
                           # if it is still valid
                           if (
                             # All choices are valid when share_scale == FALSE
                             plot_selections$share_scale() == FALSE |
                             # When share_scale == TRUE, current value can be
                             # preserved if it is not "feature"
                             (plot_selections$share_scale() == TRUE &
                              input$legend_title != "feature")
                             ){
                             input$legend_title
                             }
                         )
                       }
                   })
                 }

                 ## 4.4. Color by Feature ####
                 # Available for feature plots when multiple
                 # features are entered and split_by is "none"
                 if (plot_type == "feature"){
                   observe(
                     label = glue("{plot_label}: color by feature menu"),
                     {
                     # Only runs when the plot is enabled
                     req(plot_switch())

                     # show is set to TRUE when conditions below are met
                     show <- FALSE
                     elem_id <- "color_by_feature"

                     if (!is.null(features_entered()) &
                         !is.null(plot_selections$split_by())){
                       if (length(features_entered()) > 1 &
                           plot_selections$split_by() == "none"){
                         # Color by feature cannot be used when
                         # share_scale is TRUE
                         if (plot_selections$share_scale() == TRUE){
                           show <- FALSE
                         } else {
                           show <- TRUE
                           }
                         }
                     }

                     # Show/hide based on outcome above
                     if (show == TRUE){
                       showElement(
                         id = elem_id
                       )
                     } else {
                       hideElement(
                         id = elem_id
                       )

                       # Set value to FALSE when hiding to avoid interference
                       # with share_scale setting
                       updateCheckboxInput(
                         session,
                         inputId = elem_id,
                         value = FALSE
                       )
                     }
                   })
                 }

                 ## 4.5. Group by menu for labels ####
                 if (plot_type == "feature"){
                   observe(
                     label = glue("{plot_label}: group_by menu for labels"),
                     {
                     # Only runs when the plot is enabled
                     req(plot_switch())

                     # Show menu when "label groups" is selected
                     show <- FALSE
                     elem_id <- "group_by"

                     if (isTruthy(plot_selections$label())){
                       show <- TRUE
                     }

                     # Show/hide based on outcome above
                     if (show == TRUE){
                       showElement(
                         id = elem_id,
                         anim = TRUE
                         )
                     } else {
                       hideElement(
                         id = elem_id,
                         anim = TRUE
                         )
                       }
                   })
                 }

                 ## 4.6. Custom x-axis limits on ridge plots ####
                 ### 4.6.1. Checkbox to enable custom limits ####
                 if (plot_type == "ridge"){
                   observe(
                     label = glue("{plot_label}: enable/disable custom limits"),
                     {
                     # Only runs when the plot is enabled
                     req(plot_switch())

                     # Show checkbox when the plot is defined
                     show <- FALSE
                     elem_id <- "custom_xlim_checkbox"

                     # Initially tried isTruthy(plot()), but this evaluated to
                     # TRUE even when the plot was not rendered.
                     # isTruthy(features_entered) will be TRUE when the plot is
                     # defined, since the plot requires a feature. This will be
                     # used instead
                     if (isTruthy(features_entered())){
                       show <- TRUE
                     }

                     if (show == TRUE){
                       showElement(
                         id = elem_id
                       )
                     } else {
                       hideElement(
                         id = elem_id
                       )
                     }
                   })
                 }

                 ### 4.6.2. Interface for setting custom limits ####
                 if (plot_type == "ridge"){
                   observe(
                     label = glue("{plot_label}: custom limits interface"),
                     {
                     # Only runs when the plot is enabled
                     req(plot_switch())

                     # Show interface when the checkbox in 4.6.1. is selected
                     show <- FALSE
                     elem_id <- "custom_xlim_interface"

                     if (isTruthy(input$use_custom_xlim)){
                       show <- TRUE
                     }

                     if (show == TRUE){
                       showElement(
                         id = elem_id,
                         anim = TRUE
                       )
                     } else {
                       hideElement(
                         id = elem_id,
                         anim = TRUE
                       )
                     }
                   })
                 }

                 ## 4.7. Show/hide settings for blended feature plot ####
                 if (plot_type == "feature"){
                   ### 4.7.1. Blend checkbox ####
                   observe(
                     label = glue("Blended feature plots: blend checkbox"),
                     {
                     # Only runs when the plot is enabled
                     req(plot_switch())

                     elem_id = "blend"

                     # The blend checkbox should only be visible when
                     # two features are entered.
                     if (length(features()) == 2){
                       showElement(
                         id = elem_id
                         )
                       } else {
                         hideElement(
                           id = elem_id
                           )

                         # Also, set the blend checkbox to FALSE
                         # if it was checked
                         updateCheckboxInput(
                           session = session,
                           inputId = elem_id,
                           value = FALSE
                         )
                         }
                     })

                   ### 4.7.2. Blend options window ####
                   observe(
                     label = glue("Blended feature plots: show/hide ncol slider"),
                     {
                     # Only runs when the plot is enabled
                     req(plot_switch())

                     elem_id <- "blend_options_container"

                     if (isTruthy(input$blend)){
                       showElement(
                         id = elem_id,
                         anim = TRUE
                       )
                     } else {
                       hideElement(
                         id = elem_id,
                         anim = TRUE
                       )
                     }
                   })

                   ### 4.7.3. Custom palette for blend ####
                   observe(
                     label = glue("Blended feature plots: custom blend palette"),
                     {
                     # Only runs when the plot is enabled
                     req(plot_switch())

                     # When a custom palette is selected, show color inputs to
                     # choose the palette.
                     elem_id <- "blend_palette_custom_colors"
                     if (isTruthy(plot_selections$blend_palette())){
                       if (plot_selections$blend_palette() == "custom"){
                         showElement(
                           id = elem_id,
                           anim = TRUE
                         )
                       } else {
                         hideElement(
                           id = elem_id,
                           anim = TRUE
                         )
                       }
                     } else {
                       hideElement(
                         id = elem_id,
                         anim = TRUE
                       )
                     }
                   })

                   ### 4.7.4. Hide ncol slider for blended featureplots ####
                   # Blended feature plots are unlikely to support a custom
                   # number of columns in the future.
                   observe(
                     label = glue("Blended feature plots: show/hide ncol slider"),
                     {
                     # Only runs when the plot is enabled
                     req(plot_switch())

                     target_id <- "ncol_slider"

                     if (isTruthy(plot_selections$blend())){
                       hideElement(
                         id = target_id
                         )
                     } else {
                       showElement(
                         id = target_id
                         )
                       }
                     })
                 }

                 ## 4.8. Custom Feature Labels on Dot Plots ####
                 observe(
                   label = glue("{plot_label}: Custom feature labels"),
                   {
                   # Only runs when the plot is enabled
                   req(plot_switch())

                   req(input$dot_x_labels)

                   # Show custom feature label picker when enabled by user
                   if (input$dot_x_labels == "custom"){
                     showElement(
                       id = "dot_x_labels_div",
                       anim = TRUE
                       )
                   } else {
                     hideElement(
                       id = "dot_x_labels_div",
                       anim = TRUE
                       )
                   }
                 })

                 ## 4.9. Legend ncol slider ####
                 observe(
                   label = glue("{plot_label}: Legend ncol slider"),
                   {
                   # Only runs when the plot is enabled
                   req(plot_switch())

                   target_id <- "legend_ncol"

                   if (!isTruthy(input$default_legend_ncol)){
                     showElement(
                       id = target_id
                     )
                   } else {
                     hideElement(
                       id = target_id
                     )
                   }
                 })

                 ## 4.10. Disable label groups checkbox when group_by == split_by ####
                 if (plot_type == "dimplot"){
                   observe({
                     req(plot_selections$group_by())
                     req(plot_selections$split_by())
                     
                     if (plot_selections$group_by() == 
                         plot_selections$split_by()){
                       # Disable checkbox
                       shinyjs::disable(
                         id = "label"
                         )
                       
                       # Change the tootip over the label checkbox
                       # Remove existing tooptip, add a new one explaining why
                       # the checkbox was disabled.
                       # shinyBS::removeTooltip(
                       #   session,
                       #   id = ns("label_tooltip")
                       #   )
                       # shinyBS::addTooltip(
                       #   session,
                       #   id = ns("label_tooltip"),
                       #   title = 
                       #     paste0("Can't label cells when the group by and ",
                       #            "split by selections are equal. To use ",
                       #            "labels, please change either selection."),
                       #   placement = "top", 
                       #   trigger = "hover",
                       #   options = NULL
                       # )
                     } else {
                       shinyjs::enable(
                         id = "label"
                         )
                       
                       # Restore the default tooltip on the label checkbox
                       # shinyBS::removeTooltip(
                       #   session,
                       #   id = ns("label_tooltip")
                       #   )
                       # shinyBS::addTooltip(
                       #   session,
                       #   id = ns("label_tooltip"),
                       #   title = 
                       #     paste0(
                       #       "Label cells by metadata on plot. If checked, a ",
                       #       "menu will appear to select a variable for ",
                       #       "grouping cells. Labels will display in the ",
                       #       "center of each group in the chosen metadata ",
                       #       "variable."
                       #     ), 
                       #   placement = "top", 
                       #   trigger = "hover",
                       #   options = NULL
                       #   )
                       
                       # Do not modify the value of the checkbox in other cases
                       # (otherwise the checkbox would be set to TRUE each time
                       # the user chages group_by and split_by)
                     }
                   })
                 }
                 
                 # 5. Process x-axis labels for dot plots----
                 if (plot_type == "dot"){
                   ## 5.1. Define default labels to show in menu ####
                   default_dot_x_labels <-
                     reactive(
                       label = glue("{plot_label}: Define default x-axis labels for menu"),
                       {
                       # Only runs when the plot is enabled
                       req(plot_switch())
                       # Require features to be defined
                       req(features())

                       # Use features, not features_entered (dot plot may have
                       # separate features)
                       feature_names <- features()

                       # Default names: each feature entered, with the assay
                       # key removed, unless the user checks "Show modality 
                       # key before feature"
                       if (raw_feature_names() == FALSE){
                         for (i in 1:length(feature_names)){
                           feature_names[i] <-
                             hr_name(
                               machine_readable_name = feature_names[i],
                               assay_config = assay_config(),
                               use_suffix = TRUE
                             )
                         }
                       }

                       feature_names
                       })

                   ## 5.2. Process user inputs for custom feature labels ####
                   user_dot_x_labels <-
                     multi_text_input_server(
                       id = "rename_dot_x_labels",
                       default_vector = default_dot_x_labels
                       )

                   ## 5.3. Use custom labels, or default ones ####
                   dot_x_labels <-
                     reactive(
                       label = glue("{plot_label}: Custom X-axis labels"),
                       {
                       # Only runs when the plot is enabled
                       req(plot_switch())

                       # Only process if input$dot_x_labels is defined
                       # (otherwise return NULL, which will leave labels unchanged)
                       if (isTruthy(input$dot_x_labels)){
                         # Use default or custom feature labels on x-axis
                         if (input$dot_x_labels == "custom"){
                           # If custom, rename feature labels to the custom order
                           user_dot_x_labels()
                         } else if (input$dot_x_labels == "truncated"){
                           # If default, use the default feature names, but
                           # condense them (if they are more than 20 characters
                           # long, remove any characters beyond the 20th and add
                           # "...")
                           # use features() (dot plot may have separate features)
                           feature_names <- features()
                           truncated_feature_names <- c()

                           for (i in 1:length(feature_names)){
                             truncated_feature_names <-
                               c(truncated_feature_names,
                                 truncate_str(
                                   # Remove assay key prefix from feature name
                                   # before truncating, unless the user requests
                                   # the assay key be plotted
                                   str =
                                     if (raw_feature_names() == FALSE){
                                       hr_name(
                                         machine_readable_name = 
                                           feature_names[i],
                                         assay_config = assay_config(),
                                         use_suffix = TRUE
                                       )
                                     } else {
                                       feature_names[i]
                                     },
                                   max_length = 20
                                   )
                                 )
                           }

                           # Return truncated feature names
                           truncated_feature_names
                         } else if (input$dot_x_labels == "full"){
                           # For "full", remove assay keys from labels
                           # without truncating, unless the user requests they
                           # be added
                           feature_names <- features()

                           if (raw_feature_names() == FALSE){
                             for (i in 1:length(feature_names)){
                               feature_names[i] <-
                                 hr_name(
                                   machine_readable_name = feature_names[i],
                                   assay_config = assay_config(),
                                   use_suffix = TRUE
                                 )
                             } 
                           }

                           # Return full feature names
                           feature_names
                         }
                       }
                     })
                 }

                 # 6. Record plot_selections -----------------------------------
                 # list of reactives for storing selected inputs
                 plot_selections <-
                   list(
                     # Group_by
                     `group_by` =
                       reactive(
                         label = glue("{plot_label}: process group_by slection"),
                         {
                         if (!is.null(input$group_by)){
                           input$group_by
                           } else NULL
                         }),

                     # Split_by
                     `split_by` =
                       reactive(
                         label = glue("{plot_label}: process split_by slection"),
                         {
                         if (!is.null(input$split_by)){
                           input$split_by
                           } else NULL
                         }),

                     # Reduction
                     `reduction` =
                       reactive(
                         label = glue("{plot_label}: process reduction slection"),
                         {
                         if (!is.null(input$reduction)){
                           input$reduction
                           } else NULL
                         }),

                     # Number of columns in multi-panel plot
                     # Special conditional used (in some cases ncol will still
                     # have a value in situations where it shouldn't, and this
                     # can affect the plots)
                     `ncol` =
                       # Conditions under which ncol should be defined vary
                       # based on plot type. Separate reactive expressions are
                       # created based on the plot type used for the module.
                       if (plot_type == "dimplot"){
                         reactive(
                           label = glue("{plot_label}: process ncol slection"),
                           {
                           if (!is.null(input$split_by)){
                             if (input$split_by != "none"){
                               input$ncol
                               } else NULL
                             } else NULL
                           })
                         } else if (plot_type == "feature"){
                           # ncol applies when multiple panels are created on
                           # feature plot
                           reactive(
                             label = glue("{plot_label}: process ncol slection"),
                             {
                             req(features_entered())

                             # Process ncol for single feature plots when
                             # split_by is set, or for multiple feature plots
                             # when a split_by category is not set
                             if (
                               (length(features_entered()) == 1 &
                                input$split_by != "none") |
                               (length(features_entered()) > 1 &
                                input$split_by == "none")
                             ){
                               input$ncol
                             } else NULL
                           })
                         } else if (plot_type == "violin"){
                           # Condition to record ncol for violin plot
                           # Equal to conditions where there are multiple panels
                           reactive(
                             label = glue("{plot_label}: process ncol slection"),
                             {
                             if (length(features_entered()) > 1){
                               input$ncol
                               } else NULL
                             })
                           },

                     # Title above all panels (feature plots)
                     `super_title` =
                       reactive(
                         label =
                           glue("{plot_label}: process super_title slection"),
                         {
                         if (!is.null(input$super_title)){
                           input$super_title
                         } else NULL
                       }),

                     # Share scale between features
                     `share_scale` =
                       reactive(
                         label =
                           glue("{plot_label}: process share_scale slection"),
                         {
                         if (!is.null(input$share_scale)){
                           input$share_scale
                         } else NULL
                       }),

                     # Use Different Colors for Features
                     `color_by_feature` =
                       reactive(
                         label =
                           glue("{plot_label}: process color_by_feature slection"),
                         {
                           if (!is.null(input$color_by_feature)){
                             input$color_by_feature
                             } else NULL
                           }),

                     # Blend features
                     `blend` =
                       reactive(
                         label =
                           glue("{plot_label}: process blend slection"),
                         {
                           if (isTruthy(input$blend)){
                             input$blend
                             } else NULL
                           }),

                     # Layout of blended plot
                     `blend_layout` =
                       reactive(
                         label =
                           glue("{plot_label}: process blend_layout slection"),
                         {
                           if (isTruthy(input$blend_layout)){
                             input$blend_layout
                           } else NULL
                         }),

                     # Palette to use for blending
                     `blend_palette` =
                       reactive(
                         label =
                           glue("{plot_label}: process blend_palette slection"),
                         {
                           if (isTruthy(input$blend_palette)){
                             input$blend_palette
                           } else NULL
                         }),

                     # Process custom blend palette input
                     `custom_blend_palette` =
                       reactive(
                         label =
                           glue(
                             "{plot_label}: process custom_blend_palette slection"
                             ),
                         {
                         # Record value when all inputs are present
                         if (isTruthy(input$blend_custom_low) &
                             isTruthy(input$blend_custom_1) &
                             isTruthy(input$blend_custom_2)){
                           # Return a character vector with all three inputs
                           c(input$blend_custom_low,
                             input$blend_custom_1,
                             input$blend_custom_2)
                         } else NULL
                       }),

                     # Order cells by expression
                     `order` =
                       reactive(
                         label =
                           glue("{plot_label}: process order slection"),
                         {
                           if (!is.null(input$order)){
                             input$order
                             } else NULL
                           }),

                     # Options for legend title
                     `legend_title` =
                       reactive(
                         label =
                           glue("{plot_label}: process legend_title slection"),
                         {
                           if (isTruthy(input$legend_title)){
                             input$legend_title
                           } else NULL
                         }),

                     # Re-order groups
                     `sort_groups` =
                       reactive(
                         label =
                           glue("{plot_label}: process sort_groups slection"),
                         {
                           if (isTruthy(input$sort_groups)){
                             input$sort_groups
                           } else NULL
                         }),

                     # Custom refactoring of group order on plots
                     `custom_refactoring` =
                       reactive(
                         label =
                           glue(
                             "{plot_label}: process custom_refactoring slection"
                             ),
                         {
                           # Custom levels for groups: either a custom order
                           # entered by the user, or a order set based on
                           # feature expression
                           if (isTruthy(input$sort_groups)){
                             if (input$sort_groups == "custom"){
                               input$custom_refactor
                               } else if (input$sort_groups == "expression"){
                                 # When "expression", use the levels computed
                                 # based on average feature expression
                                 expr_sort_levels()
                                 } else if (input$sort_groups == "proportion"){
                                   # When "propportion" use levels from sorting
                                   # based on cell proportions
                                   prop_sort_levels()
                                 } else NULL
                             # NULL is returned otherwise,
                             # or when input$sort_groups does not exist
                             } else NULL
                           }),

                     # Colors for min and max values
                     `min_color` =
                       reactive(
                         label =
                           glue("{plot_label}: process min_color slection"),
                         {
                         # Pass color values if the custom colors checkbox
                         # exists and is selected
                         if (isTruthy(input$custom_colors)){
                           input$min_color
                         } else NULL
                       }),

                     `max_color` =
                       reactive(
                         label =
                           glue("{plot_label}: process max_color slection"),
                         {
                         if (isTruthy(input$custom_colors)){
                           input$max_color
                           } else NULL
                         }),

                     # Super title checkbox (feature plots with more than one
                     # split_by category)

                     # Include legend
                     `legend` =
                       reactive(
                         label =
                           glue("{plot_label}: process legend slection"),
                         {
                           if (!is.null(input$legend)){
                             input$legend
                             } else NULL
                           }),

                     # Label groups
                     `label` =
                       reactive(
                         label =
                           glue("{plot_label}: process label slection"),
                         {
                           if (!is.null(input$label)){
                             input$label
                             } else NULL
                           }),

                     # Original axes limits
                     # isolate(names(input)) does not work for testing the
                     # existence of the original axes limits checkbox.
                     # input$original_limits will be either TRUE or FALSE if
                     # the checkbox exists, or NULL if it does not. A suitable
                     # existence test is !is.null(input$original_limits)
                     `limits` =
                       reactive(
                         label =
                           glue("{plot_label}: process limits slection"),
                         {
                           if (!is.null(input$original_limits)){
                             input$original_limits
                             } else NULL
                           }),

                     # X-axis feature for scatterplots
                     `scatter_1` =
                       reactive(
                         label =
                           glue("{plot_label}: process scatter_1 slection"),
                         {
                           if (plot_type == "scatter"){
                             input$scatter_1
                             } else NULL
                           }),

                     # Y-axis feature for scatterplots
                     `scatter_2` =
                       reactive(
                         label =
                           glue("{plot_label}: process scatter_2 slection"),
                         {
                         if (plot_type == "scatter"){
                           input$scatter_2
                           } else NULL
                         }),

                     # Remove Title
                     `display_coeff` =
                       reactive(
                         label =
                           glue("{plot_label}: process display_coeff slection"),
                         {
                           if (!is.null(input$display_coeff)){
                             input$display_coeff
                             } else NULL
                           }),

                     # Number of columns in legend
                     `legend_ncol` =
                       reactive(
                         label =
                           glue("{plot_label}: process legend_ncol slection"),
                         {
                           # Record value of the legend_ncol slider if the
                           # "use default" checkbox is not checked
                           if (isTruthy(input$legend_ncol) &
                               !isTruthy(input$default_legend_ncol)){
                             input$legend_ncol
                           } else {
                             NULL
                           }
                         }),

                     `legend_font_size` =
                       reactive(
                         label =
                           glue("{plot_label}: process legend_font_size slection"),
                         {
                           # Value depends on qualitative value of legend size
                           # slider (limits from 1 to 9)
                           v <- input$legend_size

                           # 1 = smallest, 9 = largest.
                           dplyr::case_when(
                             v == 1 ~ 8,
                             v == 2 ~ 9.5,
                             v == 3 ~ 10,
                             v == 4 ~ 11,
                             v == 5 ~ 11,
                             # Default
                             v == 6 ~ 12,
                             v == 7 ~ 14,
                             v == 8 ~ 16,
                             v == 9 ~ 18
                           )
                         }),

                     `legend_key_size` =
                       reactive(
                         label =
                           glue("{plot_label}: process legend_key_size slection"),
                         {
                           # Value depends on qualitative value of legend size
                           # slider (limits from 1 to 9)
                           v <- input$legend_size

                           # Different values from the same slider are used for
                           # violin, proportion, and ridge plots
                           if (!plot_type %in% c("violin", "proportion", "ridge")){
                             # Standard values
                             # 1 = smallest, 9 = largest.
                             dplyr::case_when(
                               v == 1 ~ 1.5,
                               v == 2 ~ 2,
                               v == 3 ~ 2,
                               v == 4 ~ 2.5,
                               v == 5 ~ 2.5,
                               # Default
                               v == 6 ~ 3,
                               v == 7 ~ 4,
                               v == 8 ~ 6,
                               v == 9 ~ 8
                             )
                           } else {
                             # 1 = smallest, 9 = largest.
                             dplyr::case_when(
                               v == 1 ~ 2,
                               v == 2 ~ 2.5,
                               v == 3 ~ 3,
                               v == 4 ~ 3.5,
                               v == 5 ~ 4,
                               # Default
                               v == 6 ~ 4.5,
                               v == 7 ~ 6,
                               v == 8 ~ 8,
                               v == 9 ~ 10
                             )
                           }
                         }),

                     `legend_key_spacing` =
                       reactive(
                         label =
                           glue(
                             "{plot_label}: process legend_key_spacing slection"
                             ),
                         {
                           # Value only applies to plots with scatter geoms
                           # (DimPlots, Scatterplots)

                           # Value depends on qualitative value of legend size
                           # slider (limits from 1 to 9)
                           v <- input$legend_size

                           # 1 = smallest, 9 = largest.
                           dplyr::case_when(
                             v == 1 ~ 0,
                             v == 2 ~ 0.5,
                             v == 3 ~ 3,
                             v == 4 ~ 8,
                             v == 5 ~ 12,
                             # Default
                             v == 6 ~ 14,
                             v == 7 ~ 17,
                             v == 8 ~ 21,
                             v == 9 ~ 26
                           )
                         })
                     )

                 # 7. Determine if a subset has been used  ---------------------
                 # This variable will be a boolean used in downstream
                 # computations
                 is_subset <-
                   eventReactive(
                     label = glue("{plot_label}: Test if Object is a Subset"),
                     object(),
                     ignoreNULL = FALSE,
                     {
                       # Throw an error if the subset does not exist or is NULL
                       validate(
                         need(
                           object(),
                           message = "error: subset is NULL"
                           )
                         )

                       # Compute number of cells in subset
                       n_cells_test <-
                         cellDIVER:::n_cells(object())

                       # Test if the number of cells in the subset differs from
                       # the number of cells in the original object. If this
                       # conditional is TRUE, then the object read is a subset
                       n_cells_original() != n_cells_test
                   })

                 # 8. Conditional UI -------------------------------------------
                 ## 8.1. ncol slider ####
                 # Conditions under which ncol slider appear differ based on
                 # plot type
                 if (plot_type == "dimplot"){
                   # UMAP plots: appears when split_by != "none"
                   ncol_slider <-
                     eventReactive(
                       c(plot_selections$split_by(),
                         object(),
                         plot_switch()
                         ),
                       label = glue("{plot_label}: Make ncol Slider"),
                       ignoreNULL = TRUE,
                       {
                         #Do not render when split_by is "none"
                         if (plot_selections$split_by() == "none"){
                           NULL
                         } else {
                           # Define min, max, and default values for slider
                           ncol_settings <-
                             ncol_settings(
                               object = object(),
                               rule = "split_by",
                               split_by = plot_selections$split_by()
                             )

                           # Create slider input
                           sliderInput(
                             inputId = ns("ncol"),
                             label = "Number of Columns: ",
                             min = ncol_settings[1],
                             # Max value: equal to the number of levels
                             # in the given variable
                             max = ncol_settings[2],
                             # Only allow integer values
                             step = 1,
                             ticks = FALSE,
                             # Default value (3rd element)
                             value = ncol_settings[3]
                           )
                         } # End else
                       })

                 } else if (plot_type == "feature"){
                   # Feature plots: appears when only one feature is entered
                   # and split by is defined
                   ncol_slider <-
                     eventReactive(
                       c(plot_selections$split_by(),
                         plot_selections$blend(),
                         features_entered(),
                         object(),
                         plot_switch()
                         ),
                       label = glue("{plot_label}: Make ncol slider"),
                       ignoreNULL = TRUE,
                       {
                         if (!is.null(features_entered())){
                           # The ncol slider may be used for single feature
                           # plots when a split by selection is made
                           condition_single <-
                             length(features_entered()) == 1 &
                             plot_selections$split_by() != "none"

                           # For multi-feature plots, ncol is only available
                           # when there *is not* a split by selection
                           condition_multiple <-
                             length(features_entered()) > 1 &
                             plot_selections$split_by() == "none" &
                             # Do not show slider for blended plots
                             !isTruthy(plot_selections$blend())

                           if (condition_single | condition_multiple){
                             # Define min, max, and default values for slider
                             if (condition_single){
                               # For single-feature plots, ncol limits are
                               # based on the number of split_by groups
                               ncol_settings <-
                                 ncol_settings(
                                   object = object(),
                                   rule = "split_by",
                                   split_by = plot_selections$split_by()
                                 )
                             } else if (condition_multiple){
                               # For multi-feature plots, ncol limits are based
                               # on the number of features entered
                               ncol_settings <-
                                 ncol_settings(
                                   object = object(),
                                   rule = "features",
                                   features_entered = features_entered()
                                 )
                             }

                             # Create slider input
                             sliderInput(
                               inputId = ns("ncol"),
                               label = "Number of Columns: ",
                               min = ncol_settings[1],
                               # Max value: equal to the number of levels
                               # in the given variable
                               max = ncol_settings[2],
                               # Only allow integer values
                               step = 1,
                               ticks = FALSE,
                               # Default value (3rd element)
                               value = ncol_settings[3]
                               )
                             } else NULL # No slider when conditions not
                           }
                       })
                 } else if (plot_type == "violin"){
                   # Violin plots: appears when multiple features are entered
                   ncol_slider <-
                     eventReactive(
                       c(features_entered(),
                         plot_switch()
                         ),
                       label = glue("{plot_label}: Make ncol Slider"),
                       ignoreNULL = TRUE,
                       {
                         # Number of panels equals number of features for violin
                         # plots. Slider is needed only when more than one
                         # feature is entered
                         if(length(features_entered()) > 1){
                           # Define min, max, and default values for slider
                           # (depends on number of features entered)
                           ncol_settings <-
                             ncol_settings(
                               object = object(),
                               rule = "features",
                               features_entered = features_entered()
                             )

                           # Create/update slider input
                           sliderInput(
                             inputId = ns("ncol"),
                             label = "Number of Columns: ",
                             min = ncol_settings[1],
                             # Max value: equal to the number of levels
                             # in the given variable
                             max = ncol_settings[2],
                             # Only allow integer values
                             step = 1,
                             ticks = FALSE,
                             # Default value (3rd element)
                             value = ncol_settings[3]
                             )
                         } else NULL
                       })
                 }

                 ## 8.2. Checkbox to Specify Original Axis Limits ####
                 limits_checkbox <-
                   reactive(
                     label = glue("{plot_label}: Limits UI"),
                     {
                       # Only runs when the plot is enabled
                       req(plot_switch())

                       # Checkbox will only appear when a subset is selected.
                       # The presence of a subset will be tested by observing
                       # the number of cells in the subset
                       if (is_subset()) {
                         checkboxInput(
                           inputId = ns("original_limits"),
                           label = "Use Original Axes Limits",
                           value = FALSE
                           )
                       } else {
                         # Display nothing when the number of cells are equal
                         # between the subset and the full dataset
                         NULL
                         }

                       })

                 ## 8.3. Dynamic UI for plot output ####
                 # UI display depends on the plot type and whether the plot
                 # has a separate features option
                 plot_output_ui <-
                   reactive(
                     label = glue("{plot_label}: Plot Output UI"),
                     {
                       # Only render UI when the switch corresponding to the
                       # current plot is enabled
                       req(plot_switch())

                       # input_error: set to TRUE or FALSE based on conditionals
                       input_error = FALSE
                       # message: message to display to user when
                       # inputs are incorrect
                       message = NULL

                       ### 8.3.1. Test for correct inputs ####
                       # UI displayed depends on if inputs have been entered
                       # correctly. "Correct" inputs depend on plot type
                       # input_error is set to TRUE or FALSE based on input
                       # conditions.
                       if (plot_type == "dimplot"){
                         # All input scenarios are "correct" for dimplots
                         input_error = FALSE

                       } else if (plot_type == "scatter") {
                         # Scatterplots: both features must be entered for a
                         # "correct" input
                         if (
                           !isTruthy(input$scatter_1) |
                           !isTruthy(input$scatter_2)
                         ){
                           # Scatterplot and neither features defined
                           input_error = TRUE
                           message =
                             glue(
                               'Please enter two features under
                               "{tolower(plot_label)} Options" to
                                view {tolower(plot_label)}.'
                               )

                         # } else if (
                         #   isTruthy(input$scatter_1) &
                         #   !isTruthy(input$scatter_2)
                         # ){
                         #   # Scatterplot and only the y-axis feature is defined
                         #   input_error = TRUE
                         #   message =
                         #     glue(
                         #       'Please enter a feature for the y-axis
                         #       to view {tolower(plot_label)}.'
                         #       )
                         # } else if (
                         #   !isTruthy(input$scatter_1) &
                         #   isTruthy(input$scatter_2)
                         # ){
                         #   # Scatterplot and only the x-axis feature is defined
                         #   input_error = TRUE
                         #   message =
                         #     glue(
                         #      'Please enter a feature for the x-axis
                         #      to view {tolower(plot_label)}.'
                         #      )
                         } else if (
                           isTruthy(input$scatter_1) &
                           isTruthy(input$scatter_2)
                         ){
                           # Scatterplot and both features are defined
                           input_error = FALSE
                         }
                         # End scatterplot conditionals

                       } else if (plot_type %in% c("feature", "violin")) {
                         # Feature and violin plots: "Correct" input depends
                         # on state of features_entered()
                         if (isTruthy(features_entered())){
                           # When features are entered, inputs are "correct"
                           input_error = FALSE
                         } else {
                           # When features are not entered, display message
                           # directing user to enter input
                           input_error = TRUE
                           message =
                             glue(
                               "Please enter a feature to view
                               {tolower(plot_label)}."
                               )
                           }
                         } else if (plot_type == "dot"){
                           # Dot plots: "correct" input depends on state of
                           # features_entered() and input$use_separate_features
                           if (
                             input$use_separate_features == FALSE &
                             length(features_entered()) == 0
                           ){
                             input_error = TRUE
                             message =
                               glue(
                                 "Please enter a feature to view
                                 {tolower(plot_label)}."
                                 )
                           } else if (
                             input$use_separate_features == TRUE &
                             length(input$separate_features) == 0
                           ){
                             input_error = TRUE
                             message =
                               glue(
                                 'Please specify at least one
                               {tolower(plot_label)} specific feature to view
                               plot. To use the same features as for other
                               plots, please uncheck "use separate features".'
                               )
                           } else {
                             input_error = FALSE
                             }
                         }

                       ### 8.3.2. Display message or plot based on inputs ####
                       if (input_error == TRUE){
                         # Message: use message defined in conditonal structure
                         ui <-
                           tags$h3(
                             message,
                             style="margin-bottom: 10em;"
                             )
                       } else if (input_error == FALSE){
                         # Plot: UI depends on whether manual
                         # dimensions are specified
                         if (
                           (isTruthy(manual_dim$width())) &
                           (isTruthy(manual_dim$height()))
                         ){
                           # If manual dimensions are specified, pass the
                           # values specified by the user to plotOutput
                           ui <-
                             plotOutput(
                               outputId = ns("plot"),
                               width = manual_dim$width()*96,
                               height = manual_dim$height()*96
                               )
                         } else {
                           # Otherwise, call plotOutput without defining
                           # width and height
                           ui <-
                             plotOutput(
                               outputId = ns("plot"),
                               height = "600px"
                               )
                         }

                       }

                       # Return UI to plot_output_ui
                       ui
                     })

                 ## 8.4. Menu for custom refactoring of Group_by metadata ####
                 if (plot_type %in% c("violin", "dot", "ridge", "proportion")){
                   ### 8.4.1. Define menu UI ####
                   if (plot_type %in% c("violin", "dot", "ridge")){
                     # Violin, dot, ridge plots: refactoring affects
                     # group by variable
                     refactor_sortable <-
                       eventReactive(
                         c(plot_selections$group_by(),
                           object(),
                           plot_switch()
                           ),
                         {
                           # Responds to both the object and the group by
                           # variable, but only runs when the group by variable is
                           # defined
                           req(plot_selections$group_by())

                           # For ridge plots, the group_by variable can also be
                           # "none", which will cause errors. The UI should not
                           # compute in this case
                           if (plot_type == "ridge"){
                             req(plot_selections$group_by() != "none")
                           }

                           # Also only runs when the plot is enabled
                           req(plot_switch())

                           # Menu choices
                           # Based on unique values or levels of current group by
                           # category
                           group_by_metadata <-
                             SCUBA::fetch_metadata(
                               object = object(),
                               vars = plot_selections$group_by(),
                               return_class = "vector"
                               )

                           # Test if the current group by category is a factor
                           if (is.factor(group_by_metadata)){
                             # If so, extract factor levels
                             menu_levels <-
                               group_by_metadata |>
                               levels()
                           } else {
                             # Otherwise, fetch the unique values
                             menu_levels <-
                               group_by_metadata |>
                               unique()
                           }

                           # Sort to initialize menu in ascending
                           # alphanumeric order
                           menu_levels <-
                             menu_levels |>
                             str_sort(
                               numeric = TRUE,
                               decreasing = FALSE
                               # Use descending order for dot plots (DotPlot()
                               # inverts factor levels when plotting)
                               #if (plot_type == "dot") TRUE else FALSE
                             )

                           # Menu UI
                           div(
                             class = "compact-options-container",
                             rank_list(
                               text = "Drag-and drop to set order of groups:",
                               labels = menu_levels,
                               input_id = ns("custom_refactor"),
                               class =
                                 c("default-sortable", "bucket-select")
                             )
                           )
                         })
                   } else if (plot_type %in% "proportion"){
                     # Proportion plots: refactoring affects split by variable
                     refactor_sortable <-
                       eventReactive(
                         c(plot_selections$split_by(),
                           object(),
                           plot_switch()
                           ),
                         {
                           # Responds to both the object and the group by
                           # variable, but only runs when the group by
                           # variable is defined
                           req(plot_selections$split_by())
                           # Also only runs when the plot is enabled
                           req(plot_switch())

                           # Menu choices
                           # Based on unique values or levels of current group by
                           # category
                           split_by_metadata <-
                             SCUBA::fetch_metadata(
                               object(),
                               vars = plot_selections$split_by(),
                               return_class = "vector"
                              )

                           # Test if the current group by category is a factor
                           if (is.factor(split_by_metadata)){
                             # If so, extract factor levels
                             menu_levels <-
                               split_by_metadata |>
                               levels()
                           } else {
                             # Otherwise, fetch the unique values
                             menu_levels <-
                               split_by_metadata |>
                               unique()
                           }

                           # Sort to initialize menu in ascending
                           # alphanumeric order
                           menu_levels <-
                             menu_levels |>
                             str_sort(
                               numeric = TRUE,
                               decreasing = FALSE
                               # Use descending order for dot plots (DotPlot()
                               # inverts factor levels when plotting)
                               #if (plot_type == "dot") TRUE else FALSE
                             )

                           # Menu UI
                           div(
                             class = "compact-options-container",
                             rank_list(
                               text = "Drag-and drop to set order of groups:",
                               labels = menu_levels,
                               input_id = ns("custom_refactor"),
                               class =
                                 c("default-sortable", "bucket-select")
                             )
                           )
                         })
                     }


                   ### 8.4.2. Show/hide menu ####
                   observe({
                     # The output container is shown/hidden
                     target_id <- "refactor_sortable"

                     # Show when "custom" is chosen from the group order menu
                     if (isTruthy(input$sort_groups)){
                       if (input$sort_groups == "custom"){
                         showElement(
                           id = target_id,
                           anim = TRUE
                           )
                       } else {
                         hideElement(
                           id = target_id,
                           anim = TRUE
                           )
                       }
                     }
                   })

                   ### 8.4.3. Special case: Show/hide entire group sorting UI ####
                   # For ridge plots, the group_by selection may be "none", which
                   # will result in the plotting of a single group, "All Cells".
                   # The entire sorting interface is not useful and should be
                   # hidden in this case
                   if (plot_type == "ridge"){
                     observe({
                       req(plot_selections$group_by())

                       target_id <- "sort_groups_menu"

                       if (plot_selections$group_by() == "none"){
                         hideElement(
                           id = target_id,
                           anim = TRUE
                         )
                       } else {
                         showElement(
                           id = target_id,
                           anim = TRUE
                         )
                       }
                     })
                   }

                   ### 8.4.4. Sorting by feature expression ####
                   # Output of the reactive below is a vector giving the factor
                   # levels to apply before plotting. The groups are sorted in
                   # ascending or descending order by the average expression of
                   # the requested feature per group.
                   # Result is passed to the `sort_groups` reactive expression
                   if (plot_type %in% c("violin", "dot", "ridge")){
                     #### 8.4.4.1. Define UI for feature expression sorting menu ####
                     expr_sort_menu <-
                       reactive({
                         # Responds to both the object and the group by
                         # variable, but only runs when the group by variable is
                         # defined
                         req(plot_selections$group_by())
                         
                         # For ridge plots, the group_by variable can also be
                         # "none", which will cause errors. The UI should not
                         # compute in this case
                         if (plot_type == "ridge"){
                           req(plot_selections$group_by() != "none")
                         }
                         
                         # Also only runs when the plot is enabled, and if a 
                         # feature has been chosen
                         req(plot_switch())
                         req(features_entered())
                         
                         # Define display names for features
                         # Remove the assay key from features and add the 
                         # assay-based "suffix" from the config file, unless
                         # the user requests the assay key be displayed before
                         # feature names
                         if (raw_feature_names() == FALSE){
                           display_names <-
                             sapply(
                               features_entered(),
                               function(feature_i, assay_config){
                                 hr_name(
                                   machine_readable_name = feature_i,
                                   assay_config = assay_config,
                                   use_suffix = TRUE
                                 )
                               },
                               assay_config()
                             )
                         } else {
                           # If the user checks "Show modality key before 
                           # feature", display the machine-readable feature 
                           # names with the assay key added
                           display_names <- features_entered()
                         }
                         
                         
                         # Set up choices vector: values are machine-readable 
                         # feature names from features_entered(), and names are 
                         # the display 
                         feature_choices <- features_entered()
                         names(feature_choices) <- display_names
                         
                         # Menu UI
                         div(
                           id = ns("expr_sort_menu"),
                           class = "compact-options-container",
                           selectInput(
                             inputId = ns("sort_expr_feature"),
                             label = "Choose a feature to sort by:",
                             choices = feature_choices,
                             # Explicitly set the selected value to the first 
                             # feature to avoid the menu being blank
                             selected = feature_choices[1]
                           ),
                           selectInput(
                             inputId = ns("sort_expr_order"),
                             label = "Order of sorting:",
                             choices = 
                               c("Descending" = "descending",
                                 "Ascending" = "ascending"
                               )
                           )
                         )
                       })
                     
                     #### 8.4.4.2. Show/hide feature sorting UI ####
                     observe({
                       # The output container is shown/hidden
                       target_id <- "expr_sort_menu"
                       
                       # Show when "custom" is chosen from the group order menu
                       if (isTruthy(input$sort_groups)){
                         if (input$sort_groups == "expression"){
                           showElement(
                             id = target_id,
                             anim = TRUE
                           )
                         } else {
                           hideElement(
                             id = target_id,
                             anim = TRUE
                           )
                         }
                       }
                     })
                     
                     #### 8.4.4.3. Perform sorting, define new levels ####
                     expr_sort_levels <-
                       reactive({
                         # Only run when the user requests to sort by expression
                         # with a feature to sort by selected
                         req(input$sort_groups == "expression")
                         req(isTruthy(input$sort_expr_feature))

                         # Create a summary table with average expression values
                         # This is currently done using the dot plot function,
                         # regardless if the plot in cellDIVER is a dot plot
                         # This will be replaced with a SCUBA summary function
                         # in the future
                         summary_table <-
                           cellDIVER::plot_dot(
                             object(),
                             features = input$sort_expr_feature,
                             group_by = input$group_by,
                           )$data

                         # Sort table by expression of the indicated feature
                         summary_table <-
                           if (input$sort_expr_order == "ascending"){
                             summary_table %>%
                               arrange(avg_exp_scaled)
                           } else {
                             summary_table %>%
                               arrange(desc(avg_exp_scaled))
                           }

                         # Return values for ID
                         # Sorted groups
                         summary_table %>%
                           pull(id)
                       })
                   }
                   
                   ### 8.4.5. Sorting by Cell type proportion ####
                   if (plot_type == "proportion") {
                     #### 8.4.5.1. UI for sorting by proportion ####
                     prop_sort_menu <- reactive({
                       req(plot_selections$group_by())
                       
                       # We'll get the unique levels in the group_by variable,
                       # so the user can choose which one to base the sorting on.
                       group_by_vals <- SCUBA::unique_values(
                         object = object(),
                         var = plot_selections$group_by()
                       )
                       
                       div(
                         id = ns("prop_sort_menu"),
                         class = "compact-options-container",
                         # Menu to choose value of metadata variable for sorting
                         selectInput(
                           inputId = ns("sort_prop_value"),
                           label = "Choose the Group By level:",
                           choices = group_by_vals,
                           selected = group_by_vals[1]
                         ),
                         # Menu to choose ascending or descending order
                         selectInput(
                           inputId = ns("sort_prop_order"),
                           label = "Order of Sorting:",
                           choices = 
                             c("Ascending" = "ascending", 
                               "Descending" = "descending"),
                           selected = "descending"
                         )
                       )
                     })
                     
                     #### 8.4.5.2. Show/hide proportion sorting UI ####
                     observe({
                       if (isTruthy(input$sort_groups) && input$sort_groups == "proportion"){
                         showElement(
                           id = "prop_sort_menu",
                           anim = TRUE
                         )
                       } else {
                         hideElement(
                           id = "prop_sort_menu",
                           anim = TRUE
                         )
                       }
                     })
                     
                     #### 8.4.5.3. Perform sorting and define new levels ####
                     prop_sort_levels <- 
                       reactive({
                         req(input$sort_groups == "proportion")
                         req(input$sort_prop_value)      # Which group_by level (e.g. "CD4 T cells")
                         req(plot_selections$split_by()) # The variable we're re-ordering
                         req(plot_switch())              # only run if plot is actually on
                         # We'll fetch the relevant metadata as a data.frame 
                         # with two columns, group_by and split_by
                         meta_df <- 
                           SCUBA::fetch_metadata(
                            object(),
                            vars = 
                              c(plot_selections$group_by(), 
                                plot_selections$split_by()
                                )
                            )
                         
                         colnames(meta_df) <- c("group_by", "split_by")
                       
                         # For each group in the split by variable, compute 
                         # proportion of cells in the chosen group by value
                         sum_df <- 
                           meta_df %>%
                           dplyr::group_by(.data$split_by) %>%
                           dplyr::summarize(
                             prop_value = 
                               sum(.data$group_by == input$sort_prop_value) / 
                               dplyr::n(),
                           .groups = "drop"
                           )
                         
                         # Sort ascending or descending by prop_value
                         if (input$sort_prop_order == "ascending"){
                           sum_df <- 
                             dplyr::arrange(sum_df, .data$prop_value)
                         } else {
                           sum_df <- 
                             dplyr::arrange(sum_df, dplyr::desc(.data$prop_value))
                         }
                       
                         # Return a character vector of the new factor 
                         # levels for "split_by"
                         sum_df$split_by
                       })
                     }
                   }
                 
                 ## 8.6. Render Dynamic UI ####
                 output$ncol_slider <-
                   renderUI({
                     ncol_slider()
                     })

                 output$limits_checkbox <-
                   renderUI({
                     limits_checkbox()
                     })

                 # output$custom_colors_ui <-
                 #   renderUI({
                 #     custom_colors_ui()
                 #   })

                 output$plot_output_ui <-
                   renderUI({
                     plot_output_ui()
                     })

                 if (plot_type %in% c("violin", "dot", "proportion", "ridge")){
                   output$refactor_sortable <-
                     renderUI({
                       refactor_sortable()
                     })

                   # The UI should still compute when hidden, so it displays
                   # smoothly when the user selects "custom"
                   outputOptions(
                     output,
                     "refactor_sortable",
                     suspendWhenHidden = FALSE
                   )
                 }
                 
                 # Interface to sort by feature expression
                 if (plot_type %in% c("violin", "dot", "ridge")){
                   output$expr_sort_menu <-
                     renderUI({
                       expr_sort_menu()
                     })
                 }
                 
                 # Interface to sort by proportion
                 if (plot_type == "proportion"){
                   output$prop_sort_menu <- renderUI({
                     prop_sort_menu()
                   })
                 }
                 
                 # 9. Separate Features Entry: Dynamic Update ------------------
                 # Observers for separate features only update for server
                 # instances where features_entered
                 if (separate_features_server ==  TRUE){
                   ## 9.1. Update Separate Features in Background ####
                   # Before the checkbox to select separate features is checked,
                   # update the text entry in the background so it is synced
                   # when it appears after the box is checked.
                   # This process ensures the features are instantly available in
                   # the separate features text box when the checkbox is checked
                   observeEvent(
                     features_entered(),
                     label =
                       glue("{plot_label}: Update Separate
                            Features Text Entry"),
                     {
                       if (input$use_separate_features == FALSE){
                         updateSelectizeInput(
                           session,
                           inputId = "separate_features",
                           choices = valid_features(),
                           selected = features_entered(),
                           server = TRUE
                           )
                         }
                     })

                   ## 9.2. Reset Separate Features Upon Checkbox Toggle ####
                   # If the "use separate features" checkbox is toggled and the
                   # features entered in the separate features text entry differ
                   # from the general features selected, update the separate
                   # features text entry to match the general features.
                   # This is necessary to ensure that selections for general
                   # features appear in the separate features text entry in the
                   # event that the user checks the box, unchecks it, changes
                   # general features, and checks the box again.
                   observeEvent(
                     input$use_separate_features,
                     label =
                       glue("{plot_label}: Set Separate Features Input"),
                     {
                       if (
                         # Check if general feature and separate feature text
                         # entries are not in sync
                         !setequal(
                           features_entered(),
                           input$separate_features
                           )
                         ){
                         updateSelectizeInput(
                           session,
                           inputId = "separate_features",
                           choices = valid_features(),
                           selected = features_entered(),
                           server = TRUE
                         )
                       }
                     })
                 }

                 # 10. Blend Palette (feature_plots) ---------------------------
                 # Initialize blend palette selection menu with blend palettes
                 if (plot_type == "feature"){
                   if (isTruthy(blend_palettes)){
                     # Form vector of choices for blend palettes
                     # (names of blend_palettes list plus custom)
                     blend_palettes_choice_vector <-
                       c(names(blend_palettes), "custom")
                     names(blend_palettes_choice_vector) <-
                       c(names(blend_palettes), "Custom")

                     # Update blend palette menu
                     updatePickerInput(
                       session = session,
                       inputId = "blend_palette",
                       # Add custom to available choices
                       choices = blend_palettes_choice_vector,
                       # Red-blue palette selected by default
                       selected = "RdBu"
                       )
                     }
                   }

                 # 11. Plot ----------------------------------------------------
                 ## 11.1 Define Features to use ####
                 # For all plots that use feature expression data
                 # Uses either the general feature entry (features_entered()),
                 # or the separate features text entry depending on whether
                 # separate features are used in the module and whether the
                 # checkbox to use them is selected.
                 if (plot_type %in% c("feature", "violin", "dot")){
                   features <-
                     reactive(
                       label = glue("{plot_label}: Features for Plot"),
                       {
                         # Runs when plot is enabled and features are entered
                         req(plot_switch())
                         req(features_entered())

                         # Test for separate_features_server first
                         # input$use_separate_features does not exist if
                         # separate_features_server == FALSE
                         if (separate_features_server == TRUE){
                           # If separate features are used in this module,
                           # input them if the user checks the box to use
                           # them
                           if (isTruthy(input$use_separate_features)){
                             #Use separate features
                             input$separate_features
                           } else {
                             #Use general features
                             features_entered()
                           }
                         } else if (separate_features_server == FALSE){
                           # Otherwise, pass features_entered() to
                           # shiny_dot() (general features)
                           features_entered()
                         }
                       })

                   if (session$userData$dev_mode == TRUE){
                     observe(
                       label = "{plot_label}: features entered (Dev mode)",
                       {
                       req(features())

                       print(glue("Value of features ({plot_type})"))
                       print(features())
                     })
                   }
                 }

                 ## 11.2. Construct Plot ####
                 # Plot created based on the type specified when this server
                 # function is called
                 if (plot_type == "dimplot"){
                   ### 11.2.1. DimPlot ####
                   plot <-
                     reactive(
                       label = glue("{plot_label}: Create Plot"),
                       {
                         # Only runs when the plot is enabled
                         req(plot_switch())

                         # Create a UMAP plot using shiny_umap()
                         shiny_umap(
                           object = object(),
                           group_by = plot_selections$group_by(),
                           split_by = plot_selections$split_by(),
                           show_label = 
                             if (plot_selections$group_by() == 
                                 plot_selections$split_by()){
                               FALSE
                             } else {
                               plot_selections$label()
                             },
                           show_legend = plot_selections$legend(),
                           ncol = plot_selections$ncol(),
                           is_subset = is_subset(),
                           original_limits = plot_selections$limits(),
                           # Original x- and y- axis limits: use the values
                           # for the currently selected reduction
                           xlim_orig =
                             lim_orig()[[
                               plot_selections$reduction()]]$xlim_orig,
                           ylim_orig =
                             lim_orig()[[
                               plot_selections$reduction()]]$ylim_orig,
                           show_title =
                             # show_title controls how NULL values for
                             # plot_title are interpreted (NULL will remove the
                             # label by default, but plot_title will be NULL if
                             # a label is not set in the config file (want the
                             # default title to be used in this case))
                             if (input$title_settings == "none"){
                               FALSE
                             } else TRUE,
                           # Plot title: from 3.7.
                           plot_title = plot_title(),
                             # Human-readable plot title
                             # Depends on title_settings
                             # if (isTruthy(input$title_settings)){
                             #   if (input$title_settings == "default"){
                             #     # Default behavior is to use the `label` property
                             #     # for the category in the config file
                             #     metadata_config()[[
                             #       plot_selections$group_by()]]$label
                             #   } else if (input$title_settings == "custom"){
                             #     # Use the custom title entered by the user
                             #     # in this case
                             #     custom_title_single()
                             #   } else if (input$title_settings == "none"){
                             #     # NULL is passed to the title argument,
                             #     # removing it
                             #     NULL
                             #   }
                             # } else{
                             #   # Use default behavior if selection menu
                             #   # does not exist
                             #   metadata_config()[[
                             #     plot_selections$group_by()]]$label
                             # },
                           reduction = plot_selections$reduction(),
                           palette = palette(),
                           # Number of columns in legend
                           legend_ncol = plot_selections$legend_ncol(),
                           # Legend font size
                           legend_font_size =
                             plot_selections$legend_font_size(),
                           # Size of legend keys
                           legend_key_size =
                             plot_selections$legend_key_size(),
                           # Vertical spacing between each key
                           legend_key_spacing =
                             plot_selections$legend_key_spacing()
                           )
                     })

                 } else if (plot_type == "feature") {
                   ### 11.2.2. Feature Plot ####
                   plot <-
                     reactive(
                       label = glue("{plot_label}: Create Plot"),
                       {
                         # Only runs when the plot is enabled
                         req(plot_switch())
                         
                         # Also, only runs if all features 
                         # exist in the current object
                         # This avoids errors observed when switching 
                         # between objects (#115)
                         valid_keyed_features <-
                           valid_features() %>% 
                           unlist() %>% 
                           unname() 
                         
                         req(
                           all(features() %in% valid_keyed_features)
                           )

                         # Feature plot using arguments relevant to
                         # shiny_feature()
                         shiny_feature(
                           object = object(),
                           features_entered = features(),
                           assay_config = assay_config(),
                           raw_feature_names = raw_feature_names(),
                           # Group by: influences label placement
                           group_by = plot_selections$group_by(),
                           split_by = plot_selections$split_by(),
                           blend = plot_selections$blend(),
                           order = plot_selections$order(),
                           show_label = plot_selections$label(),
                           show_legend = plot_selections$legend(),
                           ncol = plot_selections$ncol(),
                           super_title = plot_selections$super_title(),
                           share_scale = plot_selections$share_scale(),
                           color_by_feature =
                             plot_selections$color_by_feature(),
                           is_subset = is_subset(),
                           legend_title = plot_selections$legend_title(),
                           original_limits = plot_selections$limits(),
                           # Original x- and y- axis limits: use the values
                           # for the currently selected reduction
                           xlim_orig =
                             lim_orig()[[plot_selections$reduction()]]$xlim_orig,
                           ylim_orig =
                             lim_orig()[[plot_selections$reduction()]]$ylim_orig,
                           palette =
                             if (plot_selections$color_by_feature() == FALSE){
                               # Use custom colors if defined; if not, use the
                               # palette if defined; if not, pass NULL to use
                               # Seurat defaults.
                               if (
                                 isTruthy(plot_selections$min_color()) &
                                 isTruthy(plot_selections$max_color())
                               ){
                                 c(plot_selections$min_color(),
                                   plot_selections$max_color())
                               } else if (isTruthy(palette())) {
                                 palette$continuous_palette()
                               } else NULL
                             } else {
                               palette$categorical_palette()
                             },
                           # Palette for blended feature plots
                           blend_palette =
                             # Use value from blend palette, unless custom
                             if (plot_selections$blend_palette() != "custom"){
                               # Input value in plot_selections represents the
                               # name of the palette. The actual value is
                               # retrieved here
                               blend_palettes[[plot_selections$blend_palette()]]
                             } else {
                               # For custom palettes, use the color inputs
                               if (
                                 isTruthy(plot_selections$custom_blend_palette())
                               ){
                                 plot_selections$custom_blend_palette()
                                 # If the inputs are not initialized, use the
                                 # default palette
                               } else NULL
                             },
                           blend_layout = plot_selections$blend_layout(),
                           reduction = plot_selections$reduction(),
                           show_title =
                             if (input$title_settings == "none") FALSE else TRUE,
                           # From 3.5.
                           custom_titles = variable_length_custom_title()
                           )
                         })

                 } else if (plot_type == "violin") {
                   ### 11.2.3. Violin Plot ####
                   plot <-
                     reactive(
                       label = glue("{plot_label}: Create Plot"),
                       {
                         # Only runs when the plot is enabled
                         req(plot_switch())
                         
                         # Also, only runs if all features 
                         # exist in the current object
                         # This avoids errors observed when switching 
                         # between objects (#115)
                         valid_keyed_features <-
                           valid_features() %>% 
                           unlist() %>% 
                           unname() 
                         
                         req(
                           all(features() %in% valid_keyed_features)
                           )

                         # Violin plot using arguments relevant to shiny_vln()
                         shiny_vln(
                           object = object(),
                           features_entered = features(),
                           group_by = plot_selections$group_by(),
                           split_by = plot_selections$split_by(),
                           show_legend = plot_selections$legend(),
                           ncol = plot_selections$ncol(),
                           palette = palette(),
                           sort_groups = plot_selections$sort_groups(),
                           set_title = 
                             # Set title based on features entered, removing the
                             # assay key and adding the suffix from the config
                             # file, unless the user requests it not be added
                             if (raw_feature_names() == FALSE){
                               sapply(
                                 features(),
                                 function(feature, assay_config){
                                   hr_name(
                                     machine_readable_name = feature, 
                                     assay_config = assay_config,
                                     use_suffix = TRUE
                                   )
                                 },
                                 assay_config()
                               )
                             } else {
                               features()
                             },
                           custom_factor_levels =
                             plot_selections$custom_refactoring(),
                           # Number of columns in legend
                           legend_ncol = plot_selections$legend_ncol(),
                           # Legend font size
                           legend_font_size =
                             plot_selections$legend_font_size(),
                           # Size of legend keys
                           legend_key_size =
                             plot_selections$legend_key_size()
                           )
                       })

                 } else if (plot_type == "dot") {
                   ### 11.2.4. Dot Plot ####
                   # Dot plot using arguments relevant to shiny_dot()
                   plot <-
                     reactive(
                       label = glue("{plot_label}: Create Plot"),
                       {
                         # Only runs when the plot is enabled
                         req(plot_switch())
                         
                         # Also, only runs if all features 
                         # in features_entered are supported
                         # This avoids errors observed when switching 
                         # between objects (#115)
                         valid_keyed_features <-
                           valid_features() %>% 
                           unlist() %>% 
                           unname() 
                         
                         req(
                           all(features() %in% valid_keyed_features)
                           )

                         shiny_dot(
                           object = object(),
                           # Features argument: uses value returned by reactive
                           features = features(),
                           # use_separate_features =
                           #   reactive({input$use_separate_features}),
                           # separate_features =
                           #   reactive({input$separate_features}),
                           group_by = plot_selections$group_by(),
                           show_legend = plot_selections$legend(),
                           palette = palette(),
                           sort_groups = plot_selections$sort_groups(),
                           custom_factor_levels =
                             plot_selections$custom_refactoring(),
                           rename_feature_labels = dot_x_labels()
                           )
                         })
                 } else if (plot_type == "scatter"){
                   ### 11.2.5. Scatterplot ####
                   # Scatterplot using relevant inputs
                   plot <-
                     reactive(
                       label = glue("{plot_label}: Create Plot"),
                       {
                         # Only runs when the plot is enabled
                         req(plot_switch())

                         # Fetch the features to be entered in the plot
                         feature_1 <- plot_selections$scatter_1()
                         feature_2 <- plot_selections$scatter_2()
                         
                         # Also only run if all features 
                         # exist in the current object
                         # This avoids errors observed when switching 
                         # between objects (#115)
                         valid_keyed_features <-
                           valid_features() %>% 
                           unlist() %>% 
                           unname() 
                         
                         req(
                           feature_1 %in% valid_keyed_features &
                             feature_2 %in% valid_keyed_features
                           )
                         
                         # Display names for features on plot: either the 
                         # raw feature name, or the "human-readable" name 
                         # constructed by removing the modality "key" and 
                         # adding the suffix for the modality defined in
                         # the config file 
                         # User sets whether to display the modality key or 
                         # not (default is FALSE, to use raw feature names)
                         if (raw_feature_names() == FALSE){
                           names(feature_1) <-
                             hr_name(
                               machine_readable_name = feature_1, 
                               assay_config = assay_config(),
                               use_suffix = TRUE
                               )
                           
                           names(feature_2) <-
                             hr_name(
                               machine_readable_name = feature_2, 
                               assay_config = assay_config(),
                               use_suffix = TRUE
                               )
                         } else {
                           names(feature_1) <- feature_1
                           names(feature_2) <- feature_2
                         }
                         
                         shiny_scatter(
                           object = object(),
                           feature_1 = feature_1,
                           feature_2 = feature_2,
                           group_by = plot_selections$group_by(),
                           show_legend = plot_selections$legend(),
                           display_coeff = plot_selections$display_coeff(),
                           palette = palette(),
                           assay_config = assay_config(),
                           # Number of columns in legend
                           legend_ncol = plot_selections$legend_ncol(),
                           # Legend font size
                           legend_font_size =
                             plot_selections$legend_font_size(),
                           # Size of legend keys
                           legend_key_size =
                             plot_selections$legend_key_size()
                           )
                       })
                 } else if (plot_type == "ridge"){
                   ### 11.2.6. Ridge Plot ####
                     plot <-
                       reactive(
                         label = glue("{plot_label}: Create Plot"),
                         {
                           # Only runs when the plot is enabled
                           req(plot_switch())
                           
                           # Also, only runs if all features 
                           # exist in the current object
                           # This avoids errors observed when switching 
                           # between objects (#115)
                           valid_keyed_features <-
                             valid_features() %>% 
                             unlist() %>% 
                             unname() 
                           
                           req(
                             all(features_entered() %in% valid_keyed_features)
                             )

                           shiny_ridge(
                             object = object(),
                             features_entered = features_entered(),
                             group_by = plot_selections$group_by(),
                             show_legend = plot_selections$legend(),
                             palette = palette(),
                             xlim =
                               # Use custom limits if the user requests them,
                               # and if they are provided
                               if (!is.null(custom_xlim) &
                                   isTruthy(input$use_custom_xlim)){
                                 custom_xlim()
                               } else {
                                 NULL
                               },
                             sort_groups = plot_selections$sort_groups(),
                             custom_factor_levels =
                               plot_selections$custom_refactoring(),
                             set_title = variable_length_custom_title(),
                             assay_config = assay_config(),
                             # Number of columns in legend
                             legend_ncol = plot_selections$legend_ncol(),
                             # Legend font size
                             legend_font_size =
                               plot_selections$legend_font_size(),
                             # Size of legend keys
                             legend_key_size =
                               plot_selections$legend_key_size()
                             )
                           }
                         )
                 } else if (plot_type == "proportion"){
                   ### 11.2.7. Stacked bar plot ####
                   # For cell type (and other metadata) proportions
                   # Compares proportions of one cell-level metadata category
                   # according to the levels of another category
                   plot <-
                     reactive(
                       label = glue("{plot_label}: Create Plot"),
                       {
                         # Only runs when the plot is enabled
                         req(plot_switch())

                         cellDIVER:::shiny_stacked_bar(
                           object = object(),
                           group_by = plot_selections$group_by(),
                           split_by = plot_selections$split_by(),
                           x_axis_title = proportion_x_axis_title(),
                           show_legend = plot_selections$legend(),
                           show_title =
                             # show_title controls how NULL values for
                             # plot_title are interpreted (NULL will remove the
                             # label by default, but plot_title will be NULL if
                             # a label is not set in the config file (want the
                             # default title to be used in this case))
                             if (input$title_settings == "none"){
                               FALSE
                             } else TRUE,
                           # Plot title: from 3.7.
                           plot_title = plot_title(),
                           palette = palette(),
                           sort_groups = plot_selections$sort_groups(),
                           custom_factor_levels =
                             plot_selections$custom_refactoring(),
                           # Number of columns in legend
                           legend_ncol = plot_selections$legend_ncol(),
                           # Legend font size
                           legend_font_size =
                             plot_selections$legend_font_size(),
                           # Size of legend keys
                           legend_key_size =
                             plot_selections$legend_key_size()
                           )
                       })
                 } else if (plot_type == "pie"){
                   ### 11.2.8. Metadata pie chart ####
                   # Currently used for patient/sample-level metadata, but could
                   # also be used for cell-level metadata
                   plot <-
                     reactive(
                       label = glue("{plot_label}: Create Plot"),
                       {
                         # Only runs when the plot is enabled
                         req(plot_switch())

                         cellDIVER:::shiny_pie(
                           object = object(),
                           patient_colname = patient_colname(),
                           group_by = plot_selections$group_by(),
                           palette = palette(),
                           show_legend = plot_selections$legend(),
                           show_title =
                             # show_title controls how NULL values for
                             # plot_title are interpreted (NULL will remove the
                             # label by default, but plot_title will be NULL if
                             # a label is not set in the config file (want the
                             # default title to be used in this case))
                             if (input$title_settings == "none"){
                               FALSE
                             } else TRUE,
                           # Plot title: from 3.7.
                           plot_title = plot_title()
                         )
                       })
                   }

                 ## 11.3. Render plot ####
                 # Height and width arguments are left undefined
                 # If undefined, they will use the values from plotOutput, which
                 # respond to the manual dimensions inputs.
                 output$plot <-
                   renderPlot({
                     # Only runs when the plot is enabled
                     req(plot_switch())

                     # Violin and proportion plots: plots behave very strangely
                     # when the group by and split by variables are the same.
                     # Instead of displaying the plots, an error message is 
                     # shown. This used to also apply to DimPlots, but doing so
                     # wasn't warrented, and the issues cause with DimPlots 
                     # ended up being due to `label` being TRUE on plots with 
                     # identical group by and split by selections (issue #309)
                     if (plot_type %in% c("violin", "proportion")){
                       validate(
                         need(
                           input$group_by != input$split_by,
                           message =
                             if (plot_type %in% c("dimplot", "violin")){
                               glue(
                                 'Invalid selections for {plot_label}:
                                 "Group By" and "Split By" must be different.'
                                 )
                             } else if (plot_type == "proportion") {
                               # For cell type proportion plots, group by and
                               # split by are renamed "proportions" and
                               # "proportion comparison", respectively.
                               glue(
                                 'Invalid selections for {plot_label}:
                                 "Proportions" and "Proportion Comparison"
                                 must be different.'
                                 )
                               }
                           )
                         )
                       }

                     if (!is.null(plots_tab_spinner)){
                       # Show spinner over main window in plots tab,
                       # unless the spinner has already been shown
                       isolate({
                         if (session$userData$plots_tab_spinner$shown == FALSE){
                           plots_tab_spinner$show()

                           # Record the scroll position of the plots tab window to
                           # Restore the position after the window is hidden and
                           # re-shown
                           shinyjs::js$getTopScroll(
                             # First parameter: target ID to get scroll position
                             session$userData$plots_tab_main_panel_id,
                             # Second parameter: input ID to record scroll position
                             # This is currently recorded outside of this module
                             # since there should only be one value for the
                             # scroll position, and if the value was defined in
                             # the context of this module there would be
                             # multiple copies of the same value.

                             # This is not best shiny practice however, and may
                             # need to be revisited.
                             "plots-topscroll"
                           )
                         }
                       })

                       # Hide container to keep user from being able to scroll
                       # underneath the spinner
                       # jQuery selector for the container to hide
                       plots_tab_container = '[id$="plot_output_ui"]'

                       shinyjs::hide(
                         selector = plots_tab_container
                         )

                       session$userData$plots_tab_spinner$shown <- TRUE

                       onFlush(
                         fun =
                           function(){
                             # Hide spinner
                             plots_tab_spinner$hide()

                             # Restore plots tab container
                             plots_tab_container = '[id$="plot_output_ui"]'

                             shinyjs::show(
                               selector = plots_tab_container
                               )

                             session$userData$plots_tab_spinner$shown <- FALSE

                             # Trigger restore of container scroll position
                             scroll_restore$trigger()
                             },
                         session = session
                         )
                     }
                     plot()
                   })

                 # 12. Restore scroll position of plot tab window --------------
                 observe(
                   label = glue("{id}: Restore scroll position"),
                   {
                     scroll_restore$depend()

                     shinyjs::js$setTopScroll(
                       # First parameter: target ID to get scroll position
                       session$userData$plots_tab_main_panel_id,
                       # Second parameter: input ID to retrieve scroll position
                       # from. This fetches the input value via Javascript
                       # instead of via `input` in Shiny, and uses an input ID
                       # defined outside of a module. This may need to be
                       # revisited if it causes bugs.
                       "plots-topscroll"
                     )
                     })

                 # 13. Custom x-axis limits server (ridge plots) ---------------
                 # Server recieves plot in 9. and outputs the chosen limits
                 if (plot_type == "ridge"){
                   custom_xlim <-
                     custom_xlim_server(
                       id = "custom_xlim",
                       plot = plot
                     )
                 }

                 # 14. Download handler ----------------------------------------
                 # Note: SVG export uses ggsave(device = "svg"), which routes
                 # through the svglite package when it is installed. svglite is
                 # listed in Imports for this reason, even though it is never
                 # called directly.
                 output$confirm_download <-
                   downloadHandler(
                     # Filename: takes the label and replaces
                     # spaces with underscores
                     filename = function(){
                       if (input$file_type == "png"){
                         glue("{sub(' ','_',plot_label)}.png")
                       } else if (input$file_type == "svg"){
                         glue("{sub(' ','_',plot_label)}.svg")
                         }
                       },
                     content = function(file){
                       # Conditional: manual dimensions are specified
                       if (
                         (isTruthy(manual_dim$width())) &
                         (isTruthy(manual_dim$height()))
                       ){
                         # If manual dimensions are specified, apply them to
                         # height and width arguments
                         ggsave(
                           file,
                           plot = plot(),
                           # Either 'png' or 'svg'
                           device = input$file_type,
                           width = manual_dim$width(),
                           height = manual_dim$height(),
                           # Set dpi to selected value so proportions of 
                           # downloaded plot match the plot in the app
                           dpi = manual_dim$dpi(),
                           units = "in",
                           # Set background color to white (background is
                           # transparent on some plots)
                           bg="#FFFFFF"
                         )
                       } else {
                         ggsave(
                           file,
                           plot = plot(),
                           # Default width/height: use a 3:2 ratio
                           # Values are in inches
                           width = 9,
                           height = 6,
                           # Either 'png' or 'svg'
                           device = input$file_type,
                           # Set background color to white (background is
                           # transparent on some plots)
                           bg ="#FFFFFF"
                         )
                       }
                     },#End content function
                     contentType =
                       # contentType: uses MIME types
                       if (input$file_type == "png"){
                         "image/png"
                         } else if (input$file_type == "svg"){
                           "image/svg+xml"
                           }
                   ) #End downloadHandler function

                 })
  }
