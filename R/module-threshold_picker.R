#' Theshold picker (UI)
#' 
#' Creates a widget used to choose an expression threshold for a feature. An 
#' interactive ridge plot is displayed, and the user clicks to chose a threshold.
#' When a threshold is chosen, statistics on the threshold will be displayed 
#' beneath the plot.
#'
#' @param id ID to use for module elements.
#' @param plot_width width of the plot, in pixels. If NULL, the width defaults 
#' to 100% of the parent container.
#' @param plot_height height of the plot, in pixels.
#' @param buttons_panel if TRUE, buttions for additional options, such as 
#' resizing the plot, will be displayed on the plot.
#' @param instruction_panel if TRUE, displays a guide saying what is being 
#' selected (either a threshold or the upper/lower bounds of a range). The 
#' default is FALSE, though TRUE is strongly recommended if the "range" mode is
#' intended to be used with the module instance.
#' @param show_stats Whether to show stats for the selected threshold or range
#' (default is TRUE). If FALSE, only the selected threshold will be displayed.
#'
#' @noRd
threshold_picker_ui <- 
  function(
    id,
    plot_width = NULL,
    plot_height = NULL,
    buttons_panel = TRUE,
    instruction_panel = FALSE,
    show_stats = TRUE
    ){
    # Namespace function: prevents conflicts with IDs defined in other modules
    ns <- NS(id)
    
    tagList(
      # Plot is hidden until a feature is passed to the module 
      hidden(
        div(
          id = ns("ridge_plot_ui"),
          # Curve the top, or the bottom of the plot, or both, based on the
          # indicator/button panels displayed above/below the plot
          class = 
            if (instruction_panel == FALSE & buttons_panel == FALSE){
              "plot-curve-all"
            } else if (instruction_panel == TRUE & buttons_panel == FALSE){
              "plot-curve-bottom"
            } else if (instruction_panel == FALSE & buttons_panel == FALSE){
              "plot-curve-top"
            } else {
              # If both panels are enabled, don't curve plot 
              # (default, no class applied)
              ""
            },
          # If enabled, indicator panel to display above the plot
          if (instruction_panel == TRUE){
            div(
              class = "plot-button-panel-top",
              # Container displaying the selection mode to the user
              div(
                class = "interactive-plot-instruction",
                textOutput(
                  outputId = ns("selection_display")
                )
              )
            )
          },
          # Ridge plot showing histogram of feature expression in current object
          plotOutput(
            outputId = ns("ridge_plot"),
            # Shiny Default width: 100% of parent element
            width = if (!is.null(plot_width)) plot_width else "100%",
            # Shiny Default height: 400px
            height = if (!is.null(plot_height)) plot_height else "400px",
            # Draw vertical line upon user click
            # Adding "plot_hover" will create `input$plot_hover`.
            # A vertical line will be drawn according to the x coordinate 
            # corresponding to the pointer location 
            hover = 
              hoverOpts(
                id = ns("plot_hover"),
                # Hover response will re-compute every 300ms, as long as cursor 
                # is within bounds of plot (throttle behavior)
                delay = 300,
                delayType = "throttle"
              ),
            click =
              clickOpts(
                id = ns("plot_click")
              )
            ),
          div(
            class = "plot-button-panel",
            # Indicator displaying hover position
            div(
              #style = "float: left",
              class = "plot-panel-indicator",
              textOutput(
                outputId = ns("hover_position")
                ) 
              ),
            # Container for buttons displaying on right hand side of plot
            div(
              style =
                "float: right;
                 display: flex;",
              # Apply a fix for dropup menus not appearing to the left
              class = "flex-dropDownleftFix",
              # Interface for adjusting x-axis limits
              div(
                dropdownButton(
                  # Space prevents "btn-" from being added to first class
                  status = 
                    " icon-button plot-button plot-button-dropdown 
                    plot-dropDownButton",
                  size = "xs",
                  tooltip = "Adjust x-axis limits",
                  right = FALSE,
                  up = TRUE,
                  icon = icon("arrows-alt-h"),
                  tagList(
                    div(
                      class = "inline-containers",
                      textInput(
                        inputId = ns("lower_xlim"),
                        label = "Lower Bound:",
                        # Default value is filled in the server function
                        value = NULL
                      ),
                      textInput(
                        inputId = ns("upper_xlim"),
                        label = "Upper Bound:",
                        value = NULL
                      )
                    ),
                    actionButton(
                      inputId = ns("apply_xlim"),
                      class = "button-primary float-right",
                      style = "margin-left: 10px;",
                      label = "Apply"
                    ),
                    actionButton(
                      inputId = ns("restore_xlim"),
                      class = "button-ghost float-right",
                      icon = icon("undo-alt"),
                      label = "Restore Original"
                    )
                  )
                )
              ),
              # Drop-up to modify range: only displays during range mode, 
              # once a range has been chosen.
              hidden(
                div(
                  id = ns("range-edit-dropdown"),
                  class = "edit-bounds-dropdown",
                  dropdownButton(
                    # Button ID: used to close menu when containing 
                    # buttons are pressed
                    inputId = ns("range-edit"),
                    # status argument adds classes to button
                    # Space is intentional. dropdownButton adds "btn-" to the 
                    # beginning of the status string
                    status = 
                      " icon-button plot-button plot-button-dropdown 
                      plot-dropDownButton",
                    size = "xs",
                    tooltip = "Modify range components",
                    right = FALSE,
                    up = TRUE,
                    icon = icon("pencil-alt"),
                    tagList(
                      div(
                        class = "button-group",
                        actionButton(
                          inputId = ns("edit_lower_bound"),
                          label = "Edit lower bound",
                          class = "button-ghost first-button"
                        ),
                        actionButton(
                          inputId = ns("edit_upper_bound"),
                          label = "Edit upper bound",
                          class = "button-ghost last-button"
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        ),
      # UI to display statistics based on selected threshold 
      # Threshold statistics are hidden until a threshold is selected
      hidden(
        div(
          id = ns("threshold_stats_ui"),
          class = "compact-options-container",
          # Show selected threshold to user
          tags$b(
            class = "center",
            "Chosen threshold"
          ),
          tags$b(
            class ="center half-space-bottom",
            style = 
              "background-color: #FFFFFF; 
               border-radius: 10px;",
            textOutput(
              outputId = ns("chosen_threshold")
            )
          ),
          # Number and percentage of cells above and below threshold
          # Displays if stats are enabled
          if (show_stats == TRUE){
            tagList(
              tags$b(
                "Number of cells above threshold:"
              ),
              textOutput(
                outputId = ns("above_stats")
              ),
              tags$b(
                class = "half-space-top",
                "Number of cells below threshold:"
              ),
              textOutput(
                outputId = ns("below_stats")
              )
            )
          }
        )
      )
    )
}

#' Theshold picker (Server)
#' 
#' Creates a widget used to choose an expression threshold for a feature. An 
#' interactive ridge plot is displayed, and the user clicks to chose a threshold.
#' When a threshold is chosen, statistics on the threshold will be displayed 
#' beneath the plot.
#'
#' @param id ID to use for module server instance.
#' @param object a single cell object or a subset. All object types supported 
#' by SCUBA may be passed to this module.
#' @param feature the feature for which thresholds are being chosen.
#' @param showhide_animation if TRUE, an animation will be used when displaying
#' or hiding the stats panel.
#' @param set_threshold A reactive value used to set the selected threshold to
#' a defined value. When this value changes, the threshold picker interface will
#' update to reflect a selection at the new value. It is reccomended that this 
#' be set to a reactive({}) call that points to a reactiveValues object that is
#' changed whenever new theshold/range data is loaded. To avoid bugs, this
#' variable should be changed to NULL whenever a threshold/range being edited 
#' is saved or discarded, so the reactive can properly respond to the loading 
#' of a new threshold in the event it is the same as the previous threshold.
#' @param mode a reactive variable directing the behavior of the widget (equal to 
#' "greater_than", "less_than", or "range"). The mode will affect the components 
#' shown to the user, and the interactive behavior of the plot. If the mode is 
#' "less_than" or "greater_than", a single threshold will be chosen, and if the 
#' mode is equal to "range", clicking the plot will choose the lower and upper
#' bounds of a range. If mode is not specified, the module will default to 
#' choosing a single threshold. The stats displayed will vary based on the mode.
#' @param assay_config The assays section of the config file. If passed to the
#' module, assay_config will be used to display human-readable feature names on
#' the interactive ridge plot.
#' 
#' @noRd
#'
threshold_picker_server <- 
  function(
    id,
    object,
    feature,
    showhide_animation = FALSE,
    set_threshold = NULL,
    mode = NULL,
    assay_config = NULL
    ){
    moduleServer(
      id,
      function(input, output, session){
        # Server namespacing function
        ns <- session$ns
        
        # Initialize module_data (used to store plots and results, which 
        # undergo state accumulation (some user interactions with plot are 
        # saved, and thus "accumulate")). This type of behavior requires 
        # reactiveValues objects.
        module_data <- reactiveValues()
        
        # Reset state variables when TRUE (sometimes this needs to be set to 
        # FALSE to avoid resetting state variables when a new plot is computed,
        # for example after a previous threshold/range is loaded)
        module_data$reset_state <- TRUE
        
        # When the user hovers over an interactive plot, the x-coordinate
        # returned is on a zero to one scale based on the position on the plot
        # instead of the true x-coordinate on the plot. Furthermore, zero and
        # one do not directly correspond to the lower and upper x-limits, 
        # respectively. The values below are used to convert the coordinates on
        # a 0-1 scale to the true coordinates, and were tuned to yield 
        # the most accurate coordinates possible.
        plot_min_coord <- 0.06
        plot_max_coord <- 0.96
        
        # Spinner that displays over ridge plot during initial computation
        plot_spinner <-
          Waiter$new(
            id = ns("ridge_plot"),
            html = spin_loaders(id = 2, color = "#555588"),
            color = "#B1B1B188",
            # Gives manual control of showing/hiding spinner
            hide_on_render = FALSE
            )
        
        # 1. Show plot when feature() is defined ####
        observe(
          label = glue("{id}: Show/hide Ridge Plot"),
          {
            target_id <- ns("ridge_plot_ui")

            if (isTruthy(feature())){
              showElement(
                id = target_id,
                # If showhide_animation is TRUE, animate plot (will slide 
                # into and out of existence)
                anim = showhide_animation,
                asis = TRUE
                )
            } else {
              # Hide the interactive plot
              hideElement(
                id = target_id,
                anim = showhide_animation,
                asis = TRUE
                )
            }
          })
        
        # 2. Create ridge plot upon feature change ####
        # The plot must be stored in a reactiveValues object (module_data) to
        # avoid reactivity issues when processing hover and click values. 
        # The initial plot is generated below.
        observeEvent(
          # Responds to changes in mode if it is provided (as a reactive)
          if (is.reactive(mode)){
            c(feature(), mode())
            } else {
              feature()
              },
          label = glue("{id}: Initial Ridge Plot Histogram"),
          ignoreNULL = FALSE,
          {            
            # When drawing a new plot, clear plots and data associated 
            # with the last plot if present
            module_data$initial_ridge_plot <- NULL
            module_data$ridge_plot_with_threshold <- NULL
            module_data$ridge_plot <- NULL
            
            module_data$original_xlim <- NULL
            module_data$threshold_stats <- NULL
            # Record hover position for display in plot panel
            module_data$hover_position <- NULL
            
            # State variables set by selecting or loading a threshold
            if (module_data$reset_state == TRUE){
              # Only reset these variables when reset_state == TRUE. This is set
              # to FALSE when data is loaded into the module
              module_data$threshold_x <- NULL
              
              # Variables only used when the mode is equal to "range"
              module_data$lower_bound <- NULL
              module_data$upper_bound <- NULL
              module_data$selection_mode <- "lower"
              # Used to show/hide edit menu for bounds of range
              module_data$show_range_edit <- FALSE
            } else {
              # If FALSE, do nothing and set state back to TRUE so the next 
              # triggering event resets the state selections
              module_data$reset_state <- TRUE
            }
            
            # Hide the menu to modify bounds of a range when the plot is 
            # first loaded, or when it is re-computed
            hideElement(
              id = "range-edit-dropdown"
              )
            
            # Set module_data$ridge_plot to the plot if a feature is entered
            # in the search bar, otherwise set the plot to NULL
            if (isTruthy(feature())){
              # Prevent unexpected behavior if more than one feature 
              # is specified
              req(length(feature()) == 1)
              
              # Show spinner over plot during computation
              plot_spinner$show()
              
              data <- 
                SCUBA::fetch_data(
                  object = object(),
                  vars = feature()
                )
              
              # Get minimum and maximum values of data
              module_data$plot_max <- 
                data |> 
                max()
              
              module_data$plot_min <-
                data |> 
                min()
              
              # The coordinates returned from hover and click events will
              # be multiplied by the range to allow the user to properly select
              # a region on the plot
              # This is a workaround, it would be ideal to find out why Shiny
              # interactive coordinates aren't working for Seurat plots
              module_data$plot_range <- 
                module_data$plot_max - module_data$plot_min
              
              module_data$initial_ridge_plot <-
                shiny_ridge(
                  object = object(), 
                  features_entered = feature(), 
                  group_by = "none", 
                  show_legend = FALSE, 
                  palette = c("#000088"),
                  center_x_axis_title = TRUE,
                  assay_config =
                    if (is.reactive(assay_config)){
                      assay_config()
                    }
                  ) 
              
              # After drawing the plot, record the original x-axis limits
              module_data$original_xlim <-
                layer_scales(module_data$initial_ridge_plot)$x$range$range
              
              # Also save to module_data$ridge_plot (this plot is the one 
              # printed to the screen)
              module_data$ridge_plot <-
                module_data$initial_ridge_plot
            } else {
              # Do not draw plot if no feature is defined
              module_data$initial_ridge_plot <-
                NULL
            }
            
            plot_spinner$hide()
          })
        
        # 3. Respond to hover event ####
        ## 3.1. Register hover coordinate ####
        # In the event the user hovers over or clicks the plot, add the 
        # corresponding vertical line at the x-coordinate of the click.
        observeEvent(
          input$plot_hover,
          label = glue("{id}: Draw Line on Hover"),
          # IgnoreNULL must be TRUE to avoid the plot computing when
          # input$plot_hover is NULL, which will erase the line from the plot
          # as soon as it is drown
          ignoreNULL = TRUE,
          ignoreInit = TRUE,
          {
            # The saved plot must be different from the initial plot, or a series
            # of vertical lines will be created each time a hover event is
            # registered

            # Transforming X-coordinates for compatibility with hover/click
            # Coordinates are incorrectly being from zero to one, with one
            # being the max value on the plot.
            plot_x_coordinate <-
              interactive_transform(
                x_coord = input$plot_hover$x,
                # Distribution_range: uses the x-axis limits of the plot,
                # as computed in 6.1.
                distribution_range = current_xlim()[2] - current_xlim()[1],
                distribution_minimum = module_data$plot_min,
                plot_min_coord = plot_min_coord,
                plot_max_coord = plot_max_coord
              )
            
            # Record hover position for display
            module_data$hover_position <- 
              # Round to 2 digits (additional precision is not necessary)
              plot_x_coordinate |> 
              round(digits = 2)
          })
        
        ## 3.2 Determine hover behavior ####
        draw_line <-
          reactive(
            label = glue("{id}: Determine hover behavior "),
            {
              # Determine whether to draw a line at the hover coordinate
              # Always draw in threshold mode, and draw in range mode if the
              # selection mode is not "none"
              behavior <-
                cellDIVER:::threshold_picker_behavior(
                  mode = mode
                )
              
              if (behavior == "threshold"){
                TRUE
              } else if (behavior == "range"){
                if (module_data$selection_mode == "none"){
                  FALSE
                } else {
                  TRUE
                }
              }
              })
        
        ## 3.3. Draw line at hover coordinate ####
        observe(
          label = glue("{id}: Draw hover line on plot"),
          {
            req(
              c(module_data$hover_position, 
                module_data$initial_ridge_plot
                )
              )
            
            if (draw_line() == TRUE){
              # Determine plot to draw hover line on
              base_plot <-
                if (!is.null(module_data$ridge_plot_with_threshold)){
                  # If the plot with a defined threshold has been drawn (after the 
                  # user clicks a plot) use that plot.
                  module_data$ridge_plot_with_threshold
                } else {
                  # Otherwise, use the inital ridge plot
                  module_data$initial_ridge_plot
                }
              
              # Draw vertical line at transformed hover coordinate
              module_data$ridge_plot <-
                base_plot +
                geom_vline(
                  xintercept = module_data$hover_position,
                  color = "#666666",
                  size = 0.75
                )
              }
            })
        
        # 4. Respond to click event ####
        ## 4.1. Record coordinates of click event ####
        observeEvent(
          input$plot_click,
          label = glue("{id}: Process click event"),
          # IgnoreNULL must be TRUE to avoid the plot computing when
          # input$plot_click is NULL, which will erase the line from the plot
          # as soon as it is drown
          ignoreNULL = TRUE,
          ignoreInit = TRUE,
          {
            # Process (transform) x-coordinate of click
            plot_x_coordinate <-
              interactive_transform(
                x_coord = input$plot_click$x, 
                # Distribution_range: uses the x-axis limits of the plot, 
                # as computed in 6.1.
                distribution_range = current_xlim()[2] - current_xlim()[1],
                distribution_minimum = module_data$plot_min, 
                plot_min_coord = plot_min_coord, 
                plot_max_coord = plot_max_coord
              )
            
            # Behavior of click depends on current mode, if provided
            behavior <-
              cellDIVER:::threshold_picker_behavior(
                mode = mode
                )
            
            req(behavior)
            if (behavior == "threshold"){
              # Record transformed x-coordinate 
              # Round threshold to two decimal places (such that the threshold 
              # displayed will be consistent with the threshold stats computed if
              # it is copied and pasted. To my knowledge, there is no need to use
              # thresholds with greater precision)
              module_data$threshold_x <- 
                plot_x_coordinate |> 
                round(digits = 2)
              
            } else if (behavior == "range"){
              # Click coordinates are recorded for the lower or upper bound
              # depending on the current selection mode
              # Selection mode initializes as "lower"  
              
              if (module_data$selection_mode == "lower"){
                # Record the lower bound
                module_data$lower_bound <-
                  plot_x_coordinate |> 
                  round(digits = 2)
                
                # Change the state of the selection based on range already
                # selected
                if (is.null(module_data$upper_bound)){
                  # Next click defines upper bound if it has not yet been chosen
                  module_data$selection_mode <- "upper" 
                } else {
                  module_data$selection_mode <- "none"
                }
                
                } else if (module_data$selection_mode == "upper"){
                  # Record the upper bound
                  module_data$upper_bound <-
                    plot_x_coordinate |> 
                    round(digits = 2)
                  
                  # Set the selection mode to "none" 
                  module_data$selection_mode <- "none"
                }
              
              # If both bounds are selected and if the upper bound is higher 
              # than the lower bound, switch the position of the bounds.
              if (!is.null(module_data$lower_bound) & 
                  !is.null(module_data$upper_bound)){
                if (module_data$lower_bound > module_data$upper_bound){
                  new_lower <- module_data$upper_bound
                  new_upper <- module_data$lower_bound
                  
                  module_data$lower_bound <- new_lower
                  module_data$upper_bound <- new_upper
                }
              }
              }
            })
        
        ## 4.2 Show stats UI when a threshold is selected ####
        observe(
          label = glue("{id}: Show/hide Threshold Stats"),
          {
            target_id <- "threshold_stats_ui"
            
            if (!is.null(module_data$threshold_x)){
              showElement(
                id = target_id
              )
            } else {
              hideElement(
                id = target_id
              )
            }
          })
        
        # 5. Draw plot according to threshold/range ####
        observe(
          label = glue("{id}: Draw vertical line on plot"),
          {
            req(module_data$initial_ridge_plot)
            req(feature())
          
          behavior <-
            cellDIVER:::threshold_picker_behavior(
              mode = mode
            )
          
          if (behavior == "threshold"){
            req(module_data$threshold_x)
            
            # Draw a single line at module_data$threshold_x()
            module_data$ridge_plot_with_threshold <-
              module_data$initial_ridge_plot +
              geom_vline(
                xintercept = module_data$threshold_x,
                color = "#000000",
                size = 0.75
              )
            
            # Update plot displayed (ridge_plot)
            module_data$ridge_plot <-
              module_data$ridge_plot_with_threshold
          } else if (behavior == "range"){
            # Draw two lines at the lower and upper bounds of the range
            # (if defined yet)
            plot <-
              module_data$initial_ridge_plot
            
            if (!is.null(module_data$lower_bound)){
              plot <-
                plot +
                geom_vline(
                  xintercept = module_data$lower_bound,
                  color = "#000000",
                  size = 0.75
                )
            }  
            
            if (!is.null(module_data$upper_bound)){
              plot <-
                plot +
                geom_vline(
                  xintercept = module_data$upper_bound,
                  color = "#000000",
                  size = 0.75
                )
            }  
            
            if (!is.null(module_data$lower_bound) & 
                !is.null(module_data$upper_bound)){
              # Show interface to edit the range
              module_data$show_range_edit <- TRUE
            }
            
            # Save plot
            module_data$ridge_plot_with_threshold <- 
              plot
            
            # Update final plot displayed
            module_data$ridge_plot <-
              module_data$ridge_plot_with_threshold
          }
        })
        
        # 6. Compute stats in response to clicking plot/loading threshold ####
        observe(
          label = glue("{id}: Compute threshold/range stats"),
          {
            req(module_data$threshold_x)
            req(feature())
            
            behavior <-
              cellDIVER:::threshold_picker_behavior(
                mode = mode
                )
            
            if (behavior == "threshold"){
              # Record threshold statistics
              module_data$threshold_stats <- 
                threshold_stats(
                  object = object(), 
                  feature = feature(), 
                  threshold = module_data$threshold_x
                )
            }
          })
        
        # 7. Update a plot with a previously defined threshold ####
        if (is.reactive(set_threshold)){
          # Observer is created only when set_threshold is defined as a 
          # reactive variable (set_threshold is a non-reactive NULL by default,
          # since it is not used on all applications of this module)
          observe(
            label = glue("{id}: Update plot with previously defined threshold"),
            {
              req(set_threshold())
              req(feature())
              if (is.reactive(mode)){
                req(mode())
              }
              
              # Determine behavior based on "mode"
              behavior <-
                cellDIVER:::threshold_picker_behavior(
                  mode = mode
                )
              
              # Prevent state variables from resetting when a new plot is 
              # drawn for the feature loaded.
              module_data$reset_state <- FALSE
              
              if (behavior == "threshold"){
                # Warn the user if set_threshold() is not a one-element vector
                if (length(set_threshold()) != 1){
                  warning(
                    glue(
                      'unexpected length ({length(set_threshold())}) for
                      set_threshold() in {id}. A one-element vector is expected
                      since the mode is currently "threshold". unexpected
                      behavior may result.'
                    )
                  )
                }
                
                # When a new value is passed to set_threshold, set the threshold
                # to the new value
                module_data$threshold_x <- set_threshold()
              } else if (behavior == "range") {
                # If a range is passed instead:
                # Test if the set_threshold value is a two-element vector
                # warn user if not (errors will likely result)
                if (length(set_threshold()) != 2){
                  warning(
                    glue(
                      "set_threshold() in {id} is not equal to a two-element
                      character vector, but range mode is enabled. Unexpected
                      behavior may result."
                      )
                    )
                }
                
                # Set values of lower and upper bound to the first and second
                # values of set_threshold(), respectively.
                module_data$lower_bound <- set_threshold()[1]
                module_data$upper_bound <- set_threshold()[2]
                
                # Set editing state to "none" (user may not edit bounds until 
                # they press the edit button)
                module_data$selection_mode <- "none"
              }
              
            })
          }
        
        # 8. Adjusting X-Axis Limits of plot ####
        ## 8.1. Current X-axis limits ####
        current_xlim <- 
          reactive(
            label = glue("{id}: Current X-Axis Limits of Plot"),
            {
              # Fetch X-axis limits from ggplot object (current plot, whenever
              # *the base plot* is re-drawn (when the feature is changed, or 
              # when x-axis limits are changed, currently))
              req(module_data$initial_ridge_plot)
              
              plot <- module_data$initial_ridge_plot
              
              # If the scale of the plot has been adjusted, the value of 
              # plot$coordinates$default will be FALSE.
              if (!plot$coordinates$default){
                # When the plot has been modified using coord_cartesan, the 
                # limits must be accessed using the limits element of the 
                # coordinates list stored in the ggplot2 object for the plot. 
                plot$coordinates$limits$x
              } else {
                # Otherwise, use the default limits for the data, which are 
                # accessed using layer_scales.
                layer_scales(module_data$initial_ridge_plot)$x$range$range
              }
            })
        
        ## 8.2. Update text entry of limits to reflect current values ####
        observe(
          label = glue("{id}: Update Text Entry of Limits"),
          {
            # Observer will update whenever the plot axes are re-drawn (when 
            # current_xlim changes)
            updateTextInput(
              session = session,
              inputId = "lower_xlim", 
              value = current_xlim()[1]
              )
            
            updateTextInput(
              session = session,
              inputId = "upper_xlim", 
              value = current_xlim()[2]
              )
            })
        
        ## 8.3. Respond to apply limits button ####
        # Re-draw Plot With New Limits 
        observeEvent(
          input$apply_xlim,
          label = glue("{id}: Draw plot with updated x limits"),
          ignoreNULL = FALSE,
          ignoreInit = TRUE,
          {
            req(feature())
            
            # Construct plot with new limits
            plot <-
              suppressWarnings(
                shiny_ridge(
                  object = object(), 
                  features_entered = feature(), 
                  group_by = "none", 
                  show_legend = FALSE, 
                  palette = c("#000088"),
                  center_x_axis_title = TRUE,
                  # Pass x-axis limits as a two-element vector
                  xlim = 
                    c(as.numeric(input$lower_xlim), as.numeric(input$upper_xlim))
                )
              )
            
            # Save plot to module_data$initial_ridge_plot
            module_data$initial_ridge_plot <- 
              plot
            
            # If a threshold is defined, add a vertical line to the plot at
            # the threshold and save to that plot to
            # module_data$ridge_plot_with_threshold (both plots must be updated
            # in this case)
            if (!is.null(module_data$threshold_x)){
              module_data$ridge_plot_with_threshold <-
                plot +
                geom_vline(
                  xintercept = module_data$threshold_x,
                  color = "#000000",
                  size = 0.75
                )
              
              # Update the plot that is printed via the render function in 7.
              # (otherwise the plot will not update on-screen until the user 
              # hovers over it)
              module_data$ridge_plot <- module_data$ridge_plot_with_threshold
            } else {
              # No threshold: update printed plot without a vertical line
              module_data$ridge_plot <- module_data$initial_ridge_plot
            }
          })
        
        ## 8.4. Respond to restore limits button ####
        observeEvent(
          input$restore_xlim,
          ignoreNULL = FALSE,
          ignoreInit = TRUE,
          {
            req(feature())
            
            # Re-draw plot with the original limits 
            plot <-
              suppressWarnings(
                shiny_ridge(
                  object = object(), 
                  features_entered = feature(), 
                  group_by = "none", 
                  show_legend = FALSE, 
                  palette = c("#000088"),
                  center_x_axis_title = TRUE,
                  # Pass NULL to use the default limits for the data
                  xlim = NULL
                  )
                )
            
            # Save plot to module_data$initial_ridge_plot
            module_data$initial_ridge_plot <- 
              plot
            
            # If a threshold is defined, draw the vertical line and then also 
            # save to module_data$ridge_plot_with_threshold
            if (!is.null(module_data$threshold_x)){
              module_data$ridge_plot_with_threshold <-
                plot +
                geom_vline(
                  xintercept = module_data$threshold_x,
                  color = "#000000",
                  size = 0.75
                )
              
              # Update the plot that is printed via the render function in 7.
              # (otherwise the plot will not update on-screen until the user 
              # hovers over it)
              module_data$ridge_plot <- module_data$ridge_plot_with_threshold
            } else {
              # No threshold: update printed plot without a vertical line
              module_data$ridge_plot <- module_data$initial_ridge_plot
              }
            })
        
        # 9. Adjusting Range ####
        ## 9.1. Show/hide adjustment interface ####
        # Display interface to edit thresholds
        observe({
          target_id <- "range-edit-dropdown"
          
          if (isTruthy(module_data$show_range_edit)){
            showElement(
              id = target_id
            )
          } else {
            hideElement(
              id = target_id
            )
          }
        })
        
        ## 9.2. Edit lower bound ####
        observeEvent(
          input$edit_lower_bound,
          label = glue("{id}: modify lower bound of range"),
          {
            # Change state to lower
            module_data$selection_mode <- "lower"
            
            # Close dropdown menu
            shinyWidgets::toggleDropdownButton(
              inputId = "range-edit"
              )
            })
        
        ## 9.3. Edit upper bound ####
        observeEvent(
          input$edit_upper_bound,
          label = glue("{id}: modify lower bound of range"),
          {
            # Change state to upper
            module_data$selection_mode <- "upper"
            
            # Close dropdown menu
            shinyWidgets::toggleDropdownButton(
              inputId = "range-edit"
            )
          })
        
        # 10. Render plot and statistics ####
        # Plot
        output$ridge_plot <-
          renderPlot({
            # Must use suppressMessages and print(plot) to suppress a
            # "Picking joint bandwidth of ___" message from ggridges, which
            # overwhelms the console when interactive hovering is used
            # SuppressMessages alone is not enough, print(plot) must also be
            # used. See https://github.com/tidyverse/ggplot2/issues/1101
            suppressMessages(
              print(module_data$ridge_plot)
            )
          })

        # Hover position indicator
        output$hover_position <-
          renderText({
            if (isTruthy(module_data$hover_position)){
              paste0(
                "Position: ",
                module_data$hover_position
                )
              }
          })
        
        # Instructions for current selection
        output$selection_display <-
          renderText({
            # Type of instruction to display to the user:
            # Default to no instruction in cases not covered by below conditional
            instruction_display <- "blank"
            
            # Determine type of instruction to dispay to user
            # depends on state of the interface mode, and the selection mode
            if (is.reactive(mode)){
              if (mode() == "range"){
                # For range mode: display instruction based on the threshold 
                # bound being selected
                if (module_data$selection_mode == "lower"){
                  instruction_display <- "lower"
                } else if (module_data$selection_mode == "upper"){
                  instruction_display <- "upper"
                } else if (module_data$selection_mode == "none"){
                  # Blank instruction when the mode is "none" (after both 
                  # ends of range are chosen)
                  instruction_display <- "blank"
                }
              } else {
                # If mode is not a range, use simple threshold instructions
                instruction_display <- "threshold"
              }
            } else {
              # If the mode is not provided to the server instance, use
              # instructions for a simple threshold
              instruction_display <- "threshold"
            }
            
            # Display text based on instruction_display
            if (instruction_display == "threshold"){
              "Select threshold"
            } else if (instruction_display == "lower"){
              "Select lower bound"
            } else if (instruction_display == "upper"){
              "Select upper bound"
            } else if (instruction_display == "blank"){
              ""
            }
          })
        
        # Chosen threshold
        output$chosen_threshold <-
          renderText({
            format(
              module_data$threshold_x,
              # Display at least three sig figs in percentage
              digits = 3,
              # Display at least two digits after decimal point
              nsmall = 2,
              scientific = FALSE
              )
          })
        
        # Number and percentage of cells above and below threshold
        output$above_stats <- 
          renderText({
            glue(
              "{module_data$threshold_stats$n_above}
               ({module_data$threshold_stats$percent_above}%)"
              )
            })
        
        output$below_stats <-
          renderText({
            glue(
              "{module_data$threshold_stats$n_below}
              ({module_data$threshold_stats$percent_below}%)"
              )
            })
        
        # 11. Return chosen threshold/range ####
        # Package into a reactive value for consistency with other modules
        reactive({
          # Determine if the module is selecting a single threshold or a range
          behavior <-
            cellDIVER:::threshold_picker_behavior(
              mode = mode
            )
          
          if (behavior == "range"){
            # Range: return a two-length vector of lower and upper bounds
            c(module_data$lower_bound,
              module_data$upper_bound)
          } else if (behavior == "threshold"){
            # Threshold: return the chosen threshold
            module_data$threshold_x
            }
          })
      })
}
