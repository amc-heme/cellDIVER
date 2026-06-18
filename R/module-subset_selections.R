#' Subset Selections Module UI
#'
#' The subset selections module displays menus for selecting the current subset.
#' It is currently used in all three tabs of the app.
#'
#' @param id id to use for the module.id to use for module namespacing. 
#' UI-server module pairs must use the same id. 
#' @param unique_metadata A list of the unique metadata values for each of the 
#' metadata categories listed in the config file. This is generated in the 
#' main server function.
#' @param metadata_config The metadata section of the config file. This does not 
#' need to be specified if the config list is stored as "config" in the global 
#' environment.
#' @param auto_dictionary_path Path to the temporary file used for storing the
#' auto-generated object dictionary (created in the main app)
#' @param string_subsetting_href URL of the string subsetting vignette.
#'
#' @return UI code for the subset selections module.
#' 
#' @noRd
subset_selections_ui <- 
  function(
    id,
    unique_metadata,
    metadata_config,
    auto_dictionary_path,
    string_subsetting_href
    ){
    # Namespace function: prevents conflicts with IDs defined in other modules 
    ns <- NS(id)
    
    div(
      id = ns("subset_selections_all"),
      style = "margin-bottom: 10px;",
      tags$b(
        "Filters Chosen for Subset:"
      ),
      uiOutput(
        outputId = ns("filters_applied")
      ),
      # Container for add filter and reset all filter buttons
      div(
        actionButton(
          inputId = ns("add_filter"),
          label = "Add Filter",
          icon = icon("plus"),
          # show-on-idle: button will be hidden while a filter is 
          # being created/edited
          class = 
            "button-primary compact-button show-on-idle half-space-bottom",
          style = "font-size: 0.95em;"
          ),
        # Reset all filters button: shown when at least one filter is present
        hidden(
          actionButton(
            inputId = ns("reset_all_filters"),
            label = "Reset All",
            icon = icon("times-circle"),
            class = 
              "button-ghost compact-button show-on-idle half-space-bottom",
            style = "font-size: 0.95em;"
            )
          )
      ),
      # Download button goes on next line (hidden if dev mode is disabled)
      div(
        downloadButton(
          outputId = ns("download_filters"),
          label = "Download Filters",
          class = 
            "button-ghost compact-button show-on-idle half-space-bottom",
          style = "font-size: 0.95em;"
        )
      ),
      hidden(
        # UI for adding/editing a subsetting filter
        div(
          class = "show-on-add show-on-edit compact-options-container",
          selectInput(
            inputId = ns("filter_type"),
            label = "Choose Filter Type:",
            choices = 
              c("Select Type" = "",
                "Categorical Metadata" = "categorical",
                "Feature Expression" = "numeric",
                "Advanced Subsetting" = "advanced"
                )
            ),
          # A. Categorical metadata filter UI ####
          hidden(
            div(
              id = ns("categorical_filter_ui"),
              tags$b(
                "Categorical Filter", 
                class = "center large"
              ),
              # Select menu to choose metadata for filtering (add mode)
              div(
                class = "show-on-add",
                selectInput(
                  inputId = ns("categorical_var"),
                  label = "Choose categorical metadata:",
                  # Choices are updated in server
                  choices = NULL
                )
              ),
              # Edit mode: display metadata being edited
              div(
                class = "show-on-edit",
                tags$b("Metadata variable:"),
                textOutput(
                  outputId = ns("edit_categorical_var")
                )
              ),
              # Picker input to choose values of metadata variable to include
              div(
                class = "picker-menu-restrict-width",
                pickerInput(
                  inputId = ns("categorical_values"),
                  label = "Choose Values to Include:",
                  # Choices are variable-dependent and are computed 
                  # in the server function
                  choices = NULL,
                  # selected: nothing selected by default. A placeholder will
                  # appear instead
                  selected = character(0),
                  multiple = TRUE,
                  # Options for pickerInput
                  options = 
                    list(
                      # Display number of items selected instead of their names
                      # when more than 3 values are selected
                      "selected-text-format" = "count > 3",
                      # Define max options to show at a time to keep menu
                      # from being cut off
                      "size" = 7,
                      # Add "select all" and "deselect all" buttons
                      "actions-box" = TRUE,
                      # Label for "deselect all" button
                      "deselectAllText" = "Remove filter",
                      # Define placeholder
                      "none-selected-text" = "Select values"
                    )
                  )
                )
            ),
            # B. Numeric filter UI ####
            div(
              id = ns("numeric_filter_ui"),
              tags$b(
                "Numeric Filter", 
                class = "center large"
              ),
              # Feature for numeric filters
              selectizeInput(
                inputId = ns("numeric_feature"),
                label = "Enter a feature to apply filter to:",
                choices = NULL,
                selected = character(0),
                options = 
                  list(
                    "placeholder" = "Enter feature",
                    "maxItems" = 1,
                    "plugins" = list("remove_button"),
                    "create" = FALSE
                    )
                ),
              # "Mode" of numeric filter (<, >, range)
              # Hidden until a feature is entered
              hidden(
                shinyWidgets::radioGroupButtons(
                  inputId = ns("numeric_mode"),
                  label = "Filter mode:",
                  choices = 
                    c(">" = "greater_than",
                      "<" = "less_than",
                      "Range" = "range"
                    ),
                  justified = TRUE,
                  status = "radio-primary"
                )
              ),
              # UI for choosing threshold for filtering
              # Interface is automatically hidden until a feature is selected
              threshold_picker_ui(
                id = ns("filter_threshold"),
                plot_height = "150px",
                instruction_panel = TRUE,
                show_stats = FALSE
                )
              ),
            # C. Advanced filter UI ####
            div(
              id = ns("advanced_filter_ui"),
              tags$b(
                "Advanced Filter", 
                class = "center large"
              ),
              tags$b("Enter R code below to apply a custom filter."),
              # textAreaInput for entering string subset
              textAreaInput(
                inputId = ns("adv_filter_code"),
                label = NULL,
                width = "100%",
                rows = 4,
                resize = "vertical"
                ),
              dropdownButton(
                inputId = ns("feature_search_dropup"),
                label = "",
                size = "sm",
                status = "feature_search_btn",
                icon = icon("search"), 
                up = TRUE,
                # Dropdown content
                div(
                  class = "feature_statistics_container",
                  onclick = "event.stopPropagation()",
                  selectizeInput(
                    inputId = ns("search_feature"),
                    label = "Feature Search:",
                    choices = NULL,
                    selected = character(0),
                    width = "200px",
                    options = 
                      list(
                        # Add remove button to inputs
                        'plugins' = list('remove_button'),
                        # Do not allow user to input features not
                        # in the list of options
                        'create' = FALSE,
                        'placeholder' = "enter feature"
                        )
                    ),
                  # Feature statistics: summary stats based on feature entered
                  uiOutput(ns("feature_statistics")),
                  # Threshold_picker module to explore stats for feature
                  threshold_picker_ui(
                    id = ns("feature_stats_interactive"),
                    plot_width = "200px",
                    plot_height = "250px"
                    )
                  )
                ),
              tags$a(
                "String Subsetting Help",
                href = string_subsetting_href,
                target = "_blank",
                rel = "noopener noreferrer",
                class = "blue_hover underline underline-hover left",
                # Decrease padding around link
                style = "padding: 3px 6px; margin-top: 15px;"
              ),
              tags$a(
                "View Object Metadata",
                href = auto_dictionary_path,
                target = "_blank",
                rel = "noopener noreferrer",
                class = "blue_hover underline underline-hover left",
                # Decrease padding around link
                style = "padding: 3px 6px;"
                )
              )
            ),
          actionButton(
            inputId = ns("filter_cancel"),
            label = "Cancel",
            #icon = icon("redo-alt"),
            class = "button-ghost compact-button"
            ),
          actionButton(
            inputId = ns("filter_confirm"),
            label = "Confirm Filter",
            class = "button-primary compact-button"
            )
          )
        ),
      bsTooltip(
        id = ns("subset_selections_all"), 
        title = 
          paste0(
            'Use "Add Filter" to subset based on metadata or ',
            'feature expression. Be sure to select "Confirm Filter" for all ',
            'filters, and "Apply Subset" when done.'
          ),
        placement = "top", 
        trigger = "hover",
        options = NULL
      )
      )
    }

#' Subset Selections Module 
#'
#' @param id id to use for module namespacing. UI-server module pairs must 
#' use the same id.
#' @param object The Seurat object for which subset criteria are to be defined.
#' @param unique_metadata A list of the unique metadata values for each of the 
#' metadata categories listed in the config file. This is generated in the main 
#' server function upon startup and object change. 
#' @param metadata_config The metadata section of the config file, loaded in the
#' main server function.
#' @param assay_config The assays section of the config file, loaded in the
#' main server function.
#' @param meta_categories A vector of the metadata categories (variables) 
#' included in the config file for the current object. This is computed in the 
#' main server function.
#' @param valid_features A list of the available features in the current object,
#' used to populate the choices of the feature selection menu when groups based
#' on feature thresholds are requested.
#' @param hide_menu (optional) a string or character vector giving 
#' the name(s) of categorical metadata variables to hide in the subset selection
#' interface. This avoids inaccurate results in the DGE tab. Subsetting for DGE 
#' occurs on the group by variable separate of the selections made in this 
#' module
#' 
#' @return Server code for the subset selections module.
#' 
#' @noRd
#'
subset_selections_server <- function(id,
                                     object,
                                     unique_metadata,
                                     metadata_config,
                                     assay_config,
                                     meta_categories,
                                     valid_features,
                                     hide_menu = NULL
                                     ){
  # Initialize module 
  moduleServer(
    id,
    # Server function for module: uses a separate set of input/output variables 
    function(input,output,session){
      # Server namespace function: used for UI elements rendered in server
      ns <- session$ns
      
      # module_data: reactiveValues object used to store the ridgeplot 
      # interactively generated in 6.3.
      module_data <- reactiveValues()
      
      # Initial states for filter menus
      # State of the entire filter interface (idle by default, and can be 
      # set to "add" or "edit" via button presses)
      module_data$filter_menu_state <- "idle"
      # Type of filter being added/edited (categorical or numeric)
      module_data$filter_type <- "none"
      # Empty list for storing subset filters as they're created
      module_data$filters <- list()
      
      # editing data: reactiveValues list storing data related to a filter 
      # being edited
      # Data must be stored in a reactiveValues object while editing to prevent 
      # information loss. 
      editing_data <- reactiveValues()
      
      # Numeric feature mode
      editing_data$mode <- NULL
      # Categorical metadata variable, or feature, being edited 
      editing_data$var <- NULL
      # Value(s) of var 
      editing_data$value <- NULL
      # Special variable for numeric filters (threshold picker module responds 
      # to this value)
      editing_data$numeric_filter_value <- NULL
      # Index of filter being edited in module_data$filters
      editing_data$target_row <- NULL
      # Label to display to user
      editing_data$label <- NULL
      
      # Confirm button for adding filters should start in a disabled state
      shinyjs::disable(
        id = "filter_confirm"
      )
      
      # 1. Respond to "add filter" button --------------------------------------
      observe({
        req(input$add_filter)
        input$add_filter
        
        # Set state to "add"
        module_data$filter_menu_state <- "add"
      })
      
      # 2. Show/hide UI based on add/edit/idle states --------------------------
      observe({
        # jQuery selectors for classes that show elements based on state
        add_selector <- "[class *= 'show-on-add']"
        edit_selector <- "[class *= 'show-on-edit']"
        idle_selector <- "[class *= 'show-on-idle']"
        
        # showElement statements are put last on purpose
        # This allows elements with show-on-add OR show-on-edit classes to be 
        # visible (if the hideElement statements come after the showElement 
        # statement, the container with both classes will be shown, then hidden)
        if (module_data$filter_menu_state == "add"){
          hideElement(
            selector = edit_selector
          )
          
          hideElement(
            selector = idle_selector
          )
          
          showElement(
            selector = add_selector
          )
        } else if (module_data$filter_menu_state == "edit"){
          hideElement(
            selector = add_selector
          )
          
          hideElement(
            selector = idle_selector
          )
          
          showElement(
            selector = edit_selector
          )
        } else if (module_data$filter_menu_state == "idle") {
          showElement(
            selector = idle_selector
          )
          
          hideElement(
            selector = add_selector
          )
          hideElement(
            selector = edit_selector
          )
        }
      })
      
      # 3. Record state from filter type selection menu ------------------------
      observe({
        if (isTruthy(input$filter_type)){
          if (input$filter_type == "categorical"){
            module_data$filter_type <- "categorical"
          } else if (input$filter_type == "numeric"){
            module_data$filter_type <- "numeric"
          } else if (input$filter_type == "advanced"){
            module_data$filter_type <- "advanced"
          }
          
          # Also reset the filter type input back to the blank state with
          # placeholder
          updateSelectInput(
            session = session,
            inputId = "filter_type",
            selected = ""
            )
        } 
      })
      
      # 4. Show/hide UI based on type of filter being edited -------------------
      observe({
        type_selection_ui_id <- "filter_type"
        categorical_ui_id <- "categorical_filter_ui"
        numeric_ui_id <- "numeric_filter_ui"
        advanced_ui_id <- "advanced_filter_ui"
        
        if (module_data$filter_type == "categorical"){
          # Show categorical filter UI
          showElement(
            id = categorical_ui_id
          )
          
          # Hide the menu to select filter type
          hideElement(
            id = type_selection_ui_id
          )
        } else if (module_data$filter_type == "numeric"){
          # Show numeric filter UI
          showElement(
            id = numeric_ui_id
          )
          
          # Hide the menu to select filter type
          hideElement(
            id = type_selection_ui_id
          )
        } else if (module_data$filter_type == "advanced") {
          # Show advanced filter UI
          showElement(
            id = advanced_ui_id
          )
          
          # Hide menu to select filter type
          hideElement(
            id = type_selection_ui_id
          )
        } else if (module_data$filter_type == "none"){
          # Show type selection menu, hide categorical and numeric interfaces
          showElement(
            id = type_selection_ui_id
          )
          
          hideElement(
            id = categorical_ui_id
          )
          
          hideElement(
            id = numeric_ui_id
          )
          
          hideElement(
            id = advanced_ui_id
          )
        }
      })
      
      # 5. Hide filters not supported by object class --------------------------
      # SingleCellExperiment objects cannot be subsetted by feature expression
      # using the current make_subset implementation. 
      # For now, "feature expression" and "advanced" filters will be hidden.
      observe(
        label = 
          glue("{id}: restrict filter choices in SingleCellExperiment objects"),
        {
          if (inherits(object(), "SingleCellExperiment")){
            updateSelectInput(
              inputId = "filter_type",
              choices =
                c("Select Type" = "",
                  "Categorical Metadata" = "categorical"
                  )
                )
          } else if (inherits(object(), "AnnDataR6")){
            # Anndata objects, hide advanced filters
            updateSelectInput(
              inputId = "filter_type",
              choices =
                c("Select Type" = "",
                  "Categorical Metadata" = "categorical",
                  "Feature Expression" = "numeric"
                  )
              )
            }
          })
      
      # 6. Categorical filters -------------------------------------------------
      ## 6.1. Update choices for categorical metadata variables ####
      observeEvent(
        module_data$filter_type,
        label = 
          glue("{id}: filter menus: update choices for categorical metadata"),
        {
          # Runs as long as the type is "categorical"
          req(module_data$filter_type == "categorical")
          # Do not run in editing mode
          req(module_data$filter_menu_state == "add")
            
          # Construct vector of metadata choices for filtering
          # Values: machine-readable variable names
          categorical_choices <-
            sapply(metadata_config(), function(var) var$meta_colname) |> 
            unname()
          
          # Names: variable names displayed to user
          names(categorical_choices) <-
            sapply(metadata_config(), function(var) var$label) |> 
            unname()
          
          # Exclude any choices in hide_menu(), if the reactive is specified
          if (is.reactive(hide_menu)){
            categorical_choices <- 
              categorical_choices[!categorical_choices %in% hide_menu()]
          }
          
          # Exclude categorical variables that have already been entered as filters
          if (length(module_data$filters) > 0){
            # Identify categorical filters
            existing_categorical_filters <- 
              # Must use single brackets to select multiple elements from a 
              # list of list (double bracket notation with [[c(i,j)]] will fetch
              # the jth element of the ith sublist)
              module_data$filters[
                # sapply: creates boolean identifying categorical filters
                sapply(
                  module_data$filters,
                  function(filter_i){
                    filter_i$type == "categorical"
                  }
                )
              ]
            
            # Get var names of invalid choices
            invalid_choices <- 
              sapply(
                existing_categorical_filters,
                function(filter_i){
                  filter_i$var
                }
              )
            
            # Remove invalid choices
            categorical_choices <- 
              categorical_choices[!categorical_choices %in% invalid_choices]
          }
          
          # If no variables are left, notify the user using a placeholder
          if (length(categorical_choices) == 0){
            categorical_choices <- 
              c("No remaining variables" = "")
          }
          
          # First element: placeholder text with "falsy" value
          # categorical_choices <-
          #   c(
          #     "Select variable" = "",
          #     categorical_choices
          #     )
            
          updateSelectInput(
            session = session,
            inputId = "categorical_var",
            choices = categorical_choices
            )
          })
      
      ## 6.2. Choices for values within a given variable ####
      observeEvent(
        c(module_data$filter_type, 
          input$categorical_var
          ),
        label = 
          glue("{id}: filter menus: update choices for categorical metadata"),
        {
          req(input$categorical_var)
          # Do not run in editing mode
          req(module_data$filter_menu_state == "add")
          
          # Determine choices to display for the selected metadata variable
          
          choices <-
            SCUBA::unique_values(
              object(), 
              var = input$categorical_var
              ) 
          
          # If choices is a factor, the integer levels will be displayed to the 
          # user instead of the corresponding values. This is corrected here.
          if (is.factor(choices)){
          
            choices <- levels(choices)
            
          } else{
            
            choices <- gtools::mixedsort(choices)
          }
          
          # Determine which choices are valid based on the current subset
          # Filter metadata table for cells that match current filter criteria
          if (length(module_data$filters) > 0){
            subset_table <-
              categorical_subset_preview(
                object(),
                filter_list = module_data$filters
              )
            
            valid_choices <- 
              subset_table[[input$categorical_var]] |> 
              unique()
            
            # If valid choices is a factor, transform to a character vector
            if (is.factor(valid_choices)){
             
              valid_choices <- levels(valid_choices)
            }
          } else {
     
            valid_choices <- gtools::mixedsort(choices)
          }
          
          # Boolean vector of choices to be diabled
          disable_boolean <- !choices %in% valid_choices
          
          # Update picker input with valid choices
          updatePickerInput(
            session,
            inputId = "categorical_values",
            choices = choices,
            # Reset selection to nothing being selected (no filters applied; 
            # use character(0) to do this)
            selected = character(0),
            choicesOpt =
              list(
                disabled = disable_boolean,
                style = ifelse(
                  disable_boolean,
                  yes = "color: #88888888;",
                  no = ""
                  )
                )
              )
            })
      
      ## 6.3. Display feature label during editing ####
      output$edit_categorical_var <-
        renderText({
          req(editing_data$label)
          
          editing_data$label
        })
      
      # 7. Numeric filters ####
      ## 7.1. Update choices for numeric metadata features ####
      # Only needs to run once at startup
      observeEvent(
        # Responds to loading of update and creation of UI (to ensure
        # feature updating is always performed after the input is 
        # created)
        c(input$add_filter, valid_features()),
        label = glue("{id}: Render choices for feature selection"),
        {
          updateSelectizeInput(
            session,
            inputId = "numeric_feature",
            choices = valid_features(),
            selected = character(0),
            server = TRUE
            )
        })
  
      ## 7.2. Hide Buttons for choosing filter mode ####
      # Hide buttons until a feature is entered
      observe({
        target_id <- "numeric_mode"
        
        if (isTruthy(input$numeric_feature)){
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
        })
      
      ## 7.3. Interface for choosing threshold ####
      numeric_filter_value <- 
        threshold_picker_server(
          # Do not namespace module server function IDs 
          id = "filter_threshold",
          object = object,
          feature = reactive({input$numeric_feature}),
          mode = reactive({input$numeric_mode}),
          # set_threshold will set the interface to a threshold/range when 
          # editing a filter
          set_threshold = 
            reactive({
              req(input$numeric_mode)
              if (input$numeric_mode == "range"){
                req(length(editing_data$numeric_filter_value) == 2)
              } else {
                req(length(editing_data$numeric_filter_value) == 1)
              }
              
              editing_data$numeric_filter_value
              }),
          showhide_animation = TRUE,
          assay_config = assay_config
        )
      
      # 8. Advanced (String) filter --------------------------------------------
      # Stats dropdown
      # Assists user by displaying feature name as it should be entered into the
      # subset function (using the assay key prefix), along with summary
      # statistics for the feature to aid in choosing bounds when subsetting.
      
      ## 8.1. Feature Search Choices ####
      # Updates occur each time the object is changed
      observeEvent(
        valid_features(),
        {
          updateSelectizeInput(
            session,
            inputId = "search_feature",
            choices = valid_features(),
            selected = character(0),
            server = TRUE,
            options = 
              list(
                # Add remove button to inputs
                'plugins' = list('remove_button'),
                # Do not allow user to input features not
                # in the list of options
                'create' = FALSE,
                'placeholder' = "enter feature"
              )
          ) 
        })
      
      ## 8.2. Feature statistics UI ####
      feature_stats_ui <-
        reactive({
          req(input$search_feature)
          
          # Obtain summary stats for feature
          feature_summary <-
            # Uses fetch_data to pull expression data for the feature
            # Data layer is pulled by default; this can be changed
            SCUBA::fetch_data(
              object(),
              vars = input$search_feature
            )[,1] |> 
            summary()
          
          # Display summary stats for feature
          tagList(
            # Feature ID
            tags$p(
              class = "bold-blue large",
              tags$b(
                "Feature ID:"
              ),
              input$search_feature
            ),
            tags$p("(This must be entered exactly as it displays above)"),
            # Summary Statistics
            tags$b("Summary Statistics", class = "bold-blue"),
            summary_tags(
              feature_summary,
              header_class = "bold-blue"
            )
          )
        })
      
      ## 8.3. Render Feature Statistics Components ####
      output$feature_statistics <- 
        renderUI({
          feature_stats_ui()
        })
      
      ## 8.4. Interactive ridge plot server ####
      threshold_picker_server(
        id = "feature_stats_interactive",
        object = object,
        feature = reactive({input$search_feature}),
        showhide_animation = TRUE,
        assay_config = assay_config
        )
      
      # 9. Disable confirm button until proper selections are made -------------
      observe(
        label = 
          glue("{id}: disable confirm button until a sensible selection is made "),
        {
          target_id <- "filter_confirm"
          
          if (module_data$filter_type == "categorical"){
            # Categorical filters: enable button when a metadata variable 
            # and values are chosen
            if (isTruthy(input$categorical_var) & 
                isTruthy(input$categorical_values)){
              shinyjs::enable(
                id = target_id
              )
            } else {
              shinyjs::disable(
                id = target_id
              )
            }
          } else if (module_data$filter_type == "numeric"){
            # Numeric filters: enable button when a feature and 
            # threshold/range are chosen
            if (isTruthy(input$numeric_feature) &
                isTruthy(numeric_filter_value())
                ){
              shinyjs::enable(
                id = target_id
              )
            } else {
              shinyjs::disable(
                id = target_id
              )
                }
          } else if (module_data$filter_type == "advanced"){
            # Advanced filters: enable button when code has been entered
            if (isTruthy(input$adv_filter_code)){
              shinyjs::enable(
                id = target_id
              )
            } else {
              shinyjs::disable(
                id = target_id
              )
            }
          } else if (module_data$filter_type == "none"){
            # Confirm button is disabled when the filter type is equal to none
            # (but the button should never show in this case)
            shinyjs::disable(
              id = target_id
            )
          } else {
            # Unforeseen cases: show warning and enable the button
            warning(
              glue(
                "Observer in {id} detected an unexpected value for `module_data$filter_type`. The confirm button for filters will be enabled in this case")
              )
            
            shinyjs::enable(
              id = target_id
            )
          }
      })
      
      # 10. Respond to confirm button ------------------------------------------
      # Save filter and reset states/menus
      observeEvent(
        input$filter_confirm,
        label = glue("{id}: respond to confirm button"),
        {
          # Record filter in list of filters
          if (module_data$filter_type == "categorical"){
            # Categorical filters
            var_use <- 
              # var_use: when editing, the variable is fixed and stored in 
              # editing_data$var instead. input$categorical var is not
              # updated since the selection menu is not displayed, and 
              # can therefore reflect a different variable than the one
              # being edited. 
              if (module_data$filter_menu_state == "add"){
                input$categorical_var
              } else if (module_data$filter_menu_state == "edit"){
                editing_data$var
              }
            
            filter_data <-
              list(
                `type` = "categorical",
                # Not used for categorical filters
                `mode` = NULL,
                `var` = var_use,
                # Add display name of variable for filter criteria display
                `label` = metadata_config()[[var_use]]$label,
                `value` = input$categorical_values
                )
            
            # if (module_data$filter_menu_state == "add"){
            #   # When adding a filter:
            #   # Append to end of list ((length + 1)'th element)
            #   # c() will collapse elements into a list at the same level
            #   module_data$filters[[length(module_data$filters) + 1]] <-
            #     filter_data
            # } else if (module_data$filter_menu_state == "edit"){
            #   # When editing a filter:
            #   # Save data to the index of the row being edited
            #   module_data$filters[[editing_data$target_row]]<-
            #     filter_data
            #   
            #   # Clear all editing data variables
            # }
          } else if (module_data$filter_type == "numeric"){
            # Reset the selected feature
            updateSelectizeInput(
              inputId = "numeric_feature",
              choices = valid_features(),
              selected = character(0),
              server = TRUE
            )
            
            # Store filter data
            filter_data <-
              list(
                `type` = "numeric",
                `mode` = input$numeric_mode,
                `var` = input$numeric_feature,
                # Uncomment the lines below if a different feature shows after
                # pressing confirm after editing one feature
                  # if (module_data$filter_menu_state == "add"){
                  #   input$numeric_feature
                  # } else if (module_data$filter_menu_state == "edit"){
                  #   # When editing, the feature is fixed and stored in 
                  #   # editing_data$var instead. input$numeric_feature is not
                  #   # updated since the selection menu is not displayed, and 
                  #   # can therefore reflect a different variable than the one
                  #   # being edited. 
                  #   editing_data$var
                  # },
                `label` = 
                  # Compute display name using config file
                  cellDIVER:::hr_name(
                    machine_readable_name = input$numeric_feature,
                    assay_config = assay_config()
                    ),
                # Use return value of threhold_picker module
                # Either a single number, or a vector of upper and lower bounds
                # if a range is chosen
                `value` = numeric_filter_value()
                )
            
          } else if (module_data$filter_type == "advanced"){
            # Store filter data (only type and value are used)
            filter_data <-
              list(
                `type` = "advanced",
                `mode` = NULL,
                `var` = NULL,
                `label` = NULL,
                # Use text entry in multi-line text box
                `value` = input$adv_filter_code
                )
            
            # Reset text (code) entry
            updateTextAreaInput(
              session = session,
              inputId = "adv_filter_code",
              value = ""
              )
          }
          
          # Save filter data to list (depends on mode)
          if (module_data$filter_menu_state == "add"){
            # Adding a filter:
            # Append to end of list ((length + 1)'th element)
            module_data$filters[[length(module_data$filters) + 1]] <-
              filter_data
          } else if (module_data$filter_menu_state == "edit"){
            # Editing a filter:
            # Save data to the index of the row being edited
            module_data$filters[[editing_data$target_row]]<-
              filter_data
          }
          
          # If a filter was being edited, also reset the editing variables
          if (module_data$filter_menu_state == "edit"){
            editing_data$mode <- NULL
            editing_data$var <- NULL
            editing_data$value <- NULL
            editing_data$numeric_filter_value <- NULL
            editing_data$target_row <- NULL
            editing_data$label <- NULL
          }
          
          # Set state to idle
          module_data$filter_menu_state <- "idle"
          
          # Also reset state of current filter type
          module_data$filter_type <- "none"
        })
      
      # 11. Respond to cancel button -------------------------------------------
      observeEvent(
        input$filter_cancel,
        label = glue("{id}: respond to cancel button"),
        {
          # If the filter was a numeric filter, reset the feature choice menu
          if (module_data$filter_type == "numeric"){
            # Reset the selected feature
            updateSelectizeInput(
              session = session,
              inputId = "numeric_feature",
              selected = character(0),
              server = TRUE
              )
          } else if (module_data$filter_type == "advanced"){
            # Reset text (code) entry
            updateTextAreaInput(
              session = session,
              inputId = "adv_filter_code",
              value = ""
              )
          }
          
          # If a filter was being edited, also reset the editing variables
          if (module_data$filter_menu_state == "edit"){
            editing_data$mode <- NULL
            editing_data$var <- NULL
            editing_data$value <- NULL
            editing_data$numeric_filter_value <- NULL
            editing_data$target_row <- NULL
            editing_data$label <- NULL
            }
          
          # Reset menu state, filter type
          module_data$filter_menu_state <- "idle"
          module_data$filter_type <- "none"
        })
      
      # 12. UI for displaying filters ------------------------------------------
      ## 12.1. HTML ####
      filters_ui <-
        reactive(
          label = glue("{id}: Compute UI for filter menus"),
          {
            if (isTruthy(module_data$filters)){
              if (length(module_data$filters) > 0){
                div(
                  # Set a max height for the filter interface, and scroll 
                  # when the hight is exceeded
                  style = 
                    "max-height: 60vh; 
                     overflow-y: auto;",
                  # Create UI for each filter criteria in the list
                  lapply(
                    1:length(module_data$filters),
                    function(i){
                      # Extract type, display name of variable, values
                      type <- module_data$filters[[i]]$type
                      # Only applies to numeric filters
                      mode <- module_data$filters[[i]]$mode
                      label <- module_data$filters[[i]]$label
                      value <- module_data$filters[[i]]$value
                      
                      # Construct "card" summarizing the filter criteria applied
                      div(
                        class = "filter-info-card",
                        # Container for storing filter information
                        if (type == "categorical"){
                          # Container for categorical filters: follows format below
                          # <metadata_variable> in:
                          # <metadata_values>
                          div(
                            #style = "float: left;",
                            tags$b(
                              glue("{label}:"), 
                              class = "center half-space-bottom"
                            ),
                            tags$p(
                              cellDIVER:::vector_to_text(
                                value
                              )
                            )
                          ) 
                        } else if (type == "numeric"){
                          div(
                            # Numeric filter: UI depends on filter mode (<, >, range) 
                            tags$b(
                              glue("{label}:"), 
                              class = "center half-space-bottom"
                            ),
                            tags$p(
                              if (mode == "less_than"){
                                paste0("< ", value)
                              } else if (mode == "greater_than"){
                                paste0("> ", value)
                              } else if (mode == "range"){
                                paste0("Range: ", value[1], "- ", value[2])
                              }
                            )
                          )
                        } else if (type == "advanced"){
                          div(
                            # Advanced filter: display code entered 
                            tags$b(
                              glue("Custom filter:"), 
                              class = "center half-space-bottom"
                            ),
                            tags$p(
                              glue("{value}")
                            )
                          )
                        },
                        # Container for edit/delete buttons
                        div(
                          div(
                            style = "float: right;",
                            class = "btn-group",
                            role = "group",
                            `aria-label` = glue("Options for filter {i})"),
                            # Content: custom button element
                            # Code adapted from 
                            # https://github.com/AntoineGuillot2/ButtonsInDataTable
                            HTML(
                              # Current list index of filter is passed to button ID
                              glue(
                                '<button type="button" class="btn icon-button edit" 
                                id = edit_{i}> 
                                <i class = "fa fa-pencil" role = "presentation" 
                                  aria-label = "Edit" style = "font-size: 1.2em;">
                                  </i>
                                </button>
                                <button type="button" class="btn icon-button 
                                  delete" id = delete_{i}> 
                                  <i class = "fa fa-times-circle" 
                                    role = "presentation" aria-label = "Delete" 
                                    style = "font-size: 1.2em;">
                                    </i>
                                  </button>'
                              )
                            )
                          )
                        )
                      )
                    }
                  )
                )
              } else {
                # Display "no filters applied" when no filters are entered
                div(
                  class = "filter-info-card",
                  tags$b(
                    "- No filters applied -", 
                    class = "center"
                    )
                  )
                }
              }
            })
      
      ## 12.2. JavaScript for buttons ####
      filter_button_js <-
        reactive(
          label = glue("{id}: edit/delete button JavaScript"),
          {
            req(module_data$filters)
            
            if (length(module_data$filters) > 0){
              tags$script(
                paste0(
                  "$(document).on(
                    'click',",
                    # Single quotes enclose # selectior, namespaced id, and the 
                    # descendent button element selector  
                    "'#", ns("filters_applied"), " button',",
                    "function () {
                      Shiny.onInputChange('", ns("lastClickId"), "', this.id);
                      Shiny.onInputChange('", ns("lastClick"), "', Math.random());
                    });"
                  )
                  # "
                  # $(document).on(
                  #   'click', 
                  #   '#{ns('filters_applied')} button', 
                  #   function () {
                  #     Shiny.onInputChange('lastClickId', this.id);
                  #     Shiny.onInputChange('lastClick', Math.random())
                  #   });
                  #   "
                )
              }
            })
      
      ## 12.3. Render UI ####
      output$filters_applied <-
        renderUI({
          # Render UI elements, scripts together
          tagList(
            filters_ui(),
            filter_button_js()
          )
        })
      
      # 13. Respond to click on edit/delete buttons ----------------------------
      observeEvent(
        # Respond to lastClick, which always changes with each click
        # (lastClickId does not necessarily change)
        input$lastClick,
        label = glue("{id}: Register click on edit/delete buttons"),
        {
         # Test if ID of click contains "edit" or "delete"
         if (grepl("edit", input$lastClickId)){
           # Determine row being edited, and store in editing information
           row_selected <- 
             gsub("edit_", "", input$lastClickId) |> 
             as.numeric()
           
           editing_data$target_row <-
             row_selected
           
           # Fetch filter data from list
           filter_data <-
             module_data$filters[[row_selected]]
           
           # Set up editing interface 
           if (filter_data$type == "categorical"){
             # Set state variable for filter type to show appropriate menus
             module_data$filter_type <- "categorical"
             
             # Store metadata variable for filter being edited
             editing_data$var <- 
               filter_data$var
             
             # Store label of metadata variable
             editing_data$label <-
               filter_data$label
               
             # Store selected values of feature
             editing_data$value <- 
               filter_data$value
             
             # Update selection menu with the values chosen
             # Determine possible choices for the selected variable
             choices <-
               SCUBA::unique_values(
                 object(), 
                 var = editing_data$var
                )
             
             # If a factor, convert the choices to a vector 
             if (is.factor(choices)){
               choices <- as.character(choices)
             }
             # Update picker input with choices, selected values
             updatePickerInput(
               session,
               inputId = "categorical_values",
               choices = choices,
               # Reset selection to nothing being selected (no filters applied; 
               # use character(0) to do this)
               selected = editing_data$value
               )
           } else if (filter_data$type == "numeric"){
             # Set state variable for filter type to show appropriate menus
             module_data$filter_type <- "numeric"
             
             # Set mode, move radio button selection to indicated mode
             editing_data$mode <- 
               filter_data$mode
             
             shinyWidgets::updateRadioGroupButtons(
               session = session,
               inputId = "numeric_mode",
               selected = editing_data$mode
               )
             
             # Store feature being edited
             editing_data$var <- 
               filter_data$var
             
             # Store threshold/range
             editing_data$numeric_filter_value <- 
               filter_data$value
             
             # Update feature selection menu
             updateSelectizeInput(
               session = session,
               inputId = "numeric_feature",
               choices = valid_features(),
               selected = editing_data$var,
               server = TRUE
               )
           } else if (filter_data$type == "advanced"){
             # Set state variable for filter type to show appropriate menus
             module_data$filter_type <- "advanced"
             
             # Store value
             editing_data$value <- 
               filter_data$value
             
             # Update code entry
             updateTextAreaInput(
               session = session,
               inputId = "adv_filter_code",
               value = editing_data$value
               )
           }
           
           # Set state of menu to edit to reveal editing interface
           module_data$filter_menu_state <- "edit"
           
         } else if (grepl("delete", input$lastClickId)){
           # Determine row deleted
           row_selected <- 
             gsub("delete_", "", input$lastClickId) |> 
             as.numeric()
           
           # Prevents errors in the event the selected_row is undefined
           if (!is.null(row_selected) | !is.na(row_selected)){
             # Delete the indicated filter from the list
             module_data$filters[row_selected] <- NULL
             
             # Reset filter menu state variables in the event a 
             # filter is deleted while editing it
             # module_data$filter_menu_state <- "idle"
             # module_data$filter_type <- "none"
             
           } else {
             warning(
               "Unable to determine the index of the row selected for deletion"
               )
           }
         }
        })
      
      # 14. Delete hidden variables from the filter list -----------------------
      # *** Avoids inaccurate results in the DGE tab ***
      # If the variables specified by hide_menu() change, delete filters that 
      # involve these variables.
      # Without this, groups/markers included in the test may be different
      # than what the user entered
      if (is.reactive(hide_menu)){
        observe({
          hide_menu()
          
          # Errors will result if there are no filters defined
          # module_data$filters is still truthy in this case, so length > 0
          # must also be tested for in the req() statement
          req(length(module_data$filters) > 0)
          
          # Avoid re-computing observer when filter list changes
          isolate({
            for (i in 1:length(module_data$filters)){
              filter_i <- module_data$filters[[i]]
              
              # Delete the filter if the variable involved is part of hide_menu()
              if (filter_i$var %in% hide_menu()){
                module_data$filters[[i]] <- NULL 
              }
            }
          })
          
        })
      }

      # 15. Reset all filters --------------------------------------------------
      ## 15.1. Show/hide reset button ####
      observe({
        target_id <- "reset_all_filters"
      
        if (length(module_data$filters) > 0){
          # Show when at least one filter has been entered
          showElement(
            id = target_id
            )
        } else {
        hideElement(
          id = target_id
          )
        }
      })
      
      ## 15.2. Respond to reset button ####
      observeEvent(
        input$reset_all_filters,
        label = "Reset Filter Menus",
        {
          # Reset the filter data to an empty list
          module_data$filters <- list()
          })
      
      # 16. Form Reactive List From Menu Selections ----------------------------
      # selections <- 
      #   reactive(
      #     label = glue("{id}: return value for selections"),
      #     {
      #       # Return filters selected in interface
      #       module_data$filters
      #     })
      
      # 17. Download Button (dev mode) -----------------------------------------
      if (session$userData$dev_mode == TRUE){
        output$download_filters <-
          downloadHandler(
            filename = "filters.yaml",
            content = 
              function(file){
                write_yaml(
                  module_data$filters,
                  file = file
                )
              }
          )
      } else {
        # If dev mode is off, hide the download button
        shinyjs::hideElement(
          id = "download_filters"
        )
      }
      
      # 18. Return Selected Filters --------------------------------------------
      return(
        list(
          `selections` = 
            reactive(
              label = glue("{id}: return value for selections"),
              {
                # Return filters selected in interface
                module_data$filters
              }),
          # Edit mode: signals upstream modues that a filter is being edited
          `edit_mode` = 
            reactive(
              label = glue("{id}: return information on edit state"),
              {
                # TRUE if the filter menu state exists, and is set to "edit"
                if (isTruthy(module_data$filter_menu_state)){
                  module_data$filter_menu_state == "edit" |
                    module_data$filter_menu_state == "add"
                } else {
                  FALSE
                }
              })
          )
        )
    }
  )
}



