#' DGE Test Selections UI
#'
#' @param id id for module. UI-server module pairs must use the same id.
#' @param meta_choices a vector giving the metadata categories included in the 
#' config file. This is computed in the main server function upon object change.
#' 
#' @noRd
dge_test_selections_ui <- 
  function(id,
           meta_choices
           ){
  # Namespace function: prevents conflicts with IDs defined in other modules 
  ns <- NS(id)
  
  # UI components: 
  # 1. Test to perform (DGE or Marker identification) --------------------------
  mode_menu <- 
    tagList(
      selectInput(
        inputId = ns("mode"),
        label = 
          tagList(
            "Choose Test to Perform:",
            a(
              id = ns("mode_tooltip"),
              icon("info-circle"), 
              href = paste0("https://amc-heme.github.io/scExploreR/articles/", 
                            "full_documentation.html"),  
              target = "_blank"
            )
          ),
        # User chooses either dge or marker identification.
        # Human- and computer- readable names given for both options
        choices = 
          c("Marker Identification" = "mode_marker",
            "Differential Expression" = "mode_dge"),
        #Marker selection on clusters is displayed at startup
        selected = "mode_marker",
        # This menu has only two fixed choices. The Selectize-enhanced control
        # can swallow mouse selections in some browser/package combinations,
        # leaving the UI stuck on Marker Identification. A native select avoids
        # that failure and does not change the input values used by the server.
        selectize = FALSE
      ),
      shinyBS::bsTooltip(
        id = ns("mode_tooltip"), 
        title = 
          paste0(
            '"Marker Identification" is used to identify genes that ',
            'distinguish a group of cells from other groups. "Differential ',
            'Expression" is used to identify genes that are expressed ',
            'differently in one group of cells vs. another.'
            ), 
        placement = "bottom", 
        trigger = "hover",
        options = NULL
      )
    )

  # 2. Statistical method ------------------------------------------------------
  # edgeR is initially available only for standard two-group DGE. The server
  # narrows these choices to Wilcoxon when marker or threshold modes are used.
  method_menu <-
    tagList(
      selectInput(
        inputId = ns("method"),
        label = "Differential Expression Method:",
        choices = dge_method_choices("mode_marker", FALSE),
        selected = "wilcoxon"
      ),
      hidden(
        div(
          id = ns("edger_options"),
          numericInput(
            inputId = ns("edger_min_cells"),
            label = "Minimum Cells per Pseudobulk Sample:",
            value = 10L,
            min = 1L,
            step = 1L
          ),
          tags$p(
            paste0(
              "edgeR uses raw counts aggregated by biological sample. ",
              "A configured sample identifier and biological replication are required."
            ),
            class = "help-block"
          ),
          uiOutput(outputId = ns("edger_sample_status"))
        )
      )
    )
  
  # 3. Choice of group by variable ---------------------------------------------
  # Default group_by choice: equal to first metadata category defined in 
  # the config file
  
  # Initial choices: metadata in config file, without "none" 
  initial_choices <- meta_choices()[!meta_choices() %in% "none"]
  
  group_by_menu <- 
    selectInput(
      # Input IDs for two modes must be named differently 
      # to avoid namespace collisions
      inputId = ns("group_by"),
      # Label: created in server based on dge_mode
      label = NULL,
      # Remove "none" from selectable options to group by
      choices = initial_choices,
      # Avoids bugs in the event the user passes a list of choices that only
      # has "none" in it (which would have a length of zero after it is removed)
      selected = 
        if (length(initial_choices) > 0) meta_choices()[1] else character(0)
    )

  # 4. Classes/Groups Menu -----------------------------------------------------
  #Selection of marker classes for group by variable, or groups if mode==dge.
  # UI for the following possibilities is created here, and conditionally shown: 
  # A: Standard DGE groups
  # B: Groups based on simple expression threshold
  # C: Marker classes
  classes_menu <- 
    tagList(
      hidden(
        ## 3.A. Standard DGE Groups UI ####
        # Display menus to select two classes from the metadata 
        # category chosen as the group_by variable. Multiple 
        # selections are possible for each group.
        div(
          id = ns("standard_groups_ui"),
          # Put choices beside one another in two-column format
          pickerInput(
            inputId = ns("group_1"),
            label = "Group 1:",
            # Choices are populated in the server
            choices = NULL, 
              #group_choices(),
            # Nothing selected by default
            selected = character(0),
            multiple = TRUE,
            options =
              list(
                "selected-text-format" = "count > 5",
                "size" = 10,
                "actions-box" = TRUE,
                # Label for "deselect all" button
                "deselectAllText" = "Clear selections"
              )
          ),
          pickerInput(
            inputId = ns("group_2"),
            label = "Group 2:",
            choices = NULL, 
              #group_choices(),
            selected = character(0),
            multiple = TRUE,
            options =
              list(
                "selected-text-format" = "count > 5",
                "size" = 10,
                "actions-box" = TRUE,
                # Label for "deselect all" button
                "deselectAllText" = "Clear selections"
              )
          )
        ),
        ## 3.B. Feature Threshold DGE UI ####
        # Eventually, will be divided into multiple separate 
        # interfaces for different types of thresholding
        div(
          id = ns("threshold_groups_ui"),
          # Simple threshold menu
          # One feature is chosen, and cells with expression above 
          # the threshold are compared to cells below threshold 
          selectizeInput(
            inputId = ns("simple_threshold_feature"),
            label = NULL,
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
          
          # Threshold picker UI (module hides plot
          # until feature is chosen above)
          threshold_picker_ui(
            id = ns("simple_threshold"),
            plot_height = "150px"
          )
        )
      )
    )
  
  # 5. Feature Expression Checkbox ---------------------------------------------
  # Option to use feature expression instead of categorical
  # metadata to define groups
  feature_expression_checkbox <-
    # This option is only visible when differential expression is the mode
    hidden(
      checkboxInput(
        inputId = ns("use_feature_expression"),
        label = "Use Feature Expression to Define Groups"
        )
      )
  
  # Combine elements above into tagList and return for display in app
  ui <- 
    tagList(
      mode_menu,
      method_menu,
      group_by_menu,
      classes_menu,
      feature_expression_checkbox
      )
  }

#' Test Selections in DGE Tab
#'
#' @param id ID to use for module namespacing. Must match the ID passed to 
#' dge_test_selection_ui.
#' @param object The Seurat Object defined in the main server function.
#' @param unique_metadata 
#' @param metadata_config the metadata section of the config file. This does not
#' need to be specified if the config list is stored as "config" in the global
#' environment.
#' @param sample_id_colname Reactive returning the configured biological sample
#' metadata column.
#' @param meta_choices metadata variables exposed by the config user.
#' @param valid_features a list of features that may be selected by the end user.
#'
#' @return A reactive list with information on the selected test.
#'         
#' @noRd
dge_test_selections_server <- 
  function(id,
           object,
           unique_metadata,
           metadata_config,
           assay_config,
           sample_id_colname,
           meta_choices,
           valid_features
           ){
    moduleServer(
      id,
      function(input,output,session){
        # Namespace function: for dynamic UI
        ns <- session$ns
        
        # 1. Process test mode -------------------------------------------------
        dge_mode <- 
          reactive({
            input$mode
            })

        # The initial edgeR implementation supports only standard two-group
        # DGE. Resolve the effective method defensively so an asynchronous input
        # update can never submit edgeR for an unsupported mode.
        edgeR_available <-
          reactive({
            identical(dge_mode(), "mode_dge") &&
              !isTruthy(input$use_feature_expression)
          })

        dge_method <-
          reactive({
            resolve_dge_method(
              requested_method = input$method,
              dge_mode = dge_mode(),
              use_feature_expression = input$use_feature_expression
            )
          })

        edgeR_sample_status <-
          reactive({
            dge_sample_column_status(
              object = object(),
              sample_id_colname = sample_id_colname()
            )
          })

        output$edger_sample_status <-
          renderUI({
            req(edgeR_available(), identical(dge_method(), "edger"))
            status <- edgeR_sample_status()

            if (status$valid){
              tags$p(
                tags$strong("Biological sample field: "),
                status$column,
                class = "text-success"
              )
            } else {
              div(
                tags$strong("edgeR unavailable: "),
                status$error,
                class = "alert alert-warning"
              )
            }
          })

        observeEvent(
          list(dge_mode(), input$use_feature_expression),
          label = "Test Selections: Update DGE Method Choices",
          ignoreNULL = FALSE,
          {
            choices <- dge_method_choices(
              dge_mode = dge_mode(),
              use_feature_expression = input$use_feature_expression
            )
            current_method <- isolate(input$method)
            selected_method <- dge_method_input_value(current_method, choices)

            updateSelectInput(
              session = session,
              inputId = "method",
              choices = choices,
              selected = selected_method
            )
          }
        )

        observe(
          label = "Test Selections: Show/Hide edgeR Options",
          {
            if (edgeR_available() && identical(dge_method(), "edger")){
              showElement(id = "edger_options")
            } else {
              hideElement(id = "edger_options")
            }
          }
        )
        
        # 2. Modify Group by Selection Menu Based on Selections ----------------
        ## 2.1 Update group by selection menu ####
        observeEvent(
          dge_mode(),
          label = "Test Selections: Update Group by Menu",
          ignoreInit = TRUE,
          {
            # Label changes based on mode, but choices remain the same
            if (dge_mode() == "mode_dge"){
              updateSelectInput(
                session = session,
                inputId = "group_by",
                label = 
                  "Choose Metadata to use for Differential Gene Expression Groups:"
                )
            } else if (dge_mode() == "mode_marker"){
              updateSelectInput(
                session = session,
                inputId = "group_by",
                label = 
                  "Choose Metadata to use for Marker Groups:"
                )
            }
          })
        
        ## 2.2. Show/hide Group by menu ####
        observeEvent(
          input$use_feature_expression,
          label = "Test Selections: Update Group by Menu",
          ignoreNULL = FALSE,
          ignoreInit = TRUE,
          {
            target_id = "group_by"
            
            # Hide menu when the user requests groups 
            # based on a feature expression threshold
            if (isTruthy(input$use_feature_expression)){
              hideElement(
                id = target_id
              )
            } else{
              showElement(
                id = target_id
              )
            }
          })
        
        # 3. Classes/Groups Menu -----------------------------------------------
        ## 3.1. Show/hide possible classes/groups interfaces ####
        observe({
          standard_groups_id <- "standard_groups_ui"
          threshold_groups_id <- "threshold_groups_ui"
          #marker_classes_id <- "marker_groups_ui"
          
          # Hide all initially, then show based on the test requested
          for (id in 
               c(standard_groups_id, threshold_groups_id)){
            hideElement(
              id = id
            )
          }
          
          # Menu to show: depends on DGE mode 
          if (!is.null(dge_mode())){
            if (dge_mode() == "mode_dge"){
              # For DGE: if user checks "define groups based on 
              # feature expression", show the simple threshold id
              if (isTruthy(input$use_feature_expression)){
                # Show simple threshold id when box is checked
                showElement(
                  id = threshold_groups_id
                )
              } else {
                # Otherwise, show groups menu
                showElement(
                  id = standard_groups_id
                )
              }
            } else if (dge_mode() == "mode_marker"){
              # Neither menu is shown during marker identification
            }
          }
        })
        
        ## 3.2. Process group_by choice ####
        # Reactive below avoids namespace collision issue while
        # pointing group by selections to one variable
        group_by_category <- 
          reactive(
            label = "Test Selection: Process Group by Choices",
            {
              # Record input unless the user requests to define groups based 
              # on a feature expression threshold
              if (!isTruthy(isolate({input$use_feature_expression}))){
                input$group_by
              } else {
                NULL
              }
              })
        
        ## 3.3. Determine unique values for the group by variable ####
        # Will be options for marker classes or groups in DGE). This process 
        # will be the same regardless of whether marker selection or 
        # differential gene expression is the chosen test
        group_choices <-
          eventReactive(
            # Reacts to group_by_category() defined in 3.
            group_by_category(),
            label = "Test Selection: Compute Valid Group by Choices",
            # Errors will arise in this function and downstream if
            # group_by_category is NULL
            ignoreNULL = TRUE,
            {
              SCUBA::unique_values(
                object(),
                var = group_by_category() 
                ) |>
                # Sort choices
                str_sort(
                  numeric = TRUE
                  )
              })
        
        ## 3.4. Group Marker/Group Choices Based on Config File ####
        # If the group by metadata category has groups defined in the config 
        # file, construct a list using the information so the choices are 
        # grouped in the menu according to the defined groups. This is used for
        # the marker choices menu but not for the DGE menus, as the choicesOpt
        # argument used for updating the DGE menus does not work with lists of
        # grouped choices
        grouped_group_choices <-
          reactive(
            label = "Test Selections: Group Valid DGE Group Choices",
            {
              if (
                !is.null(metadata_config()[[group_by_category()]]$groups)
              ){
                group_metadata_choices(
                  group_info =
                    metadata_config()[[group_by_category()]]$groups,
                  choices =
                    unique_metadata()[[group_by_category()]]
                  )
              } else {
                # Otherwise, use the group_choices() vector
                group_choices()
              }
            })
        
        ## 3.5. Update groups/classes menus ####
        ### 3.5.1. Update groups menu ####
        observe(
          label = "Test Selection: Update Groups Menu",
          {
            req(dge_mode())
            
            # Update proceeds when simple DGE is selected
            if (dge_mode() == "mode_dge"){
              # Isolate input$use_feature_expression (prevents choices from
              # being erased if the user switches back and forth between 
              # enabling and disabling the thresholding checkbox)
              if (!isTruthy(isolate({input$use_feature_expression}))){
                # Update both group menus
                updatePickerInput(
                  session = session,
                  inputId = "group_1",
                  # Use group_choices value computed in 3.3
                  choices = group_choices(),
                  # No groups selected by default
                  selected = character(0)
                  )
                
                updatePickerInput(
                  session = session,
                  inputId = "group_2",
                  choices = group_choices(),
                  # No groups selected by default
                  selected = character(0)
                  )
                }
              }
          })
        
        ## 3.6. Show/hide menu for groups based on feature expression ####
        observe(
          label = "Test Selection: Show/Hide Groups by Expression Checkbox",
          {
            # Only evaluate when dge_mode() is defined
            req(dge_mode())
            
            target_id <- "use_feature_expression"

            if (dge_mode() == "mode_dge"){
              showElement(
                id = target_id
                )
              } else {
                hideElement(
                  id = target_id
                  )
                }
            })
        
        ## 3.7. Update Standard DGE Group Menus ####
        ### 3.7.1. Update Group 2 based on Group 1 Selection ####
        # Removes the value(s) in group 1 from the group 2 menu to keep 
        # user from selecting the same groups for DE comparison
        observeEvent(
          input$group_1,
          # Menu update not necessary at startup
          ignoreInit = TRUE,
          # NULL values cause errors in this function
          ignoreNULL = FALSE,
          {
            # Define valid choices for group 2 (excludes the choice 
            # currently selected for group 1)
            new_choices <- 
              if (!is.null(input$group_1)){
                group_choices()[!group_choices() %in% input$group_1] |> 
                  # Sort choices
                  str_sort(numeric = TRUE)
              } else {
                group_choices() |> 
                  # Sort choices
                  str_sort(numeric = TRUE)
              }
              
            # Create a boolean vector from choices that are now invalid
            disable_boolean <-
              if (!is.null(input$group_1)){
                group_choices() %in% input$group_1
              } else {
                # If no choices are selected for group 1, disable none 
                # A boolean vector of all FALSE elements is created 
                rep_len(FALSE, length(group_choices()))
              }              
            
            # Define values from group 2 to leave selected
            # If currently selected choices in group 2 are in group 1, define
            # which choices are in the group. These will be de-selected. 
            if (any(input$group_2 %in% input$group_1)){
              # Can't use input$group2[!input$group_2 %in% input$group_1]
              # Not sure why
              select_boolean <- !input$group_2 %in% input$group_1
              select_values <- input$group_2[select_boolean]
            } else {
              # If there is no overlap, preserve original selection for group 2
              select_values <- input$group_2
            }
            
            # Update group 2 input with new valid choices
            updatePickerInput(
              session,
              inputId = "group_2",
              label = "Group 2",
              # Choices never change, but they must be specified 
              # for choicesOpt updates to apply
              choices = group_choices(),
              # If any selected choices from group 2 are now selected for 
              # group 1, de-select those choices. 
              selected = select_values,
              # Disable choices that are selected for group 1
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
        
        ### 3.7.2. Update Group 1 based on Group 2 Selection ####
        observeEvent(
          input$group_2,
          # Menu update not necessary at startup
          ignoreInit = TRUE,
          # NULL values cause errors in this function
          ignoreNULL = FALSE,
          {
            # Define valid choices for group 2 (excludes the choice
            # currently selected for group 1)
            new_choices <-
              if (!is.null(input$group_2)){
                group_choices()[!group_choices() %in% input$group_2] |>
                  # Sort choices
                  str_sort(numeric = TRUE)
              } else {
                group_choices() |> 
                  # Sort choices
                  str_sort(numeric = TRUE)
                }

            # Create a boolean vector from choices that are now invalid
            disable_boolean <-
              if (!is.null(input$group_2)){
                group_choices() %in% input$group_2
              } else {
                # If no choices are selected for group 2, disable none 
                # A boolean vector of all FALSE elements is created 
                rep_len(FALSE, length(group_choices()))
              }
              
            # Define values from group 1 to leave selected
            # If currently selected choices in group 1 are in group 2, define
            # which choices are in the group. These will be de-selected. 
            if (any(input$group_1 %in% input$group_2)){
              # Can't use input$group1[!input$group_1 %in% input$group_2]
              # Not sure why
              select_boolean <- !input$group_1 %in% input$group_2
              select_values <- input$group_1[select_boolean]
            } else {
              # If there is no overlap, preserve original selection for group 1
              select_values <- input$group_1
            }

            # Update group 1 input with new valid choices
            updatePickerInput(
              session,
              inputId = "group_1",
              label = "Group 1",
              # Choices never change, but they must be specified
              # for choicesOpt updates to apply
              choices = group_choices(),
              # De-select and disable choices that are selected for group 2
              selected = select_values,
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
        
        ## 3.8. Simple Feature Threshold Processing ####
        ### 3.8.1. Populate Feature Choices for Simple Thresholding ####
        # Populate menu with valid features when simple thresholding is selected
        observeEvent(
          label = "Test Selection: Fill Available Features",
          input$use_feature_expression,
          {
            if (!isTruthy(input$simple_threshold_feature)){
              # Update unless the user has entered a feature (updating in this 
              # case is not necessary, and it would erase the user's input)
              updateSelectizeInput(
                session,
                # Do not namespace IDs in update* functions
                inputId = "simple_threshold_feature", 
                choices = valid_features(), 
                selected = character(0),
                server = TRUE
                )
            }
          })
        
        ### 3.8.2. Server for Interactive Ridge Plot ####
        simple_threshold <- 
          threshold_picker_server(
            # Do not namespace module server function IDs 
            id = "simple_threshold",
            object = object,
            feature = reactive({input$simple_threshold_feature}),
            showhide_animation = TRUE,
            assay_config = assay_config
            )
        
        # 4. Process Test Selections -------------------------------------------
        # Group 1: process input if DGE is the mode selected
        group_1 <- 
          reactive({
            # dge_mode() must be defined to avoid errors
            if (!is.null(dge_mode())){
              if (dge_mode() == "mode_dge") input$group_1 else NULL
              }
            })
        
        # Group 1: process input if DGE is the mode selected
        group_2 <-
          reactive({
            # dge_mode() must be defined to avoid errors
            if (!is.null(dge_mode())){
              if (dge_mode() == "mode_dge") input$group_2 else NULL
              }
            })
        
        # 5. Return information ------------------------------------------------
        # Return a reactive list with inputs, depending on the selected DGE mode
        selections <- 
          reactive(
            label = "Test Selection: Return Values",
            {
              # dge_mode() must be defined to avoid errors
              if (!is.null(dge_mode())){
                if (dge_mode() == "mode_dge"){
                  # DGE Mode: multiple possible return formats
                  if (isTruthy(input$use_feature_expression)){
                    # A. Groups defined using simple thresholding 
                    # Return feature name and threshold value
                    return(
                      list(
                        `dge_mode` = dge_mode(),
                        `dge_method` = dge_method(),
                        # Indicate simple threshold being used for groups
                        `group_mode` = "simple_threshold",
                        `threshold_feature` = 
                          input$simple_threshold_feature,
                        `threshold_value` = simple_threshold()
                        )
                      )
                    } else {
                      # B. Standard DGE
                      # Return selected groups
                      return(
                        list(
                          `dge_mode` = dge_mode(),
                          `dge_method` = dge_method(),
                          # Indicate that standard DGE is selected
                          `group_mode` = "standard",
                          `group_by` = group_by_category(),
                          `group_1` = group_1(),
                          `group_2` = group_2(),
                          `edger_sample_col` =
                            if (identical(dge_method(), "edger") &&
                                edgeR_sample_status()$valid){
                              edgeR_sample_status()$column
                            } else {
                              NULL
                            },
                          `edger_sample_error` =
                            if (identical(dge_method(), "edger") &&
                                !edgeR_sample_status()$valid){
                              edgeR_sample_status()$error
                            } else {
                              NULL
                            },
                          `edger_min_cells` =
                            if (identical(dge_method(), "edger")){
                              input$edger_min_cells
                            } else {
                              NULL
                            }
                          )
                        )
                      }
                  } else if (dge_mode() == "mode_marker"){
                    # Include classes when marker identification 
                    # is the selected mode
                    return(
                      list(
                        `dge_mode` = dge_mode(),
                        `dge_method` = dge_method(),
                        # Leave group_mode defined for compatability with 
                        # conditional statements in DGE tab, but set to "none"
                        `group_mode` = "none",
                        `group_by` = group_by_category()
                        )
                      )
                  }
                }
              }) # End reactive
        
        return(selections)
        })
}

# Return method choices for the current analysis mode. Named values are passed
# directly to Shiny select inputs.
dge_method_choices <- function(dge_mode, use_feature_expression = FALSE){
  choices <- c("Wilcoxon rank-sum" = "wilcoxon")

  if (identical(dge_mode, "mode_dge") && !isTRUE(use_feature_expression)){
    choices <- c(
      choices,
      "edgeR pseudobulk" = "edger"
    )
  }

  choices
}

# Resolve an effective method independently of the timing of Shiny input
# updates. Unsupported combinations always resolve to Wilcoxon.
resolve_dge_method <- function(requested_method,
                               dge_mode,
                               use_feature_expression = FALSE){
  allowed_methods <- unname(
    dge_method_choices(dge_mode, isTRUE(use_feature_expression))
  )

  if (is.null(requested_method) || !requested_method %in% allowed_methods){
    "wilcoxon"
  } else {
    requested_method
  }
}

# Human-readable statistical method used in the submitted analysis. This is
# shared by the result summary so it never reports Wilcoxon after an edgeR run.
dge_method_label <- function(method){
  if (identical(method, "edger")){
    "edgeR pseudobulk quasi-likelihood"
  } else {
    "Wilcoxon rank sum"
  }
}

# Resolve the select-input value during Shiny initialization, when the browser
# may not have supplied the current value yet.
dge_method_input_value <- function(current_method, choices){
  if (isTruthy(current_method) && current_method %in% unname(choices)){
    current_method
  } else {
    "wilcoxon"
  }
}

# Validate the configuration-level biological sample field. Cell-level missing
# values and biological replication are validated later on the submitted subset.
dge_sample_column_status <- function(object, sample_id_colname){
  metadata_columns <-
    if (inherits(object, "Seurat")){
      colnames(object[[]])
    } else if (inherits(object, "SingleCellExperiment")){
      if (!requireNamespace("SummarizedExperiment", quietly = TRUE)){
        character()
      } else {
        colnames(SummarizedExperiment::colData(object))
      }
    } else if (inherits(object, "AnnDataR6")){
      colnames(as.data.frame(object$obs))
    } else {
      character()
    }

  if (length(metadata_columns) == 0L){
    return(list(
      valid = FALSE,
      column = NULL,
      error = paste0(
        "Could not inspect metadata columns for this object. edgeR supports ",
        "Seurat, SingleCellExperiment, and AnnData objects."
      )
    ))
  }

  if (is.null(sample_id_colname) || length(sample_id_colname) == 0L ||
      is.na(sample_id_colname[[1]]) || !nzchar(sample_id_colname[[1]])){
    return(list(
      valid = FALSE,
      column = NULL,
      error = paste0(
        "No biological sample metadata field is configured. Select a ",
        "Patient/Sample Metadata Variable in the dataset configuration."
      )
    ))
  }

  if (!is.character(sample_id_colname) || length(sample_id_colname) != 1L){
    return(list(
      valid = FALSE,
      column = NULL,
      error = "The configured biological sample field must be one column name."
    ))
  }

  if (!sample_id_colname %in% metadata_columns){
    return(list(
      valid = FALSE,
      column = sample_id_colname,
      error = paste0(
        "Configured biological sample field \"", sample_id_colname,
        "\" is not present in the current object."
      )
    ))
  }

  list(valid = TRUE, column = sample_id_colname, error = NULL)
}
