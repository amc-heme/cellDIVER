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
              href = paste0("https://amc-heme.github.io/cellDIVER/articles/", 
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
  # edgeR pseudobulk is offered in both modes (marker identification runs it
  # one-vs-rest) whenever the dataset has a usable sample column. The server
  # narrows the choices to Wilcoxon otherwise, and when groups are defined by
  # a feature expression threshold.
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
          # The biological sample (replicate) of each cell. Choices are the
          # categorical metadata of the dataset; the column configured in the
          # dataset config is selected by default.
          selectInput(
            inputId = ns("edger_sample_col"),
            label = "Sample / Replicate Column:",
            choices = NULL
          ),
          # The contrast selector only applies to differential expression:
          # marker identification always runs each group vs. the rest
          div(
            id = ns("edger_contrast_mode_ui"),
            selectInput(
              inputId = ns("edger_contrast_mode"),
              label = "Contrasts to Run:",
              choices = c(
                "Single contrast" = "single",
                "All pairwise comparisons" = "pairwise",
                "Each selected group vs. rest" = "one_vs_rest",
                "Selected groups vs. reference" = "reference"
              ),
              selected = "single"
            )
          ),
          hidden(
            div(
              id = ns("edger_multicontrast_options"),
              pickerInput(
                inputId = ns("edger_groups"),
                label = "Groups to Test:",
                choices = NULL,
                selected = character(0),
                multiple = TRUE,
                options = list(
                  "actions-box" = TRUE,
                  "selected-text-format" = "count > 5",
                  "none-selected-text" = "All groups"
                )
              ),
              hidden(
                div(
                  id = ns("edger_reference_options"),
                  selectInput(
                    inputId = ns("edger_reference_group"),
                    label = "Reference Group:",
                    choices = NULL
                  )
                )
              ),
              tags$p(
                "Leave Groups to Test empty to include all available groups.",
                class = "help-block"
              )
            )
          ),
          numericInput(
            inputId = ns("edger_min_cells"),
            label = "Minimum Cells per Pseudobulk Sample:",
            value = 10L,
            min = 1L,
            step = 1L
          ),
          tags$p(
            paste0(
              "edgeR sums raw counts per sample and group and needs at least ",
              "two samples per group. Contrasts without enough samples are ",
              "tested with the cell-level Wilcoxon test instead; the plan ",
              "below updates as you change the selections."
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
#' @param subset_filters Reactive returning the filter list of the subset
#' selections module. The edgeR preflight resolves the categorical filters from
#' metadata so it can report the replication of the subset the user is about
#' to submit.
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
           valid_features,
           subset_filters = reactive(list())
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

        ## 1.1. Sample columns usable for pseudobulk ####
        # Categorical metadata with at least two values, from the config's
        # unique values (plus the configured sample column when it is not an
        # exposed category). Metadata only: nothing is read from the matrix.
        sample_column_candidates <-
          reactive(
            label = "Test Selections: Sample Column Candidates",
            {
              dge_sample_column_candidates(
                object = object(),
                unique_metadata = unique_metadata(),
                configured_column = sample_id_colname()
              )
            })

        # Resolve the effective method defensively so an asynchronous input
        # update can never submit edgeR for an unsupported configuration:
        # there must be a sample column to aggregate by, and groups defined
        # by a feature expression threshold stay cell-level.
        edgeR_available <-
          reactive({
            length(sample_column_candidates()) > 0 &&
              dge_object_supports_edger(object()) &&
              !isTruthy(input$use_feature_expression)
          })

        dge_method <-
          reactive({
            resolve_dge_method(
              requested_method = input$method,
              dge_mode = dge_mode(),
              use_feature_expression = input$use_feature_expression,
              sample_column_available = edgeR_available()
            )
          })

        observeEvent(
          list(dge_mode(), input$use_feature_expression, edgeR_available()),
          label = "Test Selections: Update DGE Method Choices",
          ignoreNULL = FALSE,
          {
            choices <- dge_method_choices(
              dge_mode = dge_mode(),
              use_feature_expression = input$use_feature_expression,
              sample_column_available = edgeR_available()
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

        # Marker identification runs every group against the rest; the
        # contrast selector only applies to differential expression.
        edger_contrast_mode <- reactive({
          if (identical(dge_mode(), "mode_marker")){
            "one_vs_rest"
          } else {
            resolve_edger_contrast_mode(input$edger_contrast_mode)
          }
        })

        observe(
          label = "Test Selections: Show/Hide Contrast Selector",
          {
            if (identical(dge_mode(), "mode_dge")){
              showElement(id = "edger_contrast_mode_ui")
            } else {
              hideElement(id = "edger_contrast_mode_ui")
            }
          }
        )

        observe({
          multicontrast <- identical(dge_method(), "edger") &&
            identical(dge_mode(), "mode_dge") &&
            !identical(edger_contrast_mode(), "single")
          if (multicontrast){
            showElement(id = "edger_multicontrast_options")
          } else {
            hideElement(id = "edger_multicontrast_options")
          }

          if (multicontrast && identical(edger_contrast_mode(), "reference")){
            showElement(id = "edger_reference_options")
          } else {
            hideElement(id = "edger_reference_options")
          }
        })
        
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
              } else if (!identical(dge_method(), "edger") ||
                         identical(edger_contrast_mode(), "single")) {
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

        observe({
          choices <- group_choices()
          selected_groups <- isolate(input$edger_groups)
          selected_groups <- selected_groups[selected_groups %in% choices]
          updatePickerInput(
            session = session,
            inputId = "edger_groups",
            choices = choices,
            selected = selected_groups
          )

          selected_reference <- isolate(input$edger_reference_group)
          if (!isTruthy(selected_reference) || !selected_reference %in% choices){
            selected_reference <- if (length(choices)) choices[[1]] else character()
          }
          updateSelectInput(
            session = session,
            inputId = "edger_reference_group",
            choices = choices,
            selected = selected_reference
          )
        })

        ## 3.3.1. Sample / replicate column menu ####
        # Candidate columns minus the current group by variable (a sample
        # cannot also be the group). The configured column is the default,
        # and a column the user picked is kept while it remains valid.
        observe(
          label = "Test Selections: Update Sample Column Menu",
          {
            choices <- setdiff(sample_column_candidates(), group_by_category())
            current <- isolate(input$edger_sample_col)
            selected <- dge_sample_column_selection(
              current = current,
              configured = sample_id_colname(),
              choices = choices
            )
            updateSelectInput(
              session = session,
              inputId = "edger_sample_col",
              choices = choices,
              selected = selected
            )
          })

        edger_sample_col <-
          reactive({
            choices <- setdiff(sample_column_candidates(), group_by_category())
            column <- input$edger_sample_col
            if (isTruthy(column) && column %in% choices) column else NULL
          })

        ## 3.3.2. edgeR preflight ####
        # Metadata-only preview of the pseudobulk design for the groups,
        # subset and minimum cells currently selected, through the same
        # replication rules scDE applies when the test runs. The inputs are
        # debounced so a picker with "select all" does not re-run the preflight
        # once per click, and nothing runs while Wilcoxon is selected.
        edger_preflight_inputs <-
          reactive(
            label = "Test Selections: edgeR Preflight Inputs",
            {
              if (!identical(dge_method(), "edger")) return(NULL)
              if (is.null(edger_sample_col())) return(NULL)
              list(
                sample_by = edger_sample_col(),
                group_by = group_by_category(),
                dge_mode = dge_mode(),
                contrast_mode = edger_contrast_mode(),
                group_1 = input$group_1,
                group_2 = input$group_2,
                groups = input$edger_groups,
                reference_group = input$edger_reference_group,
                min_cells = input$edger_min_cells,
                filters = subset_filters()
              )
            })

        edger_preflight_inputs_debounced <-
          debounce(edger_preflight_inputs, millis = 400)

        edger_preflight <-
          reactive(
            label = "Test Selections: edgeR Preflight",
            {
              inputs <- edger_preflight_inputs_debounced()
              if (is.null(inputs)) return(NULL)
              tryCatch(
                dge_edger_preflight(object = object(), inputs = inputs),
                error = function(err_cnd){
                  log_warn(
                    paste0("edgeR preflight failed: ", conditionMessage(err_cnd))
                  )
                  list(status = "error", message = conditionMessage(err_cnd))
                }
              )
            })

        output$edger_sample_status <-
          renderUI({
            req(edgeR_available(), identical(dge_method(), "edger"))
            edger_preflight_ui(edger_preflight(), edger_sample_col())
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

            if (show_feature_expression_control(dge_mode(), dge_method())){
              showElement(
                id = target_id
                )
              } else {
                hideElement(
                  id = target_id
                  )
                }
            })

        # edgeR requires groups defined by sample-level categorical metadata.
        # Clear a stale threshold selection before hiding its control.
        observeEvent(
          dge_method(),
          label = "Test Selection: Disable Feature Expression for edgeR",
          ignoreInit = TRUE,
          {
            if (identical(dge_method(), "edger") &&
                isTruthy(input$use_feature_expression)){
              updateCheckboxInput(
                session = session,
                inputId = "use_feature_expression",
                value = FALSE
              )
            }
          }
        )
        
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
                          `edger_contrast_mode` =
                            if (identical(dge_method(), "edger")){
                              edger_contrast_mode()
                            } else {
                              "single"
                            },
                          `edger_groups` =
                            if (identical(dge_method(), "edger") &&
                                !identical(edger_contrast_mode(), "single")){
                              input$edger_groups
                            } else {
                              NULL
                            },
                          `edger_reference_group` =
                            if (identical(dge_method(), "edger") &&
                                identical(edger_contrast_mode(), "reference")){
                              input$edger_reference_group
                            } else {
                              NULL
                            },
                          `edger_sample_col` =
                            if (identical(dge_method(), "edger")){
                              edger_sample_col()
                            } else {
                              NULL
                            },
                          `edger_sample_error` =
                            if (identical(dge_method(), "edger") &&
                                is.null(edger_sample_col())){
                              dge_missing_sample_column_message
                            } else {
                              NULL
                            },
                          `edger_min_cells` =
                            if (identical(dge_method(), "edger")){
                              input$edger_min_cells
                            } else {
                              NULL
                            },
                          `edger_preflight` =
                            if (identical(dge_method(), "edger")){
                              edger_preflight()
                            } else {
                              NULL
                            }
                          )
                        )
                      }
                  } else if (dge_mode() == "mode_marker"){
                    # Include classes when marker identification
                    # is the selected mode. With edgeR, every group is tested
                    # against the rest (a one-vs-rest multi-contrast run).
                    return(
                      list(
                        `dge_mode` = dge_mode(),
                        `dge_method` = dge_method(),
                        # Leave group_mode defined for compatability with
                        # conditional statements in DGE tab, but set to "none"
                        `group_mode` = "none",
                        `group_by` = group_by_category(),
                        `edger_contrast_mode` =
                          if (identical(dge_method(), "edger")){
                            "one_vs_rest"
                          } else {
                            "single"
                          },
                        `edger_groups` = NULL,
                        `edger_reference_group` = NULL,
                        `edger_sample_col` =
                          if (identical(dge_method(), "edger")){
                            edger_sample_col()
                          } else {
                            NULL
                          },
                        `edger_sample_error` =
                          if (identical(dge_method(), "edger") &&
                              is.null(edger_sample_col())){
                            dge_missing_sample_column_message
                          } else {
                            NULL
                          },
                        `edger_min_cells` =
                          if (identical(dge_method(), "edger")){
                            input$edger_min_cells
                          } else {
                            NULL
                          },
                        `edger_preflight` =
                          if (identical(dge_method(), "edger")){
                            edger_preflight()
                          } else {
                            NULL
                          }
                        )
                      )
                  }
                }
              }) # End reactive
        
        return(selections)
        })
}

# Metadata column names cellDIVER adds to a subset for the DGE tab
dge_metacluster_column <- "metacluster"
dge_threshold_column <- "simple_expr_threshold"

dge_missing_sample_column_message <- paste0(
  "Choose a sample / replicate column for edgeR, or select Wilcoxon."
)

# Return method choices for the current analysis mode. Named values are passed
# directly to Shiny select inputs. edgeR is offered in both modes (marker
# identification runs it one-vs-rest) when a sample column is available,
# except for groups defined by a feature expression threshold.
dge_method_choices <- function(dge_mode,
                               use_feature_expression = FALSE,
                               sample_column_available = TRUE){
  choices <- c("Wilcoxon rank-sum" = "wilcoxon")

  # `dge_mode` is NULL while the mode input initializes: `NULL %in% x` is
  # logical(0), and `logical(0) && ...` is NA, so wrap it in isTRUE()
  if (isTRUE(dge_mode %in% c("mode_dge", "mode_marker")) &&
      !isTRUE(use_feature_expression) &&
      isTRUE(sample_column_available)){
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
                               use_feature_expression = FALSE,
                               sample_column_available = TRUE){
  allowed_methods <- unname(
    dge_method_choices(
      dge_mode, isTRUE(use_feature_expression), isTRUE(sample_column_available)
    )
  )

  if (is.null(requested_method) || !requested_method %in% allowed_methods){
    "wilcoxon"
  } else {
    requested_method
  }
}

# Human-readable statistical method used in the submitted analysis. This is
# shared by the result summary so it never reports Wilcoxon after an edgeR run.
# `design_type` ("independent" / "paired") qualifies an edgeR run; `fallback`
# marks a run where every contrast fell back to the cell-level test.
dge_method_label <- function(method, design_type = NULL, fallback = FALSE){
  if (identical(method, "edger")){
    if (isTRUE(fallback)){
      return("Cell-level Wilcoxon rank sum (edgeR could not run)")
    }
    label <- "edgeR pseudobulk quasi-likelihood"
    if (identical(design_type, "paired")){
      label <- paste0(label, " (paired by sample)")
    }
    label
  } else {
    "Wilcoxon rank sum"
  }
}

# Objects on which the pseudobulk test can run. MuData objects loaded through
# reticulate have no SCUBA accessors; they need the file-backed loader.
dge_object_supports_edger <- function(object){
  !inherits(object, c("md._core.mudata.MuData", "mudata._core.mudata.MuData"))
}

# Categorical metadata usable as a sample / replicate column: the configured
# categories with at least two values, plus the configured sample column when
# it is not among them (it may be hidden from the browser menus).
dge_sample_column_candidates <- function(object,
                                         unique_metadata,
                                         configured_column = NULL){
  candidates <- names(unique_metadata)[
    vapply(unique_metadata, function(values) length(values) >= 2L, logical(1))
  ]
  configured <- dge_sample_column_status(object, configured_column)
  if (configured$valid && !configured$column %in% candidates){
    n_values <- tryCatch(
      SCUBA::n_unique(object, meta_var = configured$column),
      error = function(err_cnd) 0L
    )
    if (isTRUE(n_values >= 2L)){
      candidates <- c(configured$column, candidates)
    }
  }
  unique(candidates)
}

# The sample column to select: the user's current choice while it is valid,
# then the configured column, then the first candidate.
dge_sample_column_selection <- function(current, configured, choices){
  if (!length(choices)) return(character(0))
  if (isTruthy(current) && current %in% choices) return(current)
  if (isTruthy(configured) && configured %in% choices) return(configured)
  choices[[1]]
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

# Normalize the multi-contrast selector while Shiny inputs initialize.
resolve_edger_contrast_mode <- function(contrast_mode){
  allowed <- c("single", "pairwise", "one_vs_rest", "reference")
  if (length(contrast_mode) == 1L && !is.na(contrast_mode) &&
      contrast_mode %in% allowed){
    contrast_mode
  } else {
    "single"
  }
}

# Feature-expression thresholds define cell-level groups and therefore remain
# available only for differential-expression Wilcoxon analyses.
show_feature_expression_control <- function(dge_mode, dge_method){
  identical(dge_mode, "mode_dge") && !identical(dge_method, "edger")
}

# Metadata column names of any object cellDIVER serves. SCUBA covers Seurat,
# SingleCellExperiment, AnnData and file-backed objects (its default method
# only warns); MuData objects loaded through reticulate are handled here.
dge_metadata_columns <- function(object){
  if (inherits(object, c("md._core.mudata.MuData", "mudata._core.mudata.MuData"))){
    return(tryCatch(colnames(object$obs), error = function(err_cnd) character()))
  }
  columns <- withCallingHandlers(
    tryCatch(SCUBA::meta_varnames(object), error = function(err_cnd) NULL),
    warning = function(warn_cnd) invokeRestart("muffleWarning")
  )
  if (is.null(columns)) character() else as.character(columns)
}

# Validate the configuration-level biological sample field. Cell-level missing
# values and biological replication are checked by the preflight.
dge_sample_column_status <- function(object, sample_id_colname){
  metadata_columns <- dge_metadata_columns(object)

  if (length(metadata_columns) == 0L){
    return(list(
      valid = FALSE,
      column = NULL,
      error = paste0(
        "Could not inspect metadata columns for this object. edgeR supports ",
        "Seurat, SingleCellExperiment, AnnData and file-backed (.h5ad / .h5mu) ",
        "objects."
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

# edgeR preflight ---------------------------------------------------------------
# A metadata-only preview of the pseudobulk design. The cells of the subset the
# user is about to submit are resolved from the categorical subset filters, the
# group of each cell from the selections (collapsed into metaclusters when a
# group picker holds several values), and scDE's replication rules decide, per
# contrast, whether edgeR will run or the cell-level Wilcoxon test will be used.

# Cells matching the categorical filters of the subset menu, or NULL for every
# cell. Expression filters are not available under edgeR, so metadata suffices.
dge_preflight_cells <- function(object, filter_list){
  filter_list <- Filter(
    function(filter_i) identical(filter_i$type, "categorical"),
    filter_list
  )
  if (!length(filter_list)) return(NULL)
  variables <- unique(vapply(filter_list, function(filter_i) filter_i$var, character(1)))
  metadata <- SCUBA::fetch_metadata(object, vars = variables)
  keep <- rep(TRUE, nrow(metadata))
  for (filter_i in filter_list){
    keep <- keep & as.character(metadata[[filter_i$var]]) %in% filter_i$value
  }
  rownames(metadata)[keep]
}

# The group label of every cell in `cells`, and the groups to contrast. For a
# single contrast, multi-valued pickers are collapsed into metaclusters named
# like the subset's metacluster column; cells outside the two groups get NA and
# are dropped by SCUBA::pseudobulk_design(). Other modes use the metadata as-is.
dge_preflight_group_labels <- function(object,
                                       group_by,
                                       cells = NULL,
                                       contrast_mode = "single",
                                       group_1 = NULL,
                                       group_2 = NULL){
  values <- SCUBA::fetch_metadata(
    object, vars = group_by, cells = cells, return_class = "vector"
  )
  values <- as.character(values)
  cell_names <- if (is.null(cells)) SCUBA::get_all_cells(object) else cells
  if (identical(contrast_mode, "single")){
    label_1 <- vector_to_text(group_1)
    label_2 <- vector_to_text(group_2)
    labels <- ifelse(
      values %in% group_1, label_1,
      ifelse(values %in% group_2, label_2, NA_character_)
    )
    names(labels) <- cell_names
    return(list(labels = labels, group_1 = label_1, group_2 = label_2))
  }
  names(values) <- cell_names
  list(labels = values, group_1 = NULL, group_2 = NULL)
}

# The contrasts a run will test, as the data.frame scDE::check_pseudobulk_design()
# expects, or NULL while the selections are incomplete.
dge_preflight_contrasts <- function(dge_mode,
                                    contrast_mode,
                                    observed_groups,
                                    group_1 = NULL,
                                    group_2 = NULL,
                                    groups = NULL,
                                    reference_group = NULL){
  observed_groups <- observed_groups[!is.na(observed_groups)]
  if (identical(dge_mode, "mode_marker") || identical(contrast_mode, "one_vs_rest")){
    selected <- if (identical(dge_mode, "mode_marker") || !length(groups)){
      observed_groups
    } else {
      intersect(groups, observed_groups)
    }
    if (length(observed_groups) < 2L || !length(selected)) return(NULL)
    return(data.frame(group_1 = selected, group_2 = "rest", stringsAsFactors = FALSE))
  }
  if (identical(contrast_mode, "single")){
    if (!length(group_1) || !length(group_2)) return(NULL)
    return(data.frame(group_1 = group_1, group_2 = group_2, stringsAsFactors = FALSE))
  }
  selected <- if (!length(groups)) observed_groups else intersect(groups, observed_groups)
  if (identical(contrast_mode, "pairwise")){
    if (length(selected) < 2L) return(NULL)
    pairs <- utils::combn(selected, 2L)
    return(data.frame(group_1 = pairs[1, ], group_2 = pairs[2, ], stringsAsFactors = FALSE))
  }
  if (identical(contrast_mode, "reference")){
    if (!isTruthy(reference_group) || !reference_group %in% observed_groups) return(NULL)
    numerators <- setdiff(selected, reference_group)
    if (!length(numerators)) return(NULL)
    return(data.frame(group_1 = numerators, group_2 = reference_group, stringsAsFactors = FALSE))
  }
  NULL
}

# Run the preflight for the current selections. Returns a list with `status`
# ("ready", "incomplete") and, when ready, scDE's verdict (`contrasts` table
# with method, design type and reason per contrast) plus the design.
dge_edger_preflight <- function(object, inputs){
  min_cells <- inputs$min_cells
  if (!isTruthy(min_cells) || !is.finite(min_cells) || min_cells < 1){
    min_cells <- 10L
  }
  if (identical(inputs$contrast_mode, "single") &&
      (!length(inputs$group_1) || !length(inputs$group_2))){
    return(list(status = "incomplete", message = "Select the groups to compare."))
  }
  cells <- dge_preflight_cells(object, inputs$filters)
  if (!is.null(cells) && !length(cells)){
    return(list(status = "incomplete", message = "The current subset filters match no cells."))
  }
  labels <- dge_preflight_group_labels(
    object, inputs$group_by, cells = cells, contrast_mode = inputs$contrast_mode,
    group_1 = inputs$group_1, group_2 = inputs$group_2
  )
  contrasts <- dge_preflight_contrasts(
    dge_mode = inputs$dge_mode,
    contrast_mode = inputs$contrast_mode,
    observed_groups = unique(labels$labels),
    group_1 = labels$group_1,
    group_2 = labels$group_2,
    groups = inputs$groups,
    reference_group = inputs$reference_group
  )
  if (is.null(contrasts)){
    return(list(status = "incomplete", message = "Select the groups to compare."))
  }
  design <- SCUBA::pseudobulk_design(
    object, sample_by = inputs$sample_by, group_by = labels$labels, cells = cells
  )
  verdict <- scDE::check_pseudobulk_design(
    design, contrasts = contrasts, min_cells = as.integer(min_cells)
  )
  list(
    status = "ready",
    sample_by = inputs$sample_by,
    min_cells = as.integer(min_cells),
    design = design,
    contrasts = verdict$contrasts,
    any_edger = verdict$any_edger,
    any_fallback = verdict$any_fallback,
    warnings = verdict$warnings
  )
}

# One line per contrast describing the test it will get and why.
edger_preflight_text <- function(preflight){
  if (is.null(preflight)) return(character())
  if (!identical(preflight$status, "ready")) return(preflight$message)
  contrasts <- preflight$contrasts
  vapply(seq_len(nrow(contrasts)), function(i){
    row <- contrasts[i, ]
    if (identical(row$method, "edgeR")){
      design <- if (identical(row$design_type, "paired")){
        paste0("paired design, ", row$paired_samples, " samples in both groups")
      } else {
        paste0("independent samples, ", row$samples_group_1, " vs. ", row$samples_group_2)
      }
      paste0(row$contrast, ": edgeR (", design, ")")
    } else {
      paste0(row$contrast, ": cell-level Wilcoxon (", row$reason, ")")
    }
  }, character(1))
}

# The status block shown under the edgeR options.
edger_preflight_ui <- function(preflight, sample_column = NULL){
  if (is.null(sample_column)){
    return(
      div(
        tags$strong("edgeR unavailable: "),
        dge_missing_sample_column_message,
        class = "alert alert-warning"
      )
    )
  }
  if (is.null(preflight)){
    return(tags$p("Checking replication...", class = "help-block"))
  }
  if (identical(preflight$status, "error")){
    return(
      div(
        tags$strong("Could not check replication: "),
        preflight$message,
        class = "alert alert-warning"
      )
    )
  }
  if (!identical(preflight$status, "ready")){
    return(tags$p(preflight$message, class = "help-block"))
  }
  lines <- edger_preflight_text(preflight)
  n_edger <- sum(preflight$contrasts$method == "edgeR")
  n_fallback <- sum(preflight$contrasts$method == "wilcoxon")
  summary <- paste0(
    "Sample column: ", sample_column, ". edgeR: ", n_edger, " contrast",
    if (n_edger == 1L) "" else "s",
    if (n_fallback > 0L) paste0("; cell-level Wilcoxon fallback: ", n_fallback) else "",
    "."
  )
  div(
    class = if (n_fallback > 0L) "alert alert-warning" else "text-success",
    tags$p(tags$strong(summary)),
    tags$ul(lapply(lines, tags$li)),
    if (length(preflight$warnings)){
      tags$p(paste(preflight$warnings, collapse = " "), class = "help-block")
    }
  )
}
