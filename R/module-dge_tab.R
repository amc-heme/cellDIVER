#' Differential Expression Tab Module
#'
#' @param id ID to use for module. All inputs and outputs created will be
#' namespaced using this ID.
#' @param unique_metadata a list of the unique metadata values for each of the
#' metadata categories listed in the config file. This is generated in the main
#' app.
#' @param metadata_config the metadata section of the config file imported in 
#' the main server function.
#' @param meta_categories the metadata variables exposed by the config user.
#' @param meta_choices a named vector giving the metadata categories defined in
#' the config file, with their respective labels for display in dropdown menus.
#' This is defined in the main server function.
#' @param auto_dictionary_path Path to the temporary file used for storing the
#' auto-generated object dictionary (created in the main app)
#' @param string_subsetting_href URL of the string subsetting vignette. 
#'
#' @return Module UI code (list of shiny.tag elements)
#'
#' @noRd
dge_tab_ui <- function(id,
                       unique_metadata,
                       metadata_config,
                       meta_categories,
                       meta_choices,
                       auto_dictionary_path,
                       string_subsetting_href
                       ){
  # Namespace function
  ns <- NS(id)
  
  # UI for DGE Tab
  fluidPage(
    sidebarLayout(
      # 1. Options Panel -------------------------------------------------------
      sidebarPanel(
        # Add a container to display a waiter over when the options are updating
        div(
          id = ns("sidebar"),
          tags$h3("Differential Gene Expression"),
          tags$p(
          "Use the dropdown menus below to select the desired test, the groups 
          to use, and the subset on which to perform the test."
          ),
          
          collapsible_panel(
            inputId = ns("dge_test_interface"),
            label = "Test Options",
            active = TRUE,
            # Menus to choose test (DGE or marker identification and 
            # classes/groups to include). Uses dge_test_selection module
            dge_test_selections_ui(
              id = ns("test_selections"),
              meta_choices = meta_choices
              ),
            
            # Checkbox to return positive markers only (shown for both modes)
            checkboxInput(
              inputId = ns("pos"),
              label = "Positive Markers Only",
              value = TRUE
            ),
            
            # Menus to choose subset (placed within collapsible panel)
            collapsible_panel(
              inputId = ns("subset_selections_collapsible"),
              label = "Subset Options",
              active = TRUE,
              class = "collapsible-panel-secondary",
              subset_selections_ui(
                id = ns("subset_selections"),
                unique_metadata = unique_metadata,
                metadata_config = metadata_config,
                auto_dictionary_path = auto_dictionary_path,
                string_subsetting_href = string_subsetting_href
                )
              ),
            
            # Submit button
            actionButton(
              inputId = ns("submit"),
              label = "Run DGE",
              style = "display: block; width: 100%;",
              class = "button-primary"
              )
            ),
          
          # Panel used for further filtering of DGE table
          dge_table_filtering_ui(
            id = ns("dge_table_filtering")
            ),
          
          # UMAP options panel (hidden, displays after plot is created)
          hidden(
            # Collapsible panel must be placed in a container for show/hide
            # to work properly (the content of the panel is not a descendent
            # of the button tag to open/close the panel, so show/hide commands
            # will only cause the button to show/hide if they target the 
            # panel's ID)
            div(
              id = ns("options_panel_ui"),
              collapsible_panel(
                inputId = ns("umap_options_panel"),
                label = "UMAP Options",
                active = TRUE,
                # Selection of group.by variable
                selectInput(
                  inputId = ns("umap_group_by"),
                  label = "Metadata to Group by:",
                  # Remove "none" from selectable
                  # options to group by
                  choices =
                    meta_choices()[!meta_choices() %in% "none"],
                  # First category in meta_choices is selected
                  # by default
                  selected =
                    meta_choices()[!meta_choices() %in% "none"][1]
                  )
                )
              )
            ),
            
          # Download Button
          # Hidden, displays when the table is created
          hidden(
            downloadButton(
              outputId = ns("download_table"),
              label = "Download Table",
              icon = icon("table")
              )
            )
          ) # End dge-sidebar div
        ), # End sidebarPanel (1.3.1)
      
      # 2. Main Panel ----------------------------------------------------------
      mainPanel(
        # Spinner that displays above main panel while results are computing
        div(
          id = ns("main_panel_spinner"), 
          class = "spinner-container-main",
          # Content of main panel (hidden until test 
          # results are submitted and computed)
          hidden(
            div(
              id = ns("main_panel"),
              # Title of stats page (text depends on test chosen)
              tags$h2(
                textOutput(
                  outputId = ns("main_panel_title"),
                  inline = TRUE
                ),
                class = "center"
              ),
              
              # Summary stats for test used
              tags$h3(
                "Test Summary", 
                class = "center"
              ),
              # Subset Stats Module 
              subset_stats_ui(
                id = ns("subset_stats"),
                tab = "dge",
                metadata_config = metadata_config,
                meta_categories = meta_categories
              ),
              
              # DGE Table (uses DT data table)
              tags$h3(
                "DGE Table",
                class = "center"
              ),
              DTOutput(
                outputId = ns("table"),
                width = "95%"
              ),
              
              # UMAP
              # Title: depends on test used
              uiOutput(
                outputId = ns("umap_title_ui")
              ),
              
              # UMAP container
              plotOutput(
                outputId = ns("umap"),
                height = "600px"
              ),
              
              verbatimTextOutput(
                outputId = ns("dge_filter_status")
                )
              
              # # TEMP: display reactive variables used in this tab while developing
              # "Test Selection Output",
              # verbatimTextOutput(outputId = ns("test_selection_output")),
              # "Subset Selection Output",
              # verbatimTextOutput(outputId = ns("subset_selection_output")),
              # "Subset information:",
              # verbatimTextOutput(outputId = ns("subset_info_output")),
              # "Subset Stats Output:",
              # verbatimTextOutput(outputId = ns("subset_stats_output"))
              )
            )
          ) 
        ) # End MainPanel
      ) # End sidebarLayout
    ) # End fluidPage
  }

#' Differential Gene Expression Tab Module
#'
#' The server instance of the differential gene expression tab module. One 
#' instance should be created for each object to avoid input collisions between 
#' objects with similarly named metadata columns.
#'
#' @param id Used to match server function to ui function. "dge" is the 
#' recommended value to use.
#' @param object The Seurat Object defined in the main server function.
#' @param metadata_config The metadata section of the config file imported in 
#' the main server function.
#' @param assay_config The assays section of the config file imported in 
#' the main server function.
#' @param designated_genes_assay the name of the assay designated in the config 
#' file as the genes assay. If NULL, the first assay will be assumed to be the 
#' genes assay.
#' @param meta_categories a named vector of metadata categories retrieved from the 
#' config file.
#' @param unique_metadata a list of all the unique metadata values in the current 
# object for the categories defined in the config file.
#' @param meta_choices a named vector with name-value pairs for the display name 
#' of the metadata category and the key used to access the category in the 
#' Seurat Object.
#' @param valid_features 
#' @param object_trigger a reactive trigger that invalidates downstream 
#' reactives when the object is changed.
#' @param error_list a list of error messages to catch and notifications to 
#' display to the user based on the errors detected
#'
#' @return server code for the differential gene expression tab. Reactive 
#' expressions are contained within the namespace set by `id`
#'
#' @noRd
dge_tab_server <- function(id,
                           object,
                           metadata_config,
                           assay_config,
                           designated_genes_assay,
                           # This will replace metadata_config at some point
                           # (It is derived from the config file and is the only
                           # information used)
                           meta_categories,
                           unique_metadata,
                           meta_choices,
                           valid_features,
                           object_trigger,
                           error_list,
                           is_HDF5SummarizedExperiment
                           ){
  moduleServer(
    id,
    function(input, output, session){
      # Server namespace function: for dynamic UI and modules
      ns <- session$ns
      
      # Create spinners to display during computation in dge 
      
      # Spinner for options panel
      # Keeps user from being able to press download 
      # buttons before content is ready
      sidebar_spinner <-
        Waiter$new(
          id = ns("sidebar"),
          html = spin_loaders(id = 2, color = "#555588"),
          color = "#B1B1B188",
          # Gives manual control of showing/hiding spinner
          hide_on_render = FALSE
          )
      
      # Spinner for main panel
      # Displays until hidden at the end of computation
      main_spinner <-
        Waiter$new(
          id = ns("main_panel_spinner"),
          html = spin_loaders(id = 2,color = "#555588"),
          color = "#FFFFFF",
          # Gives manual control of showing/hiding spinner
          hide_on_render = FALSE
          )
      
      # 1. Process Selections for DGE Test --------------------------
      ## 1.1. Selections module
      test_selections <-
        dge_test_selections_server(
          id = "test_selections",
          object = object,
          unique_metadata = unique_metadata,
          metadata_config = metadata_config,
          assay_config = assay_config,
          meta_choices = meta_choices,
          valid_features = valid_features
          )
      
      ## 1.2 Chosen group by category 
      # Used in several downstream reactives
      group_by_category <-
        reactive(
          label = "DGE Tab: Group_by_category",
          {
            test_selections()$group_by
          })
      
      # 2. Process Subset Selection Options -------------------------
      ## 2.1. Remove the current group by variable from subsetting choices ####
      # For standard DGE, the selections for the group by metadata category 
      # are used to subset the object, so that scDE only compares 
      # the selected groups. The metadata variable used for forming groups must
      # be hidden from the subsetting menu to keep the user from being able to 
      # select a subset that does not allow comparison of the selected groups.
      hidden_variable <- 
        reactive(
          label = "DGE Tab: Define Hidden Subset Menu",
          {
            req(test_selections())
            
            # Metadata choices are not hidden during marker identification, 
            # or during DGE based on a feature expression threshold. 
            if (test_selections()$dge_mode == "mode_marker"){
              # Return NULL, to hide no choices
              NULL
            } else if (test_selections()$dge_mode == "mode_dge"){
              # DGE mode: hide choices in all cases except 
              # grouping based on a "simple_threshold".
              if (test_selections()$group_mode == "simple_threshold"){
                NULL
              } else {
                group_by_category()
              }
            }
          })
      

      ## 2.2. Subset selections module ####
      subset_selections <-
        subset_selections_server(
          id = "subset_selections",
          object = object,
          unique_metadata = unique_metadata,
          metadata_config = metadata_config,
          assay_config = assay_config,
          meta_categories = meta_categories,
          valid_features = valid_features,
          hide_menu = hidden_variable
          )
      
      # 3. Calculations ran after submit button is pressed ----------
      # Includes table, stats, and UMAP
      # Subset criteria (3.1) processed first,
      # UMAP (3.6) processed last
      
      ## 3.1. Process Submit button input ####
      # Pass value of action button to nested modules
      # to control reactivity
      submit_button <-
        reactive({
          input$submit
        })
      
      ## 3.2 Show Spinner
      # Runs after submit button is pressed
      show_spinner <-
        eventReactive(
          submit_button(),
          label = "DGE: Show Spinner",
          {
            print("DGE 3.2: Show Spinner")
            # Hide the main panel UI while calculations are performed
            # print(glue("hiding {ns('main_panel')}"))
            # Do not namespace IDs for hideElement 
            hideElement(id = "main_panel")
            
            # If the DGE table download button is visible, hide while 
            # a new table is being computed
            hideElement(
              id = "download_table"
              )
            
            # Do the same for the UMAP options panel
            hideElement(
              id = "options_panel_ui"
              )
            
            # Display spinners
            log_session(session)
            log_info("DGE Tab: Submit button pressed")
            sidebar_spinner$show()
            main_spinner$show()
            log_session(session)
            log_info("DGE Tab: Spinners displayed.")
            
            # Return the value of submit_button()
            # This makes this eventReactive change each time it is
            # ran, triggering the next reactive in the series
            submit_button()
          })
      
      ## 3.3. Determine if Metaclusters are Requested ####
      # This reactive expression saves the need to include the conditional 
      # statements within each time they are needed during the DGE computation
      metaclusters_present <- 
        eventReactive(
          show_spinner(),
          label = "DGE: Determine if Metaclusters are present",
          # All reactives in 3. must run at startup for output to be
          # properly generated (endless spinner results otherwise)
          # TODO: switch ignoreInit to TRUE and try un-suspending the dge table
          ignoreInit = FALSE,
          {
            print("DGE 3.3: metaclusters_present")
            # Return TRUE when all conditions below are met
            # DGE, with standard groups (thresholding not used)
            if (test_selections()$dge_mode == "mode_dge" &
                test_selections()$group_mode == "standard"){
              if (!is.null(test_selections()$group_1) &
                  !is.null(test_selections()$group_2)){
                # At least one group must have more than one 
                # member to create metaclusters 
                if (length(test_selections()$group_1) > 1 |
                    length(test_selections()$group_2) > 1){
                  return(TRUE)
                }
              }
            }
            
            # FALSE is returned if `return(TRUE)` is not reached in the series
            # of conditionals above (reduces the need to write else statements)
            # for each level of conditionals
            FALSE
          })
      
      ## 3.4. Determine if thresholding is being performed ####
      thresholding_present <-
        eventReactive(
          metaclusters_present(),
          label = "DGE: Determine if Thresholding is Requested",
          # All reactives in 3. must run at startup for output to be
          # properly generated (endless spinner results otherwise)
          # TODO: switch ignoreInit to TRUE and try un-suspending the dge table
          ignoreInit = FALSE,
          {
            print("DGE 3.4: thresholding_present")
            # Return TRUE when all conditions below are met
            # DGE, with standard groups (thresholding not used)
            if (test_selections()$dge_mode == "mode_dge" &
                test_selections()$group_mode %in% "simple_threshold"){
              TRUE
            } else {
              FALSE
              }
          })
      
      ## 3.5. Define subset criteria ####
      # Combines outputs from test selection and subset selection modules. 
      # The subset criteria are used in the make_subset() function and the 
      # subset stats module.
      dge_subset_criteria <-
        eventReactive(
          thresholding_present(),
          label = "DGE: Subset Criteria",
          # All reactives in 3. must run at startup for output to be
          # properly generated (endless spinner results otherwise)
          ignoreInit = FALSE,
          ignoreNULL = TRUE,
          {
            print("DGE 3.5: subset criteria")
            # Define subset criteria conditionally and return
            # The subset criteria are equal to the data selected in 
            # the subset_selections module, except in the case where 
            # two groups are being compared based on categorical metadata
            if (test_selections()$dge_mode == "mode_dge"){
              if (test_selections()$group_mode == "simple_threshold"){
                # "Simple threshold" DGE, based on feature expression
                # Return subset selections as-is
                subset_criteria <- subset_selections$selections()
              } else if (test_selections()$group_mode == "standard"){
                # "Standard" DGE
                # Add extra subset filter for the two groups selected
                # Fetch choices for group 1 and group 2
                choices <-
                  c(test_selections()$group_1, 
                    test_selections()$group_2)
                
                # Fetch subset selections
                # Must unpack from reactive to avoid modifying the
                # reactive with test_selections data
                subset_criteria <- subset_selections$selections()
                # Add group by metadata category with 
                # classes/groups to subset instructions
                subset_criteria[[length(subset_criteria) + 1]] <- 
                  list(
                    `type` = "categorical",
                    `mode` = NULL,
                    `var` = test_selections()$group_by,
                    # Add display name of variable for filter criteria display
                    `label` = metadata_config()[[test_selections()$group_by]]$label,
                    `value` = choices
                  )
              } else {
                # If the group mode is undefined, throw a warning 
                # and return subset selections as-is
                # This warning is very unlikely to be triggered, but may cause
                # errors downstream if it ever is
                warning(
                  paste0(
                    "DGE 3.5: Unrecognized group_mode setting from test_selections: ",
                    test_selections()$group_mode
                  )
                )
                
                subset_criteria <- subset_selections$selections()
              }
            } else if (test_selections()$dge_mode == "mode_marker"){
              # Marker identification
              # Return subset selections as-is
              subset_criteria <- subset_selections$selections()
            } else {
              # If the mode is undefined, throw a warning 
              # and return subset selections as-is
              # This warning is very unlikely to be triggered, but may cause
              # errors downstream if it ever is
              warning(
                paste0(
                  "DGE 3.5: Unrecognized dge_mode setting ",
                  "from test_selections: ",
                  test_selections()$dge_mode
                  )
                )
              
              subset_criteria <- subset_selections$selections()
            }
            
            # Return subset_criteria defined above
            return(subset_criteria)
          })
      
      ## 3.6. Form subset ####
      # Initialize variables to control reset of subset when the 
      # object is changed
      
      # object_init: a reactive value set to TRUE when a new object 
      # is loaded. The currently saved subset will be cleared when
      # object_init is set to TRUE
      object_init <- reactiveVal(FALSE)
      
      # Create a reactive trigger (triggers reset of subset when
      # new object is loaded)
      subset_trigger <- makeReactiveTrigger()
      
      # Upon object change: Set object_init to TRUE when an object 
      # is loaded, and trigger the subset eventReactive to run
      observeEvent(
        # Reacts to object_trigger defined in main server function
        object_trigger$depend(),
        label = "DGE: object_init(TRUE)",
        # Must use ignoreNULL = FALSE for reactive triggers
        ignoreNULL = FALSE,
        # Don't want this to run at startup or upon module creation
        ignoreInit = TRUE,
        {
          object_init(TRUE)
          subset_trigger$trigger()
        })
      
      subset <-
        eventReactive(
          c(dge_subset_criteria(), 
            subset_trigger$depend()),
          label = "DGE: Subset",
          ignoreNULL = FALSE,
          {
            print("DGE 3.6: make subset")
            
            subset <- 
              tryCatch(
                # Convention is to use function(cnd) for tryCatch, but "cnd"
                # is a function in the rlang package, as is "error"
                error = function(err_cnd){
                  # Log interpreted subset filters in the event of an error
                  log_info(
                    paste0(
                      "Subset filters entered:",
                      cellDIVER:::log_subset(
                        filter_list = subset_selections$selections()
                        )
                      )
                    )
                  
                  # Use error_handler to display notification to user
                  error_handler(
                    session,
                    err_cnd = err_cnd,
                    # Uses a list of
                    # subset-specific errors
                    error_list = error_list$subset_errors,
                    source_reactive = "subset, dge_tab module"
                  )
                  
                  # Hide the spinners
                  main_spinner$hide()
                  sidebar_spinner$hide()
                  
                  # Return NULL for subset to 
                  # discontinue downstream calculations
                  NULL
                  },
                # Begin tryCatch code
                {
                  # If object_init == TRUE, return the full object
                  if (object_init() == TRUE){
                    return(object())
                  } else {
                    # Otherwise, create subset from selections
                    subset <- 
                      make_subset(
                        object = object(),
                        filter_list = dge_subset_criteria()
                        )
                    
                    # Log subset created
                    cellDIVER:::log_subset(
                      filter_list = subset_selections$selections()
                    )
                    
                    # Modification of subset
                    # Metacluster Creation (if applicable)
                    if (metaclusters_present()){
                      # Unpack variables from test_selections for simplified code
                      metadata_column <- test_selections()$group_by
                      group_1 <- test_selections()$group_1
                      group_2 <- test_selections()$group_2
                      
                      # Create metacluster metadata
                      # For object class-agnostic code, the metadata table 
                      # must be pulled via an S3 method, edited as a table, and
                      # then saved to the object via another S3 method.
                      meta_table <- 
                        SCUBA::fetch_metadata(
                          subset,
                          full_table = TRUE
                        )
                      
                      # Use case_when to create metacluster 
                      meta_table$metacluster <- 
                        case_when(
                          # If the cell has a value for the group by category, 
                          # assign a group name to it based on the members of the
                          # group (for example, a group with "R" and "S" will be
                          # renamed ("R and S"))
                          meta_table[[metadata_column]] %in% group_1 ~
                            vector_to_text(group_1),
                          meta_table[[metadata_column]] %in% group_2 ~
                            vector_to_text(group_2),
                          TRUE ~ "Unspecified"
                          )
                      
                      # Save new metadata table to subset
                      subset <-
                        cellDIVER:::update_object_metadata(
                          subset,
                          table = meta_table
                        )
                    } # End if (metaclusters_present())
                    
                    # Groups based on feature expression thresholds
                    # Create new metadata column based on threshold
                    if (thresholding_present()){
                      print("Add metadata column for threshold")
                      # Fetch threshold defined in test_selections module
                      threshold <- test_selections()$threshold_value
                      # Fetch feature used in thresholding
                      feature <- test_selections()$threshold_feature
                      
                      # Fetch expression data for the selected feature
                      expr_data <- 
                        SCUBA::fetch_data(
                          object = subset,
                          vars = feature
                          )[,1]
                      
                      # Remove assay tag from feature name
                      # (Must be done after fetch_data, which requires 
                      # the machine-readable feature name)
                      feature <- 
                        hr_name(
                          feature,
                          assay_config = assay_config(),
                          use_suffix = FALSE
                          )
                      
                      if (session$userData$dev_mode == TRUE){
                        print("DGE simple expression threshold")
                        print("threshold")
                        print(threshold)
                        print("feature expression data")
                        print(head(expr_data))
                        print("Test of expr_data >= threshold")
                        print(head(expr_data >= threshold))
                        }
                      
                      # Pull metadata table from subset
                      # For object class-agnostic code, the metadata table 
                      # must be pulled via an S3 method, edited as a table, and
                      # then saved to the object via another S3 method.
                      meta_table <- 
                        SCUBA::fetch_metadata(
                          subset,
                          full_table = TRUE
                          )
                      
                      # Create metadata column based on simple threshold
                      meta_table$simple_expr_threshold <-
                        case_when(
                          # Avoiding "+" and "-" declarations for now
                          # "High" when expresssion value is greater than
                          # or equal to threshold, otherwise "Low".
                          expr_data >= threshold ~ glue("{feature} High"),
                          expr_data < threshold ~ glue("{feature} Low"),
                          TRUE ~ "error"
                        )
                      
                      # Save metadata table with expression threshold column
                      subset <- 
                        cellDIVER:::update_object_metadata(
                          subset,
                          meta_table
                        )
                      }
                    
                    # Return subset from tryCatch
                    subset
                  }
                }) # End tryCatch
            
            # Return subset from eventReactive
            subset
          })
      
      ## TEMP: Check Memory usage after making subset
      observeEvent(
        label = "DGE: Subset Memory Query",
        subset(),
        ignoreNULL = FALSE,
        {
          log_session(session)
          log_info(
            glue(
              "Memory used after creating subset in dge tab {to_GB(mem_used())}"
              )
            )
          })
      
      ## 3.7. DGE Continuation Conditional ####
      # After subset is computed, downstream computations should only 
      # proceed if the subset has been created as a result of 
      # pressing the submit button, and not as a result of the reset 
      # that ocurrs when switching tabs. 
      
      # Create reactive trigger: proceeds with computation if the 
      # subset is reset due to switching tabs 
      # (when object_init()==TRUE)
      continue <- makeReactiveTrigger()
      
      observeEvent(
        label = "DGE: Continuation Conditional",
        subset(),
        {
          print("DGE 3.7: Continuation conditional")
          
          if (object_init() == FALSE){
            # If object_init == FALSE, continue with DGE calculations
            continue$trigger()
          } else {
            # If object_init == TRUE, do not continue.
            
            # Set object_init() back to FALSE in this case
            object_init(FALSE)
            
            # In case spinners are displayed, remove them
            sidebar_spinner$hide()
            main_spinner$hide()
          }
          
        })
      
      ## 3.8. Compute subset stats ####
      subset_stats <- 
        subset_stats_server(
          id = "subset_stats",
          tab = "dge",
          subset = subset,
          meta_categories = meta_categories,
          # Responds to continuation conditional 
          event_expr = dge_table_content,
          group_by_category = group_by_category,
          metaclusters_present = metaclusters_present,
          thresholding_present = thresholding_present,
          dge_simple_threshold = 
            # Must put module output back into a reactive
            reactive({test_selections()$threshold_value}) 
          )
      
      ## 3.9. Run DGE ####
      dge_table_content <-
        eventReactive(
          # Chose the first reactive variable in the subset stats
          # list (all are updated simultaneously, and it is desired
          # for DGE to run after stats are computed)
          continue$depend(),
          label = "DGE: Run DGE",
          ignoreNULL = FALSE,
          ignoreInit = TRUE,
          {
            print("DGE 3.9: Run DGE")
            
            log_session(session)
            log_info("DGE Tab: Begin scDE DGE")
            
            dge_table <- 
              tryCatch(
                error = function(err_cnd){
                  # Use error_handler to display notification to user
                  error_handler(
                    session,
                    err_cnd = err_cnd,
                    error_list = error_list$dge_test_errors,
                    source_reactive = "dge_table, dge_tab module"
                  )
                  
                  # Hide the spinners
                  main_spinner$hide()
                  sidebar_spinner$hide()
                  
                  # Return nothing for the dge table in the event of an error
                  return(NULL)
                  },
                {
                  # Marker identification: check that the metadata used for 
                  # the test has at least two marker groups in the selected
                  # subset
                  if (test_selections()$dge_mode == "mode_marker"){
                    marker_groups <- SCUBA::n_unique(
                      subset(), 
                      meta_var = test_selections()$group_by
                      )
                    
                    # If less than two groups, return an error
                    if (marker_groups == 0){
                      stop("Selected subset has 0 marker groups.")
                    } else if (marker_groups == 1){
                      stop("Selected subset has only 1 marker group.")
                    }
                  }
                  
                  # Note: designated genes assay is no longer used
                  dge_table <-
                    # Use DGE generic to determine test to run
                    scDE::run_dge(
                      object = subset(),
                      group_by = 
                        if (metaclusters_present()){
                          "metacluster"
                        } else if (thresholding_present()){
                          "simple_expr_threshold"
                        } else {
                          group_by_category()
                        },
                      # Seurat assay: designated genes assay, or the first
                      # assay if undefined.
                      # This is only used for Seurat objects
                      seurat_assay =
                        if (isTruthy(designated_genes_assay())){
                          designated_genes_assay()
                        } else names(assay_config())[[1]],
                      # Positive genes only: based on user input
                      positive_only = input$pos,
                      # Report results using a log2 fold change
                      lfc_format = "log2",
                      # Show only adjusted p-value column
                      remove_raw_pval = FALSE
                      )
                  
                  log_session(session)
                  log_info("DGE Tab: Completed scDE DGE")
                  
                  # Compute on values of table (for use with automated tests)
                  # Commented out, column formatting here does not apply for 
                  # Seurat v5 objects with BPCells matrices
                  # if (session$userData$dev_mode == TRUE){
                  #   print("Colsums vector")
                  #   colsums_vector <- colSums(dge_table[3:8])
                  #   # Print vector to screen as code to be copied to the test
                  #   # scripts
                  #   cat(
                  #     paste0(
                  #       "c(", 
                  #       paste0(colsums_vector, collapse = ", "),
                  #       ")"
                  #       ),
                  #     "\n"
                  #     )
                  # }

                  return(dge_table)
                })
            
            dge_table
          })
      
      ## 3.10. Filter and display table ####
      ### 3.10.1. Table filtering interface ####
      dge_table_filters <-
        dge_table_filtering_server(
          id = "dge_table_filtering",
          # Uses table from 3.9 to populate interface
          dge_table = dge_table_content
          )
      
      ### 3.10.2. Filter dge table based on filtering inputs ####
      filtered_dge_table <-
        reactive(
          label = "DGE: Filter DGE Table",
          {
          # Runs only if a DGE test has completed
          req(dge_table_content())
          # Store the current state of the dge table
          # If none of the filter operations below run, the table will be 
          # returned from the function unchanged.
          dge_table <- dge_table_content()
          
          tryCatch(
            # Convention is to use function(cnd) for tryCatch, but "cnd"
            # is a function in the rlang package, as is "error", so err_cnd 
            # was used instead to avoid namespace conflicts
            error = function(err_cnd){
              # Log interpreted subset filters in the event of an error
              log_error(
                paste0(
                  "Error in filtering of dge table: \n",
                  err_cnd$message
                )
              )
              
              # Print state of filter inputs?
              
              # Use error_handler to display notification to user
              error_handler(
                session,
                err_cnd = err_cnd,
                # Uses a list of
                # subset-specific errors
                error_list = error_list$subset_errors
              )
              
              # Hide the spinners
              main_spinner$hide()
              sidebar_spinner$hide()
              
              # Return NULL for subset to 
              # discontinue downstream calculations
              NULL
            },
            # Begin tryCatch code
            {
              # Print statements for debugging when dev mode is TRUE
              # TODO: tests for filtering should be written, perhaps from the 
              # number of rows at each step. Store number of rows at each step as 
              # a special list for output to testing?
              if (session$userData$dev_mode == TRUE){
                print("Filtering of DGE table")
                print(paste0("Number of rows in original table: ", nrow(dge_table)))
                print("Pre-filtering table summary:")
                print(str(dge_table))
                print("Filtering by group:")
              }
              
              # Filter by group
              if (isTruthy(dge_table_filters$group())){
                dge_table <-
                  dge_table %>% 
                  dplyr::filter(
                    group %in% dge_table_filters$group()
                  )
              }
              
              if (session$userData$dev_mode == TRUE){
                print(paste0("Number of rows remaining: ", nrow(dge_table)))
                print("Filtering by feature:")
              }
              
              # Filter by feature
              if (isTruthy(dge_table_filters$feature())){
                dge_table <-
                  dge_table %>% 
                  dplyr::filter(
                    feature %in% dge_table_filters$feature()
                  )
              }
              
              if (session$userData$dev_mode == TRUE){
                print(paste0("Number of rows remaining: ", nrow(dge_table)))
                print("Filtering by expression:")
              }
              
              # Filter based on average expression within group
              # Proceed with this filter only if "avgExpr" is present in the table
              if ("avgExpr" %in% colnames(dge_table)){
                if (isTruthy(dge_table_filters$expression())){
                  if (!is.null(dge_table_filters$expression()$min) & 
                      !is.null(dge_table_filters$expression()$max)){
                    # If both values are defined, filter based on both values
                    dge_table <-
                      dge_table %>% 
                      dplyr::filter(avgExpr >= dge_table_filters$expression()$min &
                                      avgExpr <= dge_table_filters$expression()$max)
                  } else if (!is.null(dge_table_filters$expression()$min) & 
                             is.null(dge_table_filters$expression()$max)){
                    # Min is defined but not the max
                    # filter based on the min
                    dge_table <-
                      dge_table %>% 
                      dplyr::filter(avgExpr >= dge_table_filters$expression()$min)
                  } else if (is.null(dge_table_filters$expression()$min) & 
                             !is.null(dge_table_filters$expression()$max)){
                    # Max is defined but not the min 
                    # filter based on the max
                    dge_table <-
                      dge_table %>% 
                      dplyr::filter(avgExpr <= dge_table_filters$expression()$max)
                  }
                }
              }
              
              if (session$userData$dev_mode == TRUE){
                print(paste0("Number of rows remaining: ", nrow(dge_table)))
                print("Filtering by log-fold change:")
              }
              
              # Filter based on lfc values
              if (isTruthy(dge_table_filters$lfc())){
                if (!is.null(dge_table_filters$lfc()$min) & 
                    !is.null(dge_table_filters$lfc()$max)){
                  # If both values are defined, filter based on both values
                  dge_table <-
                    dge_table %>% 
                    dplyr::filter(log2FC >= dge_table_filters$lfc()$min &
                                    log2FC <= dge_table_filters$lfc()$max)
                } else if (!is.null(dge_table_filters$lfc()$min) & 
                           is.null(dge_table_filters$lfc()$max)){
                  # Min is defined but not the max
                  # filter based on the min
                  dge_table <-
                    dge_table %>% 
                    dplyr::filter(log2FC >= dge_table_filters$lfc()$min)
                } else if (is.null(dge_table_filters$lfc()$min) & 
                           !is.null(dge_table_filters$lfc()$max)){
                  # Max is defined but not the min 
                  # filter based on the max
                  dge_table <-
                    dge_table %>% 
                    dplyr::filter(log2FC <= dge_table_filters$lfc()$max)
                }
              }
              
              if (session$userData$dev_mode == TRUE){
                print(paste0("Number of rows remaining: ", nrow(dge_table)))
                print("Filtering by AUC value:")
              }
              
              # Filter based on AUC value
              # Proceed with this filter only if "avgExpr" is present in the table
              if ("auc" %in% colnames(dge_table)){
                if (isTruthy(dge_table_filters$auc())){
                  dge_table <-
                    dge_table %>% 
                    dplyr::filter(auc >= dge_table_filters$auc()[1] &
                                    auc <= dge_table_filters$auc()[2])
                }
              }
              
              if (session$userData$dev_mode == TRUE){
                print(paste0("Number of rows remaining: ", nrow(dge_table)))
                print("Filtering by adjusted p-value:")
              }
              
              # Filter by adjusted p-value
              if (isTruthy(dge_table_filters$pval_adj())){
                dge_table <-
                  dge_table %>% 
                  dplyr::filter(
                    pval_adj <= dge_table_filters$pval_adj()
                  )
              }
              
              if (session$userData$dev_mode == TRUE){
                print(paste0("Number of rows remaining: ", nrow(dge_table)))
                print("Filtering by percentage in group:")
              }
              
              # Filter based on percent expression within group/class
              # Proceed with this filter only if "pct_in" is present in the table
              if ("pct_in" %in% colnames(dge_table)){
                if (isTruthy(dge_table_filters$pct_in())){
                  dge_table <-
                    dge_table %>% 
                    dplyr::filter(
                      pct_in >= dge_table_filters$pct_in()[1] &
                        pct_in <= dge_table_filters$pct_in()[2]
                    )
                }
              }
              
              if (session$userData$dev_mode == TRUE){
                print(paste0("Number of rows remaining: ", nrow(dge_table)))
                print("Filtering by percentage outside group:")
              }
              
              # Filter based on percent expression outside group/class
              # Proceed with this filter only if "pct_out" is present in the table
              if ("pct_out" %in% colnames(dge_table)){
                if (isTruthy(dge_table_filters$pct_out())){
                  dge_table <-
                    dge_table %>% 
                    dplyr::filter(
                      pct_out >= dge_table_filters$pct_out()[1] &
                        pct_out <= dge_table_filters$pct_out()[2]
                    )
                }
              }
              
              if (session$userData$dev_mode == TRUE){
                print(paste0("Number of rows remaining: ", nrow(dge_table)))
              }
              
              # The filtered table should automatically return from both the 
              # TryCatch expression and the parent reactive expression
              dge_table
            }
          )
        })
      
      ### 3.10.3. Display filtered table using DT package ####
      dge_DT_content <-
        eventReactive(
          filtered_dge_table(),
          label = "DGE: DT Generation",
          {
            print("DGE DT table")
            
            # Add Genecards link for each gene
            table <- filtered_dge_table()
            # Vector of feature names to pass to links
            features <- table$feature 
            
            table["Additional Info"] <- 
              glue(
                # Link to gene cards search, for each feature in the table
                '<a href=
                  https://www.genecards.org/Search/Keyword?queryString={features} 
                  target="_blank",
                  rel="noopener noreferrer">GeneCards</a>'
                )
            
            # Rename "group" column based on the test selected
            # This used to be called "Group" or "Class", but is now always 
            # "Group" (issue #324)
            group_rename <- "Group"
            
            # Renamed using base R syntax because new name is dynamic
            names(table)[names(table) == "group"] <-
              group_rename
            
            # New names for pct_in and pct_out columns
            pct_in_rename <- glue("Percent Expression Within {group_rename}")
            pct_out_rename <- glue("Percent Expression Outside {group_rename}")
            
            names(table)[names(table) == "pct_in"] <-
              pct_in_rename
            
            names(table)[names(table) == "pct_out"] <-
              pct_out_rename
            
            # Rename feature, AUC, avg exp., adjusted p-value columns, if 
            # they exist in the DGE table
            # Renaming vector (new_name = old_name)
            rename_cols <- 
              c(`Feature` = "feature",
                `Average Expression` = "avgExpr", 
                `AUC` = "auc",
                `Adjusted p-value` = "pval_adj"
                )
            
            table <- dplyr::rename(table, any_of(rename_cols))
            
            datatable(
              table,
              # DT classes applied
              # See https://datatables.net/manual/styling/classes
              class = "compact stripe cell-border hover",
              # Disallow selection of rows/cells
              selection = "none",
              # Add filter interface above columns
              filter = "top",
              # Remove rownames
              rownames = FALSE,
              # Set escape to FALSE to render the HTML for GeneCards links 
              escape = FALSE
              ) %>%
              # Format numeric columns
              formatSignif(
                # Apply format to numeric columns (format is applied to columns
                # where the boolean vector generated below is TRUE)
                sapply(
                  colnames(table), 
                  function(col) is.numeric(table[[col]])
                  ),
                # Use 5 sig figs (3 or more is sufficient)
                5
              )
          })
      
      ## 3.11. UMAP of DE Selected Groups ####
      dge_umap <-
        eventReactive(
          c(dge_DT_content(), 
            input$umap_group_by),
          ignoreNULL = FALSE,
          label = "DGE: UMAP",
          {
            print("DGE: UMAP")

            # Determine value of ncol
            # ncol depends on number of panels.
            # Number of panels equals number of split by groups.

            # Define metadata used to split plot (DGE groups/marker classes) 
            metadata_column <- 
              if (metaclusters_present()){
                "metacluster"
              } else if (thresholding_present()){
                  "simple_expr_threshold"
              } else {
                group_by_category()
                }

            n_panel <-
              n_unique(
                object = subset(),
                meta_var = metadata_column
                )

            #Set ncol to number of panels if less than four
            #Panels are created
            if (n_panel < 4){
              ncol = n_panel
              }

            #Use three columns for 4-9 panels
            else if (n_panel >= 4 & n_panel < 9){
              ncol = 3
              }

            #Use four columns for 9+ panels
            else if (n_panel >= 9){
              ncol = 4
              }

            # Create DimPlot of subsetted object, showing the groups tested
            SCUBA::plot_reduction(
              object = subset(),
              # Split by groups or marker classes used for DGE
              split_by = 
                if (metaclusters_present()){
                  # Use "metacluster" when metaclusers present
                  "metacluster"
                } else if (thresholding_present()){
                  # When thresholding present, use "simple_expr_threshold"
                  "simple_expr_threshold"
                } else {
                  # Standard DGE: split by the group by category
                  group_by_category()
                },
              # Group by variable set in UMAP options panel
              group_by = input$umap_group_by,
              ncol = ncol
            )
          })
      
      ## 3.12. Title for Main Panel ####
      main_panel_title <-
        eventReactive(
          dge_umap(),
          label = "DGE: Main Panel Title",
          {
            print("DGE: Main Panel Title")
            
            if (thresholding_present()){
              print("Conditional: thresholding present")
              feature_label <- 
                hr_name(
                  test_selections()$threshold_feature,
                  assay_config = assay_config(),
                  use_suffix = FALSE
                  )

              # Header for simple thresholding
              glue("Differential Expression Results Based on
                   {feature_label} Expression Threshold")
            } else {
              # Standard DGE or metaclusters: add group by category to
              # header text
              # Label for group by category in config file
              # TODO: make sure this updates when a different group
              # by variable is submitted
              group_by_label <-
                metadata_config()[[group_by_category()]]$label

              # Standard header
              glue("Differential Expression/Marker Genes by
                   {group_by_label} in Subset")
              }
            })
      
      ## 3.13. UMAP Title (Dynamic UI) ####
      umap_title <-
        eventReactive(
          main_panel_title(),
          label = "DGE: UMAP Title",
          {
            print("DGE: UMAP Title")
            # UI returned depends on DGE mode
            if (test_selections()$dge_mode == "mode_dge"){
              # DGE Title
              tags$h3(
                "UMAP of DGE Groups",
                class = "center"
              )
            } else if (test_selections()$dge_mode == "mode_marker"){
              # Marker identification title (and subtext)
              tagList(
                # Center text
                tags$h3(
                  "UMAP of Marker Groups",
                  class = "center"
                  ),
                tags$p(
                  "(Markers are computed for each group shown)",
                  class="center"
                  )
                )
              }
            })
      
      ## 3.14. Hide Spinners ####
      observeEvent(
        umap_title(),
        label = "DGE: Hide Spinner",
        {
          # Show UI (do not namespace ID for showElement)
          showElement(
            id = "main_panel"
            )
          
          # Show DGE Table Download button
          showElement(
            id = "download_table"
            )
          
          # Show UMAP options panel
          showElement(
            id = "options_panel_ui"
            )
          
          # Hide spinners
          sidebar_spinner$hide()
          main_spinner$hide()
          log_session(session)
          log_info("DGE Tab: Spinners removed.")
          })
      
      # 4. Render DGE UI, table, and UMAP --------------------------------------
      # Main Panel Header
      output$main_panel_title <- 
        renderText({
          main_panel_title()
        })
      
      # Table
      output$table <- 
        renderDT({
          dge_DT_content()
          })
      
      # UMAP Title
      output$umap_title <-
        renderUI({
          umap_title()
        })
      
      # UMAP plot
      output$umap <- 
        suppressGraphics(
          renderPlot({
            dge_umap()
            })
          )
      
      # 5. Download Handler for DGE Table --------------------------------------
      output$download_table <- 
        downloadHandler(
          filename = function() {
            if (thresholding_present()){
              # When thresholding is used, form name based on threshold used and
              # the value
              thresh_feature <- test_selections()$threshold_feature
              thresh_value <- test_selections()$threshold_value
              
              glue("DGE_table_{thresh_feature}_threshold-{thresh_value}.csv")
            } else {
              # Otherwise, form name using group by metadata category
              group_var <- test_selections()$group_by
              
              # Test if the group by variable is defined. If it is not, output
              # a generic file name
              if (!is.null(group_var) & !is.na(group_var)){
                glue("DGE_table_{group_var}.csv")
              } else {
                "DGE_table.csv"
                }
              }
            },
          content = function(file) {
            write.csv(
              filtered_dge_table(),
              file = file,
              row.names = FALSE
              )
            },
          contentType = "text/csv"
          ) # End downloadHandler
      
      # Dev mode: status of filter table values
      if (session$userData$dev_mode == TRUE){
        output$dge_filter_status <-
          renderPrint({
            lapply(
              dge_table_filters,
              # Unpack all reactive values in the list
              function(x){x()}
            )
          })
      }
      
      # 6. Testing -------------------------------------------------------------
      exportTestValues(
        # Raw DGE table produced by scDE
        dge_table = dge_table_content(),
        # Filtered DGE table
        filtered_dge_table = filtered_dge_table()
      )
      
      }
    )
  }
  
