#' Correlations Tab Module
#'
#' Module UI function for the correlations tab. This UI function contains 
#' reactive expressions and therefore must be called within a reactive 
#' expression.
#'
#' @param id ID to use for module elements. It is reccomended for this to be 
#' equal to "corr".
#' @param unique_metadata a list of the unique metadata values for each of the 
#' metadata categories listed in the config file. This is generated in the main 
#' server function at startup.
#' @param metadata_config the metadata section of the config file imported in 
#' the main server function
#' @param auto_dictionary_path Path to the temporary file used for storing the
#' auto-generated object dictionary (created in the main app)
#' @param string_subsetting_href URL of the string subsetting vignette.
#'
#' @return A list of `shiny.tag` elements forming the UI for the correlations tab.
#' 
#' @noRd
corr_tab_ui <- function(id,
                        unique_metadata,
                        metadata_config,
                        auto_dictionary_path,
                        string_subsetting_href
                        ){
  # Namespace function: prevents conflicts with inputs/outputs defined in 
  # other modules 
  ns <- NS(id)
  
  fluidPage(
    sidebarLayout(
      # 1.3.1. Options Panel
      sidebarPanel(
        # Add a block to display a waiter over when the options are updating
        div(id = ns("sidebar"),
            # 1.3.1.1. Restrict correlation table by metadata
            tags$h3("Correlation Coefficients"),
            tags$p("Enter one feature to view the top features positively and 
                   negatively correlated with the feature in the data. You may 
                   optionally restrict the correlation analysis by metadata 
                   variables using the dropdown menus below."),
            
            # Feature selection: only one feature can be entered
            selectizeInput(
              inputId = ns("feature_selection"),
              label = "Gene Selection",
              # Feature choices populated in server, as in the plots tab
              choices = NULL,
              selected = character(0),
              options = list("placeholder" = "Enter gene name")),
            
            # Module for subset selections              
            subset_selections_ui(
              # Create separate module instances for each datset to avoid 
              # namespace collisions between datasets. Use key of dataset in id
              id = ns("subset_selections"),
              unique_metadata = unique_metadata,
              metadata_config = metadata_config,
              auto_dictionary_path = auto_dictionary_path,
              string_subsetting_href = string_subsetting_href
              ),
            
            # Submit button
            actionButton(
              inputId = ns("submit"),
              label = "Submit",
              style = "display: block; width: 100%;",
              class = "button-primary"
              ),
            
            # Download buttons: display after the correlations table is computed
            hidden(
              downloadButton(
                outputId = ns("download_table"),
                label = "Download Table",
                icon = icon("table"),
                # Add space before button, and display in block format
                class = "half-space-top button-ghost",
                style = "display: block;"
                )
              ),
            
            # Options for scatterplot: a collapsible panel of options
            # appears after a scatterplot is displayed
            #uiOutput(outputId = ns("scatter_options_ui"))
            hidden(
              div(
                id = ns("scatterplot_options_showhide"),
                collapsible_panel(
                  inputId = ns("scatter_options"),
                  label = "Scatterplot Options",
                  active = TRUE,
                  # group.by selection
                  selectInput(
                    inputId = ns("scatter_group_by"),
                    label = "Metadata to Group by:",
                    # Available options are set in the server
                    choices = NULL
                  ),
                  
                  # Download button for scatterplot (subset)
                  # Displays only if a subset is selected
                  hidden(
                    downloadButton(
                      outputId = ns("download_scatter_subset"),
                      label = "Download Scatterplot (Subset)",
                      icon = icon("poll"),
                      style = "display: block;",
                      class = "button-ghost"
                    )
                  ),
                  # Download button for scatterplot (full data)
                  downloadButton(
                    outputId = ns("download_scatter_global"),
                    # Label will change based on whether a subset is selected
                    label = "Download Scatterplot",
                    icon = icon("poll"),
                    # Adds space before button (this class is removed if a 
                    # subset is not selected) 
                    class = "half-space-top button-ghost",
                    style = "display: block;"
                  ) # End downloadButton
                ) # End collapsible_panel  
              )
            )
        ) # End corr-sidebar div
      ), # End sidebarPanel (1.3.1)
      
      # 1.3.2 Main Panel
      mainPanel(
        # main_panel is the spinner container
        div(
          id = ns("main_panel"), 
          class = "spinner-container-main", 
          hidden(
            # Container for main UI (hidden at first; displays when results 
            # are done computing)
            div(
              id = ns("main_panel_ui"),
              # Contents
              tags$h2(
                "Correlation Analysis for",
                textOutput(
                  outputId = ns("corr_header_gene"),
                  inline = TRUE
                  ),
                class = "center"
                ),
              
              # Subset stats module UI
              # Uses subset stats module to print metatdata included in subset
              # and the amount of nonzero reads for the selected gene
              uiOutput(
                outputId = ns("subset_stats_ui")
                ),
              
              # Correlations table and plots
              tags$h3(
                "Correlated Genes",
                class = "center"
              ),
              
              # Table: rendered inline 
              div(
                id = ns("corr_table_container"),
                class = "center",
                #style = "width: 40%; float: left;",
                tags$strong(
                  "Correlation Table", 
                  class = "center single-space-bottom"
                  ),
                tags$p(
                  "(click on a gene and scroll down to view 
                  correlation scatterplots)",
                  class = "center single-space-bottom"
                ),
                # Use a DT data table
                DTOutput(
                  outputId = ns("corr_table")
                  )
                ),
              
              # Scatterplot
              # Appears after the user makes a selection on the table
              hidden(
                div(
                  id = ns("scatterplot_container"),
                  # UI for scatterplot rendered in 
                  # separate eventReactive function
                  uiOutput(
                    outputId = ns("scatterplot_ui")
                    )
                  )
                )
              )
            ) # End main_panel_ui
          ) # End hidden
      ) # End MainPanel
    ) # End sidebarLayout
  ) # End fluidPage
}

#' Correlations Tab Module
#' 
#' The server instance of the correlations tab module. One instance should be 
#' created for each object to avoid input collisions between objects with 
#' similarly named metadata columns.
#'
#' @param id Used to match server function to ui function. "corr" is the 
#' recommended value to use.
#' @param object A single cell object defined in the main server function.
#' @param metadata_config the metadata section of the config file imported in 
#' the main server function.
#' @param assay_config the assays section of the config file imported in the
#'  main server function.
#' @param designated_genes_assay the name of the assay designated in the config 
#' file as the genes assay. If NULL, the first assay will be assumed to be the 
#' genes assay.
#' @param meta_categories a named vector of metadata categories retrieved from the 
#' config file.
#' @param unique_metadata A list of the unique metadata values for each of the
#' metadata categories listed in the config file. This is generated in the main
#' server function at startup.
#' @param n_cells_original Number of cells in full Seurat object. Calculated in main 
# server function.
#' @param nonzero_threshold The minimum acceptable proportion of cells with
#' nonzero reads for the feature used to compute correlations. This is defined 
#' in the main server function.
#' @param meta_choices a named vector giving the metadata categories defined in
#' the config file, with their respective labels for display in dropdown menus.
#' This is defined in the main server function.
#' @param valid_features a list giving the valid features that can be selected 
#' from each assay. This is generated from the config file in the main server
#' function.
#' @param error_list a list of error messages to print custom notifications for, 
#' if they are encountered while the correlations table is being calculated. 
#' This is defined in the main server function.
#' @param update_features a reactive trigger created by the local 
#' \code{makeReactiveTrigger()} function. The trigger is created in the main 
#' server function and ensures that the feature selection input is updated after 
#' the input is created (input will have no choices otherwise.)
#' @param object_trigger a reactive trigger that invalidates downstream 
#' reactive expressions when the object is changed.
#'
#' @return server code for the correlations tab. Reactive expressions are contained within 
#' the namespace set by `id`
#'
#' @noRd
corr_tab_server <- function(id,
                            object,
                            metadata_config,
                            assay_config,
                            designated_genes_assay,
                            # meta_categories will replace metadata_config at 
                            # some point (It is derived from the config file and 
                            # is the only information used)
                            meta_categories,
                            unique_metadata,
                            n_cells_original,
                            nonzero_threshold,
                            meta_choices,
                            valid_features,
                            error_list,
                            update_features,
                            object_trigger
                            ){

  #Function to initialize module server
  moduleServer(id,
               function(input, output, session){
                 # Server namespace function: required for ids defined within 
                 # renderUI functions, but not for other ids 
                 ns <- session$ns
                 
                 
                 
                 # Correlations Tab Server ###
                 # TODO: Entire correlations tab only supports 
                 # the RNA assay. Should expand to other assays at some point.
                 
                 # Define spinners to display during computation
                 # Spinner for options panel
                 sidebar_spinner <- 
                   Waiter$new(
                     id = ns("sidebar"),
                     html = spin_loaders(id = 2, color = "#555588"),
                     color = "#B1B1B188",
                     #Gives manual control of showing/hiding spinner
                     hide_on_render = FALSE
                   )
                 
                 #Spinner for main panel
                 main_spinner <-
                   Waiter$new(
                     id = ns("main_panel"),
                     html = spin_loaders(id = 2, color = "#555588"),
                     color = "#FFFFFF",
                     #Gives manual control of showing/hiding spinner
                     hide_on_render = FALSE
                   )
                 
                 # Subset error: set to TRUE when an error occurs with the 
                 # subset, preventing downstream calculations
                 subset_error <- 
                   reactiveVal(
                     FALSE, 
                     label = ns("subset_error")
                     )
                 
                 # 1. Define assay to use for correlation computation ----------
                 assay_used <- 
                   reactive({
                     # If a designated gene assay is defined, use that assay.
                     # Otherwise, use the first assay included in the config file.
                     if (isTruthy(designated_genes_assay())){
                       designated_genes_assay()
                     } else {
                       names(assay_config())[[1]]
                     }
                   })
                 
                 # 2. Update input menu choices ---------------------
                 ## 2.1. Feature Selection ####
                 # Reactive - updates in response to change in dataset 
                 observeEvent(
                   # Responds to loading of update and creation of UI (to ensure
                   # feature updating is always performed after the input is 
                   # created)
                   c(valid_features(), update_features$depend()),
                   label = "Render choices for feature selection",
                   {
                     # Features list uses the label of the assay instead of the
                     # assay key. Fetch the label from the config file:
                     target_assay_label <-
                       assay_config()[[assay_used()]]$dropdown_title
                     
                     updateSelectizeInput(
                       session,
                       inputId = "feature_selection",
                       # Include only genes (genes assay determined in 1.)
                       choices = valid_features()[[target_assay_label]],
                       selected = character(0),
                       server = TRUE
                       )
                   })
                 
                 ## 2.2. Group by metadata for scatterplots ####
                 observe(
                   label = "Corr: Update Scatterplot Options Menus",
                   {
                     req(meta_choices())
                     
                     # Responds to update_features reactive trigger, ensuring
                     # that the select input menu is not updated before the 
                     # correlations UI is created
                     update_features$depend()
                     
                     # Available options: all available metadata, 
                     # excluding "none"
                     valid_choices <- 
                       meta_choices()[!meta_choices() %in% "none"]
                     
                     updateSelectInput(
                       session = session,
                       inputId = "scatter_group_by",
                       choices = valid_choices, 
                       selected = valid_choices[1]
                       )
                     })
                 
                 # 3. Process Inputs -------------------------------------------
                 # Inputs are packaged into reactive values for proper 
                 # processing in downstream modules and functions
                 
                 ## 3.1. Gene selection for correlation analysis ####
                 corr_main_gene <- 
                   eventReactive(
                     input$feature_selection,
                     ignoreNULL = FALSE,
                     label = "Corr: Process Selected Feature",
                     {
                       # Only run if a feature is defined 
                       # (avoids downstream errors)
                       # req(input$feature_selection, cancelOutput = TRUE)
                        
                       # HARD CODING: remove "rna_" prefix 
                       # from feature entered. Currently specific 
                       # to RNA assay in this Seurat object
                       return(sub("rna_", "", input$feature_selection))
                       })
                 
                 ## 3.2. Inputs in subset selections menus ####
                 # Module to record selections made in for subsetting based on 
                 # metadata categories
                 
                 ### 3.2.1 Create module instances for each dataset
                 subset_selections <- 
                   subset_selections_server(
                     # Use key of dataset in the id to avoid collisions
                     id = "subset_selections",
                     object = object,
                     unique_metadata = unique_metadata,
                     metadata_config = metadata_config,
                     assay_config = assay_config,
                     meta_categories = meta_categories,
                     valid_features = valid_features
                     )
                 
                 # 4. Computation of Correlation Table ####
                 # A chain of reactive expressions is used, beginning with a 
                 # reactive showing the spinners after the submit button is 
                 # pressed.
                 
                 ## 4.1. Process Submit button input ####
                 submit_button <- 
                   eventReactive(
                     input$submit,
                     label = "Corr: Submit Button (Show Spinners)",
                     ignoreNULL = FALSE,
                     ignoreInit = TRUE,
                     {
                       #print("3.1. Show Spinners")
                       # Show spinners if the submit button is pressed and a 
                       # feature has been selected (requirement to begin 
                       # calculation)
                       log_session(session)
                       log_info("Corr tab: Submit button pressed")
                       
                       # If subset_error was TRUE, set back to false at 
                       # the start of a new calculation
                       subset_error(FALSE)
                       
                       # Hide the main panel UI while calculations are performed
                       # print(glue("hiding {ns('main_panel_ui')}"))
                       hideElement(
                         id = "main_panel_ui"
                         )
                       
                       # Also hide the scatterplot options panel in the sidebar
                       hideElement(
                         id = "scatterplot_options_showhide"
                       )
                       
                       # And the download table button
                       hideElement(
                         id = "download_table"
                       )
                       
                       # Show spinners (if the feature is entered properly)
                       if (isTruthy(input$feature_selection)){
                         log_info("Displaying spinners")
                         sidebar_spinner$show()
                         main_spinner$show()
                       } 
                       
                       # Un-suspends the correlation table, allowing it and 
                       # the reactive expressions it depends upon to compute
                       # while it is hidden (preventing endless spinners)
                       outputOptions(
                         output, 
                         "corr_table", 
                         suspendWhenHidden = FALSE
                       )
                       
                       # Return value of submit button
                       input$submit
                       }
                     )
                 
                 ## 4.2 Define gene to display in UI ####
                 # The gene to display is equal to the feature selected
                 # when the submit button is pressed. 
                 corr_header_gene <-
                   eventReactive(
                     submit_button(),
                     ignoreNULL = FALSE,
                     ignoreInit = TRUE,
                     {
                       # Use corr_main_gene (value has assay key removed)
                       corr_main_gene() 
                     })

                 ## 4.3 Form subset based on chosen criteria ####
                 # Initialize variables to control reset of subset when the 
                 # object is changed
                 
                 # object_init: a reactive value set to TRUE when a new object 
                 # is loaded. The currently saved subset will be cleared when
                 # object_init is set to TRUE
                 object_init <- reactiveVal(FALSE)
                 
                 # Create a reactive trigger (triggers reset of subset when
                 # new object is loaded)
                 subset_trigger <- makeReactiveTrigger()
                 
                 # Upon object change: set object_init to TRUE and trigger 
                 # subset eventReactive
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
                 
                 # Store in reactive variable
                 subset <- 
                   eventReactive(
                     c(submit_button(), 
                       subset_trigger$depend()),
                     label = "Corr: Subset",
                     ignoreNULL = FALSE,
                     ignoreInit = TRUE,
                     {
                       #print("3.3 make subset")
                       
                       subset <- 
                         tryCatch(
                           error = function(err_cnd){
                             # If the user has entered an advanced subsetting
                             # string, log what was entered
                             log_info(
                               "Error in corr tab subsetting. ",
                               "Subset filters entered:"
                               )
                             cellDIVER:::log_subset(
                               filter_list = subset_selections$selections()
                             )
                             
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
                             
                             # Set subset error to TRUE (prevents downstream
                             # calculation)
                             subset_error(TRUE)
                             
                             # Return NULL for the subset 
                             NULL
                           },
                           # Begin tryCatch code
                           {
                             if (object_init() == TRUE){
                               # If object_init==TRUE, return full object
                               return(object())
                             } else {
                               # Otherwise, create subset from selections 
                               # and return
                               
                               # Log subset created
                               cellDIVER:::log_subset(
                                 filter_list = subset_selections$selections()
                                 )
                               
                               return(
                                 make_subset(
                                   object = object(),
                                   filter_list = 
                                     subset_selections$selections()
                                   )
                               )
                               }
                             }
                         )
                       
                       # Return subset from eventReactive
                       subset
                       })
                 
                 ## 4.3.a. Check Memory usage after making subset
                 observeEvent(
                   label = "Corr: Subset Memory Query",
                   subset(),
                   {
                     log_session(session)
                     log_info(
                       glue(
                         "Memory used after creating subset in corr tab: {to_GB(mem_used())}"
                         )
                       )
                   })
                 
                 ## 4.4. Correlations Tab Continuation Conditional ####
                 # After subset is computed, downstream computations should only 
                 # proceed if the subset has been created as a result of 
                 # pressing the submit button, and not as a result of the reset 
                 # that occurs when switching tabs.
                 
                 # Reactive trigger: proceeds with computation if the subset is 
                 # reset due to switching tabs (when object_init()==TRUE)
                 continue <- makeReactiveTrigger()
                 
                 observeEvent(
                   label = "Corr: Continuation Conditional",
                   subset(),
                   ignoreNULL = FALSE,
                   # Does not work properly when ignoreInit == TRUE
                   ignoreInit = FALSE,
                   {
                     # print("4.4. Continuation Conditional")
                     
                     # Continue only if the conditions below are met
                     if (
                       object_init() == FALSE &
                       subset_error() == FALSE &
                       isTruthy(input$feature_selection)
                     ){
                       print("continuation trigger")
                       continue$trigger()
                     }
                     
                     # Additional actions if conditions are not met
                     # A. object_init() == TRUE
                     if (object_init() == TRUE){
                       # Set object_init() to FALSE and hide spinners
                       object_init(FALSE)
                       
                       sidebar_spinner$hide()
                       main_spinner$hide()
                     }
                     
                     # B. !isTruthy(input$feature_selection)
                     if (!isTruthy(input$feature_selection)){
                       # Hide spinners and notify user
                       sidebar_spinner$hide()
                       main_spinner$hide()

                       showNotification(
                         ui = 
                           icon_notification_ui(
                             icon = "exclamation-triangle",
                             # Change to feature when other 
                             # features are supported
                             "A gene must be specified to 
                                 recieve correlation results."
                           ),
                         #Show notification for 5 seconds
                         duration = 5,
                         session = session
                         )
                       }
                   })
                 
                 ## 4.5. Determine if the subset created is a subset ####
                 is_subset <- 
                   eventReactive(
                     continue$depend(),
                     ignoreNULL = FALSE,
                     ignoreInit = TRUE,
                     label = "Corr: Determine if Object is a Subset",
                     {
                       # print("4.5 is_subset")
                       
                       # Print an error if the subset does not exist or is NULL
                       validate(
                         need(
                           subset(),
                           message = "subset is NULL"
                         )
                       )
                       
                       # Test if the number of cells in the subset differs from
                       # the number of cells in the original object. If this
                       # conditional is TRUE, then the object read is a subset
                       n_cells_original() != n_cells(subset())
                       })
                 
                 ## 4.6. Subset Stats Module ####
                 subset_stats_server(
                   id = "stats",
                   tab = "corr",
                   subset = subset,
                   meta_categories = meta_categories,
                   # Reactive expressions in module will execute after 
                   # is_subset is computed
                   event_expr = is_subset,
                   gene_selected = corr_main_gene,
                   nonzero_threshold = nonzero_threshold
                 )
                 
                 ## 4.7. Compute correlation tables ####
                 # Calculations used depend on whether the object is a subset
                 corr_table_content <-
                   eventReactive(
                     is_subset(),
                     label = "Corr: Corr Table",
                     ignoreNULL = FALSE,
                     # Does not execute properly when ignoreInit == TRUE
                     ignoreInit = FALSE,
                     {
                       log_session(session)
                       log_info("Corr tab: Begin correlation computations")
                       # print("4.7 Compute Correlation Tables")
                       if (is_subset() == TRUE){
                         # Subset is selected: compute tables for full object 
                         # and subset, then merge
                         # Full object
                         table_full <- 
                           compute_correlation(
                             gene_selected = corr_main_gene(),
                             object = object(),
                             colnames =
                               c("Feature",
                                 "Correlation_Global"),
                             seurat_assay = assay_used()
                           )
                         
                         # Subset
                         table_subset <- 
                           compute_correlation(
                             gene_selected = corr_main_gene(),
                             object = subset(),
                             colnames = 
                               c("Feature",
                                 "Correlation_Subset"),
                             seurat_assay = assay_used()
                           )
                         
                         # Merge individual tables and arrange in descending 
                         # order by the subset correlation coefficient
                         corr_table <- 
                           merge(
                             table_full,
                             table_subset,
                             by = "Feature"
                             ) |>
                           arrange(
                             desc(
                               .data[["Correlation_Subset"]]
                             )
                           )
                       } else {
                         # If a subset is not present: compute the table for the 
                         # full data only (which is the "subset" in this case)
                         corr_table <- 
                           compute_correlation(
                             gene_selected = corr_main_gene(),
                             object = subset(),
                             colnames = c("Feature", "Correlation_Global"),
                             seurat_assay = assay_used()
                           )
                       }
                       
                       log_session(session)
                       log_info("Corr tab: Computations complete")
                       
                       # Return the computed table
                       corr_table
                     })

                 ## 4.8. Store table in DT format for display in app ####
                 # Reactive trigger to remove spinners upon completion
                 remove_spinners <- makeReactiveTrigger()
                 corr_DT_content <- 
                   eventReactive(
                     corr_table_content(),
                     label = "Corr: DT Content",
                     ignoreNULL = FALSE,
                     {
                       # print("4.8. DT Table")
                       
                       ### Table HTML Header ####
                       if (is_subset() == TRUE){
                         # If a subset is selected, the header will have three 
                         # columns for the feature, the global correlation 
                         # coefficients, and the correlation coefficients 
                         # for the subset
                         header <- 
                           tags$table(
                             # center-colnames class: centers the 
                             # column names in the header
                             class = "compact stripe cell-border 
                             hover center-colnames",
                             tags$thead(
                               tags$tr(
                                 tags$th("Feature"),
                                 tags$th(
                                   tagList(
                                     "Correlation",
                                     tags$br(),
                                     "(Global)"
                                     )
                                   ),
                                 tags$th(
                                   tagList(
                                     "Correlation",
                                     tags$br(),
                                     "(Subset)"
                                     )
                                   ), # End th
                                 # Header for GeneCards Links
                                 tags$th(
                                   "Additional Info"
                                  )
                                 ) # End tr
                               ) # End thead
                             ) # End table
                         
                         } else {
                           # If the full data is used, display two columns
                           # (feature and correlation coefficient in full data)
                           header <- 
                             tags$table(
                               # center-colnames class: centers the
                               # column names in the header
                               class = "compact stripe cell-border 
                               hover center-colnames",
                               tags$thead(
                                 tags$tr(
                                   tags$th("Feature"),
                                   tags$th(
                                     tagList(
                                       "Correlation Coefficient",
                                       tags$br(),
                                       "(Global)"
                                       )
                                     ), # End th
                                   # Header for GeneCards Links
                                   tags$th(
                                     "Additional Info"
                                     )
                                   ) # End tr
                                 ) # End thead
                             ) # End table tag
                           }
                       
                       ### Genecards link ####
                       corr_table <- corr_table_content()
                       
                       # Vector of all genes in the table, used for creating links
                       features <- corr_table$Feature 
                       
                       corr_table["Additional Info"] <- 
                         glue(
                           # Link to gene cards search, for each feature in the table
                           '<a href=
                            https://www.genecards.org/Search/Keyword?queryString={features} 
                            target="_blank",
                            rel="noopener noreferrer">GeneCards</a>'
                          )
                       
                       ### Construct DT datatable ####
                       DT <-
                         datatable(
                           corr_table,
                           class = "compact stripe cell-border hover",
                           selection = "single",
                           filter = "top",
                           rownames = FALSE,
                           # Set escape to FALSE to render the HTML for \
                           # GeneCards links 
                           escape = FALSE,
                           container = header
                           ) %>%
                         # Use 5 sig figs for pearson coefficientcolumn(s). If a 
                         # subset is used, this will be columns 2 and 3; 
                         # if not, this will be column 2.
                         formatSignif(
                           columns = if (is_subset() == TRUE) c(2,3) else 2,
                           digits = 5
                           )
                       
                       remove_spinners$trigger()
                       
                       # Return DT datatable
                       DT
                       })
                 
                 ## 4.9 Hide spinners after the DT datatable is computed ####
                 observeEvent(
                   remove_spinners$depend(),
                   label="Corr: Hide Spinners",
                   ignoreNULL = FALSE,
                   ignoreInit = TRUE,
                   {
                     # print("3.9. Hide spinners")
                     # Hide loading screen
                     main_spinner$hide()
                     sidebar_spinner$hide()
                     log_session(session)
                     log_info("Corr tab: Hide spinners")
                     # Show main panel
                     showElement(id = "main_panel_ui")
                     })
                 
                 # 5. Correlations UI ------------------------------------------
                 ## 5.1. Subset Stats UI ####
                 subset_stats_content <-
                   eventReactive(
                     submit_button(),
                     label = "Corr: Subset Stats UI",
                     {
                       subset_stats_ui(
                         id = ns("stats"),
                         tab = "corr",
                         metadata_config = metadata_config,
                         meta_categories = meta_categories,
                         gene_selected = corr_main_gene
                         )
                       })
                 
                 ## 5.2. Correlations scatterplot UI ####
                 # The UI for showing scatterplot depends on whether a subset 
                 # is selected, and is re-computed each time an analysis is ran.
                 scatterplot_ui <- 
                   eventReactive(
                     is_subset(),
                     label = "Corr: Scatterplot UI",
                     ignoreNULL = TRUE,
                     {
                       if (is_subset() == TRUE){
                         # If a subset is selected, display two plots: 
                         # one for the subset and one for the full data.
                         tagList(
                           div(
                             class = "corr-layout-left",
                             tags$strong(
                               "Scatterplot for Subset",
                               class = "center single-space-bottom"
                             ),
                             plotOutput(
                               outputId = ns("subset_scatterplot"), 
                               height = "400px"
                             )
                           ),
                           div(
                             class = "corr-layout-right",
                             tags$strong(
                               "Scatterplot for Full Data",
                               class = "center single-space-bottom"
                             ),
                             plotOutput(
                               outputId = ns("full_data_scatterplot"), 
                               height = "400px"
                             )
                           )
                         )
                         
                       } else {
                         # Otherwise, display only one scatterplot.
                         div(
                           class = "center",
                           tags$strong(
                             "Scatterplot",
                             class = "center single-space-bottom"),
                           plotOutput(
                             outputId = ns("full_data_scatterplot"),
                             height = "400px"
                             )
                           )
                         }
                       })
                 
                 ## 5.3. Show/hide scatterplot UI ####
                 # UI for scatterplots is computed when an analysis is ran.
                 # The UI should only display when a row is selected from the 
                 # table.
                 observe({
                   if (isTruthy(input$corr_table_rows_selected)){
                     showElement(
                       id = "scatterplot_container",
                       anim = TRUE
                     )
                   } else {
                     hideElement(
                       id = "scatterplot_container",
                       anim = TRUE
                     )
                   }
                 })
                 
                 ## 5.4. UI for customizing the scatterplot ####
                 # This appears in the sidebar and displays a list of options 
                 # used for customizing the scatterplot
                 ### 5.4.1. Show/hide options panel ####
                 observe(
                   label = "Corr: show/hide scatterplot options UI",
                   {
                     # Show panel when a gene is selected from the table
                     if (isTruthy(input$corr_table_rows_selected)){
                       showElement(
                         id = "scatterplot_options_showhide",
                         anim = TRUE
                         )
                     } else {
                       hideElement(
                         id = "scatterplot_options_showhide",
                         anim = TRUE
                       )
                     }
                   })
                 
                 ### 5.4.2. Show/hide download buttons for subset scatterplot
                 # The download button for the full object scatterplot is always
                 # shown
                 observe(
                   label = "Corr: show/hide subset download button",
                   {
                     # If the analysis ran includes a subset, show the download
                     # button for the subset scatterplot
                     if (isTruthy(is_subset())){
                       showElement(
                         id = "download_scatter_subset",
                         anim = TRUE
                         )
                     } else {
                       hideElement(
                         id = "download_scatter_subset",
                         anim = TRUE
                         )
                       }
                   })
                 
                 ### 5.4.3. Change label on global scatterplot download button ####
                 observe(
                   label = "Corr: show/hide scatterplot options UI",
                   {
                     # Label depends on whether a subset is selected
                     if (isTruthy(is_subset())){
                       print("Subset text")
                       shinyjs::html(
                         id = "download_scatter_global",
                         html = 
                           # shiny.tag format must be converted to text to be
                           # understood by shinyjs::html()
                           as.character(
                             tagList(
                               icon("poll"),
                               "Download Scatterplot (Full Data)"
                             )
                           )
                         )
                     } else {
                       print("Full text")
                       shinyjs::html(
                         id = "download_scatter_global",
                         html = 
                           as.character(
                             tagList(
                               icon("poll"),
                               "Download Scatterplot"
                               )
                             )
                         )
                       }
                   })
                 
                 ### 5.4.4. Add/remove CSS classes from buttons ####
                 observe(
                   label = "Corr: add/remove CSS class from scatterplot download button",
                   {
                   if (is_subset() == TRUE){
                     # "half-space-top": adds space before the global 
                     # scatterplot download button
                     shinyjs::addClass(
                       id = "download_scatter_global",
                       class = "half-space-top"
                       )
                     
                     # Responsive button label: changes size of button label
                     # in response to the window size
                     # (button labels are very long when a subset is selected)
                     shinyjs::addClass(
                       id = "download_scatter_global",
                       class = "responsive-button-label"
                     )
                     
                     shinyjs::addClass(
                       id = "download_scatter_subset",
                       class = "responsive-button-label"
                     )
                   } else {
                     # If a subset is not present, remove the
                     # labels described above
                     shinyjs::removeClass(
                       id = "download_scatter_global",
                       class = "half-space-top"
                       )
                     
                     shinyjs::removeClass(
                       id = "download_scatter_global",
                       class = "responsive-button-label"
                     )
                     
                     shinyjs::removeClass(
                       id = "download_scatter_subset",
                       class = "responsive-button-label"
                     )
                   }
                 })
                 
                 scatter_options <- 
                   eventReactive(
                     c(input$corr_table_rows_selected, 
                       is_subset()),
                     label="Corr: Scatterplot Options UI",
                     ignoreNULL = FALSE,
                     {
                       # # If a selection in the table is made, display a 
                       # # collapsible_panel with a list of options for 
                       # # customization
                       # if (length(input$corr_table_rows_selected) > 0){
                       #   collapsible_panel(
                       #     inputId = ns("scatter_options"),
                       #     label = "Scatterplot Options",
                       #     active = TRUE,
                       #     # group.by selection
                       #     selectInput(
                       #       inputId = ns("scatter_group_by"),
                       #       label = "Metadata to Group by:",
                       #       # Remove "none" from selectable options to group by
                       #       choices =
                       #         meta_choices()[!meta_choices() %in% "none"], 
                       #       selected = "clusters"
                       #       ),
                       #     
                       #     # Download button for scatterplot (subset)
                       #     # Displays only if a subset is selected
                       #     if (is_subset() == TRUE){
                       #       downloadButton(
                       #         outputId = ns("download_scatter_subset"),
                       #         label = "Download Scatterplot (Subset)",
                       #         # Adds space before button
                       #         class = "half-space-top",
                       #         icon = icon("poll")
                       #         )
                       #       } else NULL, # End downloadButton tag
                       #     
                       #     # Download button for scatterplot (full data)
                       #     downloadButton(
                       #       outputId = ns("download_scatter_global"),
                       #       # Label changes based on whether 
                       #       # a subset is selected
                       #       label = if (is_subset() == TRUE){
                       #         "Download Scatterplot (Full Data)"
                       #         } else {
                       #           "Download Scatterplot"
                       #           },
                       #       
                       #       # half-space-top class: adds space before button 
                       #       # this is only needed when a subset is 
                       #       # selected and there are two buttons
                       #       class = if (is_subset() == TRUE){
                       #         "half-space-top"
                       #         } else NULL,
                       #       icon = icon("poll")
                       #       ) # End downloadButton
                       #     ) # End collapsible_panel  
                       #   } # End if statement
                       })
                 
                 ## 5.5. Show/hide download button for table ####
                 observe(
                   # c(submit_button(),
                   #   input$corr_table_rows_selected),
                   label = "Corr: Show/Hide Table Download Button UI",
                   # ignoreNULL = FALSE,
                   {
                     # Display download button only when rows have been selected 
                     # on the table
                     if (isTruthy(corr_table_content())){
                       showElement(
                         id = "download_table",
                         anim = TRUE
                         )
                     } else {
                       hideElement(
                         id = "download_table",
                         anim = TRUE
                         )
                       }
                     })
                 
                 # 6. Server Value for Gene Selected from Table ----------------
                 # Value is used for creating scatterplots
                 secondary_gene <-
                   reactive({
                     req(input$corr_table_rows_selected)
                     
                     # Index of row selected
                     row_idx <- input$corr_table_rows_selected
                     
                     # Return gene based on that row index
                     as.character(corr_table_content()[row_idx,1])
                   })
                 
                 
                 # 7. Plot of feature selected from table ----------------------
                 ## 7.1. Correlation scatterplot for subset
                 subset_scatterplot <- 
                   reactive(
                     label = "Correlation Scatterplot Content (Subset)",
                     {
                       # Make a scatterplot if a row is selected
                       req(input$corr_table_rows_selected)
                       
                       FeatureScatter(
                         subset(), 
                         feature1 = corr_main_gene(),
                         feature2 = secondary_gene(),
                         group.by = input$scatter_group_by
                       )
                     })
                 
                 ## 7.2. Correlation plot for full data
                 full_data_scatterplot <- 
                   reactive(
                     label = "Correlation Scatterplot Content (Global)",
                     {
                       # Make a scatterplot from the full object if a row
                       # is selected
                       req(input$corr_table_rows_selected)
                       
                       FeatureScatter(
                         object(), 
                         feature1 = corr_main_gene(),
                         feature2 = secondary_gene(),
                         group.by = input$scatter_group_by
                         )
                     })
                 
                 # 8. Render Correlation UI, table, scatterplot, and statistics ----
                 # Main panel UI
                 output$corr_header_gene <- 
                   renderText({
                     corr_main_gene()
                   })
                 
                 # Subset stats
                 output$subset_stats_ui <-
                   renderUI({
                     subset_stats_content()
                   })
                 
                 # Container for scatterplots (main panel)
                 output$scatterplot_ui <-
                   renderUI({
                     scatterplot_ui()
                     })
                 
                 # Scatterplot UI should render when hidden
                 outputOptions(
                   output, 
                   "scatterplot_ui", 
                   suspendWhenHidden = FALSE
                 )
                 
                 # Scatterplot (main panel, in UI container)
                 output$subset_scatterplot <- 
                   renderPlot({
                     subset_scatterplot()
                   })
                 
                 # Scatterplot (main panel, in UI container)
                 output$full_data_scatterplot <- 
                   renderPlot({
                     full_data_scatterplot()
                   })
                 
                 # Table
                 output$corr_table <- 
                   renderDT({
                     corr_DT_content()
                     })
                 
                 # 9. Download Handlers ----------------------------------------
                 # Correlations Table
                 output$download_table <- 
                   downloadHandler(
                     filename = 
                       function(){
                         glue("Corr_table_{corr_main_gene()}.csv")
                         },
                     content = 
                       function(file){
                         write.csv(
                           corr_table_content(),
                           file = file,
                           row.names=FALSE
                           )
                         },
                     contentType = "text/csv"
                     ) # End downloadHandler 
                 
                 # Scatterplot (for selected subset)
                 output$download_scatter_subset <- 
                   downloadHandler(
                     filename = 
                       function(){
                         glue(
                           "Corr_scatter_{corr_main_gene()}-vs-{secondary_gene()}_subset.png"
                           )
                         },
                     content = function(file){
                       ggsave(
                         file, 
                         plot = subset_scatterplot(), 
                         device = "png",
                         bg = "#FFFFFF"
                         )
                       },
                     contentType = "image/png"
                     ) # End downloadHandler
                 
                 # Scatterplot (for full data)
                 output$download_scatter_global <- 
                   downloadHandler(
                     filename = 
                       function(){
                         glue(
                           "Corr_scatter_{corr_main_gene()}-vs-{secondary_gene()}_global.png"
                           )
                         },
                     content =
                       function(file){
                         ggsave(
                           file, 
                           plot = full_data_scatterplot(), 
                           device = "png",
                           bg = "#FFFFFF"
                           )
                         },
                     contentType = "image/png"
                   )# End downloadHandler
                 }
               )
}
