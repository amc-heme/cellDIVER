#' Add object metadata
#' 
#' Creates a modal with an interface to add metadata to the object. Unlike other 
#' modules in cellDIVER, this module only has a server component. Modals create 
#' UI in the server function. 
#' 
#' @param id Used to set a namespace for elements created in the module. 
#' Elements with an id property will have the value passed to `id` as a prefix 
#' @param modal_open_button The value of an action button or link that opens 
#' the modal. The value should be passed inside a reactive expression. For 
#' example, if the button has an id "button_id", you would pass
#' `reactive({input$button_id})`.
#' @param object description
#' @param object_meta_varnames description
#'
#' @noRd
add_metadata_server <- 
  function(
    id,
    modal_open_button,
    object,
    object_meta_varnames
    ){
    moduleServer(
      id,
      function(input, output, session){
        # Server namespace function 
        # Used to add proper namespace to components created in the modal
        ns <- session$ns
        
        # Variables for recording state of menu
        add_metadata_state <- reactiveValues()
        # Window state: starts as "closed" and is set to "open" when the link
        # to open the modal is pressed
        add_metadata_state$window_state <- "closed"
        # Tracks if errors have occurred in the join preview
        add_metadata_state$preview_error <- FALSE
        # Preview status: tracks state of preview of join for adding metadata 
        # "not_performed": initial state
        # "in_progress": used to display a spinner while results are computing
        # "complete": hides spinner and directs the UI to show the result of 
        # the preview
        add_metadata_state$preview_status <- "not_performed"
        
        # State variables for storing output
        # Variables are updated only when the user selects "confirm"
        # Table
        add_metadata_state$uploaded_table <- NULL
        # Join instructions
        add_metadata_state$join_instructions <- c()
        # New variables added by join
        add_metadata_state$vars_added <- c()
        
        example_add_metadata_df <- data.frame(
          patient_id = c(11292, 14384, 44438, 93940),
          new_group   = c("group_a",  "group_a",  "group_b",  "group_b"),
          stringsAsFactors = FALSE
        )
        
        # 1. Control state of window ####
        # Set state to open when the open link is selected
        observe({
          req(modal_open_button())
          
          modal_open_button()
          
          add_metadata_state$window_state <- "open"
        })
        
        # 2. Open/close window based on state ####
        observe({
          req(add_metadata_state$window_state)
          
          if (add_metadata_state$window_state == "open"){
            showModal(
              modalDialog(
                title = "Add New Metadata",
                footer = 
                  tagList(
                    actionButton(
                      inputId = ns("cancel"),
                      label = "Cancel",
                      class = "button-ghost"
                    ),
                    actionButton(
                      inputId = ns("confirm"),
                      label = "Confirm",
                      class = "button-primary"
                    )
                  ),
                size = "xl",
                # Modal Content
                div(
                  style = "min-height: 500px;",
                  # Left-hand side of modal: buttons to choose dataset
                  tags$p(
                    paste0(
                      "Upload a table below with the IDs of a patient/sample ",
                      "variable as they appear in the app in one column, and ",
                      "group IDs in a second column. For example:"
                    )
                  ),
                  # Insert example table defined in server
                  DT::datatable(
                    example_add_metadata_df,
                    class = "compact stripe cell-border",
                    # Disallow selection of rows/cells
                    selection = "none",
                    # Remove rownames
                    rownames = FALSE
                  ),
                  # Add interface to download a sample CSV to fill out?
                  # Upload window for CSV
                  fileInput(
                    inputId = ns("file"),
                    label = "Upload a CSV file with groups to add:",
                    accept = ".csv",
                    placeholder = "Upload File"
                  ),
                  checkboxInput(
                    inputId = ns("has_header"), 
                    label = "My file has headers", 
                    value = TRUE
                  ),
                  # Preview and options: shows when upload is complete
                  hidden(
                    # Column *in the uploaded table* to use for the sample 
                    # column
                    selectInput(
                      inputId = ns("upload_sample_colname"),
                      label = "Choose column from your table to map to 
                               sample variable (eg. patient_id):",
                      choices = NULL,
                      selected = NULL
                      ),
                    # Column *in the object* to map the object too
                    selectInput(
                      inputId = ns("object_sample_colname"),
                      label = 
                        "Choose sample variable from the object in this 
                         deployment to map the above variable to:",
                      choices = NULL,
                      selected = NULL
                      ),
                    # Preview table
                    div(
                      id = ns("table_summary"),
                      tags$b("Summary of table uploaded:"),
                      DTOutput(
                        outputId = ns("table")
                        )
                      ),
                    div(
                      id = ns("join_summary"),
                      #tags$h3("")
                      # Summary of columns in uploaded table, and in object
                      fluidRow(
                        # This is the same as column(width = 6), but allows
                        # additional columns to be assigned
                        div(
                          class = "col-sm-6 summary_ui_column_left",
                          uiOutput(
                            outputId = ns("upload_sample_var_summary")
                            )
                          ),
                        div(
                          class = "col-sm-6 summary_ui_column_right",
                          uiOutput(
                            outputId = ns("object_sample_var_summary")
                            )
                          )
                        )
                      ),
                    # Loading spinner that shows while a preview 
                    # of the join proceeds
                    div(
                      id = ns("preview_spinner"),
                      waiter::spin_loaders(id = 2, color = "#555588"),
                      ),
                    # Shows status of preview join to test for warnings, errors
                    uiOutput(
                      outputId = ns("preview_results")
                      )
                    )
                  )
                )
              )
            
            # Initialize confirm button in a disabled state 
            shinyjs::disable(
              id = "confirm"
            )
          } else if (add_metadata_state$window_state == "closed"){
            # When the window state is set to "closed", remove the modal
            removeModal()
          }
        })
        
        # 3. Store uploaded table ####
        metadata_table_upload <-
          reactive({
            req(input$file)
            
            read.csv(
              file = input$file$datapath,
              header = input$has_header
              )
            })
        
        # 4. Summary of selected variables ####
        ## 4.1. Upload Sample Var Summary ####
        output$upload_sample_var_summary <-
          renderUI({
            req(input$upload_sample_colname)       
            
            # Compute the unique values to summarize column
            column_values <- 
              metadata_table_upload()[,input$upload_sample_colname] |> 
              unique() |> 
              # Sort in ascending alphanumeric order
              stringr::str_sort(
                decreasing = FALSE,
                numeric = TRUE
                )
            
            div(
              tags$h4("Uploaded Table", class = "center"),
              div(
                class = "variable_summary",
                # Header
                div(
                  # Name of column in uploaded table
                  tags$b(
                    paste0(input$upload_sample_colname)
                  ),
                ),
                
                # Content
                # Print unique values of table (up to 20)
                if (length(column_values) <= 20){
                  tags$p(
                    paste(column_values, collapse = ", ")
                  )
                } else {
                  # UI for summarizing table variables with more than 20 
                  # unique values: collapse excess variables into a "..."
                  # with a BS tooltip that displays them on hover
                  tags$p(
                    paste(column_values, collapse = ", "),
                    tags$span(
                      # The tooltip needs an id to function
                      # id = ns(paste0("upload_", input$upload_sample_colname, "_extra")),
                      "..."
                      )
                    )
                  }
                )
              )
            })
        
        ## 4.2. Summary of Sample Variable from Object ####
        output$object_sample_var_summary <-
          renderUI({
            req(input$object_sample_colname)
            
            meta_var <- input$object_sample_colname
            
            # Label of the variable, from config file, via object_meta_varnames
            # input$object_sample_colname is the value, not the name, so 
            # the data must be retrieved by subsetting the vector for the value
            # equaling the column name selected
            meta_var_label <- 
              names(
                object_meta_varnames()[object_meta_varnames() == meta_var]
                )
            
            # Fetch unique values in var column for display
            meta_var_values <- 
              SCUBA::unique_values(
                object(),
                var = meta_var
                ) |> 
              # Sort in ascending alphanumeric order
              stringr::str_sort(
                decreasing = FALSE,
                numeric = TRUE
              )
            
            div(
              tags$h4("Object Metadata", class = "center"),
              div(
                class = "variable_summary object_var",
                # Header
                div(
                  # Name of column in uploaded table
                  tags$b(
                    paste0(meta_var_label), 
                  )
                ),
                
                # Content
                # Print unique values of table (up to 20)
                if (length(meta_var_values) <= 20){
                  tags$p(
                    paste(meta_var_values, collapse = ", ")
                  )
                } else {
                  # UI for summarizing table variables with more than 
                  # 20 unique values: collapse excess variables into 
                  # a "..." with a BS tooltip that displays 
                  # them on hover
                  tags$p(
                    paste(meta_var_values, collapse = ", "),
                    tags$span(
                      # The tooltip needs an id to function
                      # id = ns(paste0("upload_", input$object_sample_colname, "_extra")),
                      "..."
                      )
                    )
                  }
                )
              )
            })
        
        ## 4.3. Show/hide ####
        ### 4.3.1. Container for join variable summary ####
        observe({
          target_id <- "join_summary"
          
          # Show summary UI after a table has been uploaded
          if (isTruthy(metadata_table_upload())){
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
        
        ### 4.3.2. Upload sample variable summary ####
        observe({
          target_id <- "upload_sample_var_summary"
          
          # Show summary UI after a table has been uploaded
          if (isTruthy(input$upload_sample_colname)){
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
        
        ### 4.3.2. Summary of sample variable from upload ####
        observe({
          target_id <- "object_sample_var_summary"
          
          # Show summary UI after a table has been uploaded
          if (isTruthy(input$object_sample_colname)){
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
        
        ## (Unused) Display summary of uploaded table ####
        ### Initialize table in DT ####
        # summary_table <-
        #   reactive({
        #     req(metadata_table_upload())
        #     
        #     # Generate HTML to display for each column
        #     # HTML is a list of <th> tags for each element
        #     column_html <-
        #       if (isTruthy(input$upload_sample_colname)){
        #         lapply(
        #           colnames(metadata_table_upload()),
        #           function(column_name){
        #             # Compute width of each column as a percentage, based on
        #             # the number of columns
        #             width_pct <- 100/length(colnames(metadata_table_upload()))
        #             
        #             # If the column is the currently selected sample column, 
        #             # print the name of the column, then add a badge indicating
        #             # it is the selected sample variable
        #             if (column_name == input$upload_sample_colname){
        #               tags$th(
        #                 span(
        #                   column_name,
        #                   span("sample variable", class = "sample_variable_badge")
        #                   ),
        #                 # Apply computed column width
        #                 style = paste0("width: ", width_pct, "%;")
        #                 )
        #             } else {
        #               # Otherwise, just include the column name
        #               tags$th(
        #                 column_name,
        #                 # Apply computed column width
        #                 style = paste0("width: ", width_pct, "%;")
        #                 )
        #             }
        #           }
        #         )
        #       } else {
        #         # If nothing has been chosen from the sample column menu,
        #         # display all column names as-is in <th> tags
        #         tagList(
        #           lapply(
        #             colnames(metadata_table_upload()),
        #             # Applies tags$th to each column name 
        #             tags$th
        #           )
        #         )
        #       }
        #     
        #     # Package html for column headers above into a table container, for
        #     # passage to DT::datatable
        #     container_html <-
        #       tags$table(
        #         tags$thead(
        #           tags$tr(
        #             column_html
        #             )
        #           )#,
        #         # DT classes applied
        #         # From https://datatables.net/manual/styling/classes
        #         # class = "compact stripe cell-border hover"
        #         )
        #     
        #     datatable(
        #       metadata_table_upload(),
        #       # DT classes applied
        #       # From https://datatables.net/manual/styling/classes
        #       class = "compact stripe cell-border hover",
        #       # Disallow selection of rows/cells
        #       selection = "none",
        #       # Add filter interface above columns
        #       # filter = "top",
        #       # Remove rownames
        #       rownames = FALSE,
        #       # Custom HTML container from above for rownames
        #       container = container_html,
        #       # Allows custom HTML in headers to be evaluated
        #       escape = FALSE
        #       )
        #   })
        
        ### Render to output container ####
        # output$table <-
        #   renderDT(
        #     summary_table()
        #   )
        
        ### Show/hide table output container ####
        # observe({
        #   target_id <- "table_summary"
        #   
        #   # Show table when a table has been loaded to the fileInput
        #   if (isTruthy(metadata_table_upload())){
        #     # showElement(
        #     #   id = target_id,
        #     #   anim = TRUE
        #     #   )
        #     } else {
        #       hideElement(
        #         id = target_id,
        #         anim = TRUE
        #       )
        #     }
        # })
        
        # 5. Populate metadata addition menus ####
        ## 5.1. Variable to map to sample column ####
        observe({
          req(metadata_table_upload())
          
          # Populate menu with columns of uploaded table
          cols <- colnames(metadata_table_upload())
          names(cols) <- cols
          
          if (isTruthy(metadata_table_upload())){
            updateSelectInput(
              inputId = "upload_sample_colname",
              # Add placholder choice to choices
              choices = 
                c("Select a column from uploaded table" = "", 
                  cols
                ),
              selected = NULL
              )
            } else {
              # In the event the upload becomes undefined, 
              # clear the selection and choices
              updateSelectInput(
                inputId = "upload_sample_colname",
                # Add placholder choice to choices
                choices = NULL,
                selected = NULL
                )
              }
            })
        
        # Show menu when the metadata table upload is complete, hide otherwise
        observe({
          target_id <- "upload_sample_colname"
          
          if (isTruthy(metadata_table_upload())){
            shinyjs::showElement(
              id = target_id,
              anim = TRUE
            )
          } else {
            shinyjs::hideElement(
              id = target_id,
              anim = TRUE
            )
          }
        })
        
        ## 5.2. Variable to map sample column to in object ####
        observe({
          cols <- object_meta_varnames()
          
          if (isTruthy(input$upload_sample_colname)){
            updateSelectInput(
              inputId = "object_sample_colname",
              choices = 
                c("Select a column in the object" = "", 
                  cols
                  ),
              selected = NULL
              )
            } else {
              # Clear choices and selection if the sample column becomes NULL
              updateSelectInput(
                inputId = "object_sample_colname",
                choices = NULL,
                selected = NULL
                )
              }
            })
        
        # Show menu when the metadata table upload is complete, hide otherwise
        observe({
          target_id <- "object_sample_colname"
          
          if (isTruthy(input$upload_sample_colname)){
            shinyjs::showElement(
              id = target_id,
              anim = TRUE
            )
          } else {
            shinyjs::hideElement(
              id = target_id,
              anim = TRUE
            )
          }
        })
        
        # 6. Preview Join ####
        ## 6.1. Join Preview Computation ####
        # Store warnings, if any, in join warnings
        # (the preview_error reactiveValues state variable handles errors)
        # observe({
        #   req(input$upload_sample_colname)
        #   req(input$object_sample_colname)
        #   
        #   input$upload_sample_colname
        #   input$object_sample_colname
        #   print("This observer will run now")
        # })
        
        join_warnings <-
          reactive({
            req(input$upload_sample_colname)
            req(input$object_sample_colname)
            
            input$upload_sample_colname
            input$object_sample_colname
            
            # Set the error state to FALSE if it was set to TRUE in a previous
            # preview
            add_metadata_state$preview_error <- FALSE
            # Set state of preview to "in_progress" to show spinner
            add_metadata_state$preview_status <- "in_progress"
            
            # Uploaded table
            new_metadata <- isolate({metadata_table_upload()})
            
            # Identify columns in the uploaded table that will be added
            # (all except the selected sample column)
            # After the join, the columns added will be tested for NA values
            other_colnames <- 
              colnames(new_metadata)[
                colnames(new_metadata) != input$upload_sample_colname
              ]
            
            # Run a join using the updated object table and a limited table
            # using the variables in the config file
            # (a full table will be pulled when the join is executed. This
            # is just to verify that the join proceeds properly).
            object_table <-
              SCUBA::fetch_metadata(
                object = isolate({object()}),
                vars = isolate({object_meta_varnames()})
              )
            
            # Form instructions for joining (`by` parameter input of left_join)
            # By will map the sample variable in the object metadata  
            # to the one in the table
            join_instructions <- input$upload_sample_colname
            names(join_instructions) <- input$object_sample_colname
            
            # Perform a test join
            new_object_metadata <- tryCatch(
              error = function(cnd){
                # If an error occurs during the join, 
                # set the error state to TRUE
                add_metadata_state$preview_error <- TRUE
                
                return(NULL)
              },
              {
                left_join(
                  new_metadata, 
                  object_table,
                  by = join_instructions
                )
              })
            
            # Assuming the join proceeded without errors, test table 
            # produced for irregularities
            
            # Store a log of irregularities in `warnings`
            # If warnings are returned, a warning triangle will appear in the 
            # interface, with the warning text as a tooltip
            warnings <- c()
            
            if (!is.null(new_object_metadata)){
              # Test if number of cells in the new table matches the number of 
              # cells in the original object
              # Number of cells in new table
              n_cells <- nrow(new_object_metadata)
              # Number of cells in object
              n_cells_orig <- nrow(object_metadata)
              
              if (n_cells != n_cells_orig){
                warnings <- 
                  c(warnings, 
                    paste0(
                      "The number of cells in the new table does not match ",
                      "the number of cells in the original table."
                      )
                    )
                }
              
              # Test each column added for NAs
              # NAs may be a sign of an improper join, 
              # especially if all values are NA.
              for (added_column in other_colnames){
                # Compute number of NA values
                n_na <- sum(is.na(new_object_metadata[,added_column]))
                
                # Percentage of NA values
                pct_na <- (n_na/n_cells_orig)*100
                
                # Show different warnings for >0 Na values, and 100% NA values
                if (pct_na > 0 & pct_na < 100){
                  c(warnings, 
                    paste0(
                      pct_na, "% of cells have undefined values for ", 
                      added_column, "."
                    )
                  )
                } else if (pct_na == 100){
                  c(warnings, 
                    paste0(
                      "All cells have undefined values for ", added_column, ". ",
                      "Please verify the variable selected in the sample maps ",
                      "correctly to the selected variable in the object, and ",
                      "that data exists for this variable in the uploaded table."
                      )
                    )
                }
              }
            } 
            
            # Set state of preview to "complete" to remove spinner
            add_metadata_state$preview_status <- "complete"
            
            # Enable the "Confirm" button if the join proceeded without errors
            if (add_metadata_state$preview_error == FALSE){
              shinyjs::enable(
                id = "confirm"
              )
            }
            
            # Return the list of warnings
            warnings
          })
        
        ## 6.2. UI to display results ####
        output$preview_results <-
          renderUI({
            req(add_metadata_state$preview_status == "complete")
            
            print("Render UI")
            print("Error")
            print(add_metadata_state$preview_error)
            print("Warnings")
            print(join_warnings())
            
            # Three possibilities for icons" success, warning, and error
            # Success icon: no error, no earnings
            if (add_metadata_state$preview_error == FALSE & 
                !isTruthy(join_warnings())){
                icon_notification_ui(
                  icon = "circle-check",
                  paste0(
                    'No issues detected with the uploaded table and selections. ',
                    'Press "Confirm" to proceed.'
                    )
                )
            } else if (add_metadata_state$preview_error == FALSE & 
                       isTruthy(join_warnings())){
              # Warning icon: when no error is present, but warnings are
              div(
                # Display a warning icon and summary text side-by-side 
                # in a flexbox container
                div(
                  style = "display: flex;",
                  div(
                    icon(
                      "exclamation-triangle", 
                      style = "display: inline-block; font-size: 1.7em;"
                      )
                    ),
                  div(
                    "Issues were detected with the join. See below for details."
                    )
                  ),
                # Show each warning as a bullet point
                tags$ul(
                  tagList(
                    lapply(
                      join_warnings(),
                      function(warning_text){
                        tags$li(warning_text)
                        }
                      )
                    )
                  )
                )
              } else if (add_metadata_state$preview_error == TRUE){
                # Error icon
                icon_notification_ui(
                  icon = "skull-crossbones",#"circle-xmark",
                  paste0(
                    'An error ocurred during the preview computation. Please ',
                    'verify that an appropriate table has been uploaded, and ',
                    'that the choice of the patient-level variable in the ',
                    'uploaded is appropriate, and that it properly maps to the ',
                    'corresponding sample-level variable in the object.'
                    )
                  )
                }
          })
        
        ## 6.3. Show/hide UI elements based on state of preview_status rv ####
        # If the preview_status reactiveValues is "in_progress", show the spinner
        observe({
          spinner <- "preview_spinner"
          preview_ui <- "preview_results"
          
          if (add_metadata_state$preview_status == "in_progress"){
            # In progress: show spinner, hide preview UI
            shinyjs::showElement(
              id = spinner,
              # Do this later
              anim = FALSE
              )
            
            shinyjs::hideElement(
              id = preview_ui,
              # Do this later
              anim = FALSE
            )
          } else if (add_metadata_state$preview_status == "complete"){
            # Complete: hide spinner, show preview UI
            shinyjs::hideElement(
              id = spinner,
              # Do this later
              anim = FALSE
            )
            
            shinyjs::showElement(
              id = preview_ui,
              # Do this later
              anim = FALSE
            )
          } else if (add_metadata_state$preview_status == "not_performed"){
            # Starting state: "not_performed"
            # Hide both elements in this case
            shinyjs::hideElement(
              id = spinner,
              # Do this later
              anim = FALSE
            )
            
            shinyjs::hideElement(
              id = preview_ui,
              # Do this later
              anim = FALSE
            )
          }
        })
        
        # TEMP. Just enable confirm button when all inputs are present ####
        observe({
          if (isTruthy(input$upload_sample_colname) &
              isTruthy(input$object_sample_colname) &
              isTruthy(metadata_table_upload())){
            shinyjs::enable(
              id = "confirm"
              )
          } else {
            shinyjs::disable(
              id = "confirm"
              )
            }
          
        })
        
        # 7. Respond to Cancel button ####
        observe({
          req(input$cancel)
          
          add_metadata_state$window_state <- "closed"
        })
        
        # 8. Respond to Confirm button ####
        observe({
          req(input$confirm)
          
          # Store the table and computed join instructions as a state variable
          # for return from this module
          # Table
          add_metadata_state$uploaded_table <- metadata_table_upload()
          
          # Join instructions (passed to `by` in left_join)
          join_instructions <- input$upload_sample_colname
          names(join_instructions) <- input$object_sample_colname
          
          add_metadata_state$join_instructions <- join_instructions
          
          # Store names of variables added 
          # (all variables that are not the indicated sample-level variable)
          add_metadata_state$vars_added <- 
            colnames(metadata_table_upload())[
              colnames(metadata_table_upload()) != input$upload_sample_colname
              ]
          
          add_metadata_state$window_state <- "closed"
        })
        
        # 9. Return Table and Join Instructions ####
        return(
          list(
            `uploaded_table` = 
              reactive(
                label = glue("{id}: Uploaded table"), 
                {add_metadata_state$uploaded_table}
                ),
            `join_instructions` = 
              reactive(
                label = glue("{id}: Join instructions"), 
                {add_metadata_state$join_instructions}
                ),
            `vars_added` =
              reactive(
                label = glue("{id}: Variables added"), 
                {add_metadata_state$vars_added}
                )
              )
            )
          }
        )
    }