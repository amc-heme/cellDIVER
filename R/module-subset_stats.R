#' Subset Stats Module
#'
#' In the DGE and correlation tabs, this module prints data on the subset 
#' selected in the main panel, above the test results. 
#'
#' @param id ID to use for module. All inputs and outputs created will be
#' namespaced using this ID.
#' @param tab Character vector giving the tab this module is being used in. This
#' should be either "dge" or "corr".
#' @param metadata_config the metadata section of the config file, loaded in the
#' main app. 
#' @param meta_categories a vector of all metadata variables (categories) exposed in the 
#' main browser by the config user.
#' @param gene_selected In the correlation tab, the gene selected by the user 
#' for computation.
#'
#' @noRd
subset_stats_ui <- function(id,
                            tab = c("dge", "corr"),
                            metadata_config,
                            meta_categories,
                            gene_selected = NULL
                            ){
  # Namespace function: prevents conflicts with 
  # inputs/outputs defined in other modules
  ns <- NS(id)
  
  if (tab == "dge"){
    #UI for stats in dge tab
    tagList(
      #Test statistics
      div(
        tags$strong(
          "Test selected",
          class = "x-large inline-block"
          ),
        textOutput(
          outputId = ns("print_mode"), 
          inline = FALSE
          ),
        # Statistical method used for the submitted test
        tags$strong(
          "Statistical Method:"
        ),
        textOutput(
          outputId = ns("dge_method"),
          inline = TRUE
        ),
        uiOutput(outputId = ns("edger_sample_summary")),
        # Threshold used: only displays for threshold-based DGE
        hidden(
          div(
            id = ns("threshold_info_div"),
            tags$strong(
              "Threshold value entered for high/low designation:",
              class = "inline-block"
              ),
            textOutput(
              outputId = ns("threshold"), 
              inline = TRUE
              )
            )
          )
        ),
      # Metadata-specific subset statistics
      # This caused performance issues when numeric metadata variables were 
      # entered, and cluttered the output with objects with many variables and 
      # many unique values for each value
      # The output as-is is of questionable value, so it was removed.
      
      # tags$strong(
      #   "Subset Used for Test", 
      #   class = "x-large inline-block half-space-top"
      #   ),
      # 
      # # Loop through the metadata categories read on app startup
      # # One container is created for each category in the subset using lapply.
      # # Unique values for each category will display inline with labels
      # lapply(
      #   X = meta_categories(),
      #   # Category is looped through by lapply
      #   # metadata_config and ns are "constants" that must be passed as 
      #   # additional arguments to lapply 
      #   FUN = 
      #     function(category, metadata_config, ns){
      #       subset_stats_metadata_output(
      #         category = category,
      #         metadata_config = metadata_config, 
      #         ns = ns
      #         )
      #       },
      #   # Constants
      #   metadata_config,
      #   ns
      #   ),
      
      # General subset statistics
      div(
        tags$strong(
          "Number of cells in subset: ",
          class = "half-space-top inline-block"
          ),
        textOutput(
          outputId = ns("n_cells"), 
          inline = TRUE
          )
        ),
      div(
        tags$strong("Number of cells per group: "),
        verbatimTextOutput(outputId = ns("n_by_class")),
        # Applies CSS from www/other.css to verbatimTextOutput
        class = "n_by_class_style"
        )
      )# End tagList
      
  } else if (tab == "corr"){
    # UI for stats in correlations tab
    ui <- tagList(
      tags$strong("Subset Summary and Quality Statistics", 
                  class = "x-large inline-block half-space-top"),
      # Restriction criteria
      # Create outputs for each subset menu
      lapply(
        # Loop through the metadata categories read on app startup
        X = meta_categories(), 
        FUN = function(category, metadata_config){
          # Get label for the metadata category used in the
          # current subset selection 
          # menu, defined in config file
          label <- metadata_config()[[category]]$label
        
          # Return HTML
          # Use of div creates a new line between each entry
          div(
            tags$strong(glue("{label}:")),
            # Text output to report values in selected category 
            # found in the subset
            textOutput(
              # Output ID: uses the category name 
              outputId = ns(glue("selected_{category}")),
              inline = TRUE)
            )
          },
        # metadata_config must be passed to the function above after it is 
        # defined (see ... argument in ?lapply)
        metadata_config
        ), # End lapply
      
      # Number of cells in subset
      div(
        tags$strong("Number of cells in subset: ",
                      class = "half-space-top inline-block"),
        textOutput(outputId = ns("n_cells"), inline = TRUE)
        ),
      
      # Number and percentage of cells with nonzero reads for the selected gene
      div(
        tags$strong(glue("Cells with non-zero reads for {gene_selected()}:")),
        textOutput(outputId = ns("n_nonzero_and_percent"), inline = TRUE)
        )
      ) # End tagList
    
    # Return UI from module to the parent UI 
    return(ui)
  }
  
}

#' Subset Stats Module
#'
#' In the DGE and correlation tabs, this module prints data on the subset 
#' selected in the main panel, above the test results. 
#'
#' @param id ID to use for module. All inputs and outputs created will be
#' namespaced using this ID.
#' @param tab Character vector giving the tab this module is being used in. This
#' should be either "dge" or "corr".
#' @param subset a subset of a Seurat object, for which stats will be displayed.
#' @param event_expr a reactive expression; all reactive expressions in this 
#' module will re-compute in response to changes in this expression.
#' @param meta_categories a vector of all metadata variables (categories) exposed in the 
#' main browser by the config user.
#' @param gene_selected In the correlation tab, the gene selected by the user 
#' for computation.
#' @param nonzero_threshold The minimum acceptable proportion of cells with
#' nonzero reads for the feature used to compute correlations. This is defined 
#' in the main server function.
#' @param group_by_category For the DGE tab, the group by metadata variable
#'  (category) currently chosen for the test. 
#' @param metaclusters_present only used in the DGE tab. If TRUE, stats displays
#' will take metaclusters into account (number cells will be shown by metacluster
#' instead of by individual metadata variables).
#' @param thresholding_present only used in the DGE tab. If TRUE, the number of cells above and below the chosen expression threshold will be displayed.
#' @param dge_simple_threshold only used in the DGE tab. If thresholding is 
#' being used, the value of the threshold chosen is passed here to be displayed.
#' @param dge_method For the DGE tab, a reactive returning the submitted
#' differential-expression method.
#' @param dge_mode For the DGE tab, a reactive returning the submitted analysis
#' mode.
#' @param dge_group_1,dge_group_2 For standard DGE, reactives returning the
#' submitted comparison groups in contrast order.
#'
#' @noRd
subset_stats_server <- 
  function(id,
           tab = c("dge", "corr"), #Non-reactive
           subset, # Reactive
           event_expr, # Reactive
           meta_categories, # Reactive
           gene_selected = NULL,
           nonzero_threshold = NULL,
           group_by_category,
           metaclusters_present = NULL,
           thresholding_present = NULL,
           dge_simple_threshold = NULL,
           dge_method = NULL,
           dge_mode = NULL,
           dge_group_1 = NULL,
           dge_group_2 = NULL
           ){
    moduleServer(
      id, 
      function(input, output, session){
        # Server namespace function (used for renderUI and 
        # JavaScript ID references)
        ns <- session$ns
        
        # For the DGE tab, ensure that metaclusters_present and 
        # thresholding_present are defined 
        if (
          tab == "dge" & 
            (!is.reactive(metaclusters_present) | 
             !is.reactive(thresholding_present) |
             !is.reactive(dge_method) |
             !is.reactive(dge_mode) |
             !is.reactive(dge_group_1) |
             !is.reactive(dge_group_2))
        ){
          stop("If `tab`=='dge', arguments `metaclusters_present` and
               all DGE selection arguments must be defined as reactive values.")
        }
        
        # 1. Compute stats for subset ------------------------------------------
        # Cells in subset (computed for both DGE and corr tabs)
        n_cells <- 
          eventReactive(
            event_expr(),
            {
              SCUBA::get_all_cells(subset()) |> 
              length()
            })
        
        # Nonzero reads, proportion of nonzero reads, and percentage
        # Computed for the correlations tab only
        if (tab == "corr"){
          # Cells with nonzero reads
          n_nonzero <- 
            eventReactive(
              event_expr(),
              {
                # Will work as long as the raw counts slot/assay/layer is 
                # "counts" (case sensitive. This is the default name for Seurat 
                # and SingleCellExperiment objects)
                expr_data <-
                  SCUBA::fetch_data(
                    subset(),
                    vars = gene_selected(),
                    layer = "counts"
                    ) 
                
                sum(expr_data > 0)
              })
          
          # Proportion of nonzero reads
          prop_nonzero <- 
            eventReactive(
              event_expr(),
              {
                n_nonzero() / n_cells()
              })
          
          # Percentage of nonzero reads
          percent_nonzero <- 
            eventReactive(
              event_expr(),
              {
                format(
                  prop_nonzero() * 100,
                  # Display at least three sig figs in percentage
                  digits = 3,
                  # Display at least two digits after decimal point
                  nsmall = 2,
                  scientific = FALSE
                )
              })
          }
        
        # For DGE tab only: stats on selected DE/marker groups, test 
        # mode, and number of cells by class
        if (tab == "dge"){
          output$dge_method <- renderText({
            results <- event_expr()
            submitted_method <- attr(results, "dge_method")
            if (is.null(submitted_method)){
              submitted_method <- dge_method()
            }
            dge_method_label(submitted_method)
          })

          output$edger_sample_summary <- renderUI({
            results <- event_expr()
            req(identical(attr(results, "dge_method"), "edger"), results)

            details <- attr(results, "edger_details")
            req(!is.null(details))

            sample_summary <- details$sample_summary
            req(is.data.frame(sample_summary))
            group_1_summary <-
              sample_summary[sample_summary$group == details$group_1, , drop = FALSE]
            group_2_summary <-
              sample_summary[sample_summary$group == details$group_2, , drop = FALSE]
            excluded_count <- nrow(details$excluded_profiles)

            excluded_text <-
              if (excluded_count > 0L){
                paste0(
                  " ", excluded_count,
                  " sample/group pseudobulk profile(s) were excluded; see ",
                  "the retained/excluded details attached to the result."
                )
              } else {
                " No pseudobulk profiles were excluded."
              }

            div(
              class = "half-space-top",
              tags$strong("Biological samples retained: "),
              tags$span(
                paste0(
                  details$group_1, ": ", group_1_summary$retained_samples, "; ",
                  details$group_2, ": ", group_2_summary$retained_samples, ".",
                  excluded_text
                )
              )
            )
          })

          # `groups`
          # The groups or markers used for DGE. 
          # Used for downstream stats
          groups <- 
            eventReactive(
              event_expr(),
              {
                # Take unique values for DGE groups
                SCUBA::unique_values(
                  subset(),
                  var = 
                    # Metadata variable to use: use metaclusters or the simple 
                    # expression threshold if either of those modes are enabled.
                    # Otherwise, use the group by variable (category).
                    if (metaclusters_present()){
                      "metacluster"
                      } else if (thresholding_present()){
                      "simple_expr_threshold"
                      } else {
                      group_by_category()
                      }
                  )
                })
          
          # Number of groups of the group_by metadata 
          # category in subset
          n_groups <- 
            eventReactive(
              event_expr(),
              {
                length(groups())
              })
          
          # Print the type of test (DE or marker identification) and a 
          # brief description of the groups selected
          mode_description <- 
            eventReactive(
              event_expr(),
              {
                dge_mode_description(
                  mode = dge_mode(),
                  observed_groups = groups(),
                  group_1 = dge_group_1(),
                  group_2 = dge_group_2(),
                  thresholding = thresholding_present()
                )
              })
          
          # Number of cells in subset by class
          n_by_class <- 
            eventReactive(
              event_expr(),
              {
                # Number of cells by class (tibble format)
                # Identify which metadata column is being used to define 
                # groups.
                
                # Attempted to use a case_when statement here, but it caused
                # errors. This issue appears to be with the reactive 
                # group_by_category(), which is only defined when 
                # metaclusters_present() and thresholding_present() are both 
                # FALSE. The reactive appears to be evaluated regardless of what 
                # the conditional statements passed evaluate to, even if the 
                # value is not assigned 
                
                grouping_column <-
                  if (metaclusters_present()){
                    # For metaclusters, "metacluster"
                    "metacluster"
                  } else if (thresholding_present()){
                    # For simple thresholding, "simple_expr_threshold"
                    "simple_expr_threshold"
                  } else {
                    # Standard behavior: use group_by category
                    group_by_category()
                  }
                  
                n_cells_tibble <-
                  # Fetch full metadata table, then group by metadata variable
                  SCUBA::fetch_metadata(
                    subset(),
                    full_table = TRUE
                    ) |>
                  group_by(.data[[grouping_column]]) |>
                  # Calculate number of cells per group
                  summarise(n = n())

                # Extract information from tibble
                # Class names: first column of tibble
                class_names <- as.character(n_cells_tibble[[1]])
                # Cell counts: second column of tibble
                n_cells <- n_cells_tibble[[2]]

                # Print list of groups and the number of cells in each
                n_cells_list = list()
                for (i in 1:nrow(n_cells_tibble)){
                  n_cells_list[[i]] <-
                    glue("{class_names[i]}: {n_cells[i]}")
                }

                # Collapse list of class-count pairs into a string
                # \n is the separator (will be read by
                # verbatimTextOutput())
                n_by_class <- paste(n_cells_list, collapse = "\n")

                n_by_class
              })
          }
        
        # 2. Notifications -----------------------------------------------------
        # 2.1. Nonzero proportion is below the defined threshold 
        # Use observe statement to reactively check prop_nonzero()
        # Applies to correlations tab only
        if (tab == "corr"){
          observeEvent(
            event_expr(),
            ignoreNULL=FALSE,
            {
              if (prop_nonzero() < nonzero_threshold){
                # Define notification UI (warning icon plus text)
                notification_ui <- span(
                  # Warning icon (inline and enlarged)
                  icon("exclamation-triangle",
                       style = "display: inline-block; 
                                font-size: 1.7em;"),
                  # Notification text with proportion and 
                  # number of non-zero cells
                  span(glue("Low gene coverage: the selected 
                                    feature was detected in {percent_nonzero()}%
                                    of cells within the selection restriction 
                                    criteria ({n_nonzero()}/{n_cells()} cells). 
                                    Correlation results may be inaccurate."),
                       # Font size of notification text
                       style="font-size: 1.17em;")#End span
                ) # End notification_ui span
                
                # Display notification UI
                showNotification(
                  ui = notification_ui,
                  # Duration=NULL will make the message persist 
                  # until dismissed
                  duration = NULL,
                  id = ns("high_zero_content"),
                  session=session
                  )
              }# End if statement
            })# End observeEvent
          }
        
        # 3. Show/hide threshold information text ------------------------------
        if (tab == "dge"){
          observeEvent(
            event_expr(),
            label = glue("{id}: show/hide threshold value"),
            {
              target_id = "threshold_info_div"
              
              # Show the threshold information when simple thresholding is 
              # being used (this will be when the threshold value is defined)
              if (!is.null(dge_simple_threshold())){
                showElement(
                  id = target_id
                )
              } else {
                hideElement(
                  id = target_id
                )
              }
            })
          
        }
        
        # 4. Display stats -----------------------------------------------------
        ## 4.1. Number of cells (both tabs)
        output$n_cells <- 
          renderText({
            n_cells()
            })
        
        ## 4.2. Cells with nonzero reads (correlations tab only)
        if (tab == "corr"){
          output$n_nonzero_and_percent <-
            renderText({
              glue("{n_nonzero()} ({percent_nonzero()}%)")
            })
          }
        
        ## 4.3. - 4.5. Outputs specific to DGE tab
        if (tab == "dge"){
          ## 4.3. Print description of test selected
          output$print_mode <- 
            renderText({
              mode_description()
            })
          
          ## 4.4. Print number of cells by class
          output$n_by_class <- 
            renderText({
              n_by_class()
            })
          
          ## 4.5. Threshold (if using threshold-based DGE)
          output$threshold <-
            renderText({
              dge_simple_threshold()
            })
        }
        
        ## 4.6. Summary of unique metadata in subset (both tabs)
        # lapply creates an output for each metadata category
        # lapply(
        #   X = meta_categories(), 
        #   FUN = function(category){
        #     output[[glue("selected_{category}")]] <-
        #       renderText({
        #         # Display unique values appearing in the subset 
        #         # for the category
        #         SCUBA::unique_values(
        #           subset(),
        #           var = category
        #           ) |> 
        #           # Sort unique values alphanumerically
        #           # May add support for custom order later
        #           str_sort(numeric = TRUE) |> 
        #           vector_to_text()
        #       }) # End renderText
        #   }
        # )
        
        # 5. Return Stats from Server ------------------------------------------
        # For dge tab: return n_cells, groups, and n_groups
        # Return a list of reactives, as opposed to a reactive list
        if (tab=="dge"){
          return(
            list(
              `n_cells` = reactive({n_cells()}),
              `groups` = reactive({groups()}),
              `n_groups` = reactive({n_groups()})
            )
          )
        } # Currently no need to return values for correlations tab
      })
}

# Describe the submitted analysis without reordering a two-group contrast based
# on metadata factor or cell order.
dge_mode_description <- function(mode,
                                 observed_groups,
                                 group_1 = NULL,
                                 group_2 = NULL,
                                 thresholding = FALSE){
  if (identical(mode, "mode_dge")){
    if (!isTRUE(thresholding) && length(group_1) > 0L && length(group_2) > 0L){
      group_1_label <- paste(group_1, collapse = " and ")
      group_2_label <- paste(group_2, collapse = " and ")
    } else {
      group_1_label <- observed_groups[[1]]
      group_2_label <- observed_groups[[2]]
    }

    return(
      paste0(
        "Differential Expression (", group_1_label,
        " vs. ", group_2_label, ")"
      )
    )
  }

  paste0("Marker Identification (", length(observed_groups), " groups)")
}
