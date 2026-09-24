#' dge_table_filtering UI
#'
#' @noRd
dge_table_filtering_ui <- function(id){
  # Namespace function
  ns <- NS(id)
  
  hidden(
    div(
      id = ns("filter_ui"),
      cellDIVER:::collapsible_panel(
        inputId = ns("filter_ui_panel"),
        label = "Filter Table",
        active = TRUE,
        
        # Filter by group
        # Use a transparent collapsible panel to cut down on clutter in filter
        # interface
        collapsible_panel(
          inputId = ns("group_header"),
          label = 
            tags$span(
              tags$span("Filter by Group: "),
              a(
                id = ns("group_tooltip"),
                icon("info-circle"), 
                href = paste0("https://amc-heme.github.io/cellDIVER/articles/", 
                              "full_documentation.html"),  
                target = "_blank"
                )
              ),
          transparent = TRUE,
          size = "s",
          # VituralSelectInput: like PickerInput, but with a more pleasant 
          # interface
          # https://sa-si-dev.github.io/virtual-select/#/?id=get-started
          # Use zIndex to ensure the dropdown displays above other inputs
          # https://github.com/dreamRs/shinyWidgets/issues/701
          shinyWidgets::virtualSelectInput(
            inputId = ns("group"),
            label = NULL,
            # Choices are populated in the server
            choices = NULL, 
            selected = character(0),
            multiple = TRUE,
            search = TRUE,
            optionsCount = 5,
            zIndex = 4
            ),
          shinyBS::bsTooltip(
            id = ns("group_tooltip"), 
            title = 
              paste0(
                'Select groups in the menu to filter the table for ',
                'those groups. When no groups are selected, all groups will ',
                'included in the table (no filtering will be applied based ',
                'on group). Use the checkbox next to "search" to select or ',
                'de-select all groups, and the "search" text box to search ',
                'for groups via text.'
                ), 
            placement = "bottom", 
            trigger = "hover",
            options = NULL
            )
          ),
        
        # By feature
        collapsible_panel(
          inputId = ns("feature_header"),
          label = 
            tags$span(
              "Filter by Feature: ",
              a(
                id = ns("feature_tooltip"),
                icon("info-circle"), 
                href = paste0("https://amc-heme.github.io/cellDIVER/articles/", 
                              "full_documentation.html"),  
                target = "_blank"
              )
            ),
          transparent = TRUE,
          size = "s",
          shiny::selectizeInput(
            inputId = ns("feature"),
            multiple = TRUE,
            label = NULL,
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
          shinyBS::bsTooltip(
            id = ns("feature_tooltip"), 
            title = 
              paste0(
                'Type genes/features in the menu below to fitler the table ',
                'for those genes. You may enter as many features as you would ',
                'like in this menu. To remove a feature from the filtering, ',
                'press the "x" button next to the feature. When no features ',
                'are entered, no filtering will be applied based on feature.'
                ), 
            placement = "bottom", 
            trigger = "hover",
            options = NULL
            )
          ),
        
        # Average Expression filter interface
        # Collapsible panel is within a container to show/hide the whole
        # interface based on the presence/absence of the average expression column
        div(
          id = ns("expr_ui"),
          collapsible_panel(
            inputId = ns("expr_header"),
            label = 
              tags$span(
                "Filter by Average Expression: ",
                a(
                  id = ns("expr_tooltip"),
                  icon("info-circle"), 
                  href = paste0("https://amc-heme.github.io/cellDIVER/articles/", 
                                "full_documentation.html"),  
                  target = "_blank"
                  )
                ),
            transparent = TRUE,
            size = "s",
            # Expression range slider
            shiny::sliderInput(
              inputId = ns("expr"),
              label = NULL,
              # Values are updated in the server function based on the table
              min = 0,
              max = 1,
              value = c(0, 1),
              round = 4
            ),
            
            # Text entry for range
            # Displays if checkbox below is enabled
            shinyWidgets::awesomeCheckbox(
              inputId = ns("expr_manual_enable"),
              label = "Manually Enter Range:",
              status = "primary"
            ),
            
            # Display a min and max checkbox side-by-side
            hidden(
              tags$div(
                id = ns("expr_manual_ui"),
                class = "compact-options-container",
                style = 'display: inline-block;',
                div(
                  # Use column() to display inputs side-by-side
                  # column() uses bootstrap layouts for placing components
                  shiny::column(
                    width = 6,
                    style = "padding: 0px 5px;",
                    shinyWidgets::searchInput(
                      inputId = ns("expr_min_text"),
                      label = "Min Expr"
                    )
                  ),
                  shiny::column(
                    width = 6,
                    style = "padding: 0px 5px;",
                    shinyWidgets::searchInput(
                      inputId = ns("expr_max_text"),
                      label = "Max Expr"
                    )
                  )
                )
              )
            ),
            shinyBS::bsTooltip(
              id = ns("expr_tooltip"), 
              title = 
                paste0(
                  'Drag either ends of the slider to set the min and max ',
                  'values to filter based on average expression. To ',
                  'manually enter thresholds for either end, select the ',
                  '"manually enter range" checkbox and type the min/max ',
                  'values in the text boxes that appear. Press enter to apply ',
                  'filters. If either box is blank, no filter is applied to ',
                  'the bound set by the text box.'
                  ), 
              placement = "bottom", 
              trigger = "hover",
              options = NULL
            )
          )
        ),
        
        # LFC filter interface
        div(
          id = ns("lfc_ui"),
          collapsible_panel(
            inputId = ns("lfc_header"),
            label = 
              tags$span(
                "Filter by Log-2 Fold Change Value:",
                a(
                  id = ns("lfc_tooltip"),
                  icon("info-circle"), 
                  href = paste0("https://amc-heme.github.io/cellDIVER/articles/", 
                                "full_documentation.html"),  
                  target = "_blank"
                  )
                ),
            transparent = TRUE,
            size = "s",
            # LFC range slider
            shiny::sliderInput(
              inputId = ns("lfc"),
              label = NULL,
              # Values are updated in the server function based on the table
              min = 0,
              max = 1,
              value = c(0, 1),
              round = 4,
              # Ticks are often unhelpful due to the non-integer range of 
              # the slider (ticks will appear in non-intuitive decimal increments)
              ticks = FALSE
              ),
            
            # Text entry of LFC range
            # Displays if checkbox below is enabled
            shinyWidgets::awesomeCheckbox(
              inputId = ns("lfc_manual_enable"),
              label = "Manually Enter Range:",
              status = "primary"
              ),
            
            # Display a min and max checkbox side-by-side
            hidden(
              tags$div(
                id = ns("lfc_manual_ui"),
                class = "compact-options-container",
                style = 'display: inline-block;',
                div(
                  # Use column() to display inputs side-by-side
                  # column() uses bootstrap layouts for placing components
                  shiny::column(
                    width = 6,
                    style = "padding: 0px 5px;",
                    shinyWidgets::searchInput(
                      inputId = ns("lfc_min_text"),
                      label = "Min LFC"
                    )
                  ),
                  shiny::column(
                    width = 6,
                    style = "padding: 0px 5px;",
                    shinyWidgets::searchInput(
                      inputId = ns("lfc_max_text"),
                      label = "Max LFC"
                      )
                    )
                  )
                )
              ),
            
            shinyBS::bsTooltip(
              id = ns("lfc_tooltip"), 
              title = 
                paste0(
                  'Drag either ends of the slider to set the min and max ',
                  'values to filter based on log-fold change. To ',
                  'manually enter thresholds for either end, select the ',
                  '"manually enter range" checkbox and type the min/max ',
                  'values in the text boxes that appear. Press enter to apply ',
                  'filters. If either box is blank, no filter is applied to ',
                  'the bound set by the text box.'
                ), 
              placement = "bottom", 
              trigger = "hover",
              options = NULL
              )
            )
          ),
        
        # AUC slider
        collapsible_panel(
          inputId = ns("auc_header"),
          label = 
            tags$span(
              "Filter by AUC Value: ",
              a(
                id = ns("auc_tooltip"),
                icon("info-circle"), 
                href = paste0("https://amc-heme.github.io/cellDIVER/articles/", 
                              "full_documentation.html"),  
                target = "_blank"
                )
              ),
          transparent = TRUE,
          size = "s",
          shiny::sliderInput(
            inputId = ns("auc"),
            label = NULL,
            # Values are updated in the server function based on the table
            min = 0,
            max = 1,
            value = c(0,1),
            step = 0.01,
            round = 2
            ),
          
          shinyBS::bsTooltip(
            id = ns("auc_tooltip"), 
            title = 
              paste0(
                'Drag either ends of the slider to set the min and max ',
                'values to filter based on auc value.'
                ), 
            placement = "bottom", 
            trigger = "hover",
            options = NULL
            )
          ),
        
        # Filter by p-value
        collapsible_panel(
          inputId = ns("pval_header"),
          label = 
            tags$span(
              "Filter by Adjusted p-value: ",
              a(
                id = ns("pval_tooltip"),
                icon("info-circle"), 
                href = paste0("https://amc-heme.github.io/cellDIVER/articles/", 
                              "full_documentation.html"),  
                target = "_blank"
              )
            ),
          transparent = TRUE,
          size = "s",
          # Adjusted p-value
          shiny::selectInput(
            inputId = ns("pval_adj"),
            label = "Choose a p-value Threshold:",
            choices = 
              c(1,
                0.05, 
                0.01, 
                0.001, 
                1e-04, 
                1e-05, 
                1e-06,
                1e-07,
                1e-08,
                1e-09,
                1e-10, 
                1e-20, 
                1e-50, 
                1e-100),
            selected = 1
            ),
          
          shinyBS::bsTooltip(
            id = ns("pval_tooltip"), 
            title = 
              paste0(
                'Use the selection menu to define a cutoff for the adjusted ',
                'p-value of test results. Only entries with adjusted p-values ',
                'less than the threshold selected will be displayed.'
              ), 
            placement = "bottom", 
            trigger = "hover",
            options = NULL
            )
          ),
        
        # Percent in group slider
        collapsible_panel(
          inputId = ns("pct_in_header"),
          label = 
            tags$span(
              "Filter by Percentage Within Group: ",
              a(
                id = ns("pct_in_tooltip"),
                icon("info-circle"), 
                href = paste0("https://amc-heme.github.io/cellDIVER/articles/", 
                              "full_documentation.html"),  
                target = "_blank"
                )
            ),
          transparent = TRUE,
          size = "s",
          shiny::sliderInput(
            inputId = ns("pct_in"),
            label = NULL,
            min = 0,
            max = 100,
            value = c(0, 100),
            step = 1,
            post = " %"
            ),
          
          shinyBS::bsTooltip(
            id = ns("pct_in_tooltip"), 
            title = 
              paste0(
                'Drag either ends of the slider to set the min and max ',
                'values to filter based on percentage expression within ',
                'a group.'
              ), 
            placement = "bottom", 
            trigger = "hover",
            options = NULL
            )
          ),
        
        # Percentage outside group slider
        collapsible_panel(
          inputId = ns("pct_out_header"),
          label = 
            tags$span(
              "Filter by Percentage Outside Group: ",
              a(
                id = ns("pct_out_tooltip"),
                icon("info-circle"), 
                href = paste0("https://amc-heme.github.io/cellDIVER/articles/", 
                              "full_documentation.html"),  
                target = "_blank"
                )
              ),
          transparent = TRUE,
          size = "s",
          shiny::sliderInput(
            inputId = ns("pct_out"),
            label = NULL,
            min = 0,
            max = 100,
            value = c(0, 100),
            step = 1,
            post = " %"
            ),
          
          shinyBS::bsTooltip(
            id = ns("pct_out_tooltip"), 
            title = 
              paste0(
                'Drag either ends of the slider to set the min and max ',
                'values to filter based on percentage expression ',
                'outside of a group.'
                ), 
            placement = "bottom", 
            trigger = "hover",
            options = NULL
          )
          )
        )
      )
    )
}

#' dge_table_filtering Server
#'
#' @param id Used to match server function to ui function.
#' 
#' @param dge_table the table produced in the DGE tab.
#' 
#' @noRd
dge_table_filtering_server <- 
  function(id,
           dge_table){
    moduleServer(
      id,
      function(input, output, session){
        # 1. Show/hide filter interface ####
        observe({
          # Show/hide based on whether or not the dge table is defined
          target <- "filter_ui"
          
          if (isTruthy(dge_table())){
            shinyjs::showElement(
              id = target,
              anim = TRUE
            )
          } else {
            shinyjs::hideElement(
              id = target,
              anim = TRUE
            )
          }
        })
        
        # 2. Manage Display of UI Elements ####
        ## 2.1. Average Expression ####
        ### 2.1.1. Show/hide Full Interface ####
        # Average expression interface is shown only for tables that contain 
        # the average expression column
        observe({
          req(dge_table())
          
          target <- "expr_ui"
          
          if ("avgExpr" %in% colnames(dge_table())){
            shinyjs::showElement(
              id = target,
              anim = TRUE
            )
          } else {
            shinyjs::hideElement(
              id = target,
              anim = TRUE
            )
          }
        })
        
        ### 2.1.2. Manage Manual Entry UI ####
        # When manual entry is enabled, show text boxes for manual entry, and 
        # disable the slider that appears by default
        observe({
          manual_interface_id = "expr_manual_ui"
          slider_id = "expr"
          
          if (isTruthy(input$expr_manual_enable)){
            # Show the manual entry interface
            shinyjs::showElement(
              id = manual_interface_id,
              anim = TRUE
            )
            
            # Disable the slider while the text interface is enabled
            shinyjs::disable(
              id = slider_id
            )
          } else {
            # Hide the manual entry interface and re-enable the slider 
            # when the checkbox is unchecked
            shinyjs::hideElement(
              id = manual_interface_id,
              anim = TRUE
            )
            
            shinyjs::enable(
              id = slider_id
            )
          }
        })
        
        ## 2.2. AUC ####
        ### 2.2.1. Show/hide full interface ####
        observe({
          req(dge_table())
          
          target <- "auc_header"
          
          if ("auc" %in% colnames(dge_table())){
            shinyjs::showElement(
              id = target,
              anim = TRUE
            )
          } else {
            shinyjs::hideElement(
              id = target,
              anim = TRUE
            )
          }
        })
        
        ### 2.2.2. Manage Manual Entry UI #### 
        # When manual entry is enabled, show text boxes for manual entry, and 
        # disable the slider that appears by default
        observe({
          manual_interface_id = "lfc_manual_ui"
          slider_id = "lfc"
          
          if (isTruthy(input$lfc_manual_enable)){
            # Show the manual min/max entry and disable the slider when 
            # the checkbox is enabled
            shinyjs::showElement(
              id = manual_interface_id,
              anim = TRUE
            )
            
            shinyjs::disable(
              id = slider_id
            )
          } else {
            # When disabled, hide the interface and re-enable the slider
            shinyjs::hideElement(
              id = manual_interface_id,
              anim = TRUE
            )
            
            shinyjs::enable(
              id = slider_id
            )
          }
        })
        
        ## 2.3. Percentage inside, Outside Group #### 
        ### 2.3.1. Show/hide interface ####
        # Show each interface the pct_in and pct_out columns are in the DGE 
        # table, and hide otherwise
        observe({
          req(dge_table())
          
          target <- "pct_in_header"
          
          if ("pct_in" %in% colnames(dge_table())){
            shinyjs::showElement(
              id = target,
              anim = TRUE
            )
          } else {
            shinyjs::hideElement(
              id = target,
              anim = TRUE
            )
          }
        })
        
        observe({
          req(dge_table())
          
          target <- "pct_out_header"
          
          if ("pct_out" %in% colnames(dge_table())){
            shinyjs::showElement(
              id = target,
              anim = TRUE
            )
          } else {
            shinyjs::hideElement(
              id = target,
              anim = TRUE
            )
          }
        })
        
        
        # 3. Update inputs based on the DGE table returned ####
        observe({
          req(dge_table())
          
          if ("group" %in% colnames(dge_table())){
            # Update filter menu with the group names present in the 
            # original table (the one returned from scDE)
            all_groups <- 
              dge_table() %>% 
              dplyr::pull(group) %>% 
              unique()
            
            shinyWidgets::updateVirtualSelect(
              session = session,
              inputId = "group",
              choices = all_groups,
              selected = character(0)
              )
          }
          
          if ("feature" %in% colnames(dge_table())){
            all_features <-
              dge_table() %>% 
              dplyr::pull(feature) %>% 
              unique()
            
            updateSelectizeInput(
              session = session,
              inputId = "feature",
              choices = all_features,
              server = TRUE
              )
          }
          
          # Expression slider: update based on range of avgExpr column
          if ("avgExpr" %in% colnames(dge_table())){
            # Determine min, max L2FC values
            min_expr <- 
              dge_table() %>% 
              pull(avgExpr) %>% 
              min(na.rm = TRUE) %>% 
              signif(digits = 4)
            
            max_expr <- 
              dge_table() %>% 
              pull(avgExpr) %>% 
              max(na.rm = TRUE) %>% 
              signif(digits = 4)
            
            # Update slider with max, min values
            shiny::updateSliderInput(
              session = session,
              inputId = "expr",
              min = min_expr,
              max = max_expr,
              value = c(min_expr, max_expr)
            )
          }
          
          # LFC range slider: update based on range of Log2FC column
          if ("log2FC" %in% colnames(dge_table())){
            # Determine min, max L2FC values
            min_lfc <- 
              dge_table() %>% 
              pull(log2FC) %>% 
              min(na.rm = TRUE) %>% 
              signif(digits = 4)
            
            max_lfc <- 
              dge_table() %>% 
              pull(log2FC) %>% 
              max(na.rm = TRUE) %>% 
              signif(digits = 4)
            
            # Update slider with max, min values
            shiny::updateSliderInput(
              session = session,
              inputId = "lfc",
              min = min_lfc,
              max = max_lfc,
              value = c(min_lfc, max_lfc)
              )
          }
        })
        
        # 4. Conditional processing of inputs ####
        # `expr_value` is Named to avoid namespace collisions with 
        # expr(), expression()
        ## 4.1. Average expression return value ####
        expr_value <- 
          reactive({
            req(dge_table())
            
            # Conditional values 
            if ("avgExpr" %in% colnames(dge_table())){
              # Return the value of the slider or the text boxes depending on
              # whether manual entry is enabled
              if (isTruthy(input$expr_manual_enable)){
                # Manual (text) entry: return values of max and min, using 
                # NULL for undefined entries (NULL will result in no filtering
                # being applied)
                min <- 
                  if (isTruthy(input$expr_min_text)){
                    as.numeric(input$expr_min_text)
                    } else NULL
                
                max <- 
                  if (isTruthy(input$expr_max_text)){
                    as.numeric(input$expr_max_text)
                    } else NULL
                
                # Explicitly specify `min` and `max` return elements
                # avoids ambiguity that arises if one value is NULL
                # (if c(min, max) were used and max was NULL, the return value 
                # would be of length 1 instead of 2. It would not be clear that
                # this value is the min)
                list(
                  `min` = min,
                  `max` = max
                  )
              } else {
                # Slider entry
                # Return values as-is *unless either bound is equal to the 
                # computed min and max of the slider*. The min and max values 
                # for the slider are rounded, which may result in results equal
                # to the non-rounded values being filtered out incorrectly.
                slider_min <- 
                  dge_table() %>% 
                  pull(avgExpr) %>% 
                  min(na.rm = TRUE) %>% 
                  signif(digits = 4)
                
                slider_max <- 
                  dge_table() %>% 
                  pull(avgExpr) %>% 
                  max(na.rm = TRUE) %>% 
                  signif(digits = 4)
                
                min <- if (input$expr[1] != slider_min) input$expr[1] else NULL
                max <- if (input$expr[2] != slider_max) input$expr[2] else NULL
                
                # Return values
                list(
                  `min` = min,
                  `max` = max
                  )
              }
            } else {
              # If the average expression column is not in the table, 
              # return NULL for both the min and the max.
              list(
                `min` = NULL,
                `max` = NULL
                )
              }
          })
        
        ## 4.2. LFC return value ####
        lfc_value <- 
          reactive({
            req(dge_table())
            
            # Conditional values 
            if ("log2FC" %in% colnames(dge_table())){
              # Return the value of the slider or the text boxes depending on
              # whether manual entry is enabled
              if (isTruthy(input$lfc_manual_enable)){
                # Manual (text) entry: return values of max and min, using 
                # NULL for undefined entries (NULL will result in no filtering
                # being applied)
                min <- 
                  if (isTruthy(input$lfc_min_text)){
                    as.numeric(input$lfc_min_text)
                  } else NULL
                
                max <- 
                  if (isTruthy(input$lfc_max_text)){
                    as.numeric(input$lfc_max_text)
                  } else NULL
                
                # Explicitly specify `min` and `max` return elements
                # avoids ambiguity that arises if one value is NULL
                # (if c(min, max) were used and max was NULL, the return value 
                # would be of length 1 instead of 2. It would not be clear that
                # this value is the min)
                list(
                  `min` = min,
                  `max` = max
                )
              } else {
                # Slider entry
                # Return values as-is *unless either bound is equal to the 
                # computed min and max of the slider*. The min and max values 
                # for the slider are rounded, which may result in results equal
                # to the non-rounded values being filtered out incorrectly.
                slider_min <- 
                  dge_table() %>% 
                  pull(log2FC) %>% 
                  min(na.rm = TRUE) %>% 
                  signif(digits = 4)
                
                slider_max <- 
                  dge_table() %>% 
                  pull(log2FC) %>% 
                  max(na.rm = TRUE) %>% 
                  signif(digits = 4)
                
                min <- if (input$lfc[1] != slider_min) input$lfc[1] else NULL
                max <- if (input$lfc[2] != slider_max) input$lfc[2] else NULL
                
                # Return values
                list(
                  `min` = min,
                  `max` = max
                )
              }
            } else {
              # If the average expression column is not in the table, 
              # return NULL for both the min and the max.
              list(
                `min` = NULL,
                `max` = NULL
              )
            }
          })
        
        ## 4.3 AUC ####
        # Explicitly return NULL if "auc" does not appear in the table
        # Avoids issue #350
        auc <-
          reactive({
            if ("auc" %in% colnames(dge_table())){
              input$auc
              } else {
                NULL
                }
            })
        
        ## 4.4. Pct in, Pct out ####
        # Return NULL for pct_in, pct_out, if they are not in the DGE table
        # (this is currently the case for BPCells objects)
        pct_in <-
          reactive({
              if ("pct_in" %in% colnames(dge_table())){
                input$pct_in
              } else {
                NULL
              }
            })
        
        pct_out <-
          reactive({
            if ("pct_out" %in% colnames(dge_table())){
              input$pct_out
            } else {
              NULL
            }
          })
        
        # 5. Return filter inputs ####
        return(
          list(
            `group` = reactive({input$group}),
            `feature` = reactive({input$feature}),
            `expression` = expr_value,
            `lfc` = lfc_value,
            `auc` = auc,
            # pval_adj selection must be converted to numeric before returning
            `pval_adj` = reactive({as.numeric(input$pval_adj)}),
            `pct_in` = pct_in,
            `pct_out` = pct_out
            )
        )
      }
  )
}