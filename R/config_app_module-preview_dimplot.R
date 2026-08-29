#' preview_dimplot_ui 
#'
#' Used in the "general" tab of the config app. Displays a DimPlot used for a
#' preview image, along with some settings from the plot module of the main
#' app for customization.
#'
#' @param id ID to use for module elements.
#' @param object a Seurat object.
#' @param non_numeric_cols non-numeric metadata variables (columns). These are
#' excluded from the choices of variables for group.by and split.by. 
#' 
#' @noRd
preview_dimplot_ui <- 
  function(
    id,
    object,
    non_numeric_cols
    ){
    # Namespace function: prevents conflicts with IDs defined in other modules
    ns <- NS(id)
    
    tagList(
      # Group by
      selectInput(
        inputId = ns("group_by"), 
        label = "Metadata to Group By",
        choices = non_numeric_cols, 
        selected = 
          if ("clusters" %in% non_numeric_cols){
            "clusters"
          } else {
            non_numeric_cols[1]
          }
        ),
      # Split by
      selectInput(
        inputId = ns("split_by"), 
        label = "Metadata to Split By",
        choices = c("none", non_numeric_cols), 
        selected = c("none", non_numeric_cols)[1]
        ),
      # Reductions menu
      selectInput(
        inputId = ns("reduction"),
        label = "Choose Reduction",
        choices = 
          cellDIVER:::reduction_names(
            object
          )
        ),
      # Label groups
      checkboxInput(
        inputId = ns("label"),
        label = "Label Groups",
        # Default value is TRUE
        value = TRUE
        ),
      # Include legend
      checkboxInput(
        inputId = ns("legend"),
        label = "Show Legend",
        # Default value is TRUE
        value = TRUE
        ),
      # ncol slider (dynamic UI)
      hidden(
        sliderInput(
          inputId = ns("ncol"),
          label = "Number of Columns: ",
          # Min/max values will be set dynamically in the server
          min = 1,
          max = 2,
          # Only allow integer values
          step = 1,
          ticks = FALSE,
          value = 1
          )
        )
      )
    }

#' preview_dimplot_server
#'
#' @param id ID to use for module server instance.
#' @param object a Seurat object.
#' 
#' @noRd
#'
preview_dimplot_server <- 
  function(
    id,
    object
    ){
    moduleServer(
      id,
      function(input, output, session){
        
        # 1. Dynamic UI for ncol slider ####
        observe({
          target_id <- "ncol"
          
          if (input$split_by != "none"){
            # If split by is not equal to "none", show the slider and adjust 
            # to match the number of columns in the split by setting
            showElement(
              id = target_id
              )
            
            # Define min, max, and default values for slider
            ncol_settings <-
              ncol_settings(
                object = object,
                rule = "split_by",
                split_by = input$split_by
              )
            
            # Update slider using values defined above
            updateSliderInput(
              session = session,
              inputId = "ncol",
              min = as.numeric(ncol_settings[1]),
              max = as.numeric(ncol_settings[2]),
              value = as.numeric(ncol_settings[3]),
              step = 1
              )
            
          } else {
            # If split by is changed to none, hide the slider
            hideElement(
              id = target_id
              )
            }
        })
        
        # 2. Disable/reset label checkbox when group_by == split_by ####
        # Mirrors the equivalent guard in module-plot_module.R (see #309):
        # Seurat::LabelClusters errors when asked to label a DimPlot whose
        # group_by and split_by are the same variable. Disabling alone does
        # not clear an existing TRUE value, so the value is also forced off.
        observe({
          req(input$group_by)
          req(input$split_by)

          if (input$group_by == input$split_by){
            shinyjs::disable(id = "label")

            updateCheckboxInput(
              session,
              inputId = "label",
              value = FALSE
              )
          } else {
            shinyjs::enable(id = "label")

            # Do not modify the value of the checkbox in other cases
            # (otherwise the checkbox would be reset to TRUE each time
            # the user changes group_by and split_by)
          }
        })

        # 3. Load plot selections from file ####
        observeEvent(
          session$userData$config(),
          {
            if (isTruthy(session$userData$config()$preview)){
              if (session$userData$config()$preview$type == "dimplot"){
                preview_plot_settings <- 
                  session$userData$config()$preview$plot_settings
                
                if (isTruthy(preview_plot_settings$group_by)){
                  updateSelectInput(
                    session = session,
                    inputId = "group_by",
                    selected = preview_plot_settings$group_by
                  )
                }
                
                if (isTruthy(preview_plot_settings$split_by)){
                  updateSelectInput(
                    session = session,
                    inputId = "split_by",
                    selected = preview_plot_settings$split_by
                  )
                }
                
                if (isTruthy(preview_plot_settings$reduction)){
                  updateSelectInput(
                    session = session,
                    inputId = "reduction",
                    selected = preview_plot_settings$reduction
                  )
                }
                
                if (preview_plot_settings$split_by != "none"){
                  # ncol slider
                  # Must also fill min/max slider settings
                  ncol_settings <-
                    ncol_settings(
                      object = object,
                      rule = "split_by",
                      split_by = preview_plot_settings$split_by
                    )
                  
                  updateSliderInput(
                    session = session,
                    inputId = "ncol",
                    min = as.numeric(ncol_settings[1]),
                    value = preview_plot_settings$ncol,
                    max = as.numeric(ncol_settings[2]),
                    step = 1
                    )
                }
                
                if (!is.null(preview_plot_settings$label)){
                  updateCheckboxInput(
                    session = session,
                    inputId = "label",
                    value = preview_plot_settings$label
                  )
                }
                
                if (!is.null(preview_plot_settings$legend)){
                  updateCheckboxInput(
                    session = session,
                    inputId = "legend",
                    value = preview_plot_settings$legend
                  )
                }
              }
            }
        })
        
        # 4. return selected options ####
        list(
          `group_by` = reactive({input$group_by}),
          `split_by` = reactive({input$split_by}),
          `reduction` = reactive({input$reduction}),
          `ncol` = reactive({input$ncol}),
          `label` = reactive({input$label}),
          `legend` = reactive({input$legend})
          )
        })
}
