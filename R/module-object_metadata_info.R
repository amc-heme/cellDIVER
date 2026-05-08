#' object_contents_info UI
#'
#' Displays information on object contents.
#'
#' @noRd
object_contents_info_ui <- 
  function(
    id
    ){
    ns <- NS(id)
  
    div(
      id = ns("object_contents_info_page"),
      # Assay information
      div(
        class = "contents-info-card",
        tags$h2("Assays"),
        shinyWidgets::pickerInput(
          inputId =  ns("assay"),
          label = "Select an Assay:",
          choices = NULL,
          selected = character(0),
          multiple = FALSE,
          options =
            pickerOptions(
              placeholder = "Loading, please wait...",
              dropupAuto = FALSE
            )
          ),
        hidden(
          div(
            id = ns("assay_info"),
            div(
              tags$b("Description: "),
              textOutput(
                outputId = ns("assay_description"),
                inline = TRUE
              )
            ),
            div(
              tags$b("Number of features: "),
              textOutput(
                outputId = ns("assay_n_features"),
                inline = TRUE
              )
            )
          )
        )
      ),
      # Metadata information
      div(
        class = "contents-info-card",
        tags$h2("Metadata"),
        shinyWidgets::pickerInput(
          inputId =  ns("metadata_var"),
          label = "Select a metadata variable:",
          choices = NULL,
          selected = character(0),
          multiple = FALSE,
          options =
            pickerOptions(
              placeholder = "Loading, please wait...",
              dropupAuto = FALSE
              )
          ),
        hidden(
          div(
            id = ns("metadata_var_info"),
            # Type of metadata variable
            div(
              tags$b("Type: "),
              textOutput(
                outputId = ns("var_type"),
                inline = TRUE
              )
            ),
            # Description of metadata variable (if provided)
            div(
              # This should go in the var_description output
              uiOutput(
                outputId = ns("var_description")
              )
            ),
            # Summary of variable 
            # Either a description of unique values (for categorical variables)
            # or summary statistics (numeric variables)
            uiOutput(
              outputId = ns("var_summary")
            )
          )
        )
        ),
      # Reductions
      div(
        class = "contents-info-card",
        tags$h2("Reductions"),
        shinyWidgets::pickerInput(
          inputId =  ns("reduction"),
          label = "Select a reduction:",
          choices = NULL,
          selected = character(0),
          multiple = FALSE,
          options =
            pickerOptions(
              placeholder = "Loading, please wait...",
              dropupAuto = FALSE
            )
        ),
        hidden(
          div(
            id = ns("reduction_info"),
            div(
              tags$b("Description: "),
              textOutput(
                outputId = ns("reduction_description"),
                inline = TRUE
                )
              )
            )
          )
        )
      )
    }

#' object_contents_info Server
#' 
#' Displays information on object contents.
#'
#' @param id ID to use for module. This must match the id provided to the ui
#' instance of the module for the module to function properly.
#' @param object A single-cell object.
#' @param meta_choices A named vector generated in run_cellDIVER with each
#' metadata variable included in the config file. This vector is used when 
#' generating the choices displayed in the metadata information. 
#' @param metadata_config The metadata section of the config file. This is 
#' loaded in the main server function at startup and when the 
#' object is changed.
#' @param include_numeric_metadata The include_numeric_metadata section of the 
#' config file. Used to determine if the app admin has enabled numeric metadata.
#' The description for numeric variables will vary based on this setting. 
#' @param assay_config The assays section of the config file. 
#' This is loaded in the main server function at startup and when the 
#' object is changed.
#' @param reduction_config The reductions section of the config file. This is 
#' loaded in the main server function at startup and when the 
#' object is changed.
#' @param is_seurat Whether an object is a Seurat object. If so, the 
#' descriptions for variables not included in the config app will mention 
#' that advanced code-based subsetting is possible.
#' 
#' @noRd
object_contents_info_server <- 
  function(
    id,
    object,
    meta_choices,
    metadata_config,
    include_numeric_metadata,
    assay_config,
    reduction_config,
    is_seurat
    ){
    moduleServer(
      id,
      function(input, output, session){
        # Server namespace function 
        ns <- session$ns

        # 1. Populate search menu with list of metadata options ####
        observe(
          label = paste0(id, ": Populate metadata list"),
          {
            # Fetch full metadata table 
            meta_table <-
              SCUBA::fetch_metadata(
                object = isolate(object()),
                full_table = TRUE
                )
            
            # Choices: display "featured" metadata from config file first.
            choices <- meta_choices()
            # remove first option ("none") from meta_choices
            choices <- choices[2:length(choices)]
            
            # Add numeric metadata variables to choices 
            # if enabled in the config app 
            if (include_numeric_metadata()){
              remaining_variables <-
                colnames(meta_table)[!colnames(meta_table) %in% meta_choices()]
              
              is_numeric <-
                sapply(
                  remaining_variables,
                  function(x){
                    class(meta_table[[x]]) %in% c("numeric", "integer")
                  }
                )
              
              numeric_variables <- 
                remaining_variables[is_numeric]
              
              # Remaining numeric variables do not have a display name. 
              # The names of the remaining variables are set as they appear 
              # in the object (downstream code requires display names for all
              # choices)
              names(numeric_variables) <-
                numeric_variables
              
              choices <- 
                c(choices,
                  numeric_variables)
              }
            
            # Additional information for each variable
            # Get the class of the variable, and the number of 
            # unique values or levels, if applicable.
            # This will be displayed beneath each variable in the selection 
            # menu
            var_info_text <-
              sapply(
                choices,
                function(meta_var){
                  var_class <- class(meta_table[,meta_var])
                  
                  # If character or factor, also fetch number of 
                  # unique values (character) or levels (factor)
                  additional_info <- ""
                  if (var_class == "character"){
                    n_unique <-
                      meta_table[,meta_var] |> 
                      unique() |> 
                      length()
                    
                    additional_info <-
                      # Use singular "unique value" if only one 
                      # value is present
                      if (n_unique == 1){
                        paste0("1 unique value")
                      } else {
                        paste0(n_unique, " unique values")
                      }
                  } else if (var_class == "factor"){
                    n_levels <-
                      meta_table[,meta_var] |> 
                      levels() |> 
                      length()
                    
                    additional_info <-
                      paste0(n_levels, " levels")
                  }
                  
                  # Text to return: class plus number of unique values/
                  # levels, if applicable
                  paste0(
                    toTitleCase(var_class),
                    # If applicable, add additional info with a space and comma
                    if (isTruthy(additional_info)){
                      paste0(", ", additional_info)
                      }
                    )
                })
            
            # Generate HTML for each choice
            # Variable display name, plus additional 
            # information computed above
            choice_html <-
              sapply(
                1:length(var_info_text),
                function(i){
                  # The names of the variable choices (display names) are 
                  # preserved when computing the info text in the above sapply
                  # operation
                  var_display_name <- names(var_info_text)[i]
                  
                  # doRenderTags converts tags from ShinyTag format to 
                  # character, so they can be properly rendered by the 
                  # pickerInput.
                  htmltools::doRenderTags(
                    tags$div(
                      # First line: display name of variable
                      tags$div(
                        var_display_name,
                        class = "bold"
                        ),
                      # Second line: additional information 
                      tags$div(
                        var_info_text[i],
                        class = "gray"
                        )
                      )
                    )
                  }
                )
            
            # updateSelectizeInput(
            #   inputId = "metadata_var",
            #   choices = choices,
            #   selected = character(0),
            #   server = TRUE,
            #   options = list(
            #     "placeholder" = "Enter a variable"
            #     )
            #   )
            
            # When numeric metadata is present, display choices grouped by
            # categorical vs numeric variables 
            if (include_numeric_metadata()){
              choices <- 
                list(
                  `Categorical Metatata` = 
                    choices[!choices %in% numeric_variables],
                  `Numeric Metadata` =
                    choices[choices %in% numeric_variables]
                )
            }
            
            updatePickerInput(
              inputId = "metadata_var",
              choices = choices,
              choicesOpt = 
                list(
                  # Display the computed HTML above for variables 
                  # instead of variable names
                  `content` = choice_html
                  ),
              options = pickerOptions(
                placeholder = "Enter a variable",
                liveSearch = TRUE,
                liveSearchPlaceholder = "Search",
                showContent = FALSE
                )
              )
            })
        
        # 2. Assays ####
        ## 2.1. Populate menu with available choices ####
        observe(
          label = paste0(id, ": Populate assay list"),
          {
            req(assay_config())
            
            # Form assay choices
            # The names of assay_config() are the machine readable names for
            # each assay exposed in the config file
            assay_choices <- names(assay_config())
            
            # Display names
            assay_names <- 
              sapply(
                names(assay_config()),
                function(assay){
                  if (!is.null(assay_config()[[assay]]$dropdown_title)){
                    # Use display name (dropdown_title) from config file 
                    # if defined
                    assay_config()[[assay]]$dropdown_title
                  } else {
                    # Otherwise, use the name of the assay as 
                    # it appears in the object
                    assay
                  }
                }
              )
            
            names(assay_choices) <- assay_names
            
            updatePickerInput(
              inputId = "assay",
              choices = assay_choices,
              options = pickerOptions(
                placeholder = "Enter an assay",
                liveSearch = TRUE,
                liveSearchPlaceholder = "Search",
                showContent = FALSE
                )
              )
            })
        
        ## 2.2. Show/hide assay information ####
        observe(
          label = paste0(id, ": show/hide summary"),
          {
            target_id <- "assay_info"
            
            if (isTruthy(input$assay)){
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
        
        ## 2.3. Display assay description ####
        output$assay_description <-
          renderText({
            req(input$assay)
            
            if (!is.null(assay_config()[[input$assay]]$description)){
              assay_config()[[input$assay]]$description
            } else {
              "No description has been provided for this assay."
            }
          })
        
        # Compute even when hidden, so user can see results for first 
        # assay without a lag
        outputOptions(
          output,
          "assay_description",
          suspendWhenHidden = FALSE
          )
        
        ## 2.4. Display number of features in assay ####
        output$assay_n_features <-
          renderText({
            req(input$assay)
            
            # Number of features: number of entries 
            # returned by SCUBA::features_in_assay
            SCUBA::features_in_assay(
              object = object(), 
              assay = input$assay
              ) |> 
              length()
          })
        
        # Compute even when hidden, so user can see results for 
        # first assay without a lag
        outputOptions(
          output,
          "assay_n_features",
          suspendWhenHidden = FALSE
          )
        
        # 3. Metadata ####
        ## 3.1. Show/hide summary stats window ####
        observe(
          label = paste0(id, ": show/hide summary"),
          {
            target_id <- "metadata_var_info"
            
            if (isTruthy(input$metadata_var)){
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
        
        ## 3.2. Metadata variable summary ####
        ### 3.2.1. Fetch metadata for variable ####
        # (separated into a separate reactive expression so calculation 
        # is only done once when the variable is changed)
        metadata_vector <-
          reactive({
            req(input$metadata_var)
            
            SCUBA::fetch_metadata(
              object = isolate(object()),
              vars = input$metadata_var,
              return_class = "vector"
              )
          })
        
        ### 3.2.2. Type (class) of variable ####
        output$var_type <- 
          renderText({
            toTitleCase(
              class(metadata_vector())
              )
          })
        
        # Compute even when hidden, so user can see results for first 
        # variable without a lag
        outputOptions(
          output,
          "var_type",
          suspendWhenHidden = FALSE
        )
        
        ### 3.2.3. Variable Description ####
        # Description of metadata variable from config file, if provided
        output$var_description <-
          renderUI({
            req(input$metadata_var)
            req(metadata_config())
            req(metadata_vector())
            
            if (input$metadata_var %in% names(metadata_config())){
              # Variables added by the browser admin
              # If the metadata variable is in the config file, check if the
              # variable description is defined
              config_var_description <-
                metadata_config()[[input$metadata_var]]$description
              
              if (isTruthy(config_var_description)){
                div(
                  class = "half-space-top",
                  tags$b("Description: "),
                  config_var_description
                  )
              } else {
                # Variables in the config file, but without a description
                # (currently, these are always character, factor, or logical)
                div(
                  class = "half-space-top",
                  tags$b("Description: "),
                  "No description has been provided for this variable."
                  )
              }
            } else {
              # Variables not added by the app admin
              # May be numeric variables, or categorical/logical 
              # variables not added
              if (class(metadata_vector()) %in% c("numeric", "integer")){
                # Numeric variables: information shown depends on whether the
                # user has enabled numeric metadata in the config app
                if (include_numeric_metadata()){
                  # If so, the variable can be used in plotting and subsetting
                  div(
                    class = "half-space-top",
                    tags$b("Description: "),
                    paste0(
                      'This is a numeric metadata variable. You may plot this ',
                      'variable in the plots tab, or use it in creating ',
                      'subsets by selecting "feature expression" in the ',
                      'subset menu.'
                      )
                    )
                } else {
                  # If not enabled, the only possible interaction is via 
                  # advanced, code based subsetting, in Seurat objects
                  div(
                    class = "half-space-top",
                    tags$b("Description: "),
                    paste0(
                      'This is a numeric metadata variable. The app admin ',
                      'has not allowed access to numeric metadata in this ',
                      'cellDIVER instance, so it is not possible to plot ',
                      'or subset based on this variable. Please contact ',
                      'the app admin for more information.'#,
                      # If the object is a Seurat object, state that advanced
                      # subsetting can be performed (this is very confusing)
                      # if (is_seurat()){
                      #   paste0(
                      #     ' Since the current dataset is a Seurat object, it is ',
                      #     'possible to subset based on this variable, however, ',
                      #     'using advanced, code based subsetting.'
                      #   )
                      # }
                    )
                  )
                }
              } else {
                # Non-numeric variables not added in the config file
                # explain they can be interacted with via advanced subsetting
                # in Seurat objects, but not elsewhere in the app.
                div(
                  class = "half-space-top",
                  tags$b("Description: "),
                  # Add a note about numeric metadata based on whether the admin
                  # enabled or disabled numeric metadata
                  # Can numeric metadata be used in numeric subsetting? Yes, if 
                  # numeric metadata enabled
                  paste0(
                    'This metadata variable is an "extra" variable not ',
                    "enabled by the app admin. It can't be used for plotting ",
                    "or subsetting. Please contact the app admin to learn ",
                    "more about this variable or request it be included."
                    )
                  )
                }
              }
          })
        
        # Compute even when hidden, so user can see results for first 
        # variable without a lag
        outputOptions(
          output,
          "var_description",
          suspendWhenHidden = FALSE
        )
        
        ### 3.2.4. Variable Summary ####
        output$var_summary <-
          renderUI({
            if (class(metadata_vector()) == "character"){
              # Character metadata variables: show unique values 
              div(
                class = "half-space-top",
                tags$b("Unique Values: "),
                paste(
                  unique(metadata_vector()), 
                  collapse = ", "
                  )
                )
            } else if (class(metadata_vector()) == "factor"){
              # Factor metadata variables: show levels
              div(
                class = "half-space-top",
                tags$b("Unique Values: "),
                paste(
                  levels(metadata_vector()), 
                  collapse = ", "
                  )
                )
            } else if (class(metadata_vector()) == "logical"){
              # Boolean (logical) vectors: show number of TRUE, FALSE, NA's
              n_true <- sum(metadata_vector() == TRUE, na.rm = TRUE)
              n_false <- sum(metadata_vector() == FALSE, na.rm = TRUE)
              n_na <- sum(is.na(metadata_vector()), na.rm = TRUE)
              
              div(
                class = "half-space-top",
                # Header
                div(
                  tags$b("Number of cells per value:")
                  ),
                # Summary of TRUE/FALSE/NA values
                div(
                  tags$b("TRUE: "),
                  n_true
                  ),
                div(
                  tags$b("FALSE: "),
                  n_false
                  ),
                div(
                  tags$b("NA: "),
                  n_na
                  )
                )
            } else if (class(metadata_vector()) %in% c("numeric", "integer")){
              # Numeric variables: show summary statistics
              # Could also show a ridge plot?
              summary_stats <- summary(metadata_vector())
              
              div(
                class = "half-space-top",
                div(
                  tags$b("Summary statistics")
                  ),
                div(
                  tags$b("Min:"),
                  summary_stats[1]
                  ),
                div(
                  tags$b("Q1:"),
                  summary_stats[2]
                  ),
                div(
                  tags$b("Median:"),
                  summary_stats[3]
                  ),
                div(
                  tags$b("Mean:"),
                  summary_stats[4]
                  ),
                div(
                  tags$b("Q3:"),
                  summary_stats[5]
                  ),
                div(
                  tags$b("Max:"),
                  summary_stats[6]
                  )
                )
              }
            })
        
        # Compute even when hidden, so user can see results for first 
        # variable without a lag
        outputOptions(
          output,
          "var_summary",
          suspendWhenHidden = FALSE
          )
        
        # 4. Reductions ####
        ## 4.1. Populate menu with available choices ####
        observe(
          label = paste0(id, ": Populate reduction list"),
          {
            req(reduction_config())
            
            # Form reduction choices
            # The names of reduction config are the machine readable names for
            # each reduction in the object
            reduction_choices <- names(reduction_config())
            
            # Display names
            reduction_names <- 
              sapply(
                names(reduction_config()),
                function(reduction){
                  if (!is.null(reduction_config()[[reduction]]$label)){
                    # Use display name (label) from config file if defined
                    reduction_config()[[reduction]]$label
                  } else {
                    # Otherwise, use the name of the reduction as 
                    # it appears in the object
                    reduction
                  }
                }
              )
            
            names(reduction_choices) <- reduction_names
            
            updatePickerInput(
              inputId = "reduction",
              choices = reduction_choices,
              options = pickerOptions(
                placeholder = "Enter a reduction",
                liveSearch = TRUE,
                liveSearchPlaceholder = "Search",
                showContent = FALSE
              )
            )
          })
        
        ## 4.2. Show/hide reduction information ####
        observe(
          label = paste0(id, ": show/hide reduction information"),
          {
            target_id <- "reduction_info"
            
            if (isTruthy(input$reduction)){
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
        
        ## 2.3. Display reduction description ####
        output$reduction_description <-
          renderText({
            req(input$reduction)
            
            if (!is.null(reduction_config()[[input$reduction]]$description)){
              reduction_config()[[input$reduction]]$description
            } else {
              "No description has been provided for this reduction."
            }
          })
        
        # Compute even when hidden, so user can see results for first 
        # reduction without a lag
        outputOptions(
          output,
          "reduction_description",
          suspendWhenHidden = FALSE
        )
      }
    )
  }
