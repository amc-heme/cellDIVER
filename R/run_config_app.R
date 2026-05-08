#' scExploreR config app
#'
#' run_config() will launch a Shiny app used to configure datasets for use in
#' the main browser.
#'
#' @param object_path path to a single-cell object to be configured. Currently, 
#' Seurat, SingleCellExperiment, and anndata objects are supported. For 
#' SingleCellExperiment objects with using HDF5 disk-backed storage via 
#' [HDF5Array], `object_path` 
#' should be a path to the directory containing the se.rds and assays.h5 files 
#' for the object.
#' @param config_path optional: if provided, the data from this file will be
#' loaded when the user selects "load config file" in the config app. This
#' should be a YAML file, though .rds files from versions 0.4.0 and earlier will
#' also be accepted.
#' @param is_HDF5SummarizedExperiment Set this to TRUE to load an HDF5-enabled
#' SingleCellExperiment object saved via 
#' [HDF5Array::saveHDF5SummarizedExperiment]. When
#' loading an HDF5-enabled object, set the object_path to the directory of the
#' HDF5-enabled object, created when saving the object via
#' `HDF5Array:saveHDF5SummarizedExperiment`.
#' @param HDF5_prefix When loading an HDF5-backed SingleCellExperiment HDF5_prefix
#' is passed to the prefix parameter of [HDF5Array::loadHDF5SummarizedExperiment] 
#' to specify the prefixes for the se.rds and assays.h5 files. 
#' @param dev_mode Used only for development. If TRUE, the server values for each option chosen by the user will be printed at the bottom of the "general" tab.
#'
#'
#' @export
run_config <-
  function(
    object_path,
    config_path = NULL,
    is_HDF5SummarizedExperiment = FALSE,
    HDF5_prefix = "",
    dev_mode = FALSE
  ){
    # Initialize libraries ####
    library(shiny)
    library(Seurat)

    # Shiny add-ons
    library(shinyWidgets)
    library(rintrojs)
    library(shinydashboard)
    library(waiter)
    # shinycssloaders: withSpinner() is called bare (without ::) throughout
    # the UI after this library() call attaches it to the search path.
    library(shinycssloaders)
    library(shinyjs)
    # library(shinyFeedback)
    # Sortable.JS: Creates a drag-and-drop menu
    library(sortable)
    # ShinyBS tooltips
    library(shinyBS, quietly = TRUE, warn.conflicts = FALSE)

    # Reactlog (for debugging): optional dev dependency listed in Suggests.
    # Enables the reactive graph visualizer at /reactlog when the app is running.
    if (requireNamespace("reactlog", quietly = TRUE)) {
      library(reactlog)
    }
    options(shiny.reactlog=TRUE)

    # Tidyverse Packages
    # library(tidyverse)
    library(stringr)
    library(dplyr)
    library(ggplot2)
    library(glue)
    library(DT)

    library(yaml)
    # Load functions in ./R directory ####
    # Get list of files
    # source_files <-
    #   list.files(
    #     path = "./R",
    #     pattern="*.R$",
    #     full.names=TRUE,
    #     ignore.case=TRUE
    #   )
    #
    # # Load modules from ./Config_App_Modules
    # source_files <-
    #   c(source_files,
    #     list.files(
    #       path = "./Config_App_Modules",
    #       pattern="*.R$",
    #       full.names=TRUE,
    #       ignore.case=TRUE
    #     )
    #   )

    # Use source() to import files into R
    # sapply(
    #   source_files,
    #   source
    # )

    # Load CSS files for app: CSS files are defined and each file is converted
    # to a <script> tag using includeCSS(). Each tag defined is passed to a
    # list, which is included in the main UI function.
    # Get list of .css files in www/ directory
    css_files <-
      list.files(
        path = system.file("css", package = "scExploreR"),
        pattern = "*.css$",
        full.names = TRUE,
        ignore.case = TRUE
      )

    # Create list of style tags for each CSS file
    css_list <-
      lapply(
        css_files,
        includeCSS
      )

    # Load Javascript files for app: find all .js files that apply to the applet
    # and create a list of script() tags using includeScript().
    # Files to include: all files in www/applet_js/ directory, and the
    # collapsible_panel.js file in the www/ directory (www/button_wizzard.js
    # must be excluded since it conflicts with 'applet_navbar_wizzard' in the
    # www/applet_js/ directory)
    js_files <-
      list.files(
        path = system.file("js", package = "scExploreR", mustWork = TRUE),
        # Use regex to search for files ending in .js (double
        # backslash used to escape '.' character)
        pattern = ".*\\.js",
        full.names = TRUE,
        ignore.case = TRUE
      )
    
    # Remove the "button wizzard" script used in the main app
    js_files <- js_files[!grepl("js/button_wizzard.js", js_files)]
    
    # Add applet_navbar_wizzard JavaScript
    js_files <- 
      c(js_files, 
        system.file(
          "js", 
          "applet_js", 
          "applet_navbar_wizzard.js", 
          package = "scExploreR", 
          mustWork = TRUE
          )
        )

    # Create list of style tags for each CSS file
    js_list <- lapply(js_files, includeScript)

    # JavaScript Functions ####
    # The raw Javascript for each function is defined here as text, and each
    # function is wrapped in shinyjs::extendShinyjs() in the main UI function
    # for use with Shiny.

    # insertElemAfter: the UI defined by elem_id is repositioned after the UI
    # defined by destination_id.
    insertAfterjs <-
      'shinyjs.insertElemAfter =
      function(params){
        var defaultParams = {
          elem_id: null,
          destination_id: null
        };
        params = shinyjs.getParams(params, defaultParams);

        // concatenate "#" with the element IDs provided
        $("#" + params.elem_id).insertAfter("#" + params.destination_id);
      }'

    # Load object ####
    # object_path and config_path are specified using run_config
    print("Loading object...")
    # Separate loading functions are needed for different object types
    # SCE objects with DelayedArray assays: path is a directory, not a file
    if (is_HDF5SummarizedExperiment == TRUE){
      # Directory is loaded via loadHDF5SummarizedExperiment
      object <- HDF5Array::loadHDF5SummarizedExperiment(object_path, prefix = HDF5_prefix)
    } else {
      # All other formats: choose loading function based on extension
      extension <- tools::file_ext(object_path)
      
      if (tolower(extension) == "rds"){
        object <- readRDS(object_path)
      } else if (tolower(extension) == "h5ad"){
        # Reticulate should not be loaded unless anndata objects are used
        # (so users that don't have anndata objects won't need to install it
        # and set up a Python environment)
        library(reticulate)
        library(anndata)
        object <- anndata::read_h5ad(object_path)
      } else if (extension == "h5mu"){
        py_require("mudata>=0.3.1")
        
        md <- reticulate::import("mudata", as = "md", convert = TRUE)
        
        object <- md$read_h5mu(object_path)
      } else if (extension == ""){
        stop(
          "No file extension detected for object at path",
          object_path,
          ". Please provide a path to an object with an extension of ",
          ".rds or .h5ad."
          )
      } else {
        stop(
          "Unrecognized file extension (.",
          extension,
          "). Currently supported extensions: .rds, and .h5ad."
          )
        }
    }

    # Test if the loaded object is of a supported class; if not, return an error
    check_dataset(
      object,
      path = object_path,
      return_error = TRUE
      )

    # Define Config file path for loading ####
    config_filename <- config_path

    # Version of config app ####
    # Printed in config file. Will be used to alert user if they are using a
    # config file that is not compatible with the current version of the main app
    config_version <-
      packageVersion("scExploreR") |>
      as.character()

    # Identify numeric metadata variables
    # Pull full metadata table first, then test the class of each variable
    meta_table <-
      SCUBA::fetch_metadata(
        object,
        full_table = TRUE
        )
    # Identify variables in table
    meta_vars <- colnames(meta_table)

    # is_numeric: a vector of boolean values used to subset meta_columns
    # for numeric metadata
    is_numeric <-
      sapply(
        meta_vars,
        function(x, object){
          class(meta_table[[x]]) %in% c("numeric", "integer")
        },
        object
        )
    
    numeric_cols <- meta_vars[is_numeric]
    non_numeric_cols <- meta_vars[!is_numeric]
    

    # Assays, reductions in object
    all_assays <-
      scExploreR:::assay_names(
        object
      )

    reductions <-
      scExploreR:::reduction_names(
        object
      )

    # Main UI and Server Functions ####
    # 1. Tabs in Main UI ####
    ## 1.1. General dataset info tab ####
    general_info_tab <- function(){
      div(
        # Structure: one centered column with fields shown from top to bottom
        class = "single-column-page",
        tags$h3("General Dataset Info"),
        tags$p("Options entered here will display in the dataset preview
               window in the main browser."),
        textInput(
          inputId = "dataset_label",
          label = "Label for Dataset",
          value = ""
        ),
        textAreaInput(
          inputId = "dataset_description",
          label = "Description of Dataset",
          width = "100%",
          rows = 6,
          resize = "vertical"
        ),
        selectInput(
          inputId = "preview_type",
          label = "Content for Dataset Preview",
          choices =
            c(
              "None" = "none",
              "DimPlot" = "dimplot"#,
              #"Image" = "image"
              ),
          selected = "none"
        ),
        hidden(
          div(
            id = "preview_dimplot_interface",
            div(
              class = "compact-options-container",
              style = "margin-bottom: 150px;",
              tags$h3(
                "Dimplot Preview",
                class = "container-header"
                ),
              div(
                class = "two-column-container",
                div(
                  class = "two-column",
                  style =
                    "float: left; width: 45%; padding: 10px;",
                  # Display a limited set of settings for the dimplot
                  preview_dimplot_ui(
                    id = "dimplot",
                    object = object,
                    non_numeric_cols = non_numeric_cols
                    )
                  ),
                div(
                  class = "two-column",
                  style =
                    "float: right; width: 55%; padding: 10px;",
                  div(
                    class = "center",
                    # tags$b(
                    #   "Preview"
                    # ),
                    tags$p(
                      "(The plot will display as below in the dataset preview window)",
                      style = "font-size: 0.9em;"
                    )
                  ),
                  plotOutput(
                    outputId = "preview_dimplot",
                    height = "auto"
                    )
                  )
                )
              )
            ),
          div(
            id = "preview_image_interface",
            style = "margin-bottom: 150px;",
            div(
              class = "compact-options-container",
              style =
                "min-height: 400px;
                width: 70%;
                margin-left: auto;
                margin-right: auto;",
              tags$h3(
                "Image Preview",
                class = "container-header"
                ),
              div(
                style =
                  "
                  margin-top: 20px;
                  margin-bottom: 20px;
                  width: 80%;
                  margin-left: auto;
                  margin-right: auto;
                  ",
                fileInput(
                  inputId = "image_upload",
                  label = "Choose path for image",
                  accept = "image/*"
                ),
                div(
                  class = "center",
                  tags$p(
                    "(The image will display as below in the dataset preview window)",
                    style = "font-size: 0.9em;"
                  )
                ),
                imageOutput(
                  outputId = "preview_image",
                  height = "auto"
                )
              )
            )#,
          )
        ),
        # Window to show all options selected in app, if app is launched in
        # dev mode
        if (dev_mode == TRUE){
          div(
            class = "optcard",
            tags$h4(
              "Current User Input",
              class = "center"
              ),
            verbatimTextOutput(
              outputId = "print_data"
            )
          )
        }
      )
    }

    ## 1.2. Assays Tab ####
    # (not the assay options module)
    assay_tab <- function(){
      bslib::layout_sidebar(
        sidebar = 
          bslib::sidebar(
            width = "50%",
            bg = "#E1E1E1",
            open = TRUE,
            class = "config-bslib-sidebar",
            # Sidebar content 
            # (everything passed to the "..." parameter of sidebar() is 
            # displayed in the sidebar)
            div(
              class = "input-no-margin",
              multiInput(
                inputId = "assays_selected",
                label = "Choose assays to include:",
                width = "100%",
                choices = all_assays,
                options =
                  list(
                    enable_search = FALSE,
                    non_selected_header = "Available Assays",
                    selected_header = "Selected Assays",
                    "hide_empty_groups" = TRUE
                  )
                )
              ) # End multiInput
            ),
        # Main panel 
        # Content below is passed to the "..." parameter of layout_sidebar,
        # to create the main panel
        tagList(
          # A "card" with assay-specific options is displayed for each
          # selected assay
          # Generic assay options (options that apply to all assays)
          div(
            class = "optcard single-space-bottom",
            tags$strong(
              glue("General Options"),
              class="large center"
            ),
            # Select metadata column to use for patient/sample
            # level metadata analysis
            selectInput(
              inputId = "genes_assay",
              label =
                "Choose genes assay (for DGE and correlation analyses)",
              # Only "none" available at first (can only select assays that
              # are included by the user). Updated server-side
              choices = "none",
              selected = "none",
              width = "380px"
            ),
            
            selectInput(
              inputId = "adt_assay",
              label =
                "Choose ADT assay",
              choices = "none",
              selected = "none",
              width = "380px"
            ),
          ),
          
          # Create an instance of the assay options UI for all possible
          # assays. Each UI creates a "card"; all are hidden at first and are
          # shown when their corresponding assay is selected by the user. The
          # "id" argument in lapply is the name of the assay.
          lapply(
            all_assays,
            function(assay){
              options_ui(
                id = assay,
                object = object,
                optcard_type = "assays"
                )
              }
            )
          )
        )
    }

    ## 1.3. Metadata Tab ####
    metadata_tab <-
      function(){
        bslib::layout_sidebar(
          sidebar = 
            bslib::sidebar(
              width = "66%",
              bg = "#E1E1E1",
              open = TRUE,
              # Sidebar content 
              # (everything passed to the "..." parameter of sidebar() is 
              # displayed in the sidebar)
              div(
                class = "input-no-margin",
                uiOutput(
                  outputId = "metadata_sortable_bucket"
                  )
                ),
                # Print server values of metadata selected if app is
                # launched in dev_mode
                if (dev_mode){
                  verbatimTextOutput(
                    outputId = "metadata_sortable_debug"
                  )
                }
              ),
          # Main panel 
          # Content below is passed to the "..." parameter of layout_sidebar,
          # to create the main panel UI
          tagList(
            # Card for generic metadata options
            div(
              class = "optcard single-space-bottom",
              tags$strong(
                glue("General Options"),
                class="large center"
              ),
              # Select metadata variable to use for patient/sample
              # level metadata analysis
              selectInput(
                inputId = "patient_colname",
                label =
                  "Patient/Sample Metadata Variable (optional, used
                    for patient-level metadata analysis)",
                # Can select "none" or any categorical metadata column
                choices = c("none", non_numeric_cols),
                selected = NULL,
                width = "380px"
              ),
              # Include numeric metadata for plotting
              awesomeCheckbox(
                inputId = "include_numeric_metadata",
                label = "Include numeric metadata for plotting",
                value = TRUE,
                # Applies standard Bootstrap classes to style of checkbox
                status = "info"
              )
            ),
            
            # Options specific to each metadata variable
            
            # Options for Numeric metadata (Numeric metadata is currently not
            # displayed)
            
            # Options for Categorical, logical metadata
            # Create a metadata options "card" for each non-numeric metadata
            # column in the object. Cards below are hidden and display when
            # the corresponding metadata category is selected
            lapply(
              non_numeric_cols,
              function(colname){
                hidden(
                  options_ui(
                    id = colname,
                    object = object,
                    optcard_type = "metadata"
                  )
                )
              }
            )#,
            # TEMP: add an additional card displaying the outputs
            # from the metadata tab
            # div(
            #   class = "optcard",
            #   verbatimTextOutput(outputId = "print_metadata")
            # )
          )
        )
      }

    ## 1.4. reductions Tab ####
    reductions_tab <-
      function(){
        bslib::layout_sidebar(
          sidebar = 
            bslib::sidebar(
              width = "66%",
              bg = "#E1E1E1",
              open = TRUE,
              class = "config-bslib-sidebar",
              # Sidebar content 
              # (everything passed to the "..." parameter of sidebar() is 
              # displayed in the sidebar)
              div(
                class = "input-no-margin",
                uiOutput(
                  outputId = "reductions_sortable_bucket"
                )
              )
            ),
          # Main panel 
          # Content below is passed to the "..." parameter of layout_sidebar,
          # to create the main panel
          tagList(
            # Card for generic options (taken from metadata tab; unused)
            # div(
            #   class = "optcard single-space-bottom",
            #   tags$strong(
            #     glue("General Options"),
            #     class="large center"
            #   ),
            #   # Select metadata column to use for patient/sample level metadata
            #   # analysis
            #   selectInput(
            #     inputId = "patient_colname",
            #     label =
            #       "Patient ID column (optional, used for patient-level
            #       metadata analysis)",
            #     # Can select "none" or any categorical metadata column
            #     choices = c("none", non_numeric_cols),
            #     selected = NULL,
            #     width = "380px"
            #   )
            # ),
            
            # Create an options "card" for each reduction included (dynamic UI)
            lapply(
              reductions,
              function(reduction_name){
                options_ui(
                  id = reduction_name,
                  object = object,
                  optcard_type = "reductions"
                )
              }
            )#,
            # TEMP: add an additional card displaying the outputs
            # from the current tab
            # div(
            #   class = "optcard",
            #   verbatimTextOutput(
            #     outputId = "print_reductions"
            #   )
            # )
          )
        )
      }

    ## 1.5. ADT Threshold Tab ####
    threshold_tab <-
      function(){
        sidebarLayout(
          position = "right",
          sidebarPanel =
            sidebarPanel(
              id = "adt_threshold_sidebar",
              style = "height: 85vh; margin-bottom: 0px;",
              width = 5,
              # Elements in sidebar display conditionally based on which function
              # the user is performing on threshold data (add new threshold data,
              # edit an existing threshold, or none of the above)
              hidden(
                # Header of options bar: depends on whether the state is "add"
                # or "edit"
                div(
                  # show-on-add: element displays when user is adding a new ADT
                  class = "show-on-add",
                  tags$h4("Add ADT Threshold"),
                ),
                div(
                  class = "show-on-edit",
                  # Header to display when editing feature
                  uiOutput(
                    outputId = "threshold_header"
                  ),
                ),
                # Menu to choose an ADT to add a threshold for
                div(
                  class = "show-on-add",
                  tags$b("Enter an ADT threshold below to add to table:"),
                  selectizeInput(
                    inputId = "selected_adt",
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
                  )
                ),
                # UI for selecting threshold: displays when a feature is entered
                # above, or when an existing threshold is being edited.
                div(
                  id = "threshold_picker_div",
                  threshold_picker_ui(
                    id = "threshold_picker",
                    plot_height = "15em"
                  )
                ),
                # Buttons to accept or discard threshold
                div(
                  class = "show-on-add show-on-edit half-space-top",
                  # Accept button: disabled at first; enabled when a feature
                  # threshold has been selected using the interactive ridge plot
                  disabled(
                    actionButton(
                      inputId = "accept_threshold",
                      class = "button-primary float-right",
                      style = "margin-left: 10px;",
                      label = "Confirm"
                    )
                  ),
                  # Cancel button: discards feature selection and threshold, and
                  # returns menus to "idle" state
                  actionButton(
                    inputId = "cancel_threshold",
                    class = "button-ghost float-right",
                    label = "Cancel"
                  )
                )
              )
            ),
          mainPanel =
            mainPanel(
              id = "adt_threshold_main",
              width = 7,
              tags$h3(
                "Defined ADT Thresholds",
                class = "Center"
              ),

              # Table of thresholds
              # Placeholder for now
              DTOutput(
                outputId = "threshold_table"
              ),

              # Button to add a new threshold
              div(
                class = "half-space-top",
                actionButton(
                  inputId = "add_threshold",
                  label = "New Threshold",
                  class = "button-primary",
                  style = "float: right;"
                )
              ),
              # JavaScript for inline edit/delete buttons on table
              # This is rendered as HTML when changes are made to the datatable
              uiOutput(
                outputId = "threshold_table_button_script"
              )
            )
        )
      }

    ## 2. Main UI ####
    ui <- fluidPage(
      # Waiter UI: spinners
      useWaiter(),
      # Shinyjs: a Shiny JavaScript extension
      useShinyjs(),
      # Initialize custom JavaScript functions defined after the source() statements
      extendShinyjs(text = insertAfterjs, functions = "insertElemAfter"),
      # Main UI
      navbarPage(
        title = "Object Configuration",
        windowTitle = "Configure Seurat Object",
        position = "fixed-top",
        id = "navbar",
        #Tabs are displayed below
        tabPanel(
          title = "General",
          general_info_tab()
        ),
        tabPanel(
          title = "Assays",
          assay_tab()
        ),
        tabPanel(
          title = "Metadata",
          metadata_tab()
        ),
        tabPanel(
          title = "Reductions",
          reductions_tab()
        ),
        tabPanel(
          title = "ADT Threshold",
          threshold_tab()
        ),
        bslib::nav_spacer()
      ),

      # Elements below will be moved to the navbar using JavaScript
      # Help button
      dropdownButton(
        inputId = "help",
        status = "info",
        right = TRUE,
        label = "",
        size = "sm",
        icon = icon("question"),
        # Dropdown menu content
        tagList(
          tags$p(
            "Help and Background",
            style =
              "color: #888888;
        margin-bottom: 0px;
        font-size: 1.17em;"
          ),

        # Tutorial Document (does not yet exist for the config applet)
        # tags$a("Tutorial Vignette",
        #        href="Shiny_Vignette.html",
        #        class="blue_hover",
        #        target="_blank", #Opens link in new tab
        #        rel="noopener noreferrer"
        # ),#End tutorial document link

        # File issue on github
        tags$a(
          "Report a Bug",
          href = "https://github.com/amc-heme/DataExploreShiny/issues",
          class = "blue_hover",
          # Opens link in new tab
          target = "_blank",
          rel = "noopener noreferrer")
        )# End tagList
      ), # End Help Button

      dropdownButton(
        inputId = "options",
        status = "info",
        right = TRUE,
        label = "",
        size = "sm",
        icon = icon("ellipsis-h"),
        downloadLink(
          outputId = "export_selections",
          label = "Save Config File",
          class = "orange_hover"
        ),
        if(!is.null(config_filename)){
          actionLink(
            inputId = "load_config",
            label = "Load Config File",
            class = "orange_hover"
          )
        }
      ),

      # Button to create/export config file
      # downloadButton(
      #   outputId = "export_selections",
      #   label="Export Configuration",
      #   class = "float_right",
      #   icon = NULL
      #   ),

      # May be more appropriate to use an action button later for more advanced
      # behavior (save config file and direct user back to app)
      # actionButton(inputId = "export_selections",
      #              label = "Export Configuration",
      #              class="float_right"),

      # Include scripts for each JavaScript file in document
      # This is added last to delay running of scripts until the elements to be
      # moved by scripts are created
      js_list,
      # Apply CSS files (placed last so elements created by scripts can be stylized)
      css_list
    ) # End fluidPage
    ## 3. Main Server Function ####
    server <- function(input, output, session) {
      # Hide ADT Threshold Tab at startup: this is shown when the user designates
      # an assay as the surface protein assay
      # Use jQuery selector (tab does not have an ID). Selector is based on content
      # of tab button
      ADT_tab_selector <- "a:contains('ADT Threshold')"

      hideElement(
        selector = ADT_tab_selector
      )

      # Initialize Variables
      # config_data: list used to store full record of user selections
      # across all tabs
      config_data <-
        list(
          # Append config app version to list that is printed to file
          `config_version` = config_version,
          # Record class of object
          `object_class` = is(object),
          # Record if a SingleCellExperiment object is HDF5 enabled
          `is_HDF5SummarizedExperiment` = is_HDF5SummarizedExperiment,
          # Track if a prefix is necessary to load HDF5 SingleCellExperiment
          `HDF5_prefix` = HDF5_prefix
          )

      # module_data: reactiveValues object for storing data specific
      # to this module
      module_data <- reactiveValues()

      # Sortable Data: Metadata ####
      # Metadata choices selected vs. not selected
      # Nothing selected by default. The variables below are modified
      # when loading a config file
      module_data$metadata_sortable_selected <- character(0)
      # Choices not selected: equal to all non-numeric metadata.
      module_data$metadata_sortable_not_selected <- non_numeric_cols
      # Reactive trigger for updating sortable menus (menus will not update
      # the second time a config file is loaded because the above variables
      # do not change)
      update_metadata_sortable <- makeReactiveTrigger()

      # Sortable Data: Reductions ####
      # All reductions are choices, and all are selected by default
      module_data$reductions_sortable_selected <- reductions
      module_data$reductions_sortable_not_selected <- character(0)
      update_reductions_sortable <- makeReactiveTrigger()

      # Store assays for which ADT thresholding modules have been created
      # (to avoid duplicates
      module_data$existing_adt_modules <- c()

      # Threshold tab data
      # State of sidebar: different menus are shown depending on what the
      # user is doing at the moment (adding a new threshold, editing a
      # threshold, etc.)
      module_data$threshold_menu_state <- "idle"

      # Tibble for storing threshold data: a blank tibble with column names for
      # the adt name and the value
      module_data$threshold_data <-
        tibble(
          `adt` = character(0),
          `value` = numeric(0)
        )

      # Variables set when a ADT in the table is being edited and cleared
      # after a new threshold is set
      editing_data <- reactiveValues()
      # Identity of ADT being edited
      editing_data$adt_target <- NULL
      # Previously defined threshold of ADT being edited
      editing_data$previous_threshold <- NULL
      # Index of row being edited
      editing_data$target_row <- NULL

      ## 3.1. General Dataset Info Tab ####
      ### 3.1.1. Plot/image window: show/hide interface ####
      observe({
        dimplot_preview_id <- "preview_dimplot_interface"
        image_preview_id <- "preview_image_interface"

        if (input$preview_type == "dimplot"){
          # Show only the dimplot preview container (hide the image selection
          # container)
          showElement(
            id = dimplot_preview_id,
            anim = TRUE
            )

          hideElement(
            id = image_preview_id,
            anim = TRUE
            )
        } else if (input$preview_type == "image"){
          # Show only the image selection container (hide the dimplot preview
          # container)
          hideElement(
            id = dimplot_preview_id,
            anim = TRUE
            )

          showElement(
            id = image_preview_id,
            anim = TRUE
            )
        } else if (input$preview_type == "none"){
          # Hide both containers
          hideElement(
            id = dimplot_preview_id,
            anim = TRUE
          )

          hideElement(
            id = image_preview_id,
            anim = TRUE
          )
        }
      })

      ### 3.1.2. Preview dimplot options module ####
      preview_dimplot_options <-
        preview_dimplot_server(
          id = "dimplot",
          object = object
          )

      ### 3.1.3. Construct preview dimplot ####
      output$preview_dimplot <-
        renderPlot(
          res = 36,
          # Dynamically adjusts height in response to width
          # at a fixed aspect ratio
          # Functions defined for width, height, are evaluated in a reactive
          # context
          height =
            function(){
              218/290 * session$clientData$output_preview_dimplot_width
              },
          {
          req(
            preview_dimplot_options$group_by(),
            preview_dimplot_options$split_by(),
            preview_dimplot_options$reduction(),
            preview_dimplot_options$ncol()
          )

          shiny_umap(
            object = object,
            group_by = preview_dimplot_options$group_by(),
            split_by = preview_dimplot_options$split_by(),
            reduction = preview_dimplot_options$reduction(),
            ncol = preview_dimplot_options$ncol(),
            show_legend =
              if (!is.null(preview_dimplot_options$legend())){
                preview_dimplot_options$legend()
              } else TRUE,
            show_label =
              if (!is.null(preview_dimplot_options$label())){
                preview_dimplot_options$label()
              } else TRUE,
            show_title = FALSE,
            is_subset = FALSE,
            original_limits = NULL
            )
        })

      ### 3.1.4. Render preview image, if loaded ####
      output$preview_image <-
        renderImage({
          req(input$image_upload)

          data_path <-
            dplyr::select(input$image_upload, "datapath") |>
            as.character()

          file_type <-
            dplyr::select(input$image_upload, "type") |>
            as.character()

          alt <-
            if (isTruthy(input$dataset_label)){
              glue("preview image for {input$dataset_label}.")
            } else {
              "preview image"
            }

          # If the file loaded is an image, render that image
          if (grepl("image", file_type)){
            # List used to render image
            list(
              `src` = data_path,
              # Width: equal to size of container
              `width` = session$clientData$output_preview_image_width,
              # Height: based on aspect ratio used in the dataset preview window
              `height` = 218/290 * session$clientData$output_preview_image_width,
              `alt` = alt
            )
          } else {
            # Otherwise, show a notification to the user
            showNotification(
              ui =
                icon_notification_ui(
                  icon = "exclamation-triangle",
                  "
                  The file uploaded is not of a supported file type. Please upload an image format.
                  "
                ),
              #Show notification for 5 seconds
              duration = 5,
              session = session
            )
          }


        },
        deleteFile = FALSE
        )

      ### 3.1.5. Show preview of image when a file is loaded ####
      observe({
        target_id <- "image_container"

        if (isTruthy(input$image_upload)){
          showElement(
            id = target_id
            )
        } else {
        hideElement(
          id = target_id
          )
        }
      })

      ### 3.1.6. RECORD: options chosen in the general info tab ####
      config_data$label <-
        reactive({input$dataset_label})

      config_data$description <-
        reactive({input$dataset_description})

      config_data$preview <-
        reactive({
          preview_info <-
            list(
              `type` = input$preview_type
            )

          if (input$preview_type == "dimplot"){
            c(
              preview_info,
              list(
                `plot_settings` =
                  print_reactive_list(preview_dimplot_options)
              )
            )
          } else if (input$preview_type == "image"){
            upload_path <-
              dplyr::select(input$image_upload, "name") |>
              as.character()

            if (isTruthy(upload_path)){
              c(
                preview_info,
                list(
                  `image_path` = upload_path
                )
              )
            }
          } else {
            # For preview type == none, and unanticipated types, return a list
            # with just the type defined.
            preview_info
          }
        })

      ## 3.2. Assay Tab ####
      ### 3.2.1. Store selected assays as a reactive variable ####
      assays_selected <-
        eventReactive(
          input$assays_selected,
          ignoreNULL=FALSE,
          {
            input$assays_selected
          })

      ### 3.2.2. Options modules for assays ####
      all_assay_options <- list()

      # Create an assay options module for each assay in the object
      for (id in all_assays){
        server_output <-
          options_server(
            id = id,
            object = object,
            categories_selected = assays_selected,
            options_type = "assays",
            dev_mode = dev_mode
            )

        all_assay_options[[id]] <- server_output
      }

      ### 3.2.3. Record list of assay module outputs in config data ####
      # Filter list of options module outputs and combine into a single
      # reactive object, which is added to the config_data list.
      config_data$assays <-
        reactive({
          # Options list is only processed when metadata columns
          # have been selected
          if (!is.null(input$assays_selected)){
            #Extracts each reactive module output and stores them in a list
            list <- lapply(all_assay_options, function(x) x())
            #Filter list for metadata columns that have been selected by the user
            return(list[names(list) %in% input$assays_selected])
          } else {
            #Return NULL if no columns are selected
            return(NULL)
          }
        })

      ### 3.2.4. Determine designated ADT assay ####
      ADT_assay <-
        reactive({
          # Return input$adt_assay, unless it is equal to "none"
          if (input$adt_assay != "none"){
            input$adt_assay
          } else {
            # Return NULL when ADT assay is set to "none"
            # ADT threshold expressions will no longer run (avoiding an error
            # when "none" is passed forward as the assay name)
            NULL
          }
        })

      ### 3.2.5. Show/Hide ADT thresholding tab ####
      observe({
        # When an assay is designated as the ADT assay, show the corresponding tab
        if (!is.null(ADT_assay())){
          showElement(
            # ADT_tab_selector is defined at the beginning of the server function
            selector = ADT_tab_selector
          )
        } else {
          # Hide the tab if there is no designated ADT assay
          hideElement(
            selector = ADT_tab_selector
          )
        }
      })

      ### 3.2.6. Generic assay options ####
      #### 3.2.6.1. Update designated assay selections with valid assays ####
      observe({
        # Update designated genes assay with valid choices
        valid_choices_gene <-
          # None is the only choice when no assays are selected
          if (!isTruthy(input$assays_selected)){
            "none"
          } else {
            # Otherwise, choose from selected assays (excluding none)
            input$assays_selected
          }

        assay_selected_gene <- isolate({input$genes_assay})

        updateSelectInput(
          session = session,
          inputId = "genes_assay",
          choices = valid_choices_gene,
          # Preserve user choice if it is still valid
          selected =
            if (assay_selected_gene %in% valid_choices_gene){
              assay_selected_gene
            } else {
              valid_choices_gene[1]
            }
          )

        # Do the same for the ADT assay
        valid_choices_adt <-
          # None is the only choice when no assays are selected
          if (!isTruthy(input$assays_selected)){
            "none"
          } else {
            # Otherwise, choose from selected assays (including none)
            c("none", input$assays_selected)
          }

        assay_selected_adt <- isolate({input$adt_assay})

        updateSelectInput(
          session = session,
          inputId = "adt_assay",
          choices = valid_choices_adt,
          # Preserve user choice if it is still valid
          selected =
            if (assay_selected_adt %in% input$assays_selected){
              assay_selected_adt
            } else {
              valid_choices_adt[1]
            }
        )
      })

      #### 3.2.6.2. RECORD: Generic assay options ####
      config_data$other_assay_options <-
        reactive({
          list(
            # Column to be used for patient/sample level metadata
            `genes_assay` = input$genes_assay,
            `adt_assay` = input$adt_assay
          )
        })

      ### 3.2.7. RECORD: value of "include numeric metadata" checkbox ####
      config_data$include_numeric_metadata <-
        reactive({
          input$include_numeric_metadata
        })

      ## 3.3. Metadata Tab ####
      ### 3.3.1. RECORD: selected metadata ####
      metadata_selected <-
        eventReactive(
          input$metadata_selected,
          ignoreNULL = FALSE,
          {
            input$metadata_selected
          })

      ### 3.3.2. Generic metadata options ####
      #### 3.3.2.1. Store selection from patient metadata column menu ####
      patient_colname <-
        reactive(
          label = "Process selection for patient level metadata column",
          {
            req(input$patient_colname)

            # Value to record/export: NULL if "none" is selected for the patient
            # column name, otherwise use the column selected
            if (input$patient_colname == "none"){
              NULL
            } else {
              input$patient_colname
            }
          })

      ### 3.3.3. Options modules for metadata ####
      # One server instance is created for each metadata category in the object
      all_metadata_options <- list()

      for (var in meta_vars){
        server_output <-
          options_server(
            id = var,
            object = object,
            categories_selected = metadata_selected,
            options_type = "metadata",
            dev_mode = dev_mode
            )

        all_metadata_options[[var]] <- server_output
      }
     
      ### 3.3.4. RECORD: metadata options in config data ####
      #### 3.3.4.1. Category-specific options ####
      config_data$metadata <-
        reactive({
          if (dev_mode == TRUE){
            print("metadata selected")
            print(input$metadata_selected)
          }

          # Options list is only processed when metadata columns
          # have been selected
          if (isTruthy(input$metadata_selected)){
            # Extracts each reactive module output and stores them in a list
            options_list <- lapply(all_metadata_options, function(x) x())
            # Filter list for metadata columns that have been selected
            # by the user
            options_list <-
              options_list[names(options_list) %in% input$metadata_selected]
            # Sort list according to the order specified by the user in the drag
            # and drop menu
            options_list <- options_list[input$metadata_selected]
            if (dev_mode == TRUE){
              print("Options list")
              print(options_list)
            }

            return(options_list)
          } else {
            # Return NULL if no columns are selected
            return(NULL)
          }
        })

      #### 3.3.4.2. RECORD: General metadata options ####
      # Stored separately from config_data$metadata, since many functions in the
      # main app use the list structure defined in the above reactive
      # expression, and they would be disrupted if general options were added
      # with the options specific to each category.
      config_data$other_metadata_options <-
        reactive({
          list(
            # Column to be used for patient/sample level metadata
            `patient_colname` = patient_colname()
          )
        })

      ### 3.3.5. Reactive UI components ####
      #### 3.3.5.1. UI for sortable menu ####
      # Uses the bucket_list input from the sortable package
      metadata_bucket_ui <-
        eventReactive(
          c(module_data$metadata_sortable_not_selected,
            module_data$metadata_sortable_selected,
            update_metadata_sortable$depend()),
          label = "Metadata: define sortable",
          {
            tagList(
              tags$b("Choose Metadata to Include:"),
              bucket_list(
                header =
                  "Drag metadata variables to \"Included Metadata\" to include.
                    Metadata will appear in app menus in the order they appear in
                    the right-hand column.",
                orientation = "horizontal",
                group_name = "metadata_bucket",
                # Use the default class, and a class specific to this app
                # Many sub-classes are tied to the default class, and styling will
                # not be applied to those classes if the default class is not also
                # passed to this argument.
                class =
                  c("default-sortable", "bucket-select"),
                add_rank_list(
                  input_id = "metadata_not_selected",
                  text = "Available Metadata",
                  labels = module_data$metadata_sortable_not_selected
                ),
                add_rank_list(
                  input_id = "metadata_selected",
                  text = "Included Metadata",
                  labels = module_data$metadata_sortable_selected
                )
              )
            )
          })

      #### 3.3.5.2. Set order of metadata cards based on sortable input ####
      observeEvent(
        c(input$metadata_selected, input$metadata_not_selected),
        label = "Metadata: set order of metadata options cards",
        ignoreNULL = FALSE,
        ignoreInit = TRUE,
        {
          # Guide vector for ordering the metadata options cards
          # Lists the metadata included by the user in the order defined in the
          # sortable, followed by all other categories (the corresponding cards
          # will be invisible, but they should be sorted after the included
          # metadata categories)
          metadata_categories_order <-
            c(input$metadata_selected, input$metadata_not_selected)

          for (i in 1:length(metadata_categories_order)){
            if (i == 1){
              # First element is left as-is
              next
            } else if (i == 2) {
              # Second element: move after first element

              # First element (destination, second element will be inserted after
              # this element)
              destination_column <- metadata_categories_order[1]
              # ID of the first options card, which is passed to the JavaScript
              # function
              destination_id <- glue("{destination_column}-optcard")

              # Second element (target)
              elem_column <- metadata_categories_order[i]
              elem_id <- glue("{elem_column}-optcard")

              # Custom JavaScript function to move options card
              js$insertElemAfter(
                elem_id = elem_id,
                destination_id = destination_id
              )

            } else {
              # All subsequent elements: move after the previous element
              # ID of previous card (destination of move)
              destination_column <- metadata_categories_order[i - 1]
              destination_id <- glue("{destination_column}-optcard")

              # ID of current card (element to be moved)
              elem_column <- metadata_categories_order[i]
              elem_id <- glue("{elem_column}-optcard")

              # Custom JavaScript function to move options card
              js$insertElemAfter(
                elem_id = elem_id,
                destination_id = destination_id
              )
            }
          }
        })
   
      #### 3.3.5.3. Show/hide Metadata Options Cards ####
      observe({
        for (colname in non_numeric_cols){
          # Show all cards that are in the "Metadata selected" column of the
          # sortable, and hide all cards that are not
          if (colname %in% input$metadata_selected){
       
            
            showElement(
              id = glue("{colname}-optcard"),
              asis = TRUE
            )
          } else {
            hideElement(
              id = glue("{colname}-optcard"),
              asis = TRUE
            )
          }
        }
      })
      
  
      #### 3.3.5.4. Render Sortable UI ####
      output$metadata_sortable_bucket <-
        renderUI({
          metadata_bucket_ui()
        })

      # Also set suspendWhenHidden to FALSE to allow reactives that lead to the
      # output to compute when the sortable is hidden
      # (this is desired when loading a config file, as the metadata tab may not
      # be active when it is loaded)
      outputOptions(
        output,
        "metadata_sortable_bucket",
        suspendWhenHidden = FALSE
      )

      ## 3.4 Reductions tab ####
      ### 3.4.1. Dynamic UI ####
      #### 3.4.1.1. Sortable Menu UI ####
      # Uses the bucket_list input from the sortable package
      reductions_bucket_ui <-
        eventReactive(
          c(module_data$reductions_sortable_not_selected,
            module_data$reductions_sortable_selected,
            update_reductions_sortable$depend()),
          label = "Reductions: define sortable",
          {
            tagList(
              tags$b("Choose Reductions to Include:"),
              bucket_list(
                header =
                  "Drag reductions categories to the \"Included Reductions\"
                  column to include. Reductions will appear in app menus in
                  the order they appear in the right-hand column.",
              orientation = "horizontal",
              group_name = "reductions_bucket",
              # Use the default class, and a class specific to this app
              # Many sub-classes are tied to the default class, and styling will
              # not be applied to those classes if the default class is not also
              # passed to this argument.
              class =
                c("default-sortable", "bucket-select"),
              add_rank_list(
                input_id = "reductions_not_selected",
                text = "Available Reductions",
                labels = module_data$reductions_sortable_not_selected
              ),
              add_rank_list(
                input_id = "reductions_selected",
                text = "Included Reductions",
                labels = module_data$reductions_sortable_selected
              )
              )
            )
          })

      #### 3.4.1.2. Change Order of Cards to Match Selected Reductions ####
      observeEvent(
        c(input$reductions_selected, input$reductions_not_selected),
        label = "Reductions: set order of reductions options cards",
        ignoreNULL = FALSE,
        ignoreInit = TRUE,
        {
          # Guide vector for ordering the reductions options cards
          # Lists the reductions included by the user in the order defined in the
          # sortable, followed by non-selected reductions (the corresponding cards
          # will be invisible, but they should be sorted after the included
          # reductions)
          reductions_categories_order <-
            c(input$reductions_selected, input$reductions_not_selected)

          for (i in 1:length(reductions_categories_order)){
            if (i == 1){
              # First element is left as-is
              next
            } else if (i == 2) {
              # Second element: move after first element

              # First element (destination, second element will be inserted after
              # this element)
              destination_column <- reductions_categories_order[1]
              # ID of the first options card, which is passed to the JavaScript
              # function
              destination_id <- glue("{destination_column}-optcard")

              # Second element (target)
              elem_column <- reductions_categories_order[i]
              elem_id <- glue("{elem_column}-optcard")

              # Custom JavaScript function to move options card
              js$insertElemAfter(
                elem_id = elem_id,
                destination_id = destination_id
              )

            } else {
              # All subsequent elements: move after the previous element
              # ID of previous card (destination of move)
              destination_column <- reductions_categories_order[i - 1]
              destination_id <- glue("{destination_column}-optcard")

              # ID of current card (element to be moved)
              elem_column <- reductions_categories_order[i]
              elem_id <- glue("{elem_column}-optcard")

              # Custom JavaScript function to move options card
              js$insertElemAfter(
                elem_id = elem_id,
                destination_id = destination_id
              )
            }
          }
        })

      #### 3.4.1.3 Show/hide Reductions Options Cards ####
      observe({
        for (reduction in reductions){
          # Show all cards that are in the "Metadata selected" column of the
          # sortable, and hide all cards that are not
          if (reduction %in% input$reductions_selected){
            showElement(
              id = glue("{reduction}-optcard"),
              asis = TRUE
            )
          } else {
            hideElement(
              id = glue("{reduction}-optcard"),
              asis = TRUE
            )
          }
        }
      })

      #### 3.4.1.4. Render Sortable UI ####
      output$reductions_sortable_bucket <-
        renderUI({
          reductions_bucket_ui()
        })

      ### 3.4.2. RECORD: selected reductions ####
      reductions_selected <-
        eventReactive(
          input$reductions_selected,
          ignoreNULL = FALSE,
          {
            input$reductions_selected
          })

      ### 3.4.3. Options for each reduction ####
      # One server instance of the options module is created for each reduction
      # in the object (this is done once at startup)
      all_reductions_options <- list()

      for (reduction in reductions){
        server_output <-
          options_server(
            id = reduction,
            object = object,
            categories_selected = reductions_selected,
            options_type = "reductions",
            dev_mode = dev_mode
            )

        all_reductions_options[[reduction]] <- server_output
      }

      ### 3.4.4. RECORD: reductions options in config data ####
      config_data$reductions <-
        reactive({
          # Options list is only processed when reductions have been selected
          if (isTruthy(input$reductions_selected)){
            # Extracts each reactive module output and stores them in a list
            options_list <- lapply(all_reductions_options, function(x) x())
            # Filter list for reductions that have been selected by the user
            options_list <-
              options_list[names(options_list) %in% input$reductions_selected]
            # Sort list according to the order specified by the user in the
            # drag and drop menu
            options_list <- options_list[input$reductions_selected]
            return(options_list)
          } else {
            # Return NULL if no reductions are selected
            return(NULL)
          }
        })

      # TEMP: print all reduction options
      # output$print_reductions <-
      #   renderPrint({
      #     config_data$reductions()
      #   })

      ## 3.5 ADT thresholding tab ####

      ### 3.5.1. Define/update available ADTs ####
      # Reactive variable will be used for updating the selection menu with
      # ADTs in the designated assay that have not already been added to the
      # table
      available_adts <-
        reactive({
          req(ADT_assay())

          # Fetch ADTs in the designated assay (reacts to assay)
          adts <-
            scExploreR:::features_in_assay(
              object,
              assay = ADT_assay()
              )

          # return adts that are not included in the table of defined thresholds
          # (also reacts to changes in the table)
          if (!is.null(module_data$threshold_data)){
            return(adts[!adts %in% module_data$threshold_data$adt])
          } else {
            return(adts)
          }
        })

      ### 3.5.2. Populate ADT Choices when designated ADT assay is changed ####
      observeEvent(
        ADT_assay(),
        ignoreNULL = TRUE,
        ignoreInit = TRUE,
        {
          # Fetch features (surface proteins) for the designated ADT assay
          adts <-
            scExploreR:::features_in_assay(
              object,
              assay = ADT_assay()
              )

          # Populate select input with ADT choices
          updateSelectizeInput(
            session = session,
            inputId = "selected_adt",
            choices = adts,
            selected = character(0),
            server = TRUE
          )

          # Also, set state of threshold menus back to "idle" if the designated
          # ADT assay changes
          module_data$threshold_menu_state <- "idle"
        })

      ### 3.5.3. Threshold picker server ####
      #### 3.5.3.1. ADT passed to server ####
      threshold_server_adt <-
        reactive({
          # Value passed to server depends on state
          if (module_data$threshold_menu_state == "add"){
            # When the menus are in the "add" state, use input$selected_adt
            # Selected ADT must be defined to avoid errors
            req(input$selected_adt)
            req(ADT_assay())

            # When adding a new threshold, use the adt selected by the user in
            # the search window
            # Add assay key to ADT (threshold server expects this)
            # paste0(
            #   Key(object[[isolate({ADT_assay()})]]),
            #   input$selected_adt
            # )
            paste0(
              scExploreR:::make_key(
                object,
                assay = isolate({ADT_assay()})
                ),
              input$selected_adt
            )
          } else if (module_data$threshold_menu_state == "edit") {
            # Will be equal to the ADT requested for editing
            # (this is set using a reactiveValues object)
            paste0(
              # Add assay key
              scExploreR:::make_key(
                object,
                assay = isolate({ADT_assay()})
                ),
              editing_data$adt_target
            )
          }
        })

      #### 3.5.3.2. Server instance ####
      threshold_value <-
        threshold_picker_server(
          id = "threshold_picker",
          # `object` argument must be reactive, but the object is non-reactive
          # in the config file
          object = reactive({object}),
          feature = threshold_server_adt,
          showhide_animation = TRUE,
          # Used to show the previous threshold on the interactive ridge
          # plot during editing
          set_threshold =
            reactive({
              editing_data$previous_threshold
            })
        )

      ### 3.5.4. "Add threshold" button ####
      #### 3.5.4.1. Respond to button ####
      # Set state to "add", which will show menus with the class "show-on-add"
      observeEvent(
        input$add_threshold,
        ignoreNULL = FALSE,
        ignoreInit = TRUE,
        {
          module_data$threshold_menu_state <- "add"
          })

      #### 3.5.4.2. Disable button while adding or editing a feature ####
      # Prevents user from adding a new feature while editing the current one
      observe({
        target_id <- "add_threshold"

        if (module_data$threshold_menu_state %in% c("add", "edit")){
          disable(
            id = target_id
          )
        } else if (module_data$threshold_menu_state == "idle"){
          enable(
            id = target_id
          )
        }
      })

      ### 3.5.5. Show/hide menus based on state ####
      #### 3.5.5.1. Generic Menus ####
      observe({
        # jQuery selectors for classes that show elements based on state
        add_selector <- "[class *= 'show-on-add']"
        edit_selector <- "[class *= 'show-on-edit']"
        idle_selector <- "[class *= 'show-on-idle']"

        if (module_data$threshold_menu_state == "add"){
          showElement(
            selector = add_selector
          )
        } else if (module_data$threshold_menu_state == "edit"){
          showElement(
            selector = edit_selector
          )
        } else if (module_data$threshold_menu_state == "idle") {
          hideElement(
            selector = add_selector
          )
          hideElement(
            selector = edit_selector
          )
        }
      })

      #### 3.5.5.2. Show/Hide threshold picker UI ####
      # Shown when the state is "add" and an adt is entered in the search input
      # OR when the state is "edit" (feature being edited is provided when
      # changing to this state)
      observe({
        target_id <- "threshold_picker_div"

        if (module_data$threshold_menu_state == "add"){
          if (!is.null(input$selected_adt)){
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
        } else if (module_data$threshold_menu_state == "edit"){
          showElement(
            id = target_id
          )
        } else {
          hideElement(
            id = target_id
          )
        }
      })

      ### 3.5.6. Cancel threshold button ####
      observeEvent(
        input$cancel_threshold,
        ignoreNULL = FALSE,
        ignoreInit = TRUE,
        {
          # Clear data for ADT being added or edited
          if (module_data$threshold_menu_state == "edit"){
            # If editing, set editing_data variables back to NULL
            editing_data$adt_target <- NULL
            editing_data$target_row <- NULL
            editing_data$previous_threshold <- NULL
          }

          # Reset ADT selection input
          # Get names of all ADTs
          adts <-
            scExploreR:::features_in_assay(
              object,
              assay = ADT_assay()
              )

          updateSelectizeInput(
            session = session,
            inputId = "selected_adt",
            # Exclude the ADTs currently in the table
            choices = adts[!adts %in% module_data$threshold_data$adt],
            selected = character(0),
            server = TRUE
          )

          # Set state of menus back to "idle"
          module_data$threshold_menu_state <- "idle"
        })


      ### 3.5.7. Accept threshold button ####
      #### 3.5.7.1. Enable "confirm" button when a threshold is selected ####
      observe({
        print("threshold value")
        print(threshold_value())

        if (!is.null(threshold_value())){
          enable(
            id = "accept_threshold"
            )
        } else {
          disable(
            id = "accept_threshold"
            )
        }
      })

      #### 3.5.7.2. Save data and close menu when the confirm button is pressed ####
      observeEvent(
        input$accept_threshold,
        ignoreNULL = FALSE,
        ignoreInit = TRUE,
        {
          if (module_data$threshold_menu_state == "add"){
            # If the state is "add", add the threshold value for the currently
            # selected ADT to the table
            module_data$threshold_data <-
              module_data$threshold_data |>
              add_row(
                adt = input$selected_adt,
                value = threshold_value()
              )
          } else if (module_data$threshold_menu_state == "edit"){
            # Set the "adt" entry of the row being edited to the feature name
            # module_data$threshold_data[editing_data$target_row, 1] <-
            #   editing_data$adt_target

            # Set the "value" entry (column 2) of the row being edited to the
            # new value chosen on the interactive plot
            module_data$threshold_data[editing_data$target_row, 2] <-
              threshold_value()

            # Set editing_data variables back to NULL
            editing_data$adt_target <- NULL
            editing_data$target_row <- NULL
            editing_data$previous_threshold <- NULL
          }

          # Update ADT choices to exclude the ADTs currently in the table
          adts <-
            scExploreR:::features_in_assay(
              object,
              assay = ADT_assay()
              )

          updateSelectizeInput(
            session = session,
            inputId = "selected_adt",
            choices = adts[!adts %in% module_data$threshold_data$adt],
            selected = character(0),
            server = TRUE
          )

          # Set state of menus back to "idle"
          module_data$threshold_menu_state <- "idle"
        })

      ### 3.5.8. Render table of ADT thresholds ####
      #### 3.5.8.1. DT datatable ####
      threshold_DT <-
        reactive({
          DT <- module_data$threshold_data

          # Add edit and delete buttons to table
          # Code adapted from
          # https://github.com/AntoineGuillot2/ButtonsInDataTable
          if (nrow(DT) > 0){
            DT[["Actions"]] <-
              glue(
                '<div class = "btn-group" style = "float: right;" role = "group"
            aria-label = "Options for {DT[[\'adt\']]}">
              <button type="button" class="btn icon-button edit"
                id = edit_{1:nrow(module_data$threshold_data)}>
                <i class = "fa fa-pencil" role = "presentation"
                  aria-label = "Edit" style = "font-size: 1.7em;"></i>
                </button>

              <button type="button" class="btn icon-button delete"
                id = delete_{1:nrow(module_data$threshold_data)}>
                  <i class = "fa fa-times-circle" role = "presentation"
                  aria-label = "Delete" style = "font-size: 1.7em;"></i>
              </button>
           </div>'
              )
          }

          # Create DT Datatable
          datatable(
            DT,
            # DT classes applied
            # See https://datatables.net/manual/styling/classes
            class = "compact stripe cell-border hover",
            # Disallow selection of rows/cells (currently)
            selection = "none",
            # Remove rownames
            rownames = FALSE,
            colnames = c("ADT" = "adt", "Chosen Threshold" = "value"),
            # Escape set to FALSE so HTML above is rendered properly
            escape = FALSE
          )
        })

      output$threshold_table <-
        renderDT({
          threshold_DT()
        })

      #### 3.5.8.2. JavaScript for inline buttons ####
      button_script <-
        reactive({
          req(module_data$threshold_data)

          # Adapted from
          # https://github.com/AntoineGuillot2/ButtonsInDataTable/blob/master/server.R
          if (nrow(module_data$threshold_data > 0)){
            # Script: when the user clicks a button within the DT table
            # (#threshold_table button) register the id of the button the user
            # clicked on as input$lastClickId, and a random number as
            # input$lastClick (this is the trigger for responding to the click,
            # and must therefore always change with each click.)
            tags$script(
              "
          $(document).on('click', '#threshold_table button', function () {
              Shiny.onInputChange('lastClickId', this.id);
              Shiny.onInputChange('lastClick', Math.random())
              });
              "
            )
          }
        })

      output$threshold_table_button_script <-
        renderUI({
          button_script()
        })

      ### 3.5.9. Respond to edit/delete buttons ####
      observeEvent(
        input$lastClick,
        ignoreNULL = FALSE,
        ignoreInit = TRUE,
        {
          # input$lastClickId stores the id of the button that was clicked
          if (grepl("edit", input$lastClickId)){
            # If the button is an edit button, initialize menus for editing.
            # Set the state of the menus to "edit"
            module_data$threshold_menu_state <- "edit"

            # Determine which row the edit button was on
            row_selected <-
              gsub("edit_", "", input$lastClickId) |>
              as.numeric()

            # Determine which ADT was on the row selected
            adt_selected <-
              module_data$threshold_data$adt[row_selected]

            if (is.na(adt_selected) | is.null(adt_selected)){
              warning(
                "Threshold table: ADT selected for editing is undefined."
              )
            }

            # Fetch previous threshold and pass to interactive ridge plot
            # (will display previous selection )
            editing_data$previous_threshold <-
              module_data$threshold_data$value[row_selected]

            # Pass feature to reactive variable to update the ridge plot
            editing_data$adt_target <-
              adt_selected

            # Store the index of the row selected for
            editing_data$target_row <-
              row_selected

            # Set the selected ADT to the one being edited.
            # The ADT selectize input is modified and is still accessed,
            # but it is not visible when the state is "edit"
            # updateSelectizeInput(
            #   session = session,
            #   inputId = "selected_adt",
            #   # Choices do not change, but must be added for update to proceed
            #   choices = available_adts(),
            #   selected = adt_selected,
            #   server = TRUE
            #   )


          } else if (grepl("delete", input$lastClickId)) {
            # Delete the row corresponding to the button from the table
            # Determine which row the delete button was on
            row_selected <-
              gsub("delete_", "", input$lastClickId) |>
              as.numeric()

            # Prevents crashing in the event the selected_row is undefined
            if (!is.null(row_selected) | is.na(row_selected)){
              # Delete the selected row from the table and save the new table
              module_data$threshold_data <-
                module_data$threshold_data[-row_selected,]

              # Add the ADT corresponding to the row to be deleted back to the
              # list of available ADTs, by updating the select input with all
              # ADTs not in the new table
              adts <-
                scExploreR:::features_in_assay(
                  object,
                  assay = ADT_assay()
                  )

              updateSelectizeInput(
                session = session,
                inputId = "selected_adt",
                choices = adts[!adts %in% module_data$threshold_data$adt],
                selected = character(0),
                server = TRUE
                )

              # Set menu state back to idle to dismiss the editing menu if it
              # is open (it is possible to delete an ADT while editing the
              # threshold)
              module_data$threshold_menu_state <- "idle"
            } else {
              warning("Unable to determine the index of the row selected for deletion")
            }
          }
        })

      ### 3.5.10. Threshold settings window header text (edit mode) ####
      output$threshold_header <-
        renderUI({
          tags$h4(
            glue("Edit threshold for {editing_data$adt_target}")
          )
        })

      ### 3.5.11. RECORD: threshold table in config data ####
      config_data$adt_thresholds <-
        reactive({
          # Store the table if it is defined and has at least one row.
          # Otherwise, set adt_thresholds to NULL.
          if (isTruthy(module_data$threshold_data)){
            if (nrow(module_data$threshold_data) > 0){
              module_data$threshold_data
            } else NULL
          } else NULL
        })

      ## 3.6. Export Config file as YAML and check values for errors ####
      output$export_selections <-
        downloadHandler(
          filename = "object-config.yaml",
          content =
            function(file){
              # Compile config file data from the config_data list
              config_data_export <-
                lapply(
                  config_data,
                  function(x){
                    # The config_data list contains a mix of reactive and
                    # non-reactive values. Reactive values are unpacked, while
                    # non-reactive values are left as-is.
                    if (is.reactive(x)){
                      x()
                    } else {
                      x
                    }
                  }
                )

              # Check config file, and don't save if the user's selections
              # will cause errors in the main app
              error <- FALSE
              # Display message if metadata does not meet specifictions
              # (simplifies conditional structure in notification)
              metadata_error <- FALSE

              # If no assays are defined, mark an error
              if (!isTruthy(config_data_export$assays)){
                error = TRUE
              }

              # Error if metadata is undefined, or less than two columns are
              # selected
              if (!isTruthy(config_data_export$metadata)){
                error = TRUE
                metadata_error = TRUE
              } else {
                if (length(config_data_export$metadata) < 2){
                  error = TRUE
                  metadata_error = TRUE
                  }
                }

              # Convert R list format to YAML and download
              if (error == FALSE){
                write_yaml(
                  config_data_export,
                  file = file
                )
              } else if (error == TRUE){
                # Message to display to user if an error was identified
                showNotification(
                  ui =
                    icon_notification_ui(
                      icon_name = "skull-crossbones",
                      'Error: Unable to save the config file for the following reasons:',
                      tags$br(),
                      tags$ul(
                        if (!isTruthy(config_data_export$assays)){
                          tags$li(
                            'No assays included. Please add at least one assay in the "assays" tab.'
                            )
                        },
                        if (metadata_error){
                          tags$li(
                            'Less than two metadata variables included. Please add at least two variables in the "metadata" tab.'
                            )
                        }
                      )
                    ),
                  duration = NULL,
                  session = session
                )
              }
            })

      ## 3.7. Load Config File ####
      ### 3.7.1. Warn user if they have already entered fields in the app ####

      # Reactive trigger for loading: used to proceed with loading without
      # showing a modal if the user has not entered any information in the app
      load_trigger <- makeReactiveTrigger()

      observeEvent(
        input$load_config,
        label = "Display warning before loading config file",
        ignoreNULL = FALSE,
        ignoreInit = TRUE,
        {
          if(is.null(config_filename)){
            showModal(modalDialog(
              title = "Important message",
              "Cannot load. No config file specified (config_path=NULL). 
              Please specify a config file."
            ))
          }
          else{
            # Tests for user input: below values are TRUE when the user has not
            # changed any settings in the app (as they are based off
            # default values)
            if (
              any(
              isTruthy(input$dataset_label),
              isTruthy(input$dataset_description),
              input$preview_type != "none",
              isTruthy(input$assays_selected),
              input$include_numeric_metadata == FALSE,
              input$genes_assay != "none",
              input$adt_assay != "none",
              isTruthy(input$metadata_selected),
              input$patient_colname != "none",
              isTruthy(input$reductions_not_selected),
              isTruthy(module_data$threshold_data$adt)
              )
            ){
              showModal(
                warning_modal(
                  confirmId = "load_confirm",
                  cancelId = "load_cancel",
                  text =
                    "Loading a config file will erase any changes made. Continue?"
                )
              )
              
            } else {
              # Trigger to proceed with loading without showing a modal
              if (dev_mode == TRUE){
                print("Triggered loading of config file")
              }
              load_trigger$trigger()
            }
          }
        })

      #### 3.7.1.1. User presses cancel ####
      # If the user presses cancel, close the modal and do nothing.
      observeEvent(
        input$load_cancel,
        label = "Load config file: display modal",
        ignoreInit = TRUE,
        {
          removeModal()
        })

      #### 3.7.1.2. User presses confirm ####
      # Close the modal and trigger loading of config file
      observeEvent(
        input$load_confirm,
        label = "Load config file: display modal",
        ignoreInit = TRUE,
        {
          removeModal()

          if (dev_mode == TRUE){
            print("Triggered loading of config file")
          }
          load_trigger$trigger()
        })

      ### 3.7.2 Load File ####
      # Loads a previously created config file and imports contents into app
      # storage in session$userdata makes file visible to all modules
      session$userData$config <-
        eventReactive(
          load_trigger$depend(),
          ignoreNULL = FALSE,
          ignoreInit = TRUE,
          {
            if (dev_mode == TRUE){
              print("Loading config file")
            }
            # config_filename: for now, use a path from config_init.yaml
            # May soon be chosen with a select input
            if (isTruthy(config_filename)){
              # Determine if the file loaded is .rds or .yaml
              # (supports both for backward compatibility)
              if (grepl(".yaml$", config_filename)){
                # Procedure for loading .yaml file
                # Use the load_config function from `./R` to convert .yaml file
                # to R format and properly initialize the ADT threshold tibble
                load_config(config_filename)
              } else if (grepl(".rds$", config_filename)){
                # Procedure for loading .rds file
                readRDS(config_filename)
              }
            } else {
              # Show error if no config file path is defined
              showNotification(
                ui =
                  icon_notification_ui(
                    icon_name = "skull-crossbones",
                    "Argument `config_path` to run_config_app() is undefined.
                    Please specify a path to load a config file."
                    ),
                duration = NULL,
                id = "load_config_error",
                session = session
                )
            }
          })

      ### 3.7.3. Notify the user when the file is loaded ####
      observeEvent(
        session$userData$config(),
        {
          if (dev_mode == TRUE){
            if (isTruthy(session$userData$config())){
              print("Config file loaded.")

              print("Contents")
              print(session$userData$config())
            }
          }

          # In the future, may notify user which fields exist and which do not
          showNotification(
            ui =
              icon_notification_ui(
                icon_name = "check-circle",
                icon_class = "fas",
                # Get path to wrap to next line instead of being cut off at the
                # right side of the notification
                notification_style = "word-break: break-all;",
                glue('Successfully loaded the config file at {config_filename}.')
              ),
            duration = NULL,
            id = "load_config",
            session = session
          )
        })

      ### 3.7.4. Update inputs in main server function with file contents ####
      #### 3.7.4.1. General dataset info tab ####
      ##### 3.7.4.1.1. Label for dataset ####
      observeEvent(
        session$userData$config(),
        {
          if (isTruthy(session$userData$config()$label)){
            if (dev_mode == TRUE){
              print("Update label")
            }
            updateTextInput(
              session = session,
              inputId = "dataset_label",
              value = session$userData$config()$label
            )
            if (dev_mode == TRUE){
              print("Complete")
            }
          }
        })

      ##### 3.7.4.1.2. Dataset description ####
      observeEvent(
        session$userData$config(),
        {
          if (isTruthy(session$userData$config()$description)){
            if (dev_mode == TRUE){
              print("Update description")
            }

            updateTextInput(
              session = session,
              inputId = "dataset_description",
              value = session$userData$config()$description,
            )

            if (dev_mode == TRUE){
              print("Complete")
            }
          }
        })

      ##### 3.7.4.1.3. Preview type and settings ####
      observeEvent(
        session$userData$config(),
        {
          preview_type <- session$userData$config()$preview$type

          # If the preview type is an image, the input can't be updated
          # fileInputs create temporary files when the user selects a file, and
          # this can't be re-created from a config file
          if (isTruthy(preview_type)){
            if (dev_mode == TRUE){
              print("Update preview type")
            }

            updateSelectInput(
              session = session,
              inputId = "preview_type",
              selected = preview_type
            )

            if (dev_mode == TRUE){
              print("Complete")
            }
          }
        })

      ##### 3.7.4.1.4. Plot settings for dimplot preview ####
      # load_inputs <-
      #   reactive({
      #     if (isTruthy(session$userData$config())){
      #       # If the preview type is a dimplot, return the dimplot settings
      #       if (session$userData$config()$preview$type == "dimplot"){
      #         print("session$userData$config()$preview$plot_settings")
      #         print(session$userData$config()$preview$plot_settings)
      #         session$userData$config()$preview$plot_settings
      #       } else {
      #         # Otherwise, return NULL
      #         NULL
      #       }
      #     } else {
      #       NULL
      #       }
      #     })

      #### 3.7.4.2. Assay tab ####
      ##### 3.7.4.2.1. Assays selected ####
      observeEvent(
        session$userData$config(),
        {
          if (dev_mode == TRUE){
            print("Update assays selected")
          }

          updateMultiInput(
            session,
            inputId = "assays_selected",
            selected =
              # Names of assays in config file are the names selected when the
              # file was created
              names(
                session$userData$config()$assays
              )
          )

          if (dev_mode == TRUE){
            print("Complete")
          }
        })

      ##### 3.7.4.2.2. Include numeric metadata ####
      observeEvent(
        session$userData$config(),
        {
          if (dev_mode == TRUE){
            print("Update include numeric metadata checkbox")
          }

          updateAwesomeCheckbox(
            session = session,
            inputId = "include_numeric_metadata",
            value = session$userData$config()$include_numeric_metadata
            )

          if (dev_mode == TRUE){
            print("Complete")
          }
        })

      ##### 3.7.4.2.3. Designated Genes assay ####
      # Load from config$other_assay_options
      observeEvent(
        session$userData$config(),
        {
          config <- session$userData$config()

          if (isTruthy(config$other_assay_options)){
            if (dev_mode == TRUE){
              print("Update designated genes assay")
            }

            updateSelectInput(
              session = session,
              inputId = "genes_assay",
              choices = all_assays,
              selected = config$other_assay_options$gene_assay
            )

            if (dev_mode == TRUE){
              print("Complete")
            }
          }
        })

      ##### 3.7.4.2.4. Designated ADT assay ####
      observeEvent(
        session$userData$config(),
        {
          # In previous versions of the config app, the ADT assay was stored
          # in assay-specific options, instead of general assay options.
          # (config$assays$asssay_i$designated_adt)
          # Now, it is stored in config$other_assay_options$adt_assay
          config <- session$userData$config()

          if (!is.null(config$other_assay_options$adt_assay)){
            # New version: update select input in main app with the designated
            # ADT assay.
            updateSelectInput(
              session = session,
              inputId = "adt_assay",
              choices = all_assays,
              selected = config$other_assay_options$adt_assay
            )
          } else {
            # Old version: use `designated_adt` field in list of assays
            # Loop through each assay in the config file
            for (assay in names(config$assays)){
              # If the designated adt field is TRUE for an assay, update the
              # new select input with the name of the assay selected
              if (isTruthy(config$assays[[assay]]$designated_adt)){

                updateSelectInput(
                  session = session,
                  inputId = "adt_assay",
                  choices = c("none", all_assays),
                  selected = assay
                )

                # break the for loop
                break
              }
            }
          }
        })

      #### 3.7.4.3. Metadata Tab ####
      ##### 3.7.4.3.1 Metadata selected ####
      observeEvent(
        session$userData$config(),
        {
          if (dev_mode == TRUE){
            print("Update selected metadata")
          }

          # Set selected vs. not selected metadata variables using the
          # information in the loaded file.
          # sortable inputs will update when the values below change.
          module_data$metadata_sortable_selected <-
            # Selected metadata: equal to the names of the metadata list in the
            # config file. The order of the sortable will reflect the order of
            # the metadata categories in the config file
            session$userData$config()$metadata |>
            names()

          # Non-selected metadata: all non-numeric metadata columns that are
          # not in the loaded file
          module_data$metadata_sortable_not_selected <-
            non_numeric_cols[
              !non_numeric_cols %in% module_data$metadata_sortable_selected
            ]

          # Explicitly trigger update of menus (menus ordinarially do not
          # respond the second time a config file is loaded because
          # module_data$metadata_sortable_selected and
          # module_data$metadata_sortable_not_selected do not change
          # in this case).
          update_metadata_sortable$trigger()

          if (dev_mode == TRUE){
            print("Complete")
            }
        })

      ##### 3.7.4.3.2 Patient level meteadata variable ####
      observeEvent(
        session$userData$config(),
        {
          # Fetch metadata variable used for determining
          # patient|sample level metadata
          config <- session$userData$config()
          sample_var <- config$other_metadata_options$patient_colname

          # Update menu with variable in config file, if it exists
          if (isTruthy(sample_var)){
            if (dev_mode == TRUE){
              print("Update patient/sample level metadata variable")
            }

            updateSelectInput(
              session = session,
              inputId = "patient_colname",
              selected = sample_var
              )

            if (dev_mode == TRUE){
              print("Complete")
            }
          }
        })

      #### 3.7.4.4 Reductions selected ####
      observeEvent(
        session$userData$config(),
        {
          if (dev_mode == TRUE){
            print("Update selected reductions")
          }

          # Set selected vs. not selected reductions using the information
          # in the loaded file. Also sets the order of reductions selected.
          module_data$reductions_sortable_selected <-
            session$userData$config()$reductions |>
            names()

          # Non-selected reductions (any reductions not in the config file)
          module_data$reductions_sortable_not_selected <-
            reductions[
              !reductions %in% module_data$reductions_sortable_selected
            ]

          # Explicitly trigger update of menus (menus ordinarially do not
          # respond to loading the config file a second time because
          # module_data$reductions_sortable_selected and
          # module_data$reductions_sortable_not_selected do not change
          # in this case).
          update_reductions_sortable$trigger()

          if (dev_mode == TRUE){
            print("Complete")
            }
          })

      #### 3.7.4.5 ADT threshold table ####
      observeEvent(
        session$userData$config(),
        {
          # Set the threshold table in the app equal to the table recorded
          # in the file being loaded
          module_data$threshold_data <-
            session$userData$config()$adt_thresholds
        })

      warning_modal_server(
        id = "warning",
        reactive_trigger = reactive({input$warning_modal})
      )

      ## 3.8. Dev mode: Show server values ####
      ### 3.8.1. Full config file ####
      if (dev_mode == TRUE){
        # Show all selected options when app is started in dev mode
        output$print_data <-
          renderPrint({
            output_list <-
              lapply(
                names(config_data),
                function(element){
                  if (is.reactive(config_data[[element]])){
                    config_data[[element]]()
                  } else {
                    config_data[[element]]
                  }
                }
              )

            names(output_list) <- names(config_data)

            output_list
          })
      }

      ## 3.8.2. Server value of metadata_selected ####
      output$metadata_sortable_debug <-
        renderPrint({
          print("module_data$metadata_sortable_selected")
          print(module_data$metadata_sortable_selected)

          print("module_data$metadata_sortable_not_selected")
          print(module_data$metadata_sortable_not_selected)
        })

      #### TEMP: Observers for Debugging ####
      # observe({
      #   print("Config file exists:")
      #   print(!is.null(session$userData$config()))
      #   print("Contents")
      #   print(session$userData$config())
      # })

      # observeEvent(
      #   input$load_config,
      #   ignoreNULL = FALSE,
      #   {
      #     print("Load Config Button")
      #   })

    }

    shinyApp(ui, server)
  }
