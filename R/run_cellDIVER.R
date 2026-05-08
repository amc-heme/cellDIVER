#' cellDIVER app
#'
#' Initializes the main cellDIVER app.
#' 
#' For more information on setting up an cellDIVER deployment, see the [dataset setup guide](https://amc-heme.github.io/scExploreR/articles/dataset_setup_walkthrough.html) on our website.
#'
#' @param browser_config path to a YAML config file giving browser specific settings. For more info on generating this config file, see [here](https://amc-heme.github.io/scExploreR/articles/dataset_setup_walkthrough.html#create-an-app-config-file).
#' @param object_path path to a single-cell object to be configured. Currently, 
#' Seurat, SingleCellExperiment, and anndata objects are supported. For 
#' SingleCellExperiment objects with using HDF5 disk-backed storage via 
#' [HDF5Array], `object_path` should be a path to the directory containing 
#' the se.rds and assays.h5 files for the object.
#' @param config_path path to the config file for the single-cell object. This is generated either in the config app via `run_config_app`, or by auto-generating a config file via `generate_config_yaml` and editing it by hand. For more information on using the config app, see [here](https://amc-heme.github.io/scExploreR/articles/docker.html#step-4).
#' @param enable_metadata_addition when TRUE, users may interactively add metadata to objects in the app. This feature is currently unstable, so it must be opted in to by setting this value to TRUE.
#' @param port specify a port for launching the browser. This is optional for a single deployment but required to run several instances of the browser at the same IP address. The port can be any number between 3000:8000, except for ports blocked by Google Chrome (for more information on this, see \link[shiny]{runApp}).
#' @param host This is passed to `shiny::runApp`. See the documentation of `host` in \link[shiny]{runApp} for more info.
#' @param launch_browser This is passed to `shiny::runApp` as `launch.browser`. See the documentation of `launch.browser` in \link[shiny]{runApp} for more info.
#' @param full_stack_trace when TRUE, the full stack trace of errors is logged to the console (FALSE by default). 
#' @param dev_mode used for development and debugging. When this is TRUE, additional logging occurrs, and the status of the app is printed in the UI. This is not reccomended to be used outside of development. 
#' 
#' @usage
#' # Option 1: Single-object deployment with 
#' # an object and an object config file
#' run_cellDIVER(
#'  object = "path_to_object",
#'  config_file = "path_to_config_file.yaml"
#'  )
#' 
#' # Option 2: Multi-object deployment with
#' # Browser config file with paths to object, 
#' # config files for any number of objects
#' #  
#' run_cellDIVER(
#'  browser_config = "path_to_browser_config_file.yaml"
#'  )
#'
#' @export
run_cellDIVER <-
  function(
    browser_config = NULL,
    object_path = NULL,
    config_path = NULL,
    enable_metadata_addition = FALSE,
    object_description_path = NULL,
    contact_info = 
      list(
        name = NULL,
        email = NULL,
        slack_channel = NULL,
        other = NULL
        ),
    deployment_name = NULL,
    port = NULL,
    host = NULL,
    launch_browser = NULL,
    full_stack_trace = FALSE,
    dev_mode = FALSE
  ){
    # Load Libraries and Data ------------------------------------------------------
    ## Initialize libraries ####
    library(shiny)
    library(shinymanager)
    library(Seurat, quietly = TRUE, warn.conflicts = FALSE)

    #library(SCUBA, quietly = TRUE, warn.conflicts = FALSE)

    # Shiny add-ons
    library(shinyWidgets, quietly = TRUE, warn.conflicts = FALSE)
    library(rintrojs, quietly = TRUE, warn.conflicts = FALSE)
    library(shinydashboard, quietly = TRUE, warn.conflicts = FALSE)
    library(waiter, quietly = TRUE, warn.conflicts = FALSE)
    # shinycssloaders: withSpinner() is called bare (without ::) throughout
    # the UI after this library() call attaches it to the search path.
    library(shinycssloaders, quietly = TRUE, warn.conflicts = FALSE)
    library(shinyjs, quietly = TRUE, warn.conflicts = FALSE)
    library(sortable, quietly = TRUE, warn.conflicts = FALSE)
    library(shinyBS, quietly = TRUE, warn.conflicts = FALSE)

    # Reactlog (for debugging): optional dev dependency, listed in Suggests.
    # Enables the reactive graph visualizer at /reactlog when the app is running.
    if (requireNamespace("reactlog", quietly = TRUE)) {
      library(reactlog, quietly = TRUE, warn.conflicts = FALSE)
    }
    options(
      shiny.reactlog = TRUE,
      shiny.fullstacktrace = full_stack_trace
    )

    # Logging and performance monitoring
    # profvis is an optional dev dependency listed in Suggests.
    if (requireNamespace("profvis", quietly = TRUE)) {
      library(profvis, quietly = TRUE, warn.conflicts = FALSE)
    }
    library(rlog, quietly = TRUE, warn.conflicts = FALSE)

    # Tidyverse packages
    #library(tidyverse, quietly = TRUE, warn.conflicts = FALSE)
    library(stringr, quietly = TRUE, warn.conflicts = FALSE)
    library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
    library(ggplot2, quietly = TRUE, warn.conflicts = FALSE)
    library(glue, quietly = TRUE, warn.conflicts = FALSE)
    library(DT, quietly = TRUE, warn.conflicts = FALSE)

    # Plotting
    library(RColorBrewer, quietly = TRUE, warn.conflicts = FALSE)
    library(viridisLite, quietly = TRUE, warn.conflicts = FALSE)
    library(colourpicker, quietly = TRUE, warn.conflicts = FALSE)
    library(ggsci, quietly = TRUE, warn.conflicts = FALSE)
    library(scales, quietly = TRUE, warn.conflicts = FALSE)
    library(patchwork, quietly = TRUE, warn.conflicts = FALSE)
    library(cowplot, quietly = TRUE, warn.conflicts = FALSE)


    # Other packages
    library(yaml, quietly = TRUE, warn.conflicts = FALSE)
    library(rlang, quietly = TRUE, warn.conflicts = FALSE)
    library(gtools, quietly = TRUE, warn.conflicts = FALSE)

    # Check inputs to run_cellDIVER ####
    # contact info for admin
    # Check that the contact info is a list
    if (!is.list(contact_info)){
      stop(
        paste0(
          "The parameter `contact_info` must be a list. Enter `?run_cellDIVER` 
          in the console for more information."
          )
        )
    }
    
    # Check that all elements in contact_info are character vectors, of length 1
    invalid_elems <- 
      sapply(
        contact_info,
        function(elem){
          # Elements not matching the required format will be marked as TRUE
          !(is.null(elem) || (class(elem) == "character" && length(elem) == 1))
        }
      )
    
    # If any elements are invalid, return error to user with the 
    # invalid elements
    if (any(invalid_elems)){
      stop(
        paste0(
          "Invalid input in `contact_info`. The following elements in ",
          "`contact_info` are not valid: ", 
          # List all invalid elements, separated by a comma
          paste(names(invalid_elems[invalid_elems == TRUE]), collapse = ", "), 
          ". \n",
          "All elements must be character vectors of length 1."
        )
      )
    }
    
    # Add Resource Paths ####
    # Add inst/www to the resource path so static html can be loaded
    addResourcePath("resources", system.file("www", package = "cellDIVER"))

    # For html files generated in-app (such as auto-generated object dictionary)
    # The auto-gemerated dictionary was previously created in "./resources",
    # in the local file structure, and was therefore not accessible in the app
    # (. is whatever directory the app was launched from in this case)
    addResourcePath("session_files", tempdir())

    # Path to Rmd file used to generate data dictionary
    auto_dictionary_markdown_path <-
      system.file(
        "www/Auto_Dictionary.Rmd",
        package = "cellDIVER"
        )
    #Copying to temp file in order to comply with Posit Connect security rules
    temp_markdown_path <- tempfile(fileext = ".Rmd")
    file.copy(auto_dictionary_markdown_path, temp_markdown_path, overwrite = TRUE)
    auto_dictionary_markdown_path <- temp_markdown_path

    # Path to string subsetting documentation (passed to subset
    # selections module)
    string_subsetting_href <-
      file.path(
        "resources",
        "advanced_subsetting_documentation.html"
      )

    # Full documentation
    full_documentation_path <-
      file.path(
        "resources",
        "cellDIVER_all_features.html"
      )

    # Determine App mode -------------------------------------------------------
    # Three app modes, depending on parameters specified.
    browser_mode <- FALSE
    local_mode <- FALSE
    local_single_object_mode <- FALSE

    if (is.null(browser_config) & !is.null(object_path) & !is.null(config_path)){
      # Local mode with one object: when only the object_path and the config_path are specified.
      local_single_object_mode <- TRUE
    } else if (!is.null(browser_config) & is.null(object_path) & is.null(config_path)){
      # Browser mode (original app): when only a config file is passed
      browser_mode <- TRUE
    } else if (is.null(browser_config) & is.null(object_path) & is.null(config_path)){
      # When no arguments are defined, run a local version with an interface
      # for picking the object and config file.
      local_mode <- TRUE
    } else {
      stop(
        "Invalid combination of defined parameters for browser_config, object_path, and config_path. See ?run_cellDIVER for more details."
        )
    }

    # Load CSS, JavaScript, and R scripts --------------------------------------
    # Load functions in ./R directory
    # # Get list of files
    # source_files <-
    #   list.files(
    #     path = "./R",
    #     # Pattern, any set of characters, followed by ".R"
    #     # Period is double escaped
    #     pattern=".*\\.R",
    #     full.names=TRUE,
    #     ignore.case=TRUE
    #     )
    #
    # # Load .R files in modules directory
    # source_files <-
    #   c(source_files,
    #     list.files(
    #       path = "./Modules",
    #       # Pattern, any set of characters, followed by ".R"
    #       # Period is double escaped
    #       pattern = ".*\\.R",
    #       full.names = TRUE,
    #       ignore.case = TRUE
    #       )
    #     )
    #
    # # Use source() to import files into R
    # sapply(source_files, source)

    # Load CSS files for app: CSS files are defined and each file is converted to a
    # <script> tag using includeCSS(). Each tag defined is passed to a list, which
    # is included in the main UI function.
    # Get list of .css files in www/ directory
    css_files <-
      list.files(
        path = system.file("css", package = "cellDIVER"),
        pattern=".*\\.css",
        full.names=TRUE,
        ignore.case=TRUE
      )

    # Create list of style tags for each CSS file
    css_list <- lapply(css_files, includeCSS)

    # Load Javascript files for app: find all .js files in www/ directory
    # and create a list of script() tags using includeScript().
    # Get list of .js files in www/ directory
    js_files <-
      list.files(
        path = system.file("js", package = "cellDIVER"),
        # Regex: uses \\. to select for files ending in ".js".
        # Pattern arguments require double backslashes for eacape
        # characters to work (R and regex use the same string
        # character)
        pattern = ".*\\.js",
        full.names = TRUE,
        ignore.case = TRUE,
        include.dirs = FALSE
      )
    # Create list of style tags for each CSS file
    js_list <- lapply(js_files, includeScript)

    # Read browser config yaml (if in browser mode)
    if (browser_mode){
      browser_config <-
        read_yaml(browser_config)
    }


    # Non-reactive Global Variables --------------------------------------------

    # Non-zero proportion threshold: if the proportion of cells for a
    # gene is below this threshold, return a warning to the user.
    nonzero_threshold <- 0.10

    # Display name for thresholded ADT features in the feature entry dropdown
    adt_threshold_dropdown_title <- NULL

    ## Color palettes for plotting categorical variables ####
    # Define available palettes

    # CU Palette
    # Created using official CU colors, along with other colors symbolic of CU
    cu_extended <-
      colors <- c(
        # Uses three of the four official CU colors
        # First four: official CU Colors
        "#CFB87C",
        "#000000",
        "#565A5C",
        #"#A2A4A3",
        # Color of walls on CU Boulder campus buildings
        "#AD7B64",
        # Color of CU Boulder campus roofs
        "#9A4A3A",
        # Color of pine trees in Flatirons
        "#3A553A",
        # Color of the sunny Colorado sky
        "#529FDF"
      )

    # Palettes are from ggsci, a package with palettes inspired by
    # scientific journals and science fiction
    # Package by Nan Xiao (https://nanx.me/ggsci/articles/ggsci.html)
    categorical_palettes <-
      list(
        # Default Palette used by Seurat (from ggplot2)
        `default` = hue_pal()(8),
        # D3 (20-color palette)
        `D3` = pal_d3("category20")(20),
        # LocusZoom
        `LocusZoom` = pal_locuszoom()(7),
        # Stepped (from Seurat::DiscretePalette)
        `stepped` = DiscretePalette(20, palette = "stepped"),
        # Rick and Morty (yes, that Rick and Morty) ;)
        `Rick and Morty` = pal_rickandmorty("schwifty")(12),
        # Star Trek
        `Star Trek` = pal_startrek()(7),
        # CU palette
        `CU_Palette` = cu_extended,
        # Journal of Clinical Ontology
        `JCO` = pal_jco()(10),
        # Lancet
        `Lancet` = pal_lancet()(9)
      )

    # Continuous palettes
    # Taken from viridisLite package, created by Simon Garnier
    # https://sjmgarnier.github.io/viridisLite/
    continuous_palettes <-
      list(
        # Seurat Default: two colors
        `default` = c("lightgrey", "blue"),
        # viridisLite palettes
        `inferno` = inferno(42, direction = -1),
        `plasma` = plasma(42, direction = -1),
        `rocket` = rocket(42, direction = -1),
        `viridis` = viridis(42, begin = 0.1, direction = -1),
        `cividis` = cividis(42, direction = -1),
        `mako` = mako(42, direction = -1)
      )

    # Blend palettes: for blended feature plots
    # First color is for low expression of both features (default "lightgrey")
    # Subsequent colors are for expression of each of the two features
    blend_palettes <-
      list(
        "RdBu" = c("lightgrey", "#FF0000", "#0000FF"),
        "GnBu" = c("lightgrey", "#00FF00", "#0000FF"),
        "RdGn" = c("lightgrey", "#FF0000", "#00FF00"),
        # Blue and orange (co-expression is pink)
        "BuOr" = c("lightgrey", "#1003FF", "#F76A0D"),
        # Red-green palette, with co-expression visible at a lower
        # threshold for both features
        "RdGn_accent" = c("lightgrey", "#FF1A1A", "#1AFF1A"),
        # Dark purple and dark yellow make peach when blended
        "DkPuDkYl" = c("lightgrey", "#55035C", "#CFAB19"),
        # Dark putple + dark red-orange -> pink blend
        "DkPuDkRd" = c("lightgrey", "#7A2180", "#9E2525")
      )

    # Error Handling: define possible errors -----------------------------------
    # Errors are defined in a list using the functions in "./R/error_handling.R".
    # The error_handler() function is executed in a tryCatch() statement and checks
    # the error message returned against a list of errors.

    ## List of errors ####
    # A multi-level list with sub-lists of errors for specific context.
    # Different sub-lists are used in different tryCatch statements
    error_list <-
      list(
        `dge_test_errors` = 
          list(
            error_data(
              message = "Selected subset has 0 marker groups.",
              notification_ui =
                icon_notification_ui(
                  icon_name = "skull-crossbones",
                  tagList(
                    paste0(
                      "There are 0 groups present for the metadata variable ",
                      "selected for marker groups in the subset selected."
                      )
                    ) #End tagList
                ), # End icon_notification_ui
              notification_id = "dge_error_1"
            ), # End error_data
            error_data(
              message = "Selected subset has only 1 marker group.",
              notification_ui =
                icon_notification_ui(
                  icon_name = "skull-crossbones",
                  tagList(
                    paste0(
                      "The metadata variable selected for marker ",
                      "identification has only one group in the subset ",
                      "selected. Please verify you have chosen a variable ",
                      "with more than one group and try the test again. ",
                      "(To see the number of groups for a metadata variable, ",
                      "you can use the group by selection menu for a ",
                      "DimPlot. There should be more than one color ",
                      "on the plot.)"
                      )
                    ) #End tagList
                ), # End icon_notification_ui
              notification_id = "dge_error_2"
            )
          ),
        `subset_errors` =
          list(
            # error_data(
            #   message = "cannot allocate vector of size",
            #   notification_ui =
            #     icon_notification_ui(
            #       icon_name = "skull-crossbones",
            #       tagList(
            #         "Memory Error: RAM is insufficient for analyzing the specified
            #     subset. Please narrow down the subset scope using the
            #     restriction criteria to the left, and feel free to",
            #     github_link(display_text = "let us know"),
            #     " ", # Space after link
            #     "if you repeatedly recieve this error.") #End tagList
            #     ), # End icon_notification_ui
            #   notification_id = "subset_error_1"
            # ), # End error_data

            # Error 2: Vector memory exhausted
            # error_data(
            #   message = "vector memory exhausted",
            #   notification_ui =
            #     icon_notification_ui(
            #       icon_name = "skull-crossbones",
            #       "Error: vector memory exhausted. If this issue persists, please ",
            #       github_link("contact us"),
            #       " with a screenshot of the response criteria selected. For now,
            #   narrowing down the subset criteria may resolve the error."
            #     ), # End icon_notification_ui
            #   notification_id = "subset_error_2"
            # ),

            # Error 3: No Cells in Subset
            error_data(
              message = "No cells found",
              notification_ui =
                icon_notification_ui(
                  icon_name = "skull-crossbones",
                  "No cells were found matching the defined subset criteria. Please
              check the subset dropdowns for mutually exclusive selections. If
              you the error persists for combinations that should be valid,
              please ",
              github_link("contact us"),
              # Period at end of link
              "."
                ), # End icon_notification_ui
              notification_id = "subset_error_3"
            ),

            # Error 4: User-defined subset string has unclosed parentheses
            error_data(
              message = "unexpected end of input",
              notification_ui =
                icon_notification_ui(
                  icon_name = "skull-crossbones",
                  "Invalid format for string subsetting entry. Please check that all
              opened parentheses have been closed and try again. If the issue
              persists, please email us with the following information:",
              tags$br(),
              "1. The entry in the string subsetting text box",
              tags$br(),
              "2. The desired subset, or question that prompted the selection of
              this subset"
                ),
              notification_id = "subset_error_4"
            ),

            # Error 5: User-defined subset string has incomplete string
            error_data(
              message = "INCOMPLETE_STRING",
              notification_ui =
                icon_notification_ui(
                  icon_name = "skull-crossbones",
                  "String subsetting entry has an incomplete string. Please make
              sure all opening quotation marks have a matching closing quotation
              and try again. If the issue persists, please email us with the
              following information:",
              tags$br(),
              "1. The entry in the string subsetting text box",
              tags$br(),
              "2. The desired subset, or question that prompted the selection of
              this subset"
                ),
              notification_id = "subset_error_5"
            ),

            # Error 6: User-defined subset string uses improper formatting (generic)
            error_data(
              message = "unexpected",
              notification_ui =
                icon_notification_ui(
                  icon_name = "skull-crossbones",
                  "Invalid format for string subsetting entry. Please check your
              entry and try again. If the issue persists, please email us with
              the following information:",
              tags$br(),
              "1. The entry in the string subsetting text box",
              tags$br(),
              "2. The desired subset, or question that prompted the selection of
              this subset"
                ),
              notification_id = "subset_error_6"
            )
          ) # End subset error sub-list
      )# End list of error definitions

    # Initialize app based on mode ####
    # (broswer mode, single object mode)
    
    ## Define Datasets ####
    log_info("R process initialization: loading datasets")

    # Construct list of datasets using config file provided by user
    datasets <-
      if (browser_mode){
        browser_config$datasets
      } else if (local_single_object_mode) {
        # For local mode with an object path and the config file specified,
        # create a one-object list with the object and a config file
        list(
          `object` =
            list(
              `object` =
                object_path,
              `config` =
                config_path,
              `object_description` = 
                object_description_path
              )
          )
      } else if (local_mode) {
        list()
      }

    ## Compile info on app admin, deployment name ####
    admin_info <-
      if (browser_mode){
        list(
          `name` = 
            if (!is.null(browser_config$admin$name)){
              browser_config$admin$name
              } else NULL,
          `email` =
            if (!is.null(browser_config$admin$email)){
              browser_config$admin$email
            } else NULL,
          `slack_channel` =
            if (!is.null(browser_config$admin$slack_channel)){
              browser_config$admin$slack_channel
            } else NULL,
          `other` =
            if (!is.null(browser_config$admin$other)){
              browser_config$admin$other
            } else NULL
        )
      } else if (local_single_object_mode){
        # Single-object apps: get contact info from list
        list(
          `name` = contact_info$name,
          `email` = contact_info$email,
          `slack_channel` = contact_info$slack_channel,
          `other` = contact_info$other
          )
      } else if (local_mode){
        NULL
      }
    
    deployment_name <-
      if (browser_mode){
        # Multi-object deployment: use deployment_name property of 
        # browser config file
        browser_config$deployment_name
      } else if (local_single_object_mode){
        # Single-object mode: use deployment name parameter
        deployment_name
      } else if (local_mode){
        NULL
      }

    ## Load Datasets ####
    # Objects must be loaded at startup. If they are loaded separately for each
    # user, the RAM will quickly be exhausted.
    # Each dataset is loaded below. The "object" variable in the YAML file is a
    # path to the dataset, and the corresponding "object" element in the R list will
    # be replaced with the dataset itself.
    for (data_key in names(datasets)){
      # Load config files first
      path <- datasets[[data_key]]$config

      # Add informative error message when a non-yaml config file is loaded
      if (!grepl("\\.yaml$", tolower(path))){
        stop("Only .yaml config files are supported as of version v0.5.0.
         Existing .rds config files can be converted to .yaml files by
         loading them into config_app.R and then re-saving as a .yaml file.")
        }

      # Load config YAML using defined path (file is converted to an R list)
      config_r <- read_yaml(path)

      # Convert the "adt_thresholds" section to a tibble (when converting from R to
      # YAML, tibble formats are converted to a YAML format that generates a named
      # list when loading back to R)
      if (isTruthy(config_r$adt_thresholds)){
        config_r$adt_thresholds <-
          as_tibble(config_r$adt_thresholds)
      }

      # Determine if the object in the config file is a SingleCellExperiment
      # object saved using the HDF5Array package (different loading code is
      # required in this case)
      is_HDF5SummarizedExperiment <-
        # Fetch is_HDF5SummarizedExperiment from the selected object's
        # config entry
       config_r$is_HDF5SummarizedExperiment
      
      # Set prefix for HDF5 SingleCellExperiment files
      # The default for the `prefix` arg to loadHDF5SummarizedExperiment
      # is an empty string "" 
      HDF5_prefix = ""
      if ("HDF5_prefix" %in% names(config_r)) {
        HDF5_prefix <- config_r$HDF5_prefix
      }
        
      # Store config file in datasets
      datasets[[data_key]]$config <- config_r

      # Load object: different loading code for SingleCellExperiment objects
      # saved via HDF5 storage
      if (isTruthy(is_HDF5SummarizedExperiment)){
        datasets[[data_key]]$object <-
          tryCatch(
            error =
              function(cnd){
                cat("Error when loading SCE object \n")
                #print(sys.calls(), sep = "\n")
                print(cnd)
                
                stop(
                  "There was an error loading the object at path\n",
                  datasets[[data_key]]$object,
                  ": \n\n",
                  "Specific error:\n",
                  cnd,
                  "\nSuggestions:\n",
                  "Loading was attempted via `HDF5Array::loadHDF5SummarizedExperiment`.",
                  "Please check that the config file for the object corresponds ",
                  "to the object.",
                  "\n",
                  "For HDF5-enabled SingleCellExperiment objects, the object ",
                  "path should be set to the folder containing the assays.h5 ",
                  "and the se.rds file. ",
                  "This is the same folder that was created ",
                  "when the object was saved via `HDF5Array::saveHDF5SummarizedExperiment()`. ",
                  "A prefix for these files can be supplied in",
                  "the config app using the HDF5_prefix argument",
                  "\n",
                  "If this object is not an HDF5-enabled SingleCellExperiment ",
                  "object, please ensure that is_HDF5SummarizedExperiment is set ",
                  "to `FALSE` when run_config_app() is called."
                  )
              },
            {
              HDF5Array::loadHDF5SummarizedExperiment(
                datasets[[data_key]]$object,
                prefix = HDF5_prefix
                )
            })
      } else {
        # All other objects: load based on file extension
        path <- datasets[[data_key]]$object

        # Convert to lowercase to detect different capitalizations of the
        # same extension (.Rds or .rds)
        extension <-
          tolower(
            tools::file_ext(path)
            )

        if (extension == "rds") {
          datasets[[data_key]]$object <-
            tryCatch(
              error =
                function(cnd){
                  stop(
                    "There was an error loading the object at path \n",
                    datasets[[data_key]]$object,
                    ": \n\n",
                    "Specific error:\n",
                    cnd,
                    "\nSuggestions:\nLoading was attempted via `readRDS()`. \n",
                    "Please check that the config file for the object corresponds \n",
                    "to the object, and that the object path points to a .rds file.\n",
                    "\n",
                    "If an object is an HDF5-enabled SingleCellExperiment\n",
                    "object, the config file must specify this. This is done\n",
                    "automatically for files produced using cellDIVER version\n",
                    "0.7.0. and later. To update an older config file, call\n",
                    "run_config_app() using the object and the config file,\n",
                    "with `isHDF5SummarizedExperiment` set to `TRUE`. Load the\n",
                    "config file in the config app, and then save the file."
                  )
                },
              {
                # Define path and load object
                path <- datasets[[data_key]]$object

                object <-
                  readRDS(
                    path
                  )

                # Check object for valid classes
                check_dataset(
                  object,
                  path = path
                )

                # Return object from TryCatch statement
                object
              })
        } else if (extension == "h5ad") {
          # .h5ad anndata::read_h5ad
          datasets[[data_key]]$object <-
            tryCatch(
              error =
                function(cnd){
                  stop(
                    "There was an error loading the object at path \n",
                    datasets[[data_key]]$object,
                    "\nLoading was attempted via `anndata::read_h5ad()`."
                  )
                },
              {
                # Define path and load object
                path <- datasets[[data_key]]$object

                object <-
                  anndata::read_h5ad(
                    path
                  )

                # Check object for valid classes
                check_dataset(
                  object,
                  path = path
                )

                # Return object from TryCatch statement
                object
              })
        } else if (extension == "h5mu"){
          # MuData objects: import Mudata python package and run read_h5mu
          datasets[[data_key]]$object <-
            tryCatch(
              error =
                function(cnd){
                  stop(
                    "There was an error loading the object at path \n",
                    datasets[[data_key]]$object,
                    "\nLoading was attempted via `read_h5mu` from the Python package."
                  )
                },
              {
                # Define path and load object
                path <- datasets[[data_key]]$object
                
                py_require("mudata>=0.3.1")
                
                md <- reticulate::import("mudata", as = "md", convert = TRUE)
                
                object <- md$read_h5mu(path)
                
                # Check object for valid classes
                check_dataset(
                  object,
                  path = path
                )
                
                # Return object from TryCatch statement
                object
              })
        } else {
          # Unrecognized file extensions
          stop(
            "File extension (.",
            extension,
            "not recognized. Currently supported extensions:\n",
            ".rds, and .h5ad."
            )
        }
      }


    }

    # Tests for location of general dataset info ####
    # Test if the general dataset info is defined in the browser config for all
    # datasets, or in the dataset config file for all. If neither of these is TRUE,
    # throw an error to the config user.
    if (
      !(browser_config_has_info(datasets) | dataset_config_has_info(datasets))
    ){
      stop("Inconsistent format detected for general dataset info (label,
         description, and plot or image). This must be defined in either the
         browser config file for all datasets, or the dataset config file for
         all datasets. If the config file does not have general information
         defined, please load them into the config app and define those fields.
         The browser config file should not have this information.")
    }

    # Also throw an error if data is defined in both the browser config and dataset
    # config files.
    if (browser_config_has_info(datasets) & dataset_config_has_info(datasets)){
      stop('General dataset info is defined both in the browser config file and the
       dataset config files. Please remove the sections "label", "description",
       and "plot" from the browser config file for all datasets, and keep this
       information in the config file for each datset.')
    }

    # And an error if data is in neither source
    if (!browser_config_has_info(datasets) & !dataset_config_has_info(datasets)){
      stop('General dataset info (dataset label, description, and plot or image) is
       not defined for all datasets. For each config file that does not have the
       sections "label", "description", or "preview", load the config file into
       the config app and fill out these fields in the "general" tab (or simply
       re-save them to leave these fields defined in the config file, but as
       empty entries).')
    }

    # Warning if the data is in the browser config but not the dataset config
    if (browser_config_has_info(datasets) & !dataset_config_has_info(datasets)){
      log_warn("The placement of general dataset info (label, description,
           plot/image) in the browser config file is depricated. Please load the
           config files for each dataset into the latest version of the config
           app and add this infomation in the 'general' tab.")
    }

    log_info("Datasets successfully loaded.")
    
    # Table of Contents ------------------------------------------------------------

    # Main UI ----------------------------------------------------------------------
    # Navigation panel and references to tabs
    ui <- tagList(
      # Add CSS from each .css file in the www/ directory
      # Uses a list of style tags defined at startup
      css_list,
      # Introjs UI: for guided tour
      introjsUI(),
      # Waiter UI: spinners
      useWaiter(),
      # Shinyjs: a Shiny JavaScript extension
      useShinyjs(),
      # Import custom ShinyJS functions from js folder
      shinyjs::extendShinyjs(
        script =
          system.file(
            "js/cellDIVER_shinyJS.js",
            package = "cellDIVER"
            ),
        functions =
          c("getTopScroll",
            "setTopScroll")
      ),
      # CSS and JS for collapsible panel
      navbarPage(
        "cellDIVER",
        windowTitle = "cellDIVER",
        position = "fixed-top",
        id = "navigator",
        lang = "en",
        # Information Tab
        tabPanel(
          "Info",
          value = "info",
          sidebarLayout(
            sidebarPanel = 
              sidebarPanel(
                fluid = FALSE,
                # Navigation menu for *information pages*
                tags$label(
                  `for` = "info-nav",
                  "Select a page below to view info:"
                  ),
                tags$nav(
                  # Apply info-nav class to avoid conflicts with the
                  # main navbar
                  id = "info-nav",
                  class = "info-nav",
                  `aria-label` = "Navigation panel for information pages",
                  tags$ul(
                    tags$li(
                      class = "tab-button active",
                      id = "tab_app_info",
                      onClick = "openTab(event, 'tab_content_app_info')",
                      span("General Information")
                    ),
                    tags$li(
                      class = "tab-button",
                      id = "tab_object_info",
                      onClick = "openTab(event, 'tab_content_object_info')",
                      span("Dataset Information")
                    ),
                    tags$li(
                      class = "tab-button",
                      id = "tab_object_metadata",
                      onClick = 
                        "openTab(event, 'tab_content_object_metadata')",
                      span("Dataset Contents")
                    )
                  )
                ),
                # Information on the current deployment, if defined in
                # the browser config file or run_cellDIVER.
                if (any(!sapply(admin_info, is.null)) | 
                    !is.null(deployment_name)){
                  collapsible_panel(
                    inputId = "admin_info",
                    label = "Deployment Info",
                    active = TRUE,
                    if (!is.null(deployment_name)){
                      div(
                        tags$b("Deployment Name:"),
                        deployment_name
                        )
                      },
                    # Deployment admin information 
                    if (any(!sapply(admin_info, is.null))){
                      div(
                        class = "half-space-top",
                        div(
                          tags$b(
                            "Admin Contact Info"
                            )
                          ),
                        if (!is.null(admin_info$name)){
                          div(
                            class = "overflow-cutoff",
                            tags$b("Name:"),
                            admin_info$name
                          )
                        },
                        if (!is.null(admin_info$email)){
                          div(
                            class = "overflow-cutoff",
                            tags$b("Email:"),
                            admin_info$email
                          )
                        },
                        if (!is.null(admin_info$slack_channel)){ 
                          div(
                            tags$a(
                              href = admin_info$slack_channel,
                              target = "_blank",
                              rel = "noopener noreferrer",
                              class = "slack-link",
                              icon(name = "slack"),
                              "Leave Feedback on Slack"
                              )
                            )
                          }
                        )
                      }
                    )
                  }
                ),
            mainPanel = 
              mainPanel(
                id = "info_tab_main",
                div(
                  class = "tab-window active",
                  id = "tab_content_app_info",
                  # App info page
                  # Welcome statement
                  tags$h1("Welcome to cellDIVER!"),
                  includeMarkdown(
                    system.file(
                      "www",
                      "intro_page.qmd",
                      package = "cellDIVER"
                      )
                    )
                  ),
                div(
                  class = "tab-window",
                  id = "tab_content_object_info",
                  uiOutput(
                    outputId = "object_info_page"
                    )
                  ),
                div(
                  class = "tab-window",
                  id = "tab_content_object_metadata",
                  object_contents_info_ui(
                    id = "object_contents_info_tab"
                    )
                  )
                )
              )
          ),
        # Plots Tab
        tabPanel(
          "Plots",
          value = "plots",
          uiOutput(
            outputId = "plots_dynamic_ui"
            )
          ),
        # Differential Expression Tab
        tabPanel(
          "Differential Expression",
           value = "dge",
           uiOutput(
             outputId = "dge_dynamic_ui"
             )
           )#,
        # tabPanel(
        #   "Gene Correlations",
        #   value = "corr",
        #   uiOutput(
        #     outputId = "corr_dynamic_ui"
        #   )
        # )
      ), # End navbarPage()

      ## Buttons on upper-right hand corner of app ---------------------------------
      # Help button - Creates a Dropdown menu when clicked
      # Button should appear in the upper right hand corner of the navbar menu
      # This will be achieved with the button_wizzard.js script
      dropdownButton(
        inputId = "help",
        status = "info",
        right = TRUE,
        label = "",
        size = "sm",
        icon = icon("question"),
        # Dropdown menu content
        div(
          # Centers all elements in the dropdown menu
          class = "center",
          # Header
          tags$p(
            "Help and Background",
            style =
              "color: #888888;
              margin-bottom: 0px;
              font-size: 1.17em;"
              ),

          # Link to auto-generated object dictionary
          # dictionary is created at a temp file, so href must reactively point
          # to the file
          uiOutput(
            outputId = "auto_dictionary_link"
            ),

          # Interpreting scRNA-seq plots
          tags$a(
            "Single-Cell Visualizations",
            href =
              paste0(
                "https://amc-heme.github.io/scExploreR/articles/",
                "scRNA_Plots_Explained.html"
                ),
              # file.path(
              #   "resources",
              #   "scRNA_Plots_Explained.html"
              # ),
            class = "blue_hover",
            # Opens link in new tab
            target = "_blank",
            # Cybersecurity measure for links that
            # open in new tab: prevents tabnapping
            rel = "noopener noreferrer"
            ),

        # Tutorial Document
        tags$a(
          "Tutorial Vignette",
          href =
            "https://amc-heme.github.io/scExploreR/articles/tutorial.html",
            # file.path(
            #   "resources",
            #   "cellDIVER_Tutorial.html"
            # ),
          class = "blue_hover",
          # Opens link in new tab
          target = "_blank",
          rel = "noopener noreferrer"
          ), # End link

        # Full Feature Documentation
        tags$a(
          "Full Documentation",
          href = 
            paste0(
              "https://amc-heme.github.io/scExploreR/articles/",
              "full_documentation.html"
              ),
            #full_documentation_path,
          class = "blue_hover",
          # Opens link in new tab
          target = "_blank",
          rel = "noopener noreferrer"
          ), # End link

        # File issue on github
        tags$a(
          "Report a Bug",
          href = "https://github.com/amc-heme/scExploreR/issues",
          class = "blue_hover",
          # Opens link in new tab
          target = "_blank",
          rel = "noopener noreferrer"
          ),

        # Link to Genecards
        tags$a(
          "GeneCards",
          href = "https://www.genecards.org/",
          class = "blue_hover",
          target = "_blank",
          rel = "noopener noreferrer"
          ),
        
        # Version of cellDIVER
        tags$p(
            class = "small",
            style = "color: #888888AA;",
            paste0("version ", packageVersion("cellDIVER"))
          )
        ) # End tagList
      ), #End Help Button

      # Dataset Button
      dropdownButton(
        inputId = "options",
        status = "info",
        right = TRUE,
        label = "",
        size = "sm",
        icon = icon("ellipsis-h"),
        div(
          class = "center",
          actionLink(
            inputId = "open_dataset_window",
            label = "Choose Dataset",
            class = "blue_hover"
            ),
          # Add metadata: only available when explicitly enabled via the 
          # browser config file or run_cellDIVER.
          if (enable_metadata_addition == TRUE){
            actionLink(
              inputId = "new_metadata_modal",
              label = "Add Metadata to Object",
              class = "blue_hover"
              )
            }
          )
        ),

      # Include list of scripts built from .js files in www/ directory
      js_list
    )

    # Main Server function ---------------------------------------------------------
    server <- function(input, output, session){
      # Record whether app is in dev mode (signals modules to display more 
      # information for testing and development)
      session$userData$dev_mode = dev_mode

      # *Filename* for storing the object dictionary (goes in tempdir())
      auto_dictionary_filename <-
        paste0(
          "auto_dictionary_",
          session$token,
          ".html"
          )

      # *Path* used to access object dictionary
      auto_dictionary_path <-
        paste0(
          "session_files/",
          auto_dictionary_filename
        )

      # Define spinner to display over main screen when the object and config files
      # are loading
      app_spinner <-
        Waiter$new(
          # When the ID is null, the waiter is applied to the
          # <body> element (entire app)
          id = NULL,
          html =
            tagList(
              spin_loaders(id = 2, color = "#555588"),
              div(
                class = "spinner_text",
                "Loading dataset, please wait...")
            ),
          color = "#FFFFFF",
          #Gives manual control of showing/hiding spinner
          hide_on_render = FALSE
        )

      render_UI_spinner <-
        Waiter$new(
          # When the ID is null, the waiter is applied to the
          # <body> element (entire app)
          id = NULL,
          html =
            tagList(
              spin_loaders(id = 2, color = "#555588"),
              div(
                class = "spinner_text",
                "Updating menus to match new dataset...")
            ),
          color = "#FFFFFF",
          #Gives manual control of showing/hiding spinner
          hide_on_render = FALSE
        )
      
      # Spinner with a generic message that displays over the full screen
      full_page_spinner <-
        Waiter$new(
          # When the ID is null, the waiter is applied to the
          # <body> element (entire app)
          id = NULL,
          html =
            tagList(
              spin_loaders(id = 2, color = "#555588"),
              div(
                class = "spinner_text",
                "Loading, please wait...")
            ),
          color = "#FFFFFF",
          #Gives manual control of showing/hiding spinner
          hide_on_render = FALSE
        )
      
      object_info_page_spinner <-
        Waiter$new(
          id = "tab_content_object_info",
          html =
            tagList(
              spin_loaders(id = 2, color = "#555588"),
              div(
                class = "spinner_text",
                "Loading information page for dataset...")
            ),
          color = "#FFFFFF",
          #Gives manual control of showing/hiding spinner
          hide_on_render = FALSE
          )

      # Create a reactive trigger to run the feature text box update after
      # the UI is created (originally the UI always ran first, but now it does
      # not update until the correlation tab is opened since it is now a
      # reactive and the output for the UI is not evaluated until the tab is
      # opened (lazy evaluation))
      update_features <- makeReactiveTrigger()

      ###
      # reactiveValues list for main server function
      main_server <-
        reactiveValues(
          # modules_created: stores the keys of the module server instances created
          modules_created = list(),
          dge_modules_created = list()
        )
      ###

      # 1. Reactively load object and config file ------------------------------
      cat("\n")
      log_info(
        glue("New Connection \n(session ID: {session$token})")
      )
      log_info(
        glue("Memory usage upon connection: {to_GB(mem_used())}")
      )
      log_session(session)

      # Initialize a reactiveVal for storing the key of the last dataset loaded
      dataset_info <- reactiveValues()
      dataset_info$last_object_key <- NULL
      # Initialize reactiveVal for storing the seurat object
      object <- reactiveVal(NULL)
      config <- reactiveVal(NULL)

      # Startup: a reactive value created to get eventReactive expression for
      # loading datasets to run at startup, in addition to when the dataset window
      # is closed
      startup <- reactiveVal(1)
      # A change to startup is made at startup only (below)
      # Value of startup is not important (though it can't be zero or it will be
      # ignored by observers with ignoreNULL arguments set to TRUE)
      startup(2)

      ## 1.1. Event observers to open and close modal ####
      observeEvent(
        input$open_dataset_window,
        label = "Open Dataset Modal",
        {
          showModal(
            data_modal(
              datasets = datasets,
              selected_key = selected_key
            )
          )
        })

      ## 1.2. When the "confirm selection" button is selected, close the window ####
      close_dataset_modal <-
        eventReactive(
          input$confirm_selection,
          label = "Close Dataset Window",
          #ignoreNULL = FALSE,
          {
            removeModal()
            # Record value of input$confirm_selection (code to load the
            # object and config file will respond to this reactive expression
            # instead of to the button itself, ensuring the modal is closed before
            # the dataset is computed, and that the window only closes when the
            # "confirm selection" button is pressed)
            input$confirm_selection
          })

      ## 1.3. Loading Conditional ####
      # Detects when the selected dataset has changed after the selection window
      # is closed.
      # Create reactive trigger that will invalidate the "load/update dataset"
      # reactive when activated in the observer below
      dataset_change <- makeReactiveTrigger()

      observeEvent(
        eventExpr =
          {
            # eventExpr: observer executes when this expression evaluates to 
            # TRUE Observer should execute at startup (when 
            # input$confirm_selection is NULL) and when the window to change
            # datasets is closed.
            if (is.null(input$confirm_selection)){
              # Before the dataset window is created for the first time,
              # respond to startup()
              isTruthy(startup())
            } else {
              # When the button to close the window is defined for the first 
              # time, execute in response to the button
              isTruthy(close_dataset_modal())
            }
          },
        label = "Loading Conditional",
        ignoreNULL = FALSE,
        {
          # Object and config files should be loaded
          # a. At startup, and
          # b. When the dataset requested is different from the one
          #    currently loaded (key of dataset selected != key of
          #    currently loaded dataset)

          # Fetch key for previously loaded dataset from dataset_info reactive list
          previous_key <- dataset_info$last_object_key

          # a. startup (previous_key == NULL)
          if (is.null(previous_key)){
            # Activate Reactive trigger to load dataset
            dataset_change$trigger()
          } else {
            # b. Dataset requested is different
            if (previous_key != selected_key()){
              # Activate Reactive trigger to load dataset
              print("Dataset change trigger")
              dataset_change$trigger()
            }

            # if previous_key == selected_key and selected_key is not NULL,
            # the dataset is the same. Do not proceed with dataset loading
            # in this case
          }
        })

      # TEMP: Inspect dataset trigger
      observeEvent(
        dataset_change$depend(),
        label = "Dataset Trigger",
        # The depend() function always evaluates to NULL when printed regardless
        # of its true server value. Therefore, ignoreNULL must be used for the
        # observer to respond to the trigger.
        ignoreNULL = FALSE,
        # Do not want the observer to run upon startup or module creation
        ignoreInit = TRUE,
        {
          print("Dataset trigger in main server ")
          print(dataset_change$depend())
        })

      ## 1.4. Load/update object ####
      observeEvent(
        # Loads when the reactive trigger in "Loading Conditional" is activated
        dataset_change$depend(),
        label = "Load/Update Object",
        ignoreNULL = FALSE,
        {
          app_spinner$show()
          # Fetch object from datasets list defined at startup and store
          # in "object" reactiveVal
          object(datasets[[selected_key()]]$object)

          app_spinner$hide()
        })

      ## 1.5. Config file
      ### 1.5.1. Load/update config file ####
      # Update config file with the one from the selected dataset,
      # if it has changed
      observeEvent(
        dataset_change$depend(),
        label = "Load/Update Config File",
        ignoreNULL = FALSE,
        {
          config(datasets[[selected_key()]]$config)
        })

      ### 1.5.2. Check version of config file ####
      # observeEvent(
      #   config(),
      #   ignoreNULL = TRUE,
      #   ignoreInit = TRUE,
      #   {
      #
      #   })

      ### 1.5.3. Apply thresholds to ADT Assay ####
      # If thresholding information is provided, copy the ADT assay to a new
      # assay, and save the new assay to the object
      observeEvent(
        config(),
        ignoreNULL = TRUE,
        #ignoreInit = TRUE,
        {
          if (!is.null(config()$adt_thresholds)){
            # First, determine which assay is designated as the ADT assay
            # Method depends on version of config file
            # Older versions have this info in the assays section
            # Newer versions have the info in other_assay_options$adt_assay
            # Test for new version (older versions do not have this section)
            if (isTruthy(config()$other_assay_options)){
              # For new version, simply fetch the designated assay from this section
              designated_ADT_assay <- config()$other_assay_options$adt_assay
            } else {
              # Method for old version
              is_designated <-
                sapply(
                  config()$assays,
                  # Fetch value of designated_adt for each assay (TRUE or FALSE)
                  function(assay) assay$designated_adt
                )

              # Subset for assays where designated_adt is TRUE
              designated_ADT_assay <- names(config()$assays)[is_designated]
            }


            # Only proceed if one assay has been designated (not possible to
            # designate multiple in app, but file could be modified to do so)
            if (!is.null(designated_ADT_assay)){
              if (length(designated_ADT_assay) == 1){
                adt_threshold_assay(
                  object(),
                  threshold_table =
                    config()$adt_thresholds,
                  designated_adt_assay =
                    designated_ADT_assay
                  ) |>
                  # Update object with output of adt_threshold_assay
                  object()

                # # Fetch copy of object
                # object_copy <- object()
                #
                # # Copy ADT assay
                # object_copy[["ADT_threshold"]] <-
                #   object_copy[[designated_ADT_assay]]
                #
                # # Clamp assays to thresholds in config app
                # # Subset assay to features for which threshold information exists
                # #  to conserve memory
                # object_copy[["ADT_threshold"]] <-
                #   subset(
                #     object_copy[["ADT_threshold"]],
                #     features = config()$adt_thresholds$adt
                #   )
                #
                # for (i in 1:nrow(config()$adt_thresholds)){
                #   # Fetch ith ADT and threshold value
                #   ADT <- config()$adt_thresholds$adt[i]
                #   threshold <- config()$adt_thresholds$value[i]
                #
                #   # Subtract threshold
                #   object_copy@assays$ADT_threshold@data[ADT,] <-
                #     object_copy@assays$ADT_threshold@data[ADT,] - threshold
                #
                #   # "Clamp" expression values for ADT to zero
                #   object_copy@assays$ADT_threshold@data[ADT,] <-
                #     sapply(
                #       object_copy@assays$ADT_threshold@data[ADT,],
                #       function(value){
                #         if (value < 0) 0 else value
                #       }
                #     )
                #}

                # Save object with new assay
                #object(object_copy)
              }
            }
          }
        })

      ## 1.6. Save Key of Dataset Selected When Window is Closed ####
      # Save the selected key for the next time the window is opened (the
      # group of buttons is re-created every time the modal is opened)
      selected_key <-
        eventReactive(
          # Must run at startup (to load dataset initially) and in response
          # to the "confirm selection" button
          eventExpr =
            {
              # eventExpr: observer executes when this expression evaluates to TRUE
              # at startup, input$confirm_selection is NULL and
              # close_dataset_modal() will not run. To get observer to run at
              # startup, a conditional is used to respond to startup() when
              # input$confirm_selection is NULL.
              if (is.null(input$confirm_selection)){
                isTruthy(startup())
              } else {
                # When close_dataset_modal is defined, execute in response to the
                # variable
                isTruthy(close_dataset_modal())
              }
            },
          label = "Save Data Key",
          ignoreNULL = FALSE,
          {
            if (!is.null(input$data_key)){
              # Log and record value of input$data_key
              rlog::log_info(paste0("Dataset selected: ", input$data_key))

              input$data_key
            } else {
              # If input$data_key is NULL, the key has not yet been defined.
              # in this case, use the default data key (the first one)
              rlog::log_info(paste0("Dataset selected: ", names(datasets)[1]))
              names(datasets)[1]
            }
          })

      ## 1.7. Key of Last Dataset Loaded ####
      # Save the key of the last dataset loaded into the app.
      # This is updated only when a change in dataset is observed upon closing the
      # window (as signaled by the loading conditional observer)
      observeEvent(
        dataset_change$depend(),
        label = "Save Key of Last Dataset Loaded",
        ignoreNULL = FALSE,
        {
          # Selected_key() is stored under last_object_key, but only when the
          # dataset is changed, or at startup (when "Loading Conditional" triggers
          # dataset_change)
          dataset_info$last_object_key <- selected_key()
        })

      # 1.8. Upload new metadata ####
      ### 1.8.1. UI for uploading metadata from a table ####
      # Modal is created from the add_metadata module
      new_metadata_module <- 
        add_metadata_server(
          id = "add_metadata", 
          modal_open_button = 
            reactive(
              label = "add_metadata-input-modal_open_button",
              {input$new_metadata_modal}),
          object = object,
          object_meta_varnames = 
            reactive(
              label = "add_metadata-input-object_meta_varnames",
              {
                # Named vector of variables in config file
                # Values are the columns as named in the object, names are the
                # labels defined in the config file
                var_labels <- 
                  sapply(
                    config()$metadata, 
                    function(category){category$label}
                    )
                
                var_choices <-
                  sapply(
                    config()$metadata, 
                    function(category){category$meta_colname}
                    )
                
                names(var_choices) <- var_labels
                
                var_choices
                })
        )
      
      ### 1.8.2. Update object in response to uploaded metadata ####
      observe(
        label = "Add uploaded metadata to object",
        {
        req(new_metadata_module$uploaded_table())
        req(new_metadata_module$join_instructions())
        
        uploaded_table <- new_metadata_module$uploaded_table()
        
        # Pull full metadata table
        object_metadata <-
          fetch_metadata(
            isolate({object()}),
            full_table = TRUE
            )
        
        # Do the join
        new_object_metadata <- tryCatch(
          error = function(cnd){
            # If an error occurs during the join, log results
            print(paste0("Join error:", cnd))
            
            # Return NULL (which will stop downstream computation)
            return(NULL)
            },
            {
              new_object_metadata <- 
                left_join(
                  # Must preserve cell names (rownames), which are overrided 
                  # by the join
                  tibble::rownames_to_column(
                    object_metadata, 
                    var = "cellDIVER_cell_id"
                    ), 
                  uploaded_table,
                  by = new_metadata_module$join_instructions()
                  )
              
              # Restore cell IDs after join
              new_object_metadata <- 
                new_object_metadata |> 
                tibble::column_to_rownames("cellDIVER_cell_id")
              
              # Return new table form tryCatch statement 
              new_object_metadata
            })
        
        
        if (!is.null(new_object_metadata)){
          # Replace NAs in the new columns with "undefined"
          # For each added variable, replace NA values with "Undefined"
          for (meta_var in new_metadata_module$vars_added()){
            new_object_metadata[,meta_var] <-
              case_when(
                is.na(new_object_metadata[,meta_var]) ~ "Undefined",
                TRUE ~ new_object_metadata[,meta_var]
                )
          }
          
          # Apply the new metadata table to the object, and save the resulting
          # object to the object() reactive variable
          print("Update object with new metadata")
          object(
            cellDIVER:::update_object_metadata(
              object = isolate({object()}), 
              table = new_object_metadata
              )
            )
          print("Object updated with new metadata.")
          }
      })
      
      # 2. Initialize Variables specific to object and config file -------------
      # Split config file into metadata and assay lists for use downstream
      ## 2.1. Metadata_config ####
      metadata_config <-
        eventReactive(
          # Must respond to object to process added metadata
          c(config(), object()),
          label = "metadata_config",
          ignoreNULL = FALSE,
          {
            metadata_config <- config()$metadata
            
            # Metadata added by user
            # If metadata has been added, create config entries for the new
            # metadata variables
            if (isTruthy(new_metadata_module$vars_added())){
              for (meta_var in new_metadata_module$vars_added()){
                metadata_config[[meta_var]] <-
                  list(
                    # Label and colname are the same for now. These are 
                    # determined by the table
                    `meta_colname` = meta_var,
                    `label` = meta_var,
                    `description` = 
                      paste0(
                        'This variable was added using ',
                        'the "Add Metadata" window.'
                        ),
                    `groups` = NULL
                  )
                }
              }
            
            metadata_config
          })
      
      ## 2.2. Assay Information ####
      ### 2.2.1. Assay_config ####
      # Assay-specific options in config file
      assay_config <-
        eventReactive(
          config(),
          label = "assay_config",
          ignoreNULL = FALSE,
          {
            config()$assays
          })

      ### 2.2.2. Designated genes assay ####
      # Only in newer config files. If not found, NULL is passed forward and
      # the first assay is assumed to be genes.
      designated_genes_assay <-
        eventReactive(
          config(),
          label = "assay_config",
          ignoreNULL = FALSE,
          {
            config()$other_assay_options$gene_assay
          })

      ## 2.3. Valid features Expressions ####
      # Create a list of valid features using the assays defined above
      ### 2.3.1. Determine whether to include numeric metadata in feature list ####
      # Determination depends on the value of `include_numeric_metadata`
      # in the config file.
      include_numeric_metadata <-
        eventReactive(
          config(),
          label = "include_numeric_metadata",
          ignoreNULL = FALSE,
          {
            if (isTruthy(config())){
              if (!is.null(config()$include_numeric_metadata)){
                config()$include_numeric_metadata
              } else {
                # If the include_metadata field does not exist in
                # the config file, return TRUE.
                TRUE
              }
            }
          })

      # Header text for numeric metadata features in selection menu
      # May be set in the config app in the future
      numeric_metadata_title <- "Metadata Features"

      ### 2.3.2. valid_features ####
      valid_features <-
        eventReactive(
          c(assay_config(), object()),
          label = "valid_features",
          ignoreNULL = FALSE,
          {
            valid_features <-
              feature_list_all(
                object = object(),
                assay_config = assay_config(),
                # include_numeric_metadata: a boolean variable
                # that is hard-coded for now and will be
                # defined in the config file
                numeric_metadata = include_numeric_metadata(),
                # The same is true for numeric_metadata_title
                numeric_metadata_title = numeric_metadata_title,
                # ADT thresholds: add to list if the ADT_threshold assay
                # has been created in the object
                adt_threshold_features =
                  if ("adtThreshold" %in% cellDIVER:::assay_names(object())){
                    TRUE
                  } else {
                    FALSE
                  },
                # Display name for threshold features (can be set in the
                # browser config file)
                adt_threshold_title =
                  if (!is.null(adt_threshold_dropdown_title)){
                    adt_threshold_dropdown_title
                  } else {
                    # Supply default if the value is undefined
                    "ADT Values (Threshold Applied)"
                  }
                )

            valid_features
          })

      ## 2.4. meta_categories ####
      # meta_categories: a vector giving the IDs of each of the categories defined
      # in the metadata section of the config file
      meta_categories <-
        eventReactive(
          metadata_config(),
          label= "meta_categories",
          ignoreNULL = FALSE,
          {
            names(metadata_config())
          })

      ## 2.5. category_labels ####
      # Create a list of labels for each metadata category (names are the
      # category IDs and the values are the labels chosen)
      category_labels <-
        eventReactive(
          metadata_config(),
          label= "category_labels",
          ignoreNULL = FALSE,
          {
            lapply(metadata_config(), function(category){category$label})
          })

      ## 2.6 Metadata categories in dropdown menus ####
      # meta_choices: a named vector with name-value pairs for the display name of
      # the metadata category and the key used to access the category in the Seurat
      # Object.
      meta_choices <-
        eventReactive(
          # Must respond to object to process added metadata
          c(meta_categories(), object()),
          label = "meta_choices generation",
          ignoreNULL = FALSE,
          {
            # Base vector: contains the "none" option
            meta_choices <- c("None" = "none")
            # Iteratively populate vector using entries in the 
            # metadata section of the config file
            for (meta_var in meta_categories()){
              # Form name-value pair for each added metadata variable 
              # Value: the `colname` entry in the config file
              var_add <- metadata_config()[[meta_var]]$meta_colname
              # Name: `label` field in config file 
              names(var_add) <- metadata_config()[[meta_var]]$label
              
              # Append name-value pair to vector
              meta_choices <- c(meta_choices, var_add)
              }
            
            # Return meta_choices vector generated above
            meta_choices
          })

      ## 2.7. Unique values for each metadata variable ####
      unique_metadata <-
        eventReactive(
          # Must respond to object to process added metadata
          c(metadata_config(), object()),
          label = "unique_metadata",
          ignoreNULL = FALSE,
          {
            # The unique values for each metadata variable listed in the config
            # file will be stored as vectors in a list
            unique_metadata <- list()

            # Store unique values for each metadata variable entered
            for (meta_var in names(metadata_config())){
              # Use sobj@meta.data[[category]] instead of object()[[category]]
              # to return a vector (sobj[[category]] returns a dataframe)
              unique_metadata[[meta_var]] <-
                SCUBA::unique_values(
                  object = object(),
                  var = meta_var
                  )

              # If the metadata variable is a factor, convert to a vector with
              # levels to avoid integers appearing in place of the
              # unique values themselves
              if (class(unique_metadata[[meta_var]]) == "factor"){
                unique_metadata[[meta_var]] <-
                  levels(unique_metadata[[meta_var]])
                }
            }
            
            unique_metadata
          })

      ## 2.8. Reductions in object ####
      reductions <-
        reactive({
          req(object())

          # If a reductions section exists in the config file, use the config file
          # to determine the reductions to include
          if (isTruthy(config()$reductions)){
            # Machine-readable names for reductions, in the order selected
            # in the config file
            ids <-
              sapply(
                config()$reductions,
                function(reduction_entry){
                  reduction_entry$reduction
                }
              )

            labels <-
              sapply(
                config()$reductions,
                function(reduction_entry){
                  reduction_entry$label
                }
              )

            # Reductions: named list. Elements are machine-readable reduction
            # names, and names are human-readable reduction names
            reductions <- ids
            names(reductions) <- labels

          } else {
            # Otherwise, get reductions in object and use display names
            # equal to how they are named in the object
            reductions <- cellDIVER:::reduction_names(object())

            # If the "umap" reduction exists, place it first
            if ("umap" %in% reductions){
              reductions <-
                c(
                  reductions[reductions=="umap"],
                  reductions[!reductions=="umap"]
                )
            }
          }

          reductions
        })

      ## 2.9. Original Axes limits by reduction ####
      # Original axes limits must be calculated for each reduction
      # Current method is to use the min and max values for coordinates in
      # the cell embeddings for each reduction

      #xmin <- min(obj@reductions[[reduction]]@cell.embeddings[,1])
      #xmax <- max(obj@reductions[[reduction]]@cell.embeddings[,1])

      lim_orig <-
        eventReactive(
          reductions(),
          ignoreNULL = FALSE,
          label = "Setup: Define orignal axes limits",
          {
            req(reductions())

            lim_orig <-
              lapply(
                reductions(),
                function(reduction, object){
                  # limits calculation: uses cell embeddings
                  # Fetch coordinates, then determine min/max values
                  reduction_coords <-
                    SCUBA::fetch_reduction(
                      object = object(),
                      reduction = reduction,
                      dims = c(1, 2)
                      )

                  xmin <- min(reduction_coords[,1])
                  xmax <- max(reduction_coords[,1])
                  ymin <- min(reduction_coords[,2])
                  ymax <- max(reduction_coords[,2])

                  return(
                    list(
                      xlim_orig = c(xmin, xmax),
                      ylim_orig = c(ymin, ymax)
                    )
                  )
                },
                # Object must called with parentheses here (if not, parentheses
                # would be required in the function above)
                object()
              )

            # Set names of list to names of reductions
            names(lim_orig) <- reductions()

            # Return list of original limits
            lim_orig
          })

      ## 2.10. Store number of cells in full object ####
      # used to determine if a subset is selected.
      # It was previously commented here that this may not apply to 
      # non-CITE-seq datasets, but I don't see how this would be the case

      n_cells_original <-
        reactive({
          req(object())
          SCUBA::get_all_cells(object()) |> 
            length()
        })

      ## 2.11. Auto-Generated Object Dictionary ####
      # Data dictionary
      # The data dictionary gives the names of all metadata in the object as a
      # guide for string subsetting.
      # When object is changed, render a new data dictionary to www/
      observeEvent(
        object(),
        label = "Render Auto-Generated Object Dictionary",
        #ignoreNULL = FALSE,
        #ignoreInit = TRUE,
        {
          # Show spinner over app while dictionary renders
          full_page_spinner$show()
          
          # Gather parameters used by document
          params <-
            list(
              object = object(),
              config = config(),
              valid_features = valid_features()
              )

          # PANDOC: path to pandoc needs to be defined in browser mode, on a
          # Linux server
          # Execute Rmarkdown document
          if (browser_mode){
            if (any(names(browser_config) == "RSTUDIO_PANDOC")) {
              if (!is.null(browser_config$RSTUDIO_PANDOC)){
                tryCatch(
                  Sys.setenv(RSTUDIO_PANDOC = browser_config$RSTUDIO_PANDOC),
                  error = function(error){
                    stop(
                      "Error in RSTUDIO_PANDOC in app (browser) config file. ",
                      "Please check the path to PANDOC. If you already have ",
                      "pandoc installed, please enter '~' for RSTUDIO_PANDOC ",
                      "in the app config file."
                      )
                  }
                  )
              }
            }
          }

          rmarkdown::render(
            # Rmd document to render
            input = auto_dictionary_markdown_path,
            # Export path
            #output_dir = "resources/",
            output_file =
              paste0(
                tempdir(),
                "/",
                auto_dictionary_filename
                ),
            # pass parameters to report
            params = params,
            # Set up a new environment that is the child of the global
            # environment (isolates document environment from app)
            envir = new.env(parent = globalenv()),
            # Do not print rendering messages to console
            quiet = TRUE
          )
          
          full_page_spinner$hide()
        })

      ## 2.12. Patient/sample level metadata category ####
      patient_colname <-
        reactive({
          # Extract category name from "other_metadata_options"
          # section of the config file
          config()$other_metadata_options$patient_colname
        })

      ## 2.13. Object class testing ####
      # Slight modifications to app behavior are required based on the 
      # class of the object. The expressions in this section test for specific
      # classes and report to downstream expressions.
      ### 2.13.1. Object is a Seurat object ####
      is_seurat <-
        reactive({
          req(config())
          
          if (!is.null(config()$object_class)){
            if (config()$object_class %in% c("Seurat")){
              TRUE
            } else {
              FALSE
            }
          } else {
            FALSE
          }
        })
      
      ### 2.13.2. Object is a SCE object with HDF5 Storage ####
      is_HDF5SummarizedExperiment <-
        reactive({
          req(config())

          isTruthy(config()$is_HDF5SummarizedExperiment)
        })

      ### 2.13.3. Object is an anndata/mudata object ####
      is_anndata <-
        reactive({
          req(config())
          
          if (!is.null(config()$object_class)){
            if ("AnnDataR6" %in% config()$object_class){
              TRUE
            } else {
              FALSE
            }
          } else {
            FALSE
          }
        })
      
      ### 2.13.4. Object is a MuData object ####
      is_mudata <-
        reactive({
          req(config())
          
          if (!is.null(config()$object_class)){
            if ("md._core.mudata.MuData" %in% config()$object_class){
              TRUE
            } else {
              FALSE
            }
          } else {
            FALSE
          }
        })
      
      ## 2.14. Object info page ####
      # If an information page has been created by the user and provided in
      # the browser config file or via the `object_description_path` parameter, 
      # load the page and display in the info tab.
      output$object_info_page <-
        renderUI({
          object_info_page_spinner$show()
          
          if (!is.null(datasets[[selected_key()]]$object_description)){
            # If a path to a md/rmd/qmd file is provided (either in the browser
            # config or via the object_description_path parameter for single
            # object instances), render the markdown content.
            info_page_ui <-
              htmltools::includeMarkdown(
                datasets[[selected_key()]]$object_description
                )
          } else {
            # Otherwise, render a message saying the page has not been 
            # provided by the app admin
            info_page_ui <-
              div(
                tags$h1(
                  paste0(
                    "No information page available"
                    )
                  ),
                div(
                  style = "font-size: 18px;",
                  paste0(
                    "The admin of this app deployment has not included an ",
                    "information page for this dataset."
                    )
                  )
                )
          }
          
          object_info_page_spinner$hide()
          
          info_page_ui
        })
      
      outputOptions(output, "object_info_page", suspendWhenHidden = FALSE)

      # 3. Initialize Modules --------------------------------------------------
      ## 3.1. Dynamic UI ####
      # All UI for modules is dynamic as it depends on the currently
      # selected object.
      ### 3.1.1. Plots tab UI ####
      # Added "dynamic" to end of variable created to prevent collision with the
      # plots_tab_ui reactive function
      plots_tab_ui_dynamic <-
        eventReactive(
          # Reactive event is set as the last initialization variable generated
          # from the config file (meta_choices reacts to meta_categories, which
          # reacts to metadata_config, which reacts to config)
          meta_choices(),
          label = "Plots Tab Dynamic UI",
          ignoreNULL = FALSE,
          {
            # Show spinner while new UI is computing
            render_UI_spinner$show()

            ui <-
              plots_tab_ui(
                id = glue("{dataset_info$last_object_key}_plots"),
                meta_choices = meta_choices,
                unique_metadata = unique_metadata,
                category_labels = category_labels,
                assay_config = assay_config,
                metadata_config = metadata_config,
                reductions = reductions,
                categorical_palettes = categorical_palettes,
                continuous_palettes = continuous_palettes,
                patient_colname = patient_colname,
                auto_dictionary_path = auto_dictionary_path,
                string_subsetting_href = string_subsetting_href
              )

            # Hide spinner and return module UI
            render_UI_spinner$hide()

            ui
          })

      ### 3.1.2. DGE tab UI ####
      dge_tab_ui_dynamic <-
        eventReactive(
          # UI should only update when the object and config files are switched
          c(config(),
            object()
            ),
          label = "DGE Tab Dynamic UI",
          ignoreNULL = FALSE,
          {
            dge_tab_ui(
              id = glue("{dataset_info$last_object_key}_dge"),
              unique_metadata = unique_metadata,
              metadata_config = metadata_config,
              meta_categories = meta_categories,
              meta_choices = meta_choices,
              auto_dictionary_path = auto_dictionary_path,
              string_subsetting_href = string_subsetting_href
            )
          })

      ### 3.1.3. Correlations tab UI ####
      # corr_tab_ui_dynamic <-
      #   eventReactive(
      #     # UI should only update when the object and config files are switched
      #     c(config(),
      #       object()
      #       ),
      #     label = "Correlations Tab Dynamic UI",
      #     ignoreNULL = FALSE,
      #     {
      #       ui <- corr_tab_ui(
      #         id = glue("{dataset_info$last_object_key}_corr"),
      #         unique_metadata = unique_metadata,
      #         metadata_config = metadata_config,
      #         auto_dictionary_path = auto_dictionary_path,
      #         string_subsetting_href = string_subsetting_href
      #       )
      #
      #       update_features$trigger()
      #
      #       ui
      #     })

      ### 3.1.4. Render Dynamic UI components ####
      output$plots_dynamic_ui <-
        renderUI({
          plots_tab_ui_dynamic()
        })

      output$dge_dynamic_ui <-
        renderUI({
          dge_tab_ui_dynamic()
        })

      # output$corr_dynamic_ui <-
      #   renderUI({
      #     corr_tab_ui_dynamic()
      #   })

      ## 3.2 Server instances ####
      # A separate module instance is needed for each dataset to avoid namespace
      # collisions, but only one module should be created for each dataset. The
      # observer below will initialize modules for each tab if they have not already
      # been created.

      ### 3.2.1. Plots tab server ####
      observe(
        label = "Create Module Servers",
        {
          current_key <- dataset_info$last_object_key

          # If the current key is not in the list of module servers created, create
          # server instances for each tab.
          if (!current_key %in% main_server$modules_created){
            print(glue("New module for plots tab (key = {current_key})"))

            plots_tab_server(
              id = glue("{current_key}_plots"),
              object = object,
              metadata_config = metadata_config,
              assay_config = assay_config,
              meta_categories = meta_categories,
              category_labels = category_labels,
              unique_metadata = unique_metadata,
              valid_features = valid_features,
              error_list = error_list,
              n_cells_original = n_cells_original,
              lim_orig = lim_orig,
              categorical_palettes = categorical_palettes,
              continuous_palettes = continuous_palettes,
              blend_palettes = blend_palettes,
              patient_colname = patient_colname,
              current_tab = reactive({input$navigator})
              )

            # Add current key to list of modules created so module is not re-created
            main_server$modules_created <-
              c(main_server$modules_created, current_key)
          }
        })

      ### 3.2.2. DGE Tab Server ####
      observe({
        current_key <- dataset_info$last_object_key

        if (!current_key %in% main_server$dge_modules_created){
          print(glue("New module for dge tab (key = {current_key})"))
          dge_tab_server(
            id = glue("{current_key}_dge"),
            object = object,
            metadata_config = metadata_config,
            assay_config = assay_config,
            designated_genes_assay = designated_genes_assay,
            meta_categories = meta_categories,
            unique_metadata = unique_metadata,
            meta_choices = meta_choices,
            valid_features = valid_features,
            object_trigger = dataset_change,
            error_list = error_list
          )

          # Add current key to list of modules created so module is not re-created
          main_server$dge_modules_created <-
            c(main_server$dge_modules_created, current_key)
        }
      })

      ### 3.2.3. Correlations Tab Server ####
      # observe({
      #   current_key <- dataset_info$last_object_key
      #
      #   if(!current_key %in% main_server$corr_modules_created){
      #     print(glue("New module for correlations tab (key = {current_key})"))
      #     corr_tab_server(
      #       id = glue("{current_key}_corr"),
      #       object = object,
      #       metadata_config = metadata_config,
      #       assay_config = assay_config,
      #       designated_genes_assay = designated_genes_assay,
      #       meta_categories = meta_categories,
      #       unique_metadata = unique_metadata,
      #       n_cells_original = n_cells_original,
      #       nonzero_threshold = nonzero_threshold,
      #       meta_choices = meta_choices,
      #       valid_features = valid_features,
      #       error_list = error_list,
      #       update_features = update_features,
      #       object_trigger = dataset_change
      #     )
      #
      #     # Add current key to list of modules created so module is not re-created
      #     main_server$corr_modules_created <-
      #       c(main_server$corr_modules_created, current_key)
      #   }
      # })
      
      ### 3.2.4 Object Metadata Information Server ####
      object_contents_info_server(
        id = "object_contents_info_tab",
        object = object,
        meta_choices = meta_choices,
        metadata_config = metadata_config,
        include_numeric_metadata = reactive({config()$include_numeric_metadata}),
        assay_config = assay_config,
        reduction_config = reactive({config()$reductions}),
        is_seurat = is_seurat
        )

      # 4. Dataset preview in modal UI -----------------------------------------
      ## 4.1. Description ####
      # Render text for the dataset modal that displays a description of the
      # dataset currently selected
      output$dataset_description <-
        renderText({
          req(input$data_key)

          # Use config file for description (newer version), or description from
          # browser config file, in datasets (depricated version)
          if (dataset_config_has_info(datasets)){
            # (key = input$data_key)
            datasets[[input$data_key]]$config$description
          } else {
            # Fetch description of the dataset selected (key = input$data_key)
            datasets[[input$data_key]]$description
          }
        })

      ## 4.2. Plot or image ####
      # Render plots from info in config file: new version only (0.5.0 and greater)
      if (dataset_config_has_info(datasets)){
        output$dataset_dimplot <-
          renderPlot(
            width = 290,
            height = 218,
            res = 36,
            {
              req(input$data_key)
              # Require preview type to be defined
              req(datasets[[input$data_key]]$config$preview$type)
              req(datasets[[input$data_key]]$config$preview$type == "dimplot")

              # If tests pass, load plot settings
              plot_settings <-
                datasets[[input$data_key]]$config$preview$plot_settings

              # Create a dimplot from the settings retrieved
              shiny_umap(
                object = datasets[[input$data_key]]$object,
                group_by = plot_settings$group_by,
                split_by = plot_settings$split_by,
                reduction = plot_settings$reduction,
                ncol = plot_settings$ncol,
                show_legend = TRUE,
                show_label = plot_settings$label,
                show_title = FALSE,
                is_subset = FALSE,
                original_limits = NULL
              )
            })
      }

      # Image: new and depricated versions
      output$dataset_image <-
        renderImage({
          req(input$data_key)

          # Path to image: depends on version of config file
          if (dataset_config_has_info(datasets)){
            # Version v0.5.0 and later: use preview section of config file

            # The type of preview in the config file must be an image for the
            # render function to proceed
            req(datasets[[input$data_key]]$config$preview$type == "image")
          } else {
            # Older versions: use browser config information stored in `datasets`
            path <- datasets[[input$data_key]]$plot
          }

          print("Proceeding to image render list")
          list(
            `src` = path,
            `width` = 290,
            `height` = 218
          )
        },
        deleteFile=FALSE
        )

      # Set suspendWhenHidden to false so the plot and image can render before
      # the dataset window is opened
      outputOptions(
        output,
        "dataset_image",
        suspendWhenHidden = FALSE
      )

      outputOptions(
        output,
        "dataset_dimplot",
        suspendWhenHidden = FALSE
      )

      outputOptions(
        output,
        "dataset_description",
        suspendWhenHidden = FALSE
      )

      ## 4.3. Show plot or image output for dataset ####
      # For config files produced with version v0.5.0 and later.
      if (dataset_config_has_info(datasets)){
        observe({
          # Observer requires input$data_key to be defined before running
          req(input$data_key)
          # Also require config$preview$type to be defined
          req(datasets[[input$data_key]]$config$preview$type)

          # Observer also runs when dataset window is opened
          input$open_dataset_window

          plot_id <- "dataset_dimplot"
          image_id <- "dataset_image"

          # If an image is the chosen preview type, show the image container and not
          # the plot container.
          if (datasets[[input$data_key]]$config$preview$type == "image"){
            showElement(
              id = image_id
            )

            hideElement(
              id = plot_id
            )
          } else if (datasets[[input$data_key]]$config$preview$type == "dimplot"){
            # If a plot is the chosen preview type, show the plot container and
            # not the image container.
            hideElement(
              id = image_id
            )

            showElement(
              id = plot_id
            )
          } else if (datasets[[input$data_key]]$config$preview$type == "none"){
            # If the type of preview is "none", hide both containers (only the
            # description will display in the dataset selection window)
            hideElement(
              id = image_id
            )

            hideElement(
              id = plot_id
            )
          }
        })
      }

      # Observe statement to determine memory usage of all objects in the environment
      # observe({
      #   env <- .GlobalEnv
      #   units <- "MB"
      #
      #   env_size(env, units)
      # })

      observeEvent(
        object(),
        {
          log_info(
            glue("Memory used after loading current object: {to_GB(mem_used())}")
          )
        })

      # 5. Link to auto-generated object dictionary ----------------------------
      output$auto_dictionary_link <-
        renderUI({
          tags$a(
            "Dataset Guide",
            # Temporary file created at start of session
            href = auto_dictionary_path,
            class = "blue_hover",
            target = "_blank",
            rel = "noopener noreferrer"
          )
        })

      # 6. Enable/Disable Tabs -------------------------------------------------
      ## 6.1. DGE Tab ####
      observe(
        label = "Enable/Disable DGE tab, SCE Objects",
        {
          # jQuery selector for the DGE navbar button
          dge_tab_button <- "nav [data-value = 'dge']"

          # Disable the DGE tab (for now) if the object is an SCE object with
          # DelayedArray (HDF5-enabled) matrices
          # DGE is also currently disabled for MuData objects
          if (isTruthy(is_HDF5SummarizedExperiment()) | isTruthy(is_mudata())){
            shinyjs::addClass(
              selector = dge_tab_button,
              class = "navbar-hide"
            )
          } else {
            shinyjs::removeClass(
              selector = dge_tab_button,
              class = "navbar-hide"
            )
          }
        })

      ## 6.2. Correlations Tab ####
      # The correlations tab will be hidden for anndata and
      # SingleCellExperiment objects
      # observe(
      #   label = "Enable/Disable Corr tab, SCE/Anndata Objects",
      #   {
      #     # jQuery selectors for the corr navbar button
      #     corr_tab_button <- "nav [data-value = 'corr']"
      #
      #     # if (isTruthy(is_HDF5SummarizedExperiment())|
      #     #     isTruthy(is_anndata())
      #     # ){
      #       shinyjs::addClass(
      #         selector = corr_tab_button,
      #         class = "navbar-hide"
      #       )
      #     # } else {
      #     #   shinyjs::removeClass(
      #     #     selector = corr_tab_button,
      #     #     class = "navbar-hide"
      #     #   )
      #     # }
      #   })


      # 7. Callbacks -----------------------------------------------------------
      ## 7.1. Code to run when the user disconnects ####
      onSessionEnded(
        function(){
          log_info(glue("Session {session$token} disconnected."))
          log_info(glue("Memory usage upon disconnection: {to_GB(mem_used())}"))
        }
      )

      # DEBUG: UI to test object and config file are properly rendered
      # output$verify_object <-
      #   renderUI({
      #     div(
      #       "Object",
      #       verbatimTextOutput(outputId = "object_summary"),
      #       "Config File",
      #       verbatimTextOutput(outputId = "config_summary"),
      #       "Valid Features",
      #       verbatimTextOutput(outputId = "valid_features_summary"),
      #       "Unique Metadata",
      #       verbatimTextOutput(outputId = "unique_metadata_summary"),
      #       "Other Reactives",
      #       verbatimTextOutput(outputId = "other_summary")
      #     )
      #   })

      # output$object_summary <-
      #   renderPrint({
      #     print(object())
      #   })
      #
      # output$config_summary <-
      #   renderPrint({
      #     print(config())
      #   })
      #
      # output$valid_features_summary <-
      #   renderPrint({
      #     str(valid_features())
      #   })
      #
      # output$unique_metadata_summary <-
      #   renderPrint({
      #     print(str(unique_metadata()))
      #   })
      #
      # output$other_summary <-
      #   renderPrint({
      #     print("meta_categories")
      #     print(meta_categories())
      #     print("category_labels")
      #     print(category_labels())
      #     print("meta_choices")
      #     print(meta_choices())
      #     print("xlim_orig")
      #     print(xlim_orig())
      #     print("ylim_orig")
      #     print(ylim_orig())
      #     print("n_cells_original")
      #     print(n_cells_original())
      #     print("reductions")
      #     print(reductions())
      #   })
    }

    # Run the application
    shinyApp(
      ui = ui,
      server = server,
      options =
        list(
          "port" =
            if (!is.null(port)) port else getOption("shiny.port"),
          "host" =
            if (!is.null(host)) host else "127.0.0.1",
          "launch.browser" =
            if (!is.null(launch_browser)){
              launch_browser
            } else {
                getOption("shiny.launch.browser", interactive())
              }
          )
      )
    }
