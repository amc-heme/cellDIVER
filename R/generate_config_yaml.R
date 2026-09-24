#' Auto-Generate Config File
#'
#' This function will automatically generate a basic config file from a 
#' single-cell object. This can greatly speed up setup for large objects with 
#' many metadata variables, but we recommend editing the file in the config app 
#' or manually afterward for the following reasons:
#' - All metadata variables will be added, in the order they appear in the 
#' object metadata table. A separate display order may be desired to highight 
#' the most important metadata variables to end users.
#' - Display names for metadata are un-set
#' - Display settings for modalities/assays, including how they appear in 
#' feature dropdown menus and whether to display a suffix after the assay name 
#' in plots, are unset.
#' - Descriptions for individual modalities/assays, metadata, and reductions 
#' are unset.
#' To load the config file in the config app for further editing, supply the 
#' file path to `config_path` in `run_config`. After loading the config app, 
#' go to the "..." menu on the top right, and select "Load Config File". Save 
#' changes via "Save Config File" when you are finished editing.
#'
#' @param object a single-cell object to be configured for use in cellDIVER 
#' (not the path to the object). Currently, Seurat, SingleCellExperiment, and 
#' anndata objects are supported.
#' @param output_yaml path specifying where the output config file should be saved. 
#' This should end in ".yaml".
#' @param object_name a single-length character vector with the display name 
#' of the object. This will be displayed to end users in the app as entered.
#' @param object_description a single-length character vector with a short 
#' description of the object. This will be displayed to end users in the 
#' dataset selection menu. This should be a few sentences long, and include 
#' object details relevant to end-users (i.e. brief descripton of methods, 
#' number of cells/samples in object, etc.).
#' @param is_HDF5SummarizedExperiment Set this to TRUE to load an HDF5-enabled
#' SingleCellExperiment object saved via saveHDF5SummarizedExperiment. When
#' loading an HDF5-enabled object, set the object_path to the directory of the
#' HDF5-enabled object, created when saving the object via
#' HDF5Array:saveHDF5SummarizedExperiment.
#' @param HDF5_prefix When loading an HDF5-backed SingleCellExperiment HDF5_prefix
#' is passed to the prefix parameter of HDF5Array::loadHDF5SummarizedExperiment 
#' to specify the prefixes for the se.rds and assays.h5 files. 
#' @param include_numeric_metadata If `TRUE`, numeric metadata in the object 
#' will searchable in cellDIVER for plotting and subsetting. The default 
#' setting is `TRUE`.
#' @param genes_assay single-length character vector giving the name of the 
#' genes assay/experiment/modality in the object. If undefined, the first assay 
#' in the object will be used. The name should be entered using the following 
#' convention:
#' - Seurat objects: use the name of the assay as it appears in `names(object@assays)`.
#' - SingleCellExperiment objects: use the name of the experiment as it appears in [SingleCellExperiment::altExpNames()] or [SingleCellExperiment::mainExpName()]
#' - anndata objects: use either "X" to specify the modality measured in X, or enter the name of a matrix stored in the `obsm` slot.
#' @param sample_level_var a sample-level metadata variable used to construct 
#' sample-level pie charts in the plots tab. This should be a sample or patient 
#' ID, for example. If this is not provided (the default), pie charts will not 
#' appear in cellDIVER.
#'
#' @returns A config file is generated at the path provided to `output_yaml`. Nothing is returned from the function.
#' 
#' @export
#'
#' @usage 
#' auto_config(
#'  # Replace with path to your object
#'  object = path_to_your_object,
#'  output_yaml = output_config_path,
#'  object_name = "Test Single-Cell Object",
#'  object_description = "This is a test object",
#'  genes_assay = "RNA",
#'  include_numeric_metadata = TRUE,
#'  is_HDF5SummarizedExperiment = FALSE,
#'  sample_level_var = NULL
#'  )
generate_config_yaml <- 
  function(
    object,
    output_yaml,
    object_name,
    object_description = "",
    is_HDF5SummarizedExperiment = FALSE,
    HDF5_prefix = NULL,
    include_numeric_metadata = TRUE,
    # Options for metadata_order
    # - "object": as it appears in object
    # - "ascending": ascending alphanumeric order
    # - "descending": descending alphanumeric order
    # metadata_order = "object",
    genes_assay = NULL,
    sample_level_var = NULL
  ){
    # Check inputs
    # object: must be a supported single-cell object
    if (!(inherits(object, "Seurat") | 
           inherits(object, "SingleCellExperiment") | 
           inherits(object, "AnnDataR6")
         )){
      # Construct error message: state that the object class is invalid, and 
      # provide the valid classes.
      err_msg <- 
        paste0(
          "The object passed to `object` is an invalid class (", 
          paste0(class(object), collapse = ", "),
          ")."
          )
      
      supported_classes <- 
        paste0(
          'Currently supported classes: Seurat, SingleCellExperiment, ',
          'and AnnDataR6 (anndata objects loaded via reticulate and the ',
          '"anndata" R package).'
          )
      
      # If a character input, the input may have been a path. Add additional
      # information to the error message in this case.
      if (inherits(object, "character")){
        err_msg <- 
          paste(
            err_msg, 
            "(did you specify the path to the object instead of the object?)",
            supported_classes,
            sep = " "
            )
        } else {
          err_msg <-
            paste(
              err_msg,
              supported_classes,
              sep = " "
            )
          }
      
      # Display the error message
      # Wrap based on the width of user console using strwrap()
      # https://stackoverflow.com/questions/38000079/wrap-error-messages-for-rstudio-preserving-words
      stop(paste(strwrap(err_msg), collapse = "\n"))
    }
    
    # genes_assay
    if (is.null(genes_assay)){
      # Set default if NULL (first assay)
      cellDIVER:::assay_names(object = object)[[1]]
    } else {
      # Check value provided if not NULL
      # genes_assay must be a character vector
      if (class(genes_assay) != "character"){
        stop("`genes_assay` must be a character vector.")
      }
      
      # genes_assay must be in the assay names 
      if (!genes_assay %in% cellDIVER:::assay_names(object = object)){
        stop(
          paste0(
            "The input for `genes_assay` does not match any of the ",
            "assays/modalities/experiments in the object provided."
          )
        )
      }
    }
    
    # General config fields 
    config <-
      list(
        `config_version` = 
          packageVersion("cellDIVER") |>
          as.character(),
        `object_class` = 
          # Test for object classes supported by SCUBA and cellDIVER
          if (inherits(object, "Seurat")){
            "Seurat"
          } else if (inherits(object, "SingleCellExperiment")){
            "SingleCellExperiment"
          } else if (inherits(object, "AnnDataR6")){
            "AnnDataR6"
          } else {
            stop(
              paste0(
                "Unrecognized object class: ", 
                paste(is(object), collapse = ",")
              )
            )
          },
        # HDF5-specific settings: supplied by the user
        `is_HDF5SummarizedExperiment` =
          is_HDF5SummarizedExperiment,
        `HDF5_prefix` = HDF5_prefix, 
        `label` = object_name,
        `description` = object_description,
        # Preview: a plot that appears in the choose dataset window 
        # to represent the dataset
        # Not supported in this function. Can be set in the config app
        `preview` = list(`type` = "none")
      )
    
    # Assays
    # Iterate through each assay and add using default name
    config$assays <-
      lapply(
        # Loop through each assay as determined by assay_names
        cellDIVER:::assay_names(object = object),
        function(assay_name){
          # Construct fields for each assay using defaults
          list(
            # Name of assay
            `assay` = assay_name,
            # "key" for feature access via fetch_data: defined via the
            # make_key method
            `key` = 
              cellDIVER:::make_key(
                object,
                assay = assay_name
              ),
            # A label for the assay which appears in parentheses after the assay 
            # name in plots. Default is "", which displays nothing 
            # (no parentheses either)
            `suffix_human` = "",
            # Name of the assay as it appears in dropdown menus
            # equal to the assay name by default and can be replaced with a "label"
            `dropdown_title` = assay_name
          )
        }
      )
    
    # Assign names to each element in config$assays (using assay_names)
    names(config$assays) <- 
      cellDIVER:::assay_names(object = object)
    
    config$other_assay_options <-
      list(
        # Genes assay: provided by the user, or the first assay if undefined
        `genes_assay` = genes_assay,
        # ADT assay (used for ADT thresholding)
        # Can't be set in this function, must be done in the config app
        `adt_assay` = "none"
      )
    
    # Metadata
    # Whether to allow numeric metadata in plotting
    config$include_numeric_metadata <- include_numeric_metadata
    
    # Produce a metadata entry for each categorical metadata variable
    # (config file is currently only intended for these variable types)
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
          inherits(meta_table[[x]], c("numeric", "integer"))
        },
        object
      )
    
    categorical_vars <- meta_vars[!is_numeric]
    
    # Iterate through each metadata variable
    config$metadata <-
      lapply(
        categorical_vars,
        function(meta_var){
          list(
            # Name of metadata variable as it appears in the object
            `meta_colname` = meta_var,
            # Display name for metadata variable
            # This function will set the labels for each variable equal to the name
            # as it appears in the object.
            `label` = meta_var,
            # Description of metadata variable
            # Used in the object guide in the main app
            # Not supported in this function. This can be modified manually or 
            # in the config app
            `description` = "",
            # "Groups" of values in metadata variable
            # Not supported in this function
            `groups` = NULL
          )
        })
    
    # Apply names to elements in metadata 
    # names are the names of metadata variables iterated through above
    names(config$metadata) <- categorical_vars
    
    # Other metadata options
    # Patient_colname: a patient-level metadata variable used for 
    # constructing sample-level metadata pie charts
    config$other_metadata_options <-
      list(
        `patient_colname` = sample_level_var
      )
    
    # Reductions
    # Iterate through each reduction, compile fields
    config$reductions <-
      lapply(
        cellDIVER:::reduction_names(object),
        function(reduction_name){
          list(
            # Name of the reduction as it appears on the object
            `reduction` = reduction_name,
            # Display name of reduction: set to the reduction name
            `label` = reduction_name
          )
        })
    
    # Apply names to elements in config$reductions
    # names are the names of reductions as they appear in the object
    names(config$reductions) <- cellDIVER:::reduction_names(object)
    
    # ADT thresholds are not supported in this function
    # Must use c() to append `adt_thresholds` with a NULL element 
    # (config$adt_thresholds <- NULL will remove the adt_thresholds element)
    config <-
      c(config, list(`adt_thresholds` = NULL))
    
    # Save config file to `output_yaml`
    yaml::write_yaml(
      config,
      file = output_yaml
    )
  }