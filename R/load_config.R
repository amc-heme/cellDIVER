library(yaml)

#' Load config file from YAML
#'
#' Loads an cellDIVER object-specific .yaml config file and converts all
#' elements in the config list to the correct R data structures.
#'
#' @param path A path to a YAML file storing object-specific config info. 
#'
#' @noRd
load_config <- 
  function(path){
    # Load config YAML using defined path
    config_r <- read_yaml(path)
    
    # Convert the "adt_thresholds" section to a tibble, if defined
    if (isTruthy(config_r$adt_thresholds)){
      config_r$adt_thresholds <-
        as_tibble(config_r$adt_thresholds)
    } else {
      # If adt_thresholds is NULL ("~" in YAML file), create a blank tibble 
      # with threshold-specific columns 
      config_r$adt_thresholds <-
        tibble(
          `adt` = character(0), 
          `value` = numeric(0)
        )
      }
    
    config_r
    }