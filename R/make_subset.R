#' Subset Creation
#'
#' @param object a single cell object
#' @param filter_list a list of filters constructed by the subset_selections module. 
#'
#' @return A Seurat object subsetted for the criteria entered.
#' 
#' @rdname make_subset
#' 
#' @keywords internal
make_subset <-
  function(
    object,
    filter_list
  ){
    UseMethod("make_subset")
  }

#' Method ran for all classes besides anndata
#'
#' @describeIn make_subset Seurat, SingleCellExperiment objects
#' @export
make_subset.default <- 
  function(
    object, 
    filter_list
  ){
    # If filter_list is empty, return the full object
    if (length(filter_list) == 0){
      return(object)
    }
    
    # vector_code sub-function
    # Converts a vector of subset criterion to a string representation of that 
    # vector, for passing to eval(parse())
    vector_code <- 
      function(vector){
        # content: separate vector elements with "," 
        content <- paste(vector, collapse = '\",\"')
        # string: add c(" and ") to the ends of the content
        string <- paste0('c("', content, '")')
        
        # Special cases
        # 1. NA
        # NA values appear as "NA" in the vector created above. 
        # "NA" (NA-as-a-string) will cause issues with data that uses the literal 
        # NA, so the string representation of the vector created here must output 
        # the literal NA
        string <- gsub('\"NA\"', 'NA', string)
        
        # 2. NaN
        # While less common, NaN should also be corrected from a string to a literal
        string <- gsub('\"NaN\"', 'NaN', string)
        
        # 3. NULL 
        # When NULL literals are passed to a vector, they do not appear in the 
        # vector at all. Thus, it is unlikely they will need to be corrected.
        # They will be substituted for literals here, but a NULL literal could 
        # cause issues downstream
        string <- gsub('\"NULL\"', 'NULL', string)
        
        return(string)
        }
    
    # Construct string for subsetting
    # Begin with empty string
    subset_str <- ""
    
    # Construct a string representation for each filter, and append to string
    for (i in 1:length(filter_list)){
      # Extract current filter entry
      entry <- filter_list[[i]]
      
      # Construct subset criterion for the current filter
      # Add backticks to variable/feature name to prevent errors in edge cases 
      # where the feature name has a dash, or another character invalid when 
      # interpreting as a literal 
      criterion <- 
        if (entry$type == "categorical"){
          # Categorical filters: use %in%
          glue("(`{entry$var}` %in% {vector_code(entry$value)})")
        } else if (entry$type == "numeric"){
          # Numeric filters: test entry *mode*
          # use < or >, or a group if the value is a range
          if (entry$mode == "less_than"){
            glue("(`{entry$var}` <= {entry$value})")
          } else if (entry$mode == "greater_than"){
            glue("(`{entry$var}` >= {entry$value})")
          } else if (entry$mode == "range"){
            glue("((`{entry$var}` >= {entry$value[1]}) & 
                  (`{entry$var}` <= {entry$value[2]}))")
          } else {
            error("Unknown mode for numeric filter")
          }
        } else if (entry$type == "advanced"){
          # Advanced (string/code) subsetting: pass value as-is
          entry$value
        }
      
      # Add "&" logical operator to criterion if it is not the last one
      if (i < length(filter_list)){
        criterion <- 
          paste0(criterion, " & ")
      }
      
      # Add the criterion to the subset string
      subset_str <- paste0(subset_str, criterion)
    }
    
    # Subset using the subset string 
    subset <- 
      eval(
        parse(
          text = 
            subset_call(
              object, 
              subset_str = subset_str
              )
          )
        )
    
    # Re-level metadata factors to exclude values that are no longer represented
    # Must pull metadata table, edit, then save to object
    meta_table <- 
      SCUBA::fetch_metadata(
        subset, 
        full_table = TRUE
        )
    
    for (meta_var in colnames(meta_table)){
      if (class(meta_table[[meta_var]]) == "factor"){
        # If the metadata variable is a factor, drop unused levels
        meta_table[[meta_var]] <- 
          droplevels(meta_table[[meta_var]])
      }
    }
    
    cellDIVER:::update_object_metadata(
      subset,
      table = meta_table
      )
    
    # Return subset
    return(subset)
  }

#' @describeIn make_subset Anndata objects
#' @export
make_subset.AnnDataR6 <-
  function(
    object,
    filter_list
  ){
    # If filter_list is empty, return the full object
    if (length(filter_list) == 0){
      return(object)
    }
    
    # Otherwise, form vector of variables included in filter criteria, 
    # and pull data on each variable for all cells in the object
    all_filter_vars <-
      sapply(
        filter_list,
        function(x) x$var
      )
    
    filter_table <-
      SCUBA::fetch_data(
        object,
        vars = all_filter_vars
      )
    
    # Test cells satisfying each filter criterion
    matching_cells <- 
      lapply(
        filter_list,
        function(filter_i, filter_table){
          if (filter_i$type == "categorical"){
            # Categorical crtieria
            # subset using anndata subset format, and return cells matching 
            # the criteria
            rownames(
              filter_table[
                filter_table[[filter_i$var]] %in% filter_i$value, , drop = FALSE
                ]
              )
          } else if (filter_i$type == "numeric"){
            # # Numeric criteria
            # Apply greater-than/less-than logic to expression data/numeric metadata
            # based on filter mode
            if (filter_i$mode == "greater_than"){
              rownames(
                filter_table[
                  filter_table[[filter_i$var]] >= filter_i$value, , drop = FALSE
                  ]
                )
            } else if (filter_i$mode == "less_than"){
              rownames(
                filter_table[
                  filter_table[[filter_i$var]] <= filter_i$value, , drop = FALSE
                  ]
                )
            } else if (filter_i$mode == "range"){
              # Range: expression must be greater than first value, and 
              # less than second value
              rownames(
                filter_table[
                  filter_table[[filter_i$var]] >= filter_i$value[1] &
                    filter_table[[filter_i$var]] <= filter_i$value[2], , drop = FALSE]
              )
            }
          }
        },
        filter_table
      )

    # Determine cells for subsetting object
    # If there is only one filter criterion, unpack the vector of matching cells
    cells <- 
      if (length(matching_cells) == 1){
        matching_cells[[1]]
      } else {
        # Multiple criteria: take the intersect of cells matching each critierion
        Reduce(f = intersect, x = matching_cells)
      }
    
    # Subset Anndata object for cells
    object[cells,]
  }
