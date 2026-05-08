#' Display Feature Stats
#'
#' @param id ID to use for module elements.
#'
#' @noRd
feature_stats_ui <- 
  function(
    id,
    label = TRUE,
    object_subset_label = FALSE
    ){
    # Namespace function: prevents conflicts with IDs defined in other modules
    ns <- NS(id)
    
    tagList(
      if (label == TRUE){
        tags$b("Statistics for features entered:")
      },
      if (object_subset_label == TRUE){
        tags$p(
          "(stats are for the full the full object or the selected subset)",
          style = "color: #4a4a4a; font-size: 0.9em;"
          )
      },
      uiOutput(
        outputId = ns("feature_statistics")
      )
    )
      
    }

#' Display Feature Stats
#'
#' @param id ID to use for module server instance.
#' @param object The Seurat object to be used for plotting. It may be a subset 
#' or the full object.
#' @param features_entered The features entered in the plots tab of the app.
#' This is currently defined in the plots tab module and passed to this module. 
#' @param assay_config The assay section of the config file loaded in the main
#' server function, at startup and upon changing the object.
#' 
#' @noRd
feature_stats_server <- 
  function(
    id,
    object,
    features_entered,
    assay_config
    ){
    moduleServer(
      id,
      function(input, output, session){
        # 1. Compute Summary Statistics for each feature entered ----
        summary_by_feature <- 
          reactive({
            req(features_entered())
            
            feature_summary <-
              lapply(
                features_entered(),
                function(feature, object){
                  # Fetch data from Seurat object, then compute summary
                  SCUBA::fetch_data(
                    object,
                    vars = feature
                    )[,1] |> 
                    # (summary is a base function)
                    summary()
                  },
                object()
                )
            
            names(feature_summary) <-
              features_entered()
            
            feature_summary
          })
        
        # 2. Display Summary Statistics ----
        output$feature_statistics <-
          renderUI({
            if (isTruthy(features_entered())){
              tagList(
                # For each feature, display the name and summary information
                lapply(
                  1:length(summary_by_feature()),
                  function(i, summary_by_feature, assay_config){
                    div(
                      tags$b(
                        cellDIVER:::hr_name(
                          names(summary_by_feature[i]),
                          assay_config = assay_config, 
                          use_suffix = TRUE
                          ),
                        class = "feature-stats-header"
                        ),
                      # summary_tags prints summary statistics for each feature
                      # (Min, Q1, median, Q3, Max)
                      cellDIVER:::summary_tags(
                        summary_results = summary_by_feature[[i]],
                        header_class = "feature-stats-stats-header",
                        text_class = "feature-stats-stats-text"
                        )
                      )
                  },
                  summary_by_feature(),
                  assay_config()
                )
              )
            } else {
              tags$p(
                "Please enter a feature to view statistics",
                style = "font-style: italic; color: #4a4a4a;"
                )
            }
          })
      })
}
