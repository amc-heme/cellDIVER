#' Error Handler Function
#' 
#' Compares an error condition to the errors in the provided error_list and 
#' shows the notification that matches the error. If no matches are found, the 
#' function will display a generic notification giving the error text and the 
#' link to the github issues page.  
#' 
#' @param session Shiny session object.
#' @param err_cnd the error condition encountered in tryCatch. This is equal 
#' to the argument in the function ran by tryCatch when there is an error 
#' (the function in the "error" parameter of the tryCatch statement.)
#' @param source_reactive character vector to display source triggering error
#' @param error_list a list of list objects generated with the error_data() 
#' function from this package, connecting error messages to UI for shiny 
#' notifications.
#' @param issue_href The URL of the github issues page
#' @param duration the duration for which the notification displays. The default 
#' is NULL, which results in the notification remaining until dismissed.
#'
#' @noRd
error_handler <- 
  function(
    session,
    err_cnd,
    error_list = list(),
    issue_href = "https://github.com/amc-heme/DataExploreShiny/issues", 
    duration = NULL,
    source_reactive = "Unknown source" #default value
    ){
    # Log full error to the console 
    if (!is.null(err_cnd$call)){
      # Include the function call that resulted in the error if present
      # (it should always be in cellDIVER, but it's possible for it not to be)
      rlog::log_error(
        paste0(
          "Error in ", deparse(err_cnd$call), " : ", 
          conditionMessage(err_cnd), 
          # Add reactive expression that triggered the error
          " (from cellDIVER reactive expression ", source_reactive, ")"
          )
        )
    } else {
      rlog::log_error(
        paste0(
          "Error: ", conditionMessage(err_cnd), "\n",
          "(from reactive expression ", source_reactive, ")"
          )
        )
      }
    
    # Define UI for error message notification
    # Check error message text against a list of errors defined in the main
    # app module in run_cellDIVER.R
    
    # Error match: conditional that is set to TRUE when an error is found, 
    # signaling the function not to run the code for displaying a generic
    # error message
    error_match <- FALSE
    # Loop through all defined error types (error_list)
    for (error_type in error_list){
      # If the condition message (the error that is returned) matches the error 
      # message of a stored error type (error_type$err_message), show the 
      # notification associated with that error type
      if (grepl(pattern = error_type$err_message, x = err_cnd$message)){
        # Display Notification
        showNotification(
          ui = error_type$notification, 
          # Duration = NULL will make the message 
          # persist until dismissed (default)
          duration = duration,
          id = session$ns(error_type$notification_id),
          session = session
          )
        
        # If error is found, set error_match to TRUE and break the loop 
        error_match <- TRUE
        break
        }
      }
      
    # Default notification
    # Shows when there is still no match after looping through all errors
    if (error_match == FALSE){
      # Display Notification
      showNotification(
        ui = 
          icon_notification_ui(
            icon_name = "skull-crossbones",
            tagList(
              glue("An error occurred. If this recurrs, please"),
              github_link("report this issue"),
              " with a screenshot of the app window."
            )
          ),
        # Duration=NULL will make the message 
        # persist until dismissed (default)
        duration = duration,
        id = session$ns("error"),
        session = session
        )
      }
}