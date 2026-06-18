## Options Module (first level) ####

#' Config App: Options Card UI
#'
#' Module displays and processes options for individual values in each tab of
#' the config app.
#'
#' @param id the module ID, used for namespacing. This will be equal to the name
#' of the assay, metadata variable, or reduction represented by the current
#' options card.
#' @param object the Seurat object for which config settings are being defined.
#' @param optcard_type the type of options card to create. Currently, cards can 
#' be created for "assays", "metadata", or "reductions", with a different UI 
#' for each. 
#' @param card_name the name of the current assay/metadata variable/reduction,
#' etc., represented by the current options card. This defaults to the id.
#' @param restore_inputs a list of input values, used for restoring inputs
#' when cards are sorted by the user.
#'
#' @noRd
options_ui <- function(id,
                       object,
                       optcard_type = c("assays", "metadata", "reductions"),
                       card_name = id,
                       restore_inputs = list()
                       ){
  # NS(id): namespace function, defined here and called for every input ID. One
  # namespace is used for each instance of the options module and is defined by
  # the id; use of namespaces allows for the same input id to be used in
  # different modules without namespace collisions.
  ns <- NS(id)

  # Calculations for options UI ---------------------------------------------
  # For metadata options, calculations are performed before UI is created.
  # Metadata-specific calculations used to define UI for each metadata type
  if (optcard_type == "metadata"){
    # Get unique values of metadata field for display of summary statistics
    values <-
      SCUBA::unique_values(
        object,
        var = card_name
        )

    # Create list of sorted values for display
    values_sorted <- str_sort(values, numeric = TRUE)
    # Determine type of metadata
    metadata_type <- metadata_type(object, card_name)

    # Metadata description
    # Display number of unique values if categorical; display range if numeric
    if (metadata_type == "Categorical"){
      # Compute the number of unique values
      n_values <- length(values)

      metadata_description <- glue("{n_values} unique values")
    } else if (metadata_type == "Numeric"){
      metadata_description <- ""
      # Previous code for describing numeric metadata: does not work
      # metadata_description <-
      #   glue("{range: {min(values)} to {max(values)}, avg {mean(values)}}")
    } else {
      # Potential unforeseen metadata types: leave the description blank
      metadata_description <- ""
    }
  }

  # Create UI for options card ----------------------------------------------
  # Assays UI
  if (optcard_type == "assays"){
    ui <-
      # Shinyjs hidden class applied initially to assay options cards
      hidden(
        div(
          id = ns("optcard"),
          class = "optcard single-space-bottom",
          tags$strong(
            glue("Options for {card_name}"),
            class = "large half-space-bottom center"
          ),

          # Human-readable suffix: appears on plots and search entries
          textInput(
            inputId = ns("hr"),
            label = "Set label for assay (will appear as entered in app)",
            width = "380px"
            ),

          # Include assay name on plots: if checked, the label entered will be
          # displayed on plots and in the feature search results.
          checkboxInput(
            inputId = ns("include_label"),
            label =
              "Include assay name on plots? (This is usually not required for
              the default assay in your data)"
            ),
          
          textAreaInput(
            inputId = ns("description"),
            label = 
              tagList(
                "Description: ",
                icon(
                  "circle-question",
                  id = ns("description_help"),
                  class = "fa-solid"
                  )
                ),
            width = "100%",
            rows = 3,
            resize = "vertical"
            ),
          shinyBS::bsTooltip(
            id = ns("description_help"), 
            title = 
              paste0(
                "The description entered here will be displayed to end users of your cellDIVER deployment. Information entered here will help users understand this assay and how it should be used in the app for analysis. Useful information may include a brief description of the type of data represented by the assay, what the assay biolocially measures or represents, methods involved in data collection, normalization methods applied, etc."
              ),
            placement = "bottom", 
            trigger = "hover",
            options = NULL
            )
        )
      )

    # UI for reductions options cards
  } else if (optcard_type == "reductions"){
    ui <-
      # Shinyjs hidden class applied initially to assay options cards
      hidden(
        div(
          id = ns("optcard"),
          class = "optcard single-space-bottom",
          tags$strong(
            glue("Options for {card_name}"),
            class = "large half-space-bottom center"
          ),

          # Human-readable name: appears in app menus
          textInput(
            inputId = ns("hr"),
            label = "Set label for reduction (will appear as entered in app)",
            # Default value is the name of the reduction as defined in
            # the object. Restore_inputs is used if the reductions card is
            # re-sorted.
            value =
              if (!is.null(restore_inputs$hr)) {
                restore_inputs$hr
              } else {
                card_name
              },
            width = "380px"
            ),
          
          # Description of reduction
          textAreaInput(
            inputId = ns("description"),
            label = 
              tagList(
                "Description: ",
                icon(
                  "circle-question",
                  id = ns("description_help"),
                  class = "fa-solid"
                )
              ),
            width = "100%",
            rows = 3,
            resize = "vertical"
            ),
          shinyBS::bsTooltip(
            id = ns("description_help"), 
            title = 
              paste0(
                "The description entered here will be displayed to end users of your cellDIVER deployment. Information entered here should help users understand why the reudction is used and how it may be used in the app to undersand the biology."
              ),
            placement = "bottom", 
            trigger = "hover",
            options = NULL
            )
          )
        )

    # Metadata UI
  } else if (optcard_type == "metadata"){
    ui <-
      div(
        id = ns("optcard"),
        class = "optcard single-space-bottom",
        tags$strong(glue("Options for {card_name}"),
                    class="large center",
                    #hidden exclamation triangle warning to show if NAs are present in the chosen metadata variable 
                    shinyjs::hidden(
                    tags$i(
                      class = "fa-solid fa-exclamation-triangle", 
                      style = "color: #D67E1A; margin-left: 5px;",
                      id = ns("warning_icon_na")
                    )
                 )
        ),

        # #add a tooltip for warning if NAs are present in the chosen metadata variable
        shinyBS::bsTooltip(
          id = ns("warning_icon_na"),
          title = 
            glue(
              paste0(
                '{card_name} contains NA values, which may cause unexpected ',
                'behavior in the main app. We recommend replacing NA values ',
                'with "undefined", "unknown", the character ',
                '"NA" (in quotes), etc.' 
                )
              ),
          placement = "right",
          trigger = "hover",
          options = NULL
        ), 
        # Print the type of metadata beneath the title, and a brief description
        tags$p(glue("({metadata_type}, {metadata_description})"),
               class = "center small half-space-bottom"),
        
        # If the metadata is categorical and there are 15 values or less,
        # print the values to screen
        if (metadata_type == "Categorical"){
          if (n_values <= 15){
            tags$p(glue("Values: {paste(values_sorted, collapse=', ')}"))
          }
        } else NULL,

        # Human-readable suffix: appears on plots and search entries
        textInput(
          inputId = ns("hr"),
          label =
            "Set label for metadata variable (will appear
            as entered in app interface)",
          width = "380px",
          value =
            if (!is.null(restore_inputs$hr)) {
              restore_inputs$hr
            } else {
             ""
            }
          ),
        
        # Description of metadata variable
        textAreaInput(
          inputId = ns("description"),
          label = 
            tagList(
              "Set description for metadata variable ",
              icon(
                "circle-question",
                id = ns("description_help"),
                class = "fa-solid"
              )
            ),
          width = "380px",
          rows = 3,
          resize = "vertical",
          value = 
            if (!is.null(restore_inputs$description)) {
              restore_inputs$description
            } else {
              ""
            } 
        ),
        shinyBS::bsTooltip(
          id = ns("description_help"), 
          title = 
            paste0(
              "The description entered here will be displayed to end users of your cellDIVER deployment. Information entered here will help end users understand how this variable should be used for visualization and analysis. This can include information such as the biological significance of this metadata variable, how it was computed, what individual values mean, etc."
            ),
          placement = "bottom", 
          trigger = "hover",
          options = NULL
        ),
      
        # Option to classify metadata into list (ex. group patients 
        # by sample conditions)
        # Only available for categorical metadata columns
        if (metadata_type == "Categorical"){
          tagList(
            div(
              id = ns("groups_disable_transparent"),
              materialSwitch(
                inputId =  ns("group_metadata"),
                label = "Group metadata into categories?",
                value =
                  if (!is.null(restore_inputs$group_metadata)){
                    restore_inputs$group_metadata
                  } else {
                    FALSE
                  },
                right = TRUE,
                status = "default"
              )
            ),
            div(
              id = ns("groups_explanation"),
              #class = "show_if_groups_enabled",
              # This no longer makes sense and I don't remember what I 
              # originally intended to say, so I removed this.
              # tags$p(
              #   "(Choices for possible values in the metadata
              # variable will appear in the app)",
              #   class = "center small"
              # )
            ),
            shinyjs::hidden(
              div(
                id = ns("disabled_groups_explanation"),
                #class = "show_if_groups_disabled",
                tags$p(
                  "(Grouping not allowed for this variable since the number
                  of unique values exceeds 250.)",
                  class = "center small"
                  )
              )
            ),
            # Metadata groups interface
            # One instance of the fields UI, with a button to
            # add more fields to the interface
            shinyjs::hidden(
              div(
                id = ns("groups_interface"),
                # Group fields module
                # A set of menus used to create groups from levels of a
                # categorical variable
                # This interface should not be created if there are an excessive
                # number of unique values in the variable. In extreme cases
                # (10,000+ values), the app will freeze. This feature is
                # effectively useless with 250+ values, since the user would
                # have to enter each value individually
                if (n_values <= 250){
                  metadata_group_fields_ui(
                    id = ns("groups"),
                    unique_values =
                      str_sort(
                        SCUBA::unique_values(
                          object = object,
                          var = card_name
                          ),
                        numeric = TRUE
                        )
                    )
                  }
              )#End tagList of input containers
            )#,
          )
        } else NULL,
    )
  }

  return(ui)
}

### Options module server
# Renders option cards for the variables of assays, metadata, etc. selected by
# the user, and processes user selections to create a config file for the
# main app.
# Applies to multiple types of selections (assays, metadata, etc.). One instance
# of the module is created for every available selection across each tab.

# Arguments
# id: the id passed to the module
# categories_selected: a reactive variable describing the variables (assays,
# metadata, etc., selected by the user)
# options_type: the type of options to create a server function for. Can be one
# of "assays" or "metadata"
# card_name: the name of the individual category that the instance of the
# module applies to. This is the id by default, and can be changed.

#' Config App: Options Card Server
#'
#' Module displays and processes options for individual values in each tab of
#' the config app.
#'
#' @param id Id to use for module. All inputs and outputs created will be
#' namespaced using this ID.
#' @param object a Seurat object for which settings are being chosen
#' @param categories_selected a reactive variable describing the variables
#' (assays, metadata, etc., selected by the user)
#' @param options_type the type of options to create a server function for. Can
#' be "assays", "metadata", or "reductions".
#' @param card_name the name of the individual category that an instance of
#' the module applies to. This is the id by default, and can be changed.
#' @param dev_mode When TRUE, additional logs are printed to the console while
#' the app runs. Set using run_config_app().
#'
#' @noRd
options_server <-
  function(
    id,
    object,
    categories_selected,
    options_type = c("assays", "metadata", "reductions"),
    card_name = id,
    dev_mode = FALSE
    ){
    # Initialize module
    moduleServer(
      id,
      function(input,
               output,
               session){
        # Define namespace function for UI elements or references to elements
        # that are not Shiny inputs or outputs
        ns <- NS(id)

        # Create a counter for group modules (used to set id of modules created)
        # Starts at 1 and counts up when the "add group" button is pressed
        group_counter <- reactiveVal(1)

        # Initialize output variables
        # group_choices applies only to metadata variables that are categorical
        # but is called for export from this module in all cases: therefore, it
        # must be initialized as NULL to avoid issues when returning for modules
        # not meeting these conditions.
        #group_choices <- NULL

        # 1. Show/hide cards based on user selections --------------------------
        # (Conditionals on non-reactive values such as options_type can be used
        # outside of server components)
        observeEvent(
          categories_selected(),
          label = glue("Show/Hide Cards: {options_type}"),
          ignoreNULL = FALSE,
          {
            # Examine the list of currently selected categories, and
            # check if the module's category name is selected.
            # If so, show the options card
            if (card_name %in% categories_selected()){
              showElement("optcard")
              # Otherwise, hide the card
            } else {
              hideElement("optcard")
            }
          })
        
       ## 1.1. Show/ hide NA warning icon ####
      
        # Pull full metadata table 
        meta_table <-
          SCUBA::fetch_metadata(
            object,
            full_table = TRUE
          )
    
        # check for NA values in metadata
        check_na <- function(meta_table, meta_vars) {
          sapply(meta_vars, function(var){
            any(is.na(meta_table[[var]]) | meta_table[[var]] == "" | is.null(meta_table[[var]]))
          })
        }
        
        is_na <- check_na(meta_table, card_name)
       
        observe({
          if (card_name %in% categories_selected()) {
            # show tooltip warning icon if NA values found
            if (isTruthy(is_na[card_name])) {
              showElement(id = "warning_icon_na")
            } else{
              #hide tooltip warning icon if no NA values found
              hideElement(id = "warning_icon_na")
            }
          }
        })

        # 2. Metadata-tab specific functions -----------------------------------
        # Determine if metadata field is categorical or numeric
        if (options_type == "metadata"){
          type <- metadata_type(object, id)
        }

        ## 2.1. Metadata Groups ####
        # User Interface to define subgroups within a metadata column
        # Only performed for categorical inputs
        if (options_type == "metadata"){
          # Test for type of metadata field after testing to see
          # if the object is a metadata entry
          if (type == "Categorical"){
            # Categorical metadata: determine number of unique values
            n_values <-
              SCUBA:::n_unique(
                object,
                meta_var = id
              )


            ### 2.1.1. Define UI for group selection ####
            observeEvent(
              input$group_metadata,
              label = "Show/Hide Groups UI",
              ignoreNULL = FALSE,
              {
                element_id <- "groups_interface"

                #Display the interface when the corresponding
                #switch is activated
                if (isTruthy(input$group_metadata)){
                  showElement(
                    id = element_id
                  )
                } else {
                  hideElement(
                    id = element_id
                  )
                }
              })

            ### 2.1.2. Module Server for Choosing Group Values ####
            # Only initiate server when there are less than 250 unique values
            # Module is not useful beyond this number of values, and performance
            # is hindered
            if (n_values <= 250){
              group_data <-
                metadata_group_fields_server(
                  id = "groups",
                  category_name = card_name,
                  unique_values =
                    str_sort(
                      SCUBA::unique_values(
                        object = object,
                        var = card_name
                        ),
                      numeric = TRUE
                      )
                  )
            }

            ### 2.1.3. Disable switch to enable metadata groups ####
            # Switch is disabled when there are more than 250 values
            if (n_values > 250){
              group_metadata_id <- "group_metadata"
              disable_style_target <- "groups_disable_transparent"

              # Diables switch to enable group formation
              shinyjs::disable(
                id = group_metadata_id
              )

              # Makes switch and label transparent
              shinyjs::addClass(
                id = disable_style_target,
                class = "disabled-input"
              )

              # Hide the label that shows beneath the groups switch by
              # default, and show a label explaining why the groups interface
              # is disabled
              shinyjs::hideElement(
                id = "groups_explanation"
                #selector = "[class *= 'show_if_groups_enabled']"
              )

              shinyjs::showElement(
                id = "disabled_groups_explanation"
                #selector = "[class *= 'show_if_groups_disabled']"
              )
            }
          }
        }

        # 3. Assay-tab Specific Functions --------------------------------------
        if (options_type == "assays"){
          ## 3.1. Disable ADT Checkbox when Selected for another assay ####
          observe(
            label = glue("{id}: Disable ADT Checkbox from Other Modules"),
            {
              # jQuery Selector to modify ADT checkboxes from other modules:
              # ends with "designate_adt" and does not have
              # a prefix equal to the current module id
              other_checkboxes_selector <-
                glue("input[id$='designate_adt']:not([id|={id}])")

              if (isTruthy(input$designate_adt)){
                # When the checkbox is selected, disable all checkboxes with the
                # id "designate_adt" that are not part of the current module
                disable(
                  selector = other_checkboxes_selector,
                  # Do not apply namespacing, since inputs from other modules
                  # are involved
                  asis = TRUE
                  )

                shinyjs::addClass(
                  selector = other_checkboxes_selector,
                  class = "disabled-label",
                  asis = TRUE
                )
              } else {
                # When the checkbox is de-selected, enable all checkboxes with
                # id "designate_adt" (across all modules)
                all_checkboxes_selector <- "input[id$='designate_adt']"

                enable(
                  selector = all_checkboxes_selector,
                  asis = TRUE
                  )

                shinyjs::removeClass(
                  class = "disabled-label",
                  selector = all_checkboxes_selector,
                  asis = TRUE
                  )
                }
              })
        }


        # 4. Restore inputs when options cards are re-sorted -------------------
        # (In metadata tab)
        # observeEvent(
        #   session$userData$config(),
        #   ignoreNULL = FALSE,
        #   ignoreInit = TRUE,
        #   label = glue("{id}: Restore Inputs Upon Rearrangement"),
        #   {
        #     # "Update" the input with the previously entered label
        #     if (!is.null(input$hr)){
        #       updateTextInput(
        #         session,
        #         inputId = "hr",
        #         # Value for this input is stored in `dropdown_title`
        #         value = input$hr
        #       )
        #     }
        #
        #     # Do the same for the groups switch
        #     if (!is.null(input$group_metadata)){
        #       updateMaterialSwitch(
        #         session,
        #         inputId = "group_metadata",
        #         value = input$group_metadata
        #       )
        #     }
        #
        #     # Will also need code to update groups input when that is completed
        #     # for 4.
        #
        #   })


        # 5. Update inputs upon loading config file ----------------------------
        if (options_type == "assays"){
          ## 5.1. Assays ####
          observeEvent(
            session$userData$config(),
            label = glue("{id}: Update Options Based on Config File"),
            {
              # Search for assay name (module ID) in loaded config file
              if (id %in% names(session$userData$config()$assays)){
                # Get config info for assay matching module ID
                config_individual <-
                  session$userData$config()$assays[[id]]

                # Update inputs
                # Text entry for assay label
                updateTextInput(
                  session,
                  inputId = "hr",
                  # Value for this input is stored in `dropdown_title`
                  value = config_individual$dropdown_title
                )

                # Checkbox to use assay label
                # If `suffix_human` is defined, a label is desired.
                # When `suffix_human` == "", a label is not desired.
                # isTruthy will cover this case, as well as this field being
                # NULL (shouldn't happen but it could)
                updateCheckboxInput(
                  session,
                  inputId = "include_label",
                  value =
                    if (isTruthy(config_individual$suffix_human)) TRUE else FALSE
                  )
                
                # Desciption of assay
                updateTextAreaInput(
                  session,
                  inputId = "description",
                  value = config_individual$description
                  )
                }
            })

        } else if (options_type == "metadata"){
          ## 5.2 Metadata ####
          observeEvent(
            session$userData$config(),
            label = glue("{id}: Update Options Based on Config File"),
            {
              # Search for variable name in loaded config file
              if (card_name %in% names(session$userData$config()$metadata)){
                # Get config info for matching metadata variable
                config_individual <-
                  session$userData$config()$metadata[[card_name]]
                
                # Freeze inputs
                freezeReactiveValue(input, "hr")
                freezeReactiveValue(input, "group_metadata")

                # Update inputs using config file
                # Label for metadata column (text input, input$hr)
                updateTextInput(
                  session,
                  inputId = "hr",
                  # #Value `label` section of config for variable
                     value = config_individual$label
                )
                
                # Metadata variable description
                updateTextInput(
                  session,
                  inputId = "description",
                  # Value `label` section of config for variable
                  value = config_individual$description
                )
                
                # Switch for defining groups (input$group_metadata)
                # Switch on if `groups` is not NULL
                updateMaterialSwitch(
                  session,
                  inputId = "group_metadata",
                  value =
                    if (!is.null(config_individual$groups)) TRUE else FALSE
                  )

                ## Create modules for each group
                # One module is already created by updateMaterialSwitch
                # if groups is not NULL

                # if (!is.null(config_individual$groups)){
                #   n_groups <- length(config_individual$groups)
                #   # If fields have already been created, delete all fields
                #   # if (group_counter() > 1){
                #   #   for (i in 2:group_counter()){
                #   #     module_id <- glue("groups-{i}")
                #   #
                #   #     # Check if groups_i has already been removed
                #   #     if (!is.null(group_choices[[module_id]])){
                #   #       removeUI(
                #   #         # ID of container with ui is
                #   #         selector = glue("groups-{i}")
                #   #       )
                #   #
                #   #       # Remove module output from groups list
                #   #       group_choices[[module_id]] <- NULL
                #   #     }
                #   #
                #   #   }
                #   # }
                #
                #
                #   # Determine number of modules to create. This depends on
                #   # the number of fields in the config file and the number of
                #   # group modules already created in the app (as defined by
                #   # group_counter)
                #
                #
                #   # Additional modules are created if more than one group is
                #   # read
                #   # from the config file
                #   if (n_groups > 1){
                #     # Also, check value of the module counter. If it is
                #     # already equal to the number of groups in the config
                #     # file,
                #
                #     for (i in 2:n_groups){
                #       print("Create module")
                #
                #       # Run exact same code as in #2.1.4 (will need to
                #       # re-structure groups model for this to work correctly)
                #       # Increase counter by one
                #       new_value <- group_counter() + 1
                #       group_counter(new_value)
                #
                #       # Use the action button's value to create an id.
                #       # Use group_counter() for ID creation
                #       nested_id <- glue("groups-{group_counter()}")
                #
                #       # Add module UI
                #       # For arguments that reference an element by id that
                #       # is not
                #       # a Shiny input or output, namespacing must be used
                #       insertUI(
                #         selector = glue("#{ns('add_group')}"),
                #         where = "beforeBegin",
                #         # Namespacing should also be used to call the UI
                #         # components of modules, but not the server component
                #         ui =
                #           metadata_group_fields_ui(
                #             ns(nested_id),
                #             remove_button = TRUE,
                #             temp_choices =
                #               str_sort(
                #                 unique(object@meta.data[[card_name]]),
                #                 numeric=TRUE
                #               )
                #           )
                #       )
                #
                #       # Add module server instance
                #       module_output <-
                #         metadata_group_fields_server(
                #           id = nested_id,
                #           possible_selections = category_values
                #           )
                #
                #       # Store output in group_choices and use observe() to
                #       # reactively update group_choices when the output changes
                #       # observe(
                #       #   label = glue("Observer for {card_name}-{nested_id}"),
                #       #   {
                #       #     print("ID")
                #       #     print(id)
                #       #     print("Groups module ID")
                #       #     print(nested_id)
                #       #     print("Add new value to reactive list")
                #       #     group_choices[[nested_id]] <-
                #       #       module_output()
                #       #     print("Done")
                #       #   })
                #      }
                #    }
                # }
                }
              })

        } else if (options_type == "reductions"){
          ## 5.3. Reductions ####

          # Loading information for reductions
          observeEvent(
            session$userData$config(),
            {
              # Search for assay name (module ID) in loaded config file
              if (id %in% names(session$userData$config()$reductions)){
                # Get config info for assay matching module ID
                config_individual <-
                  session$userData$config()$reductions[[id]]

                # Update inputs
                # Text entry for reduction label
                updateTextInput(
                  session,
                  inputId = "hr",
                  # Use `label` value for the current reduction
                  value = config_individual$label
                  )
                
                # Reduction description
                updateTextAreaInput(
                  session,
                  inputId = "description",
                  value = config_individual$description
                )
              }

            })
        }

        # TEMP Observer for status of metadata outputs ####
        # observe({
        #   if (id %in% c("htb", "treatment", "response", "clusters")){
        #     print(glue("{id}: change in value of input$hr:"))
        #     print(input$hr)
        #   }
        # })

        # 6. Returns from Module -----------------------------------------------
        if (options_type == "assays"){
          ## 6.1. Assays ####

          # For assays, return
          # 1. Assay: the name of the assay as defined in the Seurat object
          # 2. Key: the prefix to be added to features server-side to search for
          # them from the assay
          # 3. Suffix_human: a suffix that is added to the feature in
          # parentheses and displayed in the app in dropdown menus and in the
          # titles of plots
          # 4. Dropdown_title: a user-defined label for the assay that will be
          # added to all dropdown menus in the app that display features from
          # multiple assays
          return_list_assays <-
            reactive({
              list(
                `assay` = card_name,
                `key` =
                  cellDIVER:::make_key(
                    object,
                    assay = card_name
                  ),
                `suffix_human` =
                  if (input$include_label == TRUE) input$hr else "",
                `dropdown_title` = input$hr,
                `description` = input$description   # new: add assay description
              )
            })
            
          return(return_list_assays)

        } else if (options_type == "metadata"){
          ## 6.2. Metadata ####

          # Return options depend on the type of metadata (Categorical metadata
          # has a reactive list of metadata group choices; numeric and other
          # types have a non-reactive value of NULL for group_choices)
          if (type == "Categorical"){
            # Return metadata-specific variables as a list
            # Returns
            # 1. The name of the category (machine-readable and used for
            #    the values of choices in the app)
            # 2. The user-specified label (human-readable and used for the
            #    keys of choices)
            # 3. The metadata groups selected, if specified by the user
            return_list_metadata <-
              reactive({
                list(
                  `meta_colname` = card_name,
                  `label` = input$hr,
                  `description` = input$description,
                  # groups: defined if the switch to group metadata is 
                  # turned on, and set to NULL otherwise.
                  `groups` =
                    if (isTruthy(input$group_metadata)){
                      group_data()
                    } else NULL
                )
              })
            
          } else {
            # Numeric metadata and other types: group_choices is NULL
            # (cards presently don't display for numeric metadata)
            return_list_metadata <- 
              reactive({
                list(
                  `meta_colname` = card_name,
                  `label` = input$hr,
                  `description` = input$description,
                  `groups` = NULL
                )
              })
          }

          return(return_list_metadata)

        } else if (options_type == "reductions"){
          ## 6.3 Reductions ####

          # For reductions, return
          # 1. Reduction: the name of the reduction as defined in the Seruat
          # object, used for accessing the reduction in the app
          # 2. Label: a human-readable name for the reduction, for display in
          # the app
          return_list_reductions <-
            reactive({
              list(
                `reduction`= card_name,
                `label` = input$hr,
                `description` = input$description
              )
            })

          return(return_list_reductions)
          }
      })
}
