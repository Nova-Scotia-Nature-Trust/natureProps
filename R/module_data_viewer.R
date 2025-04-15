module_data_viewer_ui <- function(id, panel_id) {
  ns <- NS(id)

  # Define choices based on panel_type
  choices_list <- if (panel_id == "panel_01") {
    list(
      "Select a view from the list" = "",
      "PIDs" = "pid_view_01",
      "Landowner Details" = "landowner_details_view",
      "Communication History" = "communication_data_view"
    )
  } else if (panel_id == "panel_02") {
    # Default choices for panel1 or any other panel
    list(
      "Select a view from the list" = "",
      "Action Items" = "pid_view_02"
    )
  }
  
  nav_panel(
    title = "Data Viewer",
    card(
      full_screen = TRUE,
      height = "100%", # Set card height to 100%
      card_header(
        selectInput(
          inputId = ns("data_view_input"),
          label = "Data Table View",
          choices = choices_list,
          selected = ""
        )
      ),
      card_body(
        style = "height: calc(100vh - 265px); padding: 0.5rem 1rem;", # Set explicit height and remove padding
        DTOutput(outputId = ns("view_df"), height = "100%") # Set output height to 100%
      )
    )
  )
}


module_data_viewer_server <- function(id, db_con, db_updated = NULL) {
  moduleServer(id, function(input, output, session) {
    ## Load data view metadata table (parameters and attribute names)
    # Change name to df_views_meta
    df_view_meta <- read_xlsx("inputs/field and function mapping tables/df_views.xlsx")

    ## Reactive to capture the selected view
    view_scenario <- reactive({
      input$data_view_input
    })

  
    ## Create a data frame to render with DT
    output_view_data <- reactive({
      
      ## This ensures the reactive will re-execute when db_updated() value changes
      ## Only use if data_changed is provided
      if (!is.null(db_updated)) {
        db_updated()
      }
      
      ## Access the selected view
      selected_view <- view_scenario()

      ## Landowner details view ----
      if (selected_view == "landowner_details_view") {
        ## Get landowner details DB data
        land_own_details <- dbReadTable(db_con, "landowner_details")

        ## Get PIDs associated with each landowner
        lan_own_pids <- dbGetQuery(
          db_con,
          statement = glue_sql(
            "SELECT pid, landowner_contact_id FROM parcels WHERE landowner_contact_id IN ({land_own_details$id*});",
            .con = db_con
          )
        ) |>
          group_by(landowner_contact_id) |>
          summarise(landowner_pids = paste(pid, collapse = ", "))

        ## Update the contact details with associated PIDs
        land_own_details <- land_own_details |>
          left_join(lan_own_pids, join_by(id == landowner_contact_id)) |>
          relocate(landowner_pids, .before = dnc)

        ## Transform to pretty column names
        pretty_col_names <- df_view_meta |>
          filter(group == "landowner_contact_details") |>
          filter(db_name %in% names(land_own_details)) |>
          select(df_name, db_name) |>
          deframe()

        ## Assign result to 'data' object
        data <- land_own_details |>
          select(all_of(pretty_col_names))

        ## Add ordering attribute for DT table
        attr(data, "order_column") <- 1
        attr(data, "order_direction") <- "asc"

        ## PID view 01/02 ----
      } else if (selected_view %in% c("pid_view_01", "pid_view_02")) {
        ## Get list of parcel table fields needed for view
        fields_to_fetch <- df_view_meta |>
          filter(!!sym(selected_view) == TRUE) |>
          select(db_name) |>
          pull()

        ## Fetch raw parcel data
        raw_df <- dbGetQuery(
          db_con,
          statement = glue_sql(
            "SELECT {`fields_to_fetch`*} FROM parcels;",
            .con = db_con
          )
        )

        ## Create a list of lookup tables
        db_lookup_tables <- list(
          phase = dbReadTable(db_con, "phase"),
          ranking = dbReadTable(db_con, "ranking"),
          acquisition_type = dbReadTable(db_con, "acquisition_type"),
          properties = dbReadTable(db_con, "properties"),
          action_item_status = dbReadTable(db_con, "action_item_status")
        )

        ## Get the lookup function mapping data from metadata file
        join_lookup_fields <- df_view_meta |>
          filter(!!sym(selected_view) == TRUE) |>
          filter(!is.na(lookup_table)) |>
          select(lookup_table, df_key, lookup_key, lookup_value, new_col_name)

        ## Define the join lookup function (NSE issues mean it can't be sourced in global)
        ##!! *** Note this function takes "pid" in last select *** !!##
        join_lookup <- function(lookup_table_name, df_key, lookup_key,
                                lookup_value, new_col_name,
                                df, db_lookup_tables) {
          lookup_table <- db_lookup_tables[[lookup_table_name]]

          output <- df |>
            left_join(lookup_table, by = join_by(!!sym(df_key) == !!sym(lookup_key))) |>
            rename(!!new_col_name := !!sym(lookup_value)) |>
            select(-all_of(df_key)) |>
            select(pid, !!new_col_name)

          return(output)
        }

        ## Iterate the function over lookup fields in parcels table
        ## "raw_df" & "db_lookup_tables" are constants in pmap
        lookup_results <- pmap(join_lookup_fields, join_lookup, raw_df, db_lookup_tables)

        ## Combine all lookup function results & rename fields
        join_dfs <- function(x, y) left_join(x, y, by = join_by(pid))
        lookup_combined <- purrr::reduce(lookup_results, join_dfs) |>
          rename_with(~ str_replace(.x, "_value$", "_id"), ends_with("_value"))

        ## Transform to pretty column names
        pretty_col_names <- df_view_meta |>
          filter(group == "parcels") |>
          filter(db_name %in% fields_to_fetch) |>
          select(df_name, db_name) |>
          deframe()

        ## Combine the lookup columns with the non-lookup data
        ## Rename fields
        data <- raw_df |>
          select(-all_of(setdiff(names(lookup_combined), "pid"))) |>
          left_join(lookup_combined, join_by(pid)) |>
          select(all_of(pretty_col_names))

        
        if(selected_view == "pid_view_01"){
          
          ## Query to get PID sizes for
          pid_size <- dbGetQuery(
            db_con,
            statement = '
              SELECT pa.pid AS "PID",
                    info.area_ha AS "Size (ha)",
                    ROUND(info.area_ha * 2.47105, 2) AS "Size (acre)"
              FROM parcels AS pa
              LEFT JOIN parcel_info AS info ON pa.id = info.parcel_id;
            '
          ) |>
            as_tibble()
          
          ## Add PID sizes
          ## Assign result to 'data' object
          data <- data |>
            left_join(pid_size, join_by(PID)) |>
            relocate(contains("size"), .after = `Date Updated`)
          
          ## Add ordering attribute for DT table
          attr(data, "order_column") <- 1
          attr(data, "order_direction") <- "desc"
          
        } else if (selected_view == "pid_view_02"){
          
          ## Add ordering attribute for DT table
          attr(data, "order_column") <- 2
          attr(data, "order_direction") <- "desc"          
          
        }

        ## Communication data view ----
      } else if (selected_view == "communication_data_view") {
        ## Fetch raw landowner communication data
        raw_df <- dbGetQuery(
          db_con,
          statement = glue_sql(
            "SELECT * FROM landowner_communication;",
            .con = db_con
          )
        ) |>
          as_tibble()

        ## Create a list of lookup tables
        db_lookup_tables <- list(
          communication_purpose = dbReadTable(db_con, "communication_purpose"),
          communication_method = dbReadTable(db_con, "communication_method")
        )

        ## Get the lookup function mapping data from metadata file
        join_lookup_fields <- df_view_meta |>
          filter(!!sym(selected_view) == TRUE) |>
          filter(!is.na(lookup_table)) |>
          select(lookup_table, df_key, lookup_key, lookup_value, new_col_name)

        ## Define the join lookup function (NSE issues mean it can't be sourced in global)
        ## Note this function takes "id" in last select
        join_lookup <- function(lookup_table_name, df_key,
                                lookup_key, lookup_value, new_col_name, df, db_lookup_tables) {
          lookup_table <- db_lookup_tables[[lookup_table_name]]

          output <- df |>
            left_join(lookup_table, by = join_by(!!sym(df_key) == !!sym(lookup_key))) |>
            rename(!!new_col_name := !!sym(lookup_value)) |>
            select(-all_of(df_key)) |>
            select(id, !!new_col_name)

          return(output)
        }

        ## Iterate the function over lookup fields in parcels table
        ## "raw_df" & "db_lookup_tables" are constants in pmap
        lookup_results <- pmap(join_lookup_fields, join_lookup, raw_df, db_lookup_tables)

        ## Combine all lookup function results & rename fields
        join_dfs <- function(x, y) left_join(x, y, by = join_by(id))
        lookup_combined <- purrr::reduce(lookup_results, join_dfs) |>
          rename_with(~ str_replace(.x, "_value$", "_id"), ends_with("_value"))

        ## Transform to pretty column names
        pretty_col_names <- df_view_meta |>
          filter(group == "landowner_communication") |>
          filter(db_name %in% names(raw_df)) |>
          select(df_name, db_name) |>
          deframe()

        # Combine with main dataframe
        data <- raw_df |>
          select(-all_of(setdiff(names(lookup_combined), "id"))) |>
          left_join(lookup_combined, join_by(id)) |>
          select(all_of(pretty_col_names))

        attr(data, "order_column") <- 0
        attr(data, "order_direction") <- "asc"
      } else if (selected_view == ""){
        
        data <- NULL
        
      }

      return(data)
    })

    # Setup table layout
    dom_layout <- "
    <'row'<'col-sm-10'l><'col-sm-2 text-right'B>><'row'<'col-sm-12'tr>><'row'<'col-sm-5'i><'col-sm-7'p>>
    "

    # Get ordering information
    table_order <- reactive({
      list(list(
        attr(output_view_data(), "order_column"),
        attr(output_view_data(), "order_direction")
      ))
    })

    # Render the datatable
    output$view_df <- renderDT({
      datatable(
        output_view_data(),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = dom_layout,
          buttons = list(
            "copy", "excel", "pdf", "print"
          ),
          order = table_order(),
          stateSave = FALSE
        ),
        filter = list(position = "top", clear = FALSE),
        rownames = FALSE,
        selection = "single",
        extensions = c("Buttons")
      )
    })
  })
}
