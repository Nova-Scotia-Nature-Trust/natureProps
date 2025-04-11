module_data_viewer_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "Data Viewer",
    card(
      full_screen = TRUE,
      height = "100%", # Set card height to 100%
      card_header(
        selectInput(
          inputId = ns("data_view_input"),
          label = "Select Data Frame View",
          choices = list(
              "PIDs" = "pid_view_01",
              "Landowner Details" = "landowner_details",
              "Communication History" = "communication_data"
          ),
          selected = "pid_view_01"
        )
      ),
      card_body(
        style = "height: calc(100vh - 265px); padding: 0.5rem 1rem;", # Set explicit height and remove padding
        dataTableOutput(outputId = ns("view_df"), height = "100%") # Set output height to 100%
      )
    )
  )
}


module_data_viewer_server <- function(id, db_con) {
  moduleServer(id, function(input, output, session) {
    
    # Load mapping tables
    df_views <- read_xlsx("inputs/field and function mapping tables/df_views.xlsx")
    
    # Reactive to capture the selected view
    view_scenario <- reactive({
      input$data_view_input
    })
    
    # Process different datasets based on selection
    processed_data <- reactive({
      # Get the selected view
      selected_view <- view_scenario()
      
      # Different processing for each view type
      if (selected_view == "landowner_details") {
        # Process landowner details data
        lan_own_details <- dbReadTable(db_con, "landowner_details")
        
        lo_pids <- dbGetQuery(
          db_con,
          statement = glue_sql(
            "SELECT pid, landowner_contact_id FROM parcels WHERE landowner_contact_id IN ({lan_own_details$id*});",
            .con = db_con
          )
        ) |>
          as_tibble() |> 
          group_by(landowner_contact_id) |> 
          summarise(landowner_pids = paste(pid, collapse = ", "))
        
        lan_own_df <- lan_own_details |> 
          left_join(lo_pids, join_by(id == landowner_contact_id)) |> 
          relocate(landowner_pids, .before = dnc)
        
        map_name_ref <- read_xlsx("inputs/field and function mapping tables/db_to_df_fields.xlsx") |>
          filter(group == "landowner_contact_details") |> 
          filter(db_name %in% names(lan_own_df))
        
        df_named_list <- map_name_ref |>
          select(df_name, db_name) |>
          deframe()
        
        data <- lan_own_df |> 
          select(all_of(df_named_list))
        
        # Add ordering attribute
        attr(data, "order_column") <- 1
        attr(data, "order_direction") <- "asc"
        
      } else if (selected_view == "pid_view_01") {
        # Process PID view types
        fields_to_fetch <- df_views |>
          filter(!!sym(selected_view) == TRUE) |>
          select(db_name) |>
          pull()
        
        # Map database names to display names
        map_name_ref <- read_xlsx("inputs/field and function mapping tables/db_to_df_fields.xlsx") |>
          filter(group == "parcels") |> 
          filter(db_name %in% fields_to_fetch)
        
        df_named_list <- map_name_ref |>
          select(df_name, db_name) |>
          deframe()
        
        # Fetch raw data
        raw_df <- dbGetQuery(
          db_con,
          statement = glue_sql(
            "SELECT {`fields_to_fetch`*} FROM parcels;",
            .con = db_con
          )
        ) |>
          as_tibble()
        
        # Process lookup tables if needed
        join_lookup_fields <- df_views |>
          filter(!!sym(selected_view) == TRUE) |>
          filter(!is.na(lookup_table)) |>
          select(-c(df_name, db_name, pid_view_01, pid_view_02, pid_view_03, df))
        

          # Load lookup tables
          db_lookup_tables <- list(
            phase = dbReadTable(db_con, "phase"),
            ranking = dbReadTable(db_con, "ranking"),
            acquisition_type = dbReadTable(db_con, "acquisition_type"),
            properties = dbReadTable(db_con, "properties")
          )
          
          # Lookup join function (scoping issues means the function needs to be called here beacuse I'm using NSE in the function)
          join_lookup <- function(lookup_table_name, df_key, lookup_key, lookup_value, new_col_name, df, db_lookup_tables) {
            lookup_table <- db_lookup_tables[[lookup_table_name]]

            output <- df |>
              left_join(lookup_table, by = join_by(!!sym(df_key) == !!sym(lookup_key))) |>
              rename(!!new_col_name := !!sym(lookup_value)) |>
              select(-all_of(df_key)) |>
              select(pid, !!new_col_name)

            return(output)
          }
          
          # Apply all lookups
          lookup_results <- pmap(join_lookup_fields, join_lookup, raw_df, db_lookup_tables)
          
          # Combine all lookup results
          join_dfs <- function(x, y) left_join(x, y, by = join_by(pid))
          lookup_combined <- purrr::reduce(lookup_results, join_dfs) |>
            rename_with(~ str_replace(.x, "_value$", "_id"), ends_with("_value"))
          
          # Combine with main dataframe
          data <- raw_df |>
            select(-all_of(setdiff(names(lookup_combined), "pid"))) |>
            left_join(lookup_combined, join_by(pid)) |>
            select(all_of(fields_to_fetch)) |>
            select(all_of(df_named_list))
        
        # Add ordering attribute
        attr(data, "order_column") <- 1
        attr(data, "order_direction") <- "desc"
      } else if (selected_view == "communication_data"){
        
        # Map database names to display names
        
        raw_df <- dbGetQuery(
          db_con,
          statement = glue_sql(
            "SELECT * FROM landowner_communication;",
            .con = db_con
          )
        ) |>
          as_tibble()
        
        print(raw_df)
        
        map_name_ref <- read_xlsx("inputs/field and function mapping tables/db_to_df_fields.xlsx") |>
          filter(group == "landowner_communication") |> 
          filter(db_name %in% names(raw_df))
        
        df_named_list <- map_name_ref |>
          select(df_name, db_name) |>
          deframe()
        
        print(str(df_named_list))
        
        comm_purpose <- dbReadTable(db_con, "communication_purpose")
        comm_method <- dbReadTable(db_con, "communication_method")
        
        
        ## Adapt this to use the lookup function and input table
        data <- raw_df |> 
          left_join(comm_purpose, 
                    join_by(communication_purpose_id == id)) |> 
          select(-communication_purpose_id) |> 
          rename(communication_purpose_id = purpose_value) |> 
          left_join(comm_method, 
                    join_by(communication_method_id == id)) |> 
          select(-communication_method_id) |> 
          rename(communication_method_id = method_value) |> 
          select(names(raw_df))
        
        data <- data |> 
          select(all_of(df_named_list))
        
        attr(data, "order_column") <- 0
        attr(data, "order_direction") <- "asc"
        
        
      }
      
      return(data)
    })
    
    # Setup table layout
    dom_layout <- "
    <'row'<'col-sm-9'l><'col-sm-3 text-right'B>><'row'<'col-sm-12'tr>><'row'<'col-sm-5'i><'col-sm-7'p>>
    "
    
    # Get ordering information
    table_order <- reactive({
      list(list(
        attr(processed_data(), "order_column"), 
        attr(processed_data(), "order_direction")
      ))
    })
    
    # Render the datatable
    output$view_df <- renderDT({
      datatable(
        processed_data(),
        options = list(
          pageLength = 10,
          scrollX = TRUE, 
          dom = dom_layout,
          buttons = list(
            "copy", "csv", "excel", "pdf", "print"
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