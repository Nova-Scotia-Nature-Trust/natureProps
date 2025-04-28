# UI ----
module_outreach_queries_ui <- function(id) {
  ns <- NS(id)

  div(
    style = "height: 100%; display: flex; flex-direction: column;",
    card(
      full_screen = TRUE,
      height = "100%", # Make card fill available space
      layout_sidebar(
        sidebar = sidebar(
          "",
          open = TRUE,
          actionButton(inputId = ns("run_query"), label = "Run query"),
          actionButton(inputId = ns("clear_inputs"), label = "Clear Inputs"),
        ),
        # Main layout
        div(
          style = "height: 100%; display: flex; flex-direction: column;",
          layout_columns(
            height = "100%",
            col_widths = c(3, 9),
            # First card
            card(
              height = "100%",
              card_header(h5("Query List")),
              card_body(
                div(
                  style = "display: flex; flex-direction: column; gap: 15px;",
                  selectizeInput(
                    ns("query_choice"),
                    "Select query",
                    choices = c("", "Focal area outreach priorities"),
                    multiple = FALSE,
                    width = "80%"
                  ),
                  uiOutput(ns("conditional_contact_ui")),
                  layout_columns(),
                  layout_columns(),
                  layout_columns(),
                  # Add a spacer div to prevent pushing everything to bottom
                  div(style = "flex-grow: 1;")
                )
              )
            ),
            # Second card
            card(
              height = "100%",
              card_header(h5("Query Result")),
              card_body(
                div(
                  style = "display: flex; flex-direction: column; gap: 15px;",
                  DTOutput(outputId = ns("view_df"), height = "100%"),
                  layout_columns(),
                  layout_columns(),
                  layout_columns(),
                  # Add a spacer div to prevent pushing everything to bottom
                  div(style = "flex-grow: 1;")
                )
              )
            )
          )
        )
      )
    )
  )
}

# Server ----
module_outreach_queries_server <- function(id, db_con, db_updated = NULL) {
  moduleServer(id, function(input, output, session) {
    df_view_meta <- read_xlsx("inputs/field and function mapping tables/df_views.xlsx")

    # Add this reactive value to track table clearing
    table_data <- reactiveVal(NULL)

    ## Conditional UI ----
    output$conditional_contact_ui <- renderUI({
      ns <- session$ns

      req(input$query_choice)

      if (input$query_choice == "Focal area outreach priorities") {
        selectizeInput(
          ns("focal_area"),
          "Select Focal Area",
          choices = NULL,
          multiple = FALSE,
          options = list(
            create = FALSE,
            placeholder = "Select a focal area"
          )
        )
      } else if (input$communication_type == "XXX") {

      }
    })

    # Define a reactive for Focal Areas that depends on db_updated, if provided
    focal_areas_reactive <- reactive({
      # Only try to use db_updated if it is not NULL.
      if (!is.null(db_updated)) {
        db_updated() # Creates the reactive dependency; ignore the return value.
      }
      # Query database for Focal Areas
      dbGetQuery(conn = db_con, "SELECT internal_value FROM focus_area_internal;") |>
        pull()
    })


    # Initialize the select inputs with data from the DB
    observe({
      req(input$query_choice)

      # Update the focal area selectize input – this will update whenever focal_areas_reactive() changes
      updateSelectizeInput(session, inputId = "focal_area", choices = focal_areas_reactive(), server = TRUE)
    })

    # Clear inputs when clear button is clicked
    observeEvent(input$clear_inputs, {
      updateSelectizeInput(session,
        inputId = "query_choice",
        choices = c("", "Focal area outreach priorities"),
        selected = character(0), server = TRUE
      )

      updateSelectizeInput(session,
        inputId = "focal_area", choices = focal_areas_reactive(),
        selected = character(0), server = TRUE
      )

      # Add this line to clear the table data
      table_data(NULL)
    })

    # Triggered only when run_query is clicked
    observeEvent(input$run_query, {
      req(input$focal_area)
      selected_view <- "pid_view_03"

      focal_area_id <- dbGetQuery(
        conn = db_con,
        statement = glue_sql(
          "SELECT id FROM focus_area_internal WHERE internal_value = {input$focal_area}",
          .con = db_con
        )
      )

      focal_properties <- dbGetQuery(
        conn = db_con,
        statement = glue_sql(
          "SELECT id, property_name FROM properties WHERE focus_area_internal_id = {focal_area_id}",
          .con = db_con
        )
      )


      focal_pids <- dbGetQuery(
        conn = db_con,
        statement = glue_sql(
          "SELECT id, pid, priority_securement_ranking_id FROM parcels WHERE property_id IN ({focal_properties$id*}) AND priority_securement_ranking_id <= 3;",
          .con = db_con
        )
      )

      fields_to_fetch <- df_view_meta |>
        filter(!!sym(selected_view) == TRUE) |>
        select(db_name) |>
        pull()

      raw_df <- dbGetQuery(
        conn = db_con,
        statement = glue_sql(
          "SELECT {`fields_to_fetch`*} FROM parcels WHERE pid IN ({focal_pids$pid*});",
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
      ## !! *** Note this function takes "pid" in last select *** !!##
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


      # Update the reactive value with the new data
      table_data(data)
    })


    # Setup table layout
    dom_layout <- "
    <'row'<'col-sm-10'l><'col-sm-2 text-right'B>><'row'<'col-sm-12'tr>><'row'<'col-sm-5'i><'col-sm-7'p>>
    "


    # Render the datatable
    output$view_df <- renderDT({
      # Use req to make sure we only render when we have data
      req(table_data())

      datatable(
        table_data(),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = dom_layout,
          buttons = list(
            "copy", "excel", "pdf", "print"
          ),
          # order = table_order(),
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
