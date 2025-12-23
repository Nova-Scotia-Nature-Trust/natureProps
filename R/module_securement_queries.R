# UI ----
module_securement_queries_ui <- function(id) {
  ns <- NS(id)

  div(
    style = "height: 100%; display: flex; flex-direction: column;",
    card(
      full_screen = TRUE,
      height = "100%",
      layout_sidebar(
        sidebar = sidebar(
          open = TRUE,
          width = 300,
          selectizeInput(
            ns("query_choice"),
            "Select query",
            choices = c(
              "",
              "Insurance view",
              "Focal area properties",
              "Securement action",
              "Property sizes"
            ),
            multiple = FALSE,
            width = "100%"
          ),
          # Conditional UI for additional inputs
          uiOutput(ns("conditional_contact_ui")),
          actionButton(
            inputId = ns("run_query"),
            label = "Run query",
            width = "100%",
            class = "btn-primary"
          ),
          actionButton(
            inputId = ns("clear_inputs"),
            label = "Clear Inputs",
            width = "100%"
          )
        ),
        # Main layout - results card
        card(
          height = "100%",
          card_header(h5("Query Result")),
          card_body(
            DTOutput(outputId = ns("view_df"), height = "100%")
          )
        )
      )
    )
  )
}

# Server ----
module_securement_queries_server <- function(
  id,
  db_con,
  db_updated = NULL,
  focal_pids_rv
) {
  moduleServer(id, function(input, output, session) {
    # Add this reactive value to track table clearing
    table_data <- reactiveVal(NULL)

    ## Conditional UI (based on query select) ----
    output$conditional_contact_ui <- renderUI({
      ns <- session$ns

      req(input$query_choice)

      if (input$query_choice == "Focal area properties") {
        selectizeInput(
          ns("focal_area"),
          "Select Focal Area",
          choices = NULL,
          multiple = TRUE,
          options = list(
            create = FALSE,
            placeholder = "Select a focal area"
          )
        )
      } else if (input$query_choice == "Securement action") {
        div(
          selectizeInput(
            ns("closing_year"),
            "Select Closing Year",
            choices = NULL,
            multiple = TRUE,
            options = list(
              create = FALSE,
              placeholder = "Select a closing year"
            )
          ),
          selectizeInput(
            ns("securement_probability"),
            "Select Securement Probability",
            choices = NULL,
            multiple = TRUE,
            options = list(
              create = FALSE,
              placeholder = "Select a probability"
            )
          ),
          input_switch(
            ns("prop_view"),
            "Properties only",
            value = FALSE
          )
        )
      } else {
        return(NULL)
      }
    })

    ## Focal area reactive ----
    # Define a reactive for Focal Areas that depends on db_updated, if provided
    focal_areas_reactive <- reactive({
      # Only try to use db_updated if it is not NULL.
      if (!is.null(db_updated)) {
        db_updated() # Creates the reactive dependency; ignore the return value.
      }
      # Query database for Focal Areas
      dbGetQuery(
        conn = db_con,
        statement = glue_sql(
          "SELECT DISTINCT fa.internal_value 
          FROM focus_area_internal fa
          INNER JOIN properties props ON fa.id = props.focus_area_internal_id
          WHERE props.ownership_id NOT IN (11, 12, 13)
          ORDER BY fa.internal_value;",
          .con = db_con
        )
      ) |>
        pull()
    })

    closing_year_reactive <- reactive({
      if (!is.null(db_updated)) {
        db_updated()
      }
      # Query database for Closing Years
      dbGetQuery(
        conn = db_con,
        statement = "SELECT DISTINCT anticipated_closing_year FROM properties;"
      ) |>
        pull()
    })

    securement_probability_reactive <- reactive({
      if (!is.null(db_updated)) {
        db_updated()
      }
      # Query database for Securement Probability values
      dbGetQuery(
        conn = db_con,
        statement = "SELECT DISTINCT probability_value 
                     FROM securement_probability 
                     ORDER BY probability_value;"
      ) |>
        pull()
    })

    ## Initialize inputs ----
    observe({
      req(input$query_choice)

      # Update the focal area selectize input (update whenever focal_areas_reactive() changes)
      updateSelectizeInput(
        session,
        inputId = "focal_area",
        choices = focal_areas_reactive(),
        server = TRUE
      )

      updateSelectizeInput(
        session,
        inputId = "closing_year",
        choices = closing_year_reactive(),
        server = TRUE
      )

      updateSelectizeInput(
        session,
        inputId = "securement_probability",
        choices = securement_probability_reactive(),
        server = TRUE
      )
    })

    # Event :: Run query ----
    observeEvent(input$run_query, {
      req(input$query_choice)

      if (input$query_choice == "Focal area properties") {
        req(input$focal_area)

        col_name <- "Focus Area (Internal)"

        data <- dbGetQuery(
          db_con,
          glue_sql(
            "SELECT * FROM view_focal_areas_securement WHERE {`col_name`} IN ({input$focal_area*})",
            .con = db_con
          )
        )
      } else if (input$query_choice == "Insurance view") {
        data <- dbGetQuery(db_con, "SELECT * FROM view_insurance;")
      } else if (input$query_choice == "Securement action") {
        req(input$closing_year)

        data <- dbGetQuery(db_con, "SELECT * FROM view_action_items;")

        additional_data <- dbGetQuery(
          conn = db_con,
          statement = "
          SELECT par.pid, 
                prop.anticipated_closing_year,
                se.probability_value
          FROM parcels par
          LEFT JOIN properties prop ON par.property_id = prop.id
          LEFT JOIN securement_probability se ON prop.securement_probability_id = se.id;"
        )

        data <- data |>
          left_join(additional_data, join_by(PID == pid))

        data <- data |>
          relocate(anticipated_closing_year, probability_value) |>
          rename(
            `Closing Year` = anticipated_closing_year,
            `Securement Probability` = probability_value
          )

        # Filter by securement probability if selected
        if (isTruthy(input$securement_probability)) {
          data <- data |>
            filter(`Securement Probability` %in% input$securement_probability)
        }

        data <- data |>
          filter(`Closing Year` == input$closing_year) |>
          arrange(`Property Name`)

        if (input$prop_view == TRUE) {
          data <- data |>
            distinct(`Property Name`, .keep_all = TRUE)
        }
      } else if (input$query_choice == "Property sizes") {
        data <- dbGetQuery(db_con, "SELECT * FROM view_property_sizes;")
      }

      table_data(data)
    })

    # Event :: Clear inputs ----
    observeEvent(input$clear_inputs, {
      updateSelectizeInput(
        session,
        inputId = "query_choice",
        choices = c(
          "",
          "Insurance view",
          "Focal area properties",
          "Securement action",
          "Property sizes"
        ),
        selected = character(0),
        server = TRUE
      )

      updateSelectizeInput(
        session,
        inputId = "focal_area",
        choices = focal_areas_reactive(),
        selected = character(0),
        server = TRUE
      )

      updateSelectizeInput(
        session,
        inputId = "closing_year",
        choices = closing_year_reactive(),
        server = TRUE
      )

      updateSelectizeInput(
        session,
        inputId = "securement_probability",
        choices = securement_probability_reactive(),
        selected = character(0),
        server = TRUE
      )

      # Add this line to clear the table data
      table_data(NULL)

      # Clear reactive pids
      focal_pids_rv(NULL)
    })

    # Render data table ----

    # Setup table layout
    dom_layout <- "
    <'row'<'col-sm-10'l><'col-sm-2 text-right'B>><'row'<'col-sm-12'tr>><'row'<'col-sm-5'i><'col-sm-7'p>>
    "

    output$view_df <- renderDT({
      req(table_data())

      # Convert character columns to factors to get select inputs
      data_for_display <- table_data() |>
        mutate(across(where(is.character), as.factor))

      DT::datatable(
        data_for_display,
        options = list(
          pageLength = 10,
          lengthMenu = list(
            c(10, 25, 50, 100, -1),
            c('10', '25', '50', '100', 'All')
          ),
          scrollX = TRUE,
          scrollY = "400px",
          fixedHeader = TRUE,
          dom = dom_layout,
          buttons = list("copy", "excel"),
          stateSave = FALSE
        ),
        filter = list(
          position = "top",
          clear = TRUE,
          plain = TRUE
        ),
        rownames = FALSE,
        selection = "single",
        extensions = c("Buttons", "FixedHeader")
      )
    })
  })
}
