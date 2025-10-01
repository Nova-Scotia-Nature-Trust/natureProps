# UI ----
module_review_projects_ui <- function(id) {
  ns <- NS(id)
  div(
    style = "height: 100%; display: flex; flex-direction: column;",
    card(
      full_screen = TRUE,
      height = "100%",
      layout_sidebar(
        ## Sidebar inputs ----
        sidebar = sidebar(
          "",
          open = TRUE,
          selectizeInput(
            ns("property"),
            "Select property",
            choices = NULL,
            multiple = FALSE,
            width = "80%"
          ),
          actionButton(ns("load_record"), "Load Record"),
          actionButton(inputId = ns("clear_inputs"), label = "Clear Inputs"),
        ),
        div(
          style = "height: 100%; display: flex; flex-direction: column;",
          layout_columns(
            height = "100%",
            col_widths = c(12, -1),
            ## Card :: Edit ----
            card(
              height = "100%",
              card_header(div(
                style = "display: flex; align-items: center; gap: 8px;",
                h5("Projects")
              )),
              card_body(
                div(
                  style = "display: flex; flex-direction: column; gap: 15px;",
                  uiOutput(ns("project_summary_ui")),
                  # Add a spacer div to prevent pushing everything to bottom
                  div(style = "flex-grow: 1;")
                )
              )
            ) #,
            ## Card :: Other content ----
            # card(
            #   height = "100%",
            #   card_header(h5("Other content")),
            #   div(
            #     style = "display: flex; flex-direction: column; gap: 15px;",
            #     "Content here.",
            #     # Add a spacer div to prevent pushing everything to bottom
            #     div(style = "flex-grow: 1;")
            #   )
            # )
          )
        )
      )
    )
  )
}

# Server ----
module_review_projects_server <- function(id, db_con, db_updated = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Reactive :: Record ID choices ----
    property_choices <- reactive({
      #' This if statement triggers a refresh of this reactive if db_updated()
      #' is incremented somewhere else (for example when a new property is
      #' added to the database)
      if (!is.null(db_updated)) {
        db_updated()
      }

      dbGetQuery(db_con, glue("SELECT property_name FROM properties;")) |>
        pull(property_name) |>
        sort()
    })

    ## Update select input with record IDs based on table
    observe({
      updateSelectizeInput(
        session,
        inputId = "property",
        choices = c("", property_choices()),
        selected = "",
        server = TRUE
      )
    })

    ## Reactive value :: Selected record ----
    selected_record <- reactiveVal(NULL)

    ## Event :: Load record ----
    observeEvent(input$load_record, {
      req(input$property)
      prop_name <- input$property

      query_01 <- glue_sql(
        "
        SELECT property_description, phase_id_description, tl.team_value as team_lead, ph.phase_value as phase
        FROM properties p
        LEFT JOIN team_lead tl ON p.team_lead_id = tl.id 
        LEFT JOIN phase ph ON p.phase_id = ph.id
        WHERE p.property_name = {prop_name};
        ",
        .con = db_con
      )
      record_01 <- dbGetQuery(db_con, query_01)

      # Check if record_01 is empty and fill with NAs if needed
      if (nrow(record_01) == 0) {
        record_01 <- record_01 |>
          add_row()
      }

      query_02 <- glue_sql(
        "   
        SELECT par.pid, par.securement_action_description, con.property_contact_description
        FROM parcels par
        LEFT JOIN properties prop ON par.property_id = prop.id
        LEFT JOIN property_contact_details con ON par.property_contact_id = con.id  
        WHERE prop.property_name = {prop_name};
        ",
        .con = db_con
      )

      record_02 <- dbGetQuery(db_con, query_02) |>
        summarise(
          pids = paste(pid, collapse = ", "),
          securement_description = unique(securement_action_description),
          property_contact = unique(property_contact_description)
        )

      record <- bind_cols(record_01, record_02)

      if (nrow(record) == 1) {
        selected_record(record)
      } else {
        selected_record(NULL)
      }
    })

    ## Create UI for database fields ----
    output$project_summary_ui <- renderUI({
      req(selected_record())

      df <- selected_record()

      tagList(
        # First row: 3 columns for first 3 fields
        layout_columns(
          col_widths = c(4, 4, 4),
          div(
            strong("Project Phase:"),
            br(),
            df$phase
          ),
          div(
            strong("PIDs:"),
            br(),
            df$pids
          ),
          div(
            strong("Team Lead:"),
            br(),
            df$team_lead
          )
        ),

        # Horizontal rule
        hr(),

        # Remaining fields: 2 columns each
        layout_columns(
          col_widths = c(6, 6),
          div(
            div(
              style = "display: flex; align-items: center; gap: 8px;",
              strong("Property & Opportunity Overview:"),
              popover(
                div(
                  icon("question-circle"),
                  style = "transform: translateY(-5px); color: #6c757d; cursor: pointer; font-size: 14px;"
                ),
                includeMarkdown("help/prop_opp_overview.md"),
                title = "Property Overview Help",
                placement = "top"
              )
            ),
            br(),
            df$property_description
          ),
          div(
            div(
              style = "display: flex; align-items: center; gap: 8px;",
              strong("Phase Description:"),
              popover(
                div(
                  icon("question-circle"),
                  style = "transform: translateY(-5px); color: #6c757d; cursor: pointer; font-size: 14px;"
                ),
                includeMarkdown("help/phase_desc.md"),
                title = "Phase Description Help",
                placement = "top"
              )
            ),
            br(),
            df$phase_id_description
          )
        ),

        layout_columns(
          col_widths = c(6, 6),
          div(
            div(
              style = "display: flex; align-items: center; gap: 8px;",
              strong("Property Contact Description:"),
              popover(
                div(
                  icon("question-circle"),
                  style = "transform: translateY(-5px); color: #6c757d; cursor: pointer; font-size: 14px;"
                ),
                includeMarkdown("help/prop_contact_desc.md"),
                title = "Property Contact Help",
                placement = "top"
              )
            ),
            br(),
            df$property_contact
          ),
          div(
            div(
              style = "display: flex; align-items: center; gap: 8px;",
              strong("Securement Status:"),
              popover(
                div(
                  icon("question-circle"),
                  style = "transform: translateY(-5px); color: #6c757d; cursor: pointer; font-size: 14px;"
                ),
                includeMarkdown("help/securement_desc.md"),
                title = "Securement Status Help",
                placement = "top"
              )
            ),
            br(),
            df$securement_description
          )
        )
      )
    })

    ## Event :: Clear inputs ----
    observeEvent(input$clear_inputs, {
      updateSelectizeInput(
        session,
        "property",
        choices = property_choices(),
        selected = character(0),
        server = TRUE
      )
    })

    ## Clear selected record and input UI elements when table changes
    observeEvent(input$clear_inputs, {
      selected_record(NULL)
    })
  })
}
