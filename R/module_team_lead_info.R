# UI ----
module_team_lead_info_UI <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      selectizeInput(
        ns("team_lead_choice"),
        "Select Team Lead",
        choices = c(""),
        multiple = FALSE,
        width = "100%"
      ),
      actionButton(
        inputId = ns("clear_inputs"),
        label = "Clear Inputs",
        width = "100%"
      )
    ),
    layout_columns(
      col_widths = c(6, 6),

      # Action card ----
      card(
        height = "auto",
        full_screen = TRUE,
        card_header(
          h5("Action Items")
        ),
        card_body(
          DTOutput(outputId = ns("actions_table"), height = "auto")
        )
      ),

      # Team Lead Property Card ----
      card(
        height = "auto",
        full_screen = TRUE,
        card_header(
          h5("Team Lead Property List")
        ),
        card_body(
          DTOutput(outputId = ns("properties_table"), height = "auto")
        )
      )
    )
  )
}

# Server ----
module_team_lead_info_server <- function(id, db_con, db_updated = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive value to store action items data
    actions_data <- reactiveVal(NULL)
    properties_data <- reactiveVal(NULL)

    # Populate team lead dropdown on module load
    team_leads <- reactive({
      dbGetQuery(
        db_con,
        "SELECT DISTINCT team_value FROM team_lead ORDER BY team_value"
      ) |>
        pull(team_value)
    })

    observe({
      updateSelectizeInput(
        session,
        inputId = "team_lead_choice",
        choices = c("", team_leads()),
        server = TRUE
      )
    })

    # Update data when team lead changes ----
    observe({
      req(input$team_lead_choice, input$team_lead_choice != "")

      if (!is.null(db_updated)) {
        db_updated()
      }

      # Query action items with property name and team lead
      actions <- dbGetQuery(
        db_con,
        glue_sql(
          "SELECT
                  p.property_name,
                  tla.action_item_description,
                  tla.due_date,
                  tla.action_complete
                FROM
                  team_lead_actions tla
                  LEFT JOIN properties p ON tla.property_id = p.id
                  LEFT JOIN team_lead tl ON tla.team_lead_id = tl.id
                WHERE
                  tl.team_value = {input$team_lead_choice}
                ORDER BY
                  tla.due_date,
                  p.property_name;",
          .con = db_con
        )
      ) |>
        rename(
          `Property Name` = property_name,
          `Action Item Description` = action_item_description,
          `Due Date` = due_date,
          `Completed` = action_complete
        )

      actions_data(actions)

      # Query properties assigned to team lead
      properties <- dbGetQuery(
        db_con,
        glue_sql(
          "SELECT
                  p.property_name,
                  ph.phase_value AS phase_id,
                  p.phase_id_followup
                FROM
                  properties p
                  LEFT JOIN team_lead tl ON p.team_lead_id = tl.id
                  LEFT JOIN phase ph ON p.phase_id = ph.id
                WHERE
                  tl.team_value = {input$team_lead_choice}
                ORDER BY
                  p.property_name;",
          .con = db_con
        )
      ) |>
        rename(
          `Property Name` = property_name,
          `Phase ID` = phase_id,
          `Phase ID Followup` = phase_id_followup
        )

      properties_data(properties)
    })

    # Event :: Clear inputs ----
    observeEvent(input$clear_inputs, {
      updateSelectizeInput(
        session,
        inputId = "team_lead_choice",
        choices = c("", team_leads()),
        selected = character(0),
        server = TRUE
      )

      actions_data(NULL)
      properties_data(NULL)
    })

    # Render actions table ----
    dom_layout <- "
    <'row'<'col-sm-10'l><'col-sm-2 text-right'B>><'row'<'col-sm-12'tr>><'row'<'col-sm-5'i><'col-sm-7'p>>
    "

    output$actions_table <- renderDT({
      req(actions_data())

      # Convert character columns to factors for select inputs
      data_for_display <- actions_data() |>
        mutate(across(where(is.character), as.factor))

      DT::datatable(
        data_for_display,
        options = list(
          pageLength = 10,
          lengthMenu = list(
            c(10, 25, 50, -1),
            c('10', '25', '50', 'All')
          ),
          scrollX = TRUE,
          dom = dom_layout,
          buttons = list(
            "copy",
            "excel"
          ),
          stateSave = FALSE
        ),
        filter = list(
          position = "top",
          clear = TRUE,
          plain = TRUE
        ),
        rownames = FALSE,
        selection = "single",
        extensions = c("Buttons")
      )
    })

    # Render properties table ----
    output$properties_table <- renderDT({
      req(properties_data())

      # Convert character columns to factors for select inputs
      data_for_display <- properties_data() |>
        mutate(across(where(is.character), as.factor))

      DT::datatable(
        data_for_display,
        options = list(
          pageLength = 10,
          lengthMenu = list(
            c(10, 25, 50, -1),
            c('10', '25', '50', 'All')
          ),
          scrollX = TRUE,
          dom = dom_layout,
          buttons = list(
            "copy",
            "excel"
          ),
          stateSave = FALSE
        ),
        filter = list(
          position = "top",
          clear = TRUE,
          plain = TRUE
        ),
        rownames = FALSE,
        selection = "single",
        extensions = c("Buttons")
      )
    })
  })
}
