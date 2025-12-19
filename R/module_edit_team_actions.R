# UI ----
module_edit_team_actions_ui <- function(id) {
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
            inputId = ns("team_lead_filter"),
            label = "Team Lead",
            choices = NULL,
            selected = NULL,
            options = list(
              create = FALSE,
              placeholder = "Select team lead"
            )
          ),
          selectizeInput(
            inputId = ns("record_id"),
            label = "Action Item",
            choices = NULL,
            selected = NULL,
            multiple = FALSE,
            options = list(
              create = FALSE,
              placeholder = "Search or select action"
            )
          ),
          actionButton(
            inputId = ns("load_record"),
            label = "Load Action",
            class = "btn-success"
          ),
          hr(),
          actionButton(
            inputId = ns("submit_edit"),
            label = "Submit Changes",
            class = "btn-primary"
          ),
          actionButton(
            inputId = ns("clear_edit"),
            label = "Clear",
            class = "btn-secondary"
          )
        ),
        ## Main panel ----
        div(
          style = "height: 100%; display: flex; flex-direction: column;",
          card(
            height = "100%",
            card_header(
              h5("Edit Team Lead Action")
            ),
            card_body(
              div(
                style = "display: flex; flex-direction: column; gap: 15px;",
                uiOutput(ns("edit_fields_ui")),
                div(style = "flex-grow: 1;")
              )
            )
          )
        )
      )
    )
  )
}

# Server ----
module_edit_team_actions_server <- function(id, db_con, db_updated = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Reactive :: Team lead choices ----
    team_lead_choices <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, team_value FROM team_lead ORDER BY team_value;"
      ) |>
        select(team_value, id) |>
        deframe()
    })

    ## Update team lead dropdown ----
    observe({
      updateSelectizeInput(
        session,
        inputId = "team_lead_filter",
        choices = c("", team_lead_choices()),
        selected = "",
        server = TRUE
      )
    })

    ## Reactive :: Action item choices ----
    action_choices <- reactive({
      req(input$team_lead_filter)

      query <- glue_sql(
        "SELECT 
          tla.id,
          pr.property_name,
          tla.due_date
        FROM team_lead_actions tla
        JOIN properties pr ON tla.property_id = pr.id
        WHERE tla.team_lead_id = {input$team_lead_filter}
        ORDER BY tla.due_date DESC, pr.property_name",
        .con = db_con
      )

      results <- dbGetQuery(db_con, query) |>
        mutate(due_date = as.Date(due_date))

      # Create named vector with format "Property Name - Due Date" = id
      set_names(
        results$id,
        paste0(
          results$property_name,
          " - ",
          ifelse(
            is.na(results$due_date),
            "No date",
            format(results$due_date, "%Y-%m-%d")
          )
        )
      )
    })

    ## Update action dropdown ----
    observe({
      updateSelectizeInput(
        session,
        inputId = "record_id",
        choices = c("", action_choices()),
        selected = "",
        server = TRUE
      )
    })

    ## Reactive value :: Selected record ----
    selected_record <- reactiveVal(NULL)

    ## Clear selected record when team lead changes ----
    observeEvent(input$team_lead_filter, {
      selected_record(NULL)
    })

    ## Event :: Load record ----
    observeEvent(input$load_record, {
      req(input$team_lead_filter)
      req(input$record_id)

      action_id <- input$record_id

      query <- glue_sql(
        "SELECT 
          team_lead_id,
          action_item_description,
          due_date,
          action_complete
        FROM team_lead_actions 
        WHERE id = {action_id}",
        .con = db_con
      )

      record <- dbGetQuery(db_con, query)

      if (nrow(record) == 1) {
        selected_record(record)
      } else {
        selected_record(NULL)
      }
    })

    ## Create UI for database fields ----
    output$edit_fields_ui <- renderUI({
      record <- selected_record()

      tagList(
        layout_columns(
          col_widths = c(6, 6),
          selectInput(
            inputId = ns("edit_team_lead_id"),
            label = "Team Lead",
            choices = c("", team_lead_choices()),
            selected = if (!is.null(record) && !is.na(record$team_lead_id)) {
              record$team_lead_id
            } else {
              ""
            }
          ),
          dateInput(
            inputId = ns("edit_due_date"),
            label = "Due Date",
            value = if (!is.null(record) && !is.na(record$due_date)) {
              record$due_date
            } else {
              ""
            }
          )
        ),
        textAreaInput(
          inputId = ns("edit_action_item_description"),
          label = "Action Item Description",
          value = if (
            !is.null(record) && !is.na(record$action_item_description)
          ) {
            record$action_item_description
          } else {
            ""
          },
          height = "200px",
          width = "100%"
        ),
        checkboxInput(
          inputId = ns("edit_action_complete"),
          label = "Action Complete",
          value = if (!is.null(record) && !is.na(record$action_complete)) {
            record$action_complete
          } else {
            FALSE
          }
        )
      )
    })

    ## Event :: Write changes ----
    observeEvent(input$submit_edit, {
      req(input$record_id)

      db_id <- as.integer(input$record_id)

      # Build update tibble
      update_tibble <- tibble(
        id = db_id,
        team_lead_id = if (input$edit_team_lead_id == "") {
          NA_integer_
        } else {
          as.integer(input$edit_team_lead_id)
        },
        action_item_description = if (
          input$edit_action_item_description == ""
        ) {
          NA_character_
        } else {
          input$edit_action_item_description
        },
        due_date = if (length(input$edit_due_date) > 0) {
          format(input$edit_due_date, "%Y-%m-%d")
        } else {
          as.Date(NA)
        },
        action_complete = input$edit_action_complete
      )

      # Update the record
      dbx::dbxUpdate(
        db_con,
        table = "team_lead_actions",
        records = update_tibble,
        where_cols = "id"
      )

      # Signal update
      if (!is.null(db_updated)) {
        db_updated(db_updated() + 1)
      }

      shinyalert(
        title = "Success",
        text = str_glue("Action item {db_id} has been successfully updated"),
        type = "success",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        timer = 10000
      )
    })

    ## Event :: Clear inputs ----
    observeEvent(input$clear_edit, {
      selected_record(NULL)

      # Clear the sidebar filters
      updateSelectizeInput(
        session,
        inputId = "team_lead_filter",
        selected = "",
        choices = c("", team_lead_choices()),
        server = TRUE
      )

      updateSelectInput(session, "edit_team_lead_id", selected = "")
      updateTextAreaInput(session, "edit_action_item_description", value = "")
      updateDateInput(session, "edit_due_date", value = "")
      updateCheckboxInput(session, "edit_action_complete", value = FALSE)

      updateSelectizeInput(
        session,
        inputId = "record_id",
        choices = c("", action_choices()),
        selected = "",
        server = TRUE
      )
    })
  })
}
