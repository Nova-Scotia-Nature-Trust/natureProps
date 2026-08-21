# UI ----
module_team_lead_actions_UI <- function(id) {
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
      ),
      input_switch(
        ns("show_all_actions"),
        "Show Completed Actions",
        value = FALSE
      ),
      hr(),
      selectizeInput(
        inputId = ns("complete_action_ids"),
        label = "Select Action(s)",
        choices = NULL,
        selected = NULL,
        multiple = TRUE,
        options = list(
          create = FALSE,
          placeholder = "Select action(s) to complete"
        ),
        width = "100%"
      ),
      dateInput(
        inputId = ns("complete_date"),
        label = "Date Completed",
        value = Sys.Date(),
        width = "100%"
      ),
      actionButton(
        inputId = ns("mark_complete"),
        label = "Mark as Complete",
        class = "btn-success",
        width = "100%"
      ),
      uiOutput(ns("ipa_panel"))
    ),

    # Action card ----
    card(
      full_screen = TRUE,
      card_header(
        class = "d-flex justify-content-between align-items-center",
        h5("General Action Items"),
        downloadButton(
          outputId = ns("download_actions"),
          label = "Download",
          class = "btn-sm"
        )
      ),
      card_body(
        style = "padding: 0.5rem 1rem;",
        min_height = "300px",
        DTOutput(outputId = ns("actions_table"), height = "100%")
      )
    )
  )
}

# Server ----
module_team_lead_actions_server <- function(id, db_con, db_updated = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive value to store action items data
    actions_data <- reactiveVal(NULL)

    # Reactive :: Check if selected team lead is Dominic Henry ----
    is_dominic <- reactive({
      req(isTruthy(input$team_lead_choice))
      selected_name <- team_leads() |>
        filter(id == as.integer(input$team_lead_choice)) |>
        pull(team_value)
      length(selected_name) > 0 && selected_name == "Dominic Henry"
    })

    # Reactive :: All properties for IPA select ----
    all_properties <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, property_name FROM properties ORDER BY property_name"
      )
    })

    # Populate team lead dropdown on module load
    team_leads <- reactive({
      dbGetQuery(
        db_con,
        "SELECT DISTINCT id, team_value 
        FROM team_lead 
        WHERE team_value NOT IN ('Rich LaPaix', 'Keith Spafford')
        ORDER BY team_value"
      )
    })

    observe({
      updateSelectizeInput(
        session,
        inputId = "team_lead_choice",
        choices = c(
          "",
          setNames(
            team_leads()$id,
            team_leads()$team_value
          )
        ),
        server = TRUE
      )
    })

    # Update data when team lead changes ----
    observe({
      req(isTruthy(input$team_lead_choice))

      if (!is.null(db_updated)) {
        db_updated()
      }

      # Query all action items with property name and team lead
      actions <- dbGetQuery(
        db_con,
        glue_sql(
          "SELECT
                  p.property_name,
                  tla.id,
                  tla.action_item_description,
                  tla.due_date,
                  tla.action_complete,
                  tla.date_completed
                FROM
                  team_lead_actions tla
                  LEFT JOIN properties p ON tla.property_id = p.id
                WHERE
                  tla.team_lead_id = {input$team_lead_choice}
                ORDER BY
                  tla.due_date,
                  p.property_name;",
          .con = db_con
        )
      ) |>
        rename(
          `Property Name` = property_name,
          `Action ID` = id,
          `Action Item Description` = action_item_description,
          `Due Date` = due_date,
          `Action Complete` = action_complete,
          `Date Completed` = date_completed
        )

      actions_data(actions)
    })

    # Reactive :: Incomplete action choices for selected team lead ----
    incomplete_action_choices <- reactive({
      req(isTruthy(input$team_lead_choice))

      if (!is.null(db_updated)) {
        db_updated()
      }

      dbGetQuery(
        db_con,
        glue_sql(
          "SELECT tla.id
          FROM team_lead_actions tla
          WHERE
            tla.team_lead_id= {input$team_lead_choice}
            AND (tla.action_complete IS FALSE OR tla.action_complete IS NULL)
          ORDER BY tla.id",
          .con = db_con
        )
      ) |>
        pull(id)
    })

    # Update action(s) selectize when team lead or data changes ----
    observe({
      choices <- if (isTruthy(input$team_lead_choice)) {
        incomplete_action_choices()
      } else {
        c()
      }

      updateSelectizeInput(
        session,
        inputId = "complete_action_ids",
        choices = choices,
        selected = character(0),
        server = TRUE
      )
    })

    # Event :: Mark as Complete ----
    observeEvent(input$mark_complete, {
      req(input$complete_action_ids)

      update_tibble <- tibble(
        id = as.integer(input$complete_action_ids),
        action_complete = TRUE,
        date_completed = input$complete_date
      )

      dbx::dbxUpdate(
        db_con,
        table = "team_lead_actions",
        records = update_tibble,
        where_cols = "id"
      )

      if (!is.null(db_updated)) {
        db_updated(db_updated() + 1)
      }

      # Refresh actions data
      actions <- dbGetQuery(
        db_con,
        glue_sql(
          "SELECT
                  p.property_name,
                  tla.id,
                  tla.action_item_description,
                  tla.due_date,
                  tla.action_complete,
                  tla.date_completed
                FROM
                  team_lead_actions tla
                  LEFT JOIN properties p ON tla.property_id = p.id
                WHERE
                  tla.team_lead_id = {input$team_lead_choice}
                ORDER BY
                  tla.due_date,
                  p.property_name;",
          .con = db_con
        )
      ) |>
        rename(
          `Property Name` = property_name,
          `Action ID` = id,
          `Action Item Description` = action_item_description,
          `Due Date` = due_date,
          `Action Complete` = action_complete,
          `Date Completed` = date_completed
        )

      actions_data(actions)

      updateSelectizeInput(
        session,
        inputId = "complete_action_ids",
        choices = incomplete_action_choices(),
        selected = character(0),
        server = TRUE
      )

      shinyalert(
        title = "Success",
        text = str_glue(
          "{length(input$complete_action_ids)} action{if (length(input$complete_action_ids) > 1) 's' else ''} marked as complete."
        ),
        type = "success",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        timer = 8000
      )
    })

    # Event :: Clear inputs ----
    observeEvent(input$clear_inputs, {
      updateSelectizeInput(
        session,
        inputId = "team_lead_choice",
        choices = c(
          "",
          setNames(
            team_leads()$id,
            team_leads()$team_value
          )
        ),
        selected = character(0),
        server = TRUE
      )

      actions_data(NULL)

      updateSelectizeInput(
        session,
        inputId = "complete_action_ids",
        choices = c(),
        selected = character(0),
        server = TRUE
      )

      updateDateInput(session, "complete_date", value = Sys.Date())

      update_switch(
        session = session,
        id = "show_all_actions",
        value = FALSE
      )
    })

    # Filtered actions reactive ----
    filtered_actions <- reactive({
      req(actions_data())

      if (isTRUE(input$show_all_actions)) {
        actions_data()
      } else {
        actions_data() |> filter(!`Action Complete`)
      }
    })

    # Render actions table ----
    output$actions_table <- renderDT({
      req(filtered_actions())

      show_completed <- isTRUE(input$show_all_actions)

      # Convert character columns to factors for select inputs, and give
      # Action Complete a readable label instead of the raw true/false
      data_for_display <- filtered_actions() |>
        mutate(
          `Action Complete` = factor(
            `Action Complete`,
            levels = c(TRUE, FALSE),
            labels = c("Complete", "Incomplete")
          ),
          across(where(is.character), as.factor)
        )

      dt <- datatable(
        data_for_display,
        # Omit "stripe" when coloring rows so the zebra-stripe box-shadow
        # doesn't paint over the formatStyle() highlighting
        # https://datatables.net/manual/core/styling/classes
        class = if (show_completed) "row-border hover" else "display",
        options = list(
          pageLength = 25,
          lengthMenu = list(
            c(10, 25, 50, -1),
            c('10', '25', '50', 'All')
          ),
          scrollX = TRUE,
          fixedHeader = TRUE,
          stateSave = FALSE
        ),
        filter = list(
          position = "top",
          clear = TRUE,
          plain = TRUE
        ),
        rownames = FALSE,
        selection = "single",
        extensions = c("Buttons"),
        fillContainer = TRUE
      )

      if (show_completed) {
        dt <- dt |>
          formatStyle(
            columns = names(data_for_display), # Needed to make backgroundColor work on rows
            valueColumns = "Action Complete",
            backgroundColor = styleEqual(
              c("Complete", "Incomplete"),
              c("#81D996", "#F1AD6A")
            ),
            color = styleEqual(
              c("Complete", "Incomplete"),
              c("black", "black")
            )
          )
      }

      dt
    })

    # Render IPA panel conditionally for Dominic Henry ----
    output$ipa_panel <- renderUI({
      req(isTruthy(input$team_lead_choice), is_dominic())

      props <- all_properties()

      tagList(
        hr(),
        selectizeInput(
          ns("ipa_properties"),
          "Select Property",
          choices = setNames(props$id, props$property_name),
          selected = NULL,
          multiple = TRUE,
          options = list(
            create = FALSE,
            placeholder = "Select one or more properties"
          ),
          width = "100%"
        ),
        dateInput(
          ns("ipa_due_date"),
          "Due Date",
          value = Sys.Date(),
          width = "100%"
        ),
        actionButton(
          ns("request_ipa"),
          "Request IPA",
          class = "btn-primary",
          width = "100%"
        )
      )
    })

    # Event :: Request IPA ----
    observeEvent(input$request_ipa, {
      req(input$ipa_properties)

      new_actions <- tibble(
        property_id = as.integer(input$ipa_properties),
        team_lead_id = as.integer(input$team_lead_choice),
        action_item_description = "Generate IPA",
        due_date = input$ipa_due_date,
        action_complete = FALSE
      )

      dbx::dbxInsert(db_con, "team_lead_actions", new_actions)

      if (!is.null(db_updated)) {
        db_updated(db_updated() + 1)
      }

      # Refresh actions data
      actions <- dbGetQuery(
        db_con,
        glue_sql(
          "SELECT
                  p.property_name,
                  tla.id,
                  tla.action_item_description,
                  tla.due_date,
                  tla.action_complete,
                  tla.date_completed
                FROM
                  team_lead_actions tla
                  LEFT JOIN properties p ON tla.property_id = p.id
                WHERE
                  tla.team_lead_id = {input$team_lead_choice}
                ORDER BY
                  tla.due_date,
                  p.property_name;",
          .con = db_con
        )
      ) |>
        rename(
          `Property Name` = property_name,
          `Action ID` = id,
          `Action Item Description` = action_item_description,
          `Due Date` = due_date,
          `Action Complete` = action_complete,
          `Date Completed` = date_completed
        )

      actions_data(actions)

      shinyalert(
        title = "Success",
        text = str_glue(
          "IPA requested for {length(input$ipa_properties)} propert{if (length(input$ipa_properties) > 1) 'ies' else 'y'}."
        ),
        type = "success",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        timer = 8000
      )
    })

    ## Download handler for actions ----
    output$download_actions <- downloadHandler(
      filename = function() {
        team_lead <- input$team_lead_choice
        if (team_lead == "") {
          team_lead <- "team_lead"
        }
        # Clean the team lead name for filename
        team_lead <- str_replace_all(team_lead, " ", "_") |>
          str_to_lower()
        glue("{team_lead}_action_items_{format(Sys.Date(), '%Y%m%d')}.csv")
      },
      content = function(file) {
        data_to_download <- filtered_actions()

        if (!is.null(data_to_download) && nrow(data_to_download) > 0) {
          write_csv(data_to_download, file)
        } else {
          # Write empty file if no data
          write_csv(data.frame(), file)
        }
      }
    )
  })
}
