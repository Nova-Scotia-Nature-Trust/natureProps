# UI ----

## CHANGE THIS NAME TO "SECUREMENT STATUS"
module_assign_priorities_ui <- function(id) {
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
          accordion(
            id = ns("sidebar_accordion"),
            open = "assign_values",
            multiple = FALSE,
            accordion_panel(
              title = "Assign Values",
              value = "assign_values",
              icon = bs_icon("pencil-square"),
              selectizeInput(
                ns("property"),
                "Select property",
                choices = NULL,
                multiple = FALSE,
                width = "100%"
              ),
              actionButton(
                inputId = ns("load_record"),
                label = "Load Record",
                class = "btn-success",
                width = "100%"
              ),
              actionButton(
                inputId = ns("clear_inputs"),
                label = "Clear Inputs",
                class = "btn-secondary",
                width = "100%"
              )
            ),
            accordion_panel(
              title = "Initiate Action Tracking",
              value = "tracking",
              icon = bs_icon("clipboard-check"),
              selectizeInput(
                ns("property_setup"),
                "Select property",
                choices = NULL,
                multiple = FALSE,
                width = "100%"
              ),
              actionButton(
                inputId = ns("setup_template"),
                label = "Setup Securement Action Template",
                class = "btn-primary",
                width = "100%"
              )
            )
          )
        ),
        div(
          style = "height: 100%; display: flex; flex-direction: column;",
          layout_columns(
            height = "100%",
            col_widths = c(6, 6),
            ## Property Card ----
            card(
              height = "100%",
              card_header(
                div(
                  style = "display: flex; align-items: center; gap: 8px;",
                  h5("Property"),
                  popover(
                    div(
                      icon("question-circle"),
                      style = "transform: translateY(-5px); color: #6c757d; cursor: pointer; font-size: 16px;"
                    ),
                    includeMarkdown("popups/probability_securement_desc.md"),
                    title = "Help",
                    placement = "right"
                  )
                )
              ),
              card_body(
                div(
                  style = "display: flex; flex-direction: column; gap: 15px;",
                  layout_columns(
                    col_widths = c(6, 6),
                    selectizeInput(
                      inputId = ns("securement_prob"),
                      label = "Securement Probability",
                      choices = NULL,
                      multiple = FALSE
                    ),
                    textInput(
                      inputId = ns("closing_year"),
                      label = "Anticipated Closing Year",
                      value = "",
                      placeholder = "e.g., 2025/26"
                    ),
                    dateInput(
                      ns("closing_date"),
                      "Anticipated Closing Date",
                      value = NA
                    ),
                    dateInput(
                      ns("conditions_date"),
                      "APS Conditions Date",
                      value = NA
                    )
                  ),
                  div(
                    style = "display: flex; align-items: center; gap: 8px; margin-bottom: 5px;",
                    tags$label(
                      "Securement action notes",
                      `for` = ns("securement_notes")
                    ),
                    popover(
                      div(
                        icon("question-circle"),
                        style = "transform: translateY(-5px); color: #6c757d; cursor: pointer; font-size: 14px;"
                      ),
                      includeMarkdown("popups/securement_desc.md"),
                      title = "Securement Notes Help",
                      placement = "top"
                    )
                  ),
                  textAreaInput(
                    ns("securement_notes"),
                    label = NULL,
                    "",
                    height = "150px",
                    width = "100%"
                  ),
                  actionButton(
                    inputId = ns("submit_edit_properties"),
                    label = "Submit Changes",
                    class = "btn-primary"
                  ),
                  layout_columns(),
                  div(),
                  div(),
                  div(style = "flex-grow: 1;")
                )
              )
            ),
            ## Parcel Card ----
            card(
              height = "100%",
              card_header(
                div(
                  style = "display: flex; align-items: center; gap: 8px;",
                  h5("Parcels"),
                  popover(
                    div(
                      icon("question-circle"),
                      style = "transform: translateY(-5px); color: #6c757d; cursor: pointer; font-size: 16px;"
                    ),
                    "More information to come",
                    title = "Help",
                    placement = "right"
                  )
                )
              ),
              card_body(
                div(
                  style = "display: flex; flex-direction: column; gap: 15px;",
                  selectizeInput(
                    inputId = ns("pid"),
                    label = "PID",
                    choices = NULL,
                    multiple = FALSE
                  ),
                  layout_columns(
                    col_widths = c(6, 6),
                    selectizeInput(
                      inputId = ns("ecological_priority"),
                      label = "Ecological Priority",
                      choices = NULL,
                      multiple = FALSE
                    ),
                    selectizeInput(
                      inputId = ns("securement_priority"),
                      label = "Securement Priority",
                      choices = NULL,
                      multiple = FALSE
                    )
                  ),
                  tableOutput(ns("parcels_table")),
                  actionButton(
                    inputId = ns("submit_edit_parcels"),
                    label = "Submit Changes",
                    class = "btn-primary"
                  ),
                  div(),
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
module_assign_priorities_server <- function(id, db_con, db_updated) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Input validation ----
    iv <- InputValidator$new()
    iv$add_rule("closing_year", function(value) {
      if (is.null(value) || value == "") {
        return() # Allow empty values
      }

      # Check format: YYYY/YY
      if (!str_detect(value, "^[0-9]{4}/[0-9]{2}$")) {
        return("Must be in format YYYY/YY (e.g., 2025/26)")
      }

      # Extract year components
      start_year <- as.integer(str_sub(value, 1, 4))
      end_year <- as.integer(str_sub(value, 6, 7))

      # Check if end year is consecutive (start_year + 1) % 100
      if ((start_year + 1) %% 100 != end_year) {
        return("Years must be consecutive (e.g., 2025/26, not 2025/27)")
      }
    })
    iv$add_rule("securement_prob", sv_required())
    iv$add_rule("closing_year", sv_required())

    iv$enable()

    ## Helper functions ----
    # get_property_choices <- function(db_con) {
    #   dbGetQuery(
    #     db_con,
    #     "SELECT DISTINCT
    #     pr.id,
    #     pr.property_name
    #   FROM
    #     securement_action_items sai
    #     LEFT JOIN properties pr ON pr.id = sai.property_id
    #   ORDER BY
    #     property_name;"
    #   ) |>
    #     pull("property_name")
    # }

    get_property_choices <- function(db_con) {
      dbGetQuery(
        db_con,
        "SELECT DISTINCT
        id,
        property_name
      FROM
       properties 
      ORDER BY
        property_name;"
      ) |>
        pull("property_name")
    }

    get_securement_prob_choices <- function(db_con) {
      dbGetQuery(db_con, "SELECT * FROM securement_probability") |>
        select(probability_value, id) |>
        deframe()
    }

    get_ranking_choices <- function(db_con) {
      dbGetQuery(db_con, "SELECT * FROM ranking") |>
        select(ranking_value, id) |>
        deframe()
    }

    load_property_record <- function(db_con, prop_name) {
      query <- glue_sql(
        "SELECT 
          p.id, 
          pa.pid,
          p.property_name, 
          p.securement_probability_id,
          p.anticipated_closing_year,
          p.anticipated_closing_date,
          p.aps_conditions_date,
          p.securement_action_description,
          pa.priority_ecological_ranking_id,
          pa.priority_securement_ranking_id
        FROM properties p
        LEFT JOIN parcels pa ON p.id = pa.property_id
        WHERE p.property_name = {prop_name}",
        .con = db_con
      )
      dbGetQuery(db_con, query)
    }

    ## Reactives :: Input choices ----
    property_choices <- reactive({
      if (!is.null(db_updated)) {
        db_updated()
      }
      get_property_choices(db_con)
    })

    securement_choices <- reactive({
      if (!is.null(db_updated)) {
        db_updated()
      }
      get_securement_prob_choices(db_con)
    })

    ranking_choices <- reactive({
      if (!is.null(db_updated)) {
        db_updated()
      }
      get_ranking_choices(db_con)
    })

    ## Property list for setup (without securement data) ----
    props_setup_reactive <- reactive({
      db_updated()
      dbGetQuery(
        db_con,
        "SELECT
          pr.id,
          pr.property_name
        FROM
          properties pr
        WHERE NOT EXISTS (
            SELECT 1
            FROM securement_action_items sai
            WHERE sai.property_id = pr.id
        )
        AND pr.securement_probability_id IS NOT NULL
        AND pr.anticipated_closing_year IS NOT NULL
        ORDER BY
          pr.property_name;"
      )
    })

    observe({
      updateSelectizeInput(
        session,
        "property_setup",
        choices = setNames(
          props_setup_reactive()$id,
          props_setup_reactive()$property_name
        ),
        selected = isolate(input$property_setup),
        server = TRUE
      )
    })

    ## Observer :: Update inputs ----
    observe({
      # Only update when db_updated changes
      if (!is.null(db_updated)) {
        db_updated()
      }

      # Preserve current selections
      current_selections <- list(
        property = isolate(input$property),
        securement_prob = isolate(input$securement_prob),
        closing_year = isolate(input$closing_year),
        closing_date = isolate(input$closing_date),
        conditions_date = isolate(input$conditions_date),
        securement_notes = isolate(input$securement_notes),
        ecological_priority = isolate(input$ecological_priority),
        securement_priority = isolate(input$securement_priority)
      )

      # Update inputs
      updateSelectizeInput(
        session,
        "property",
        choices = c("", property_choices()),
        selected = current_selections$property,
        server = TRUE
      )

      updateSelectizeInput(
        session,
        "securement_prob",
        choices = c("", securement_choices()),
        selected = current_selections$securement_prob
      )

      updateTextInput(
        session,
        "closing_year",
        value = current_selections$closing_year
      )

      updateDateInput(
        session,
        "closing_date",
        value = current_selections$closing_date
      )

      updateDateInput(
        session,
        "conditions_date",
        value = current_selections$conditions_date
      )

      updateTextInput(
        session,
        "securement_notes",
        value = current_selections$securement_notes
      )

      updateSelectizeInput(
        session,
        "ecological_priority",
        choices = c("", ranking_choices()),
        selected = current_selections$ecological_priority
      )

      updateSelectizeInput(
        session,
        "securement_priority",
        choices = c("", ranking_choices()),
        selected = current_selections$securement_priority
      )
    })

    ## Reactive value :: Selected record ----
    selected_record <- reactiveVal(NULL)

    ## Event:: Load Record  ----
    observeEvent(input$load_record, {
      req(input$property)
      record <- load_property_record(db_con, input$property) |>
        arrange(pid)

      if (nrow(record) >= 1) {
        selected_record(record)

        updateSelectizeInput(
          session,
          inputId = "securement_prob",
          selected = unique(record$securement_probability_id)
        )

        updateTextInput(
          session,
          inputId = "closing_year",
          value = unique(record$anticipated_closing_year)
        )

        updateDateInput(
          session,
          inputId = "closing_date",
          value = unique(record$anticipated_closing_date)
        )

        updateDateInput(
          session,
          inputId = "conditions_date",
          value = unique(record$aps_conditions_date)
        )

        updateTextInput(
          session,
          inputId = "securement_notes",
          value = unique(record$securement_action_description)
        )

        updateSelectizeInput(
          session,
          inputId = "pid",
          choices = c("", record$pid),
          selected = record$pid[1]
        )
      } else {
        selected_record(NULL)
      }

      print(record)
      print(paste("Selected value:", record$securement_probability_id))
      print("Available choices:")
      print(securement_choices())
    })

    ## Event:: PID selected ----
    observeEvent(input$pid, {
      req(input$pid, selected_record())

      # Find the ecological priority for the selected PID
      record <- selected_record()
      selected_parcel <- record |>
        filter(pid == input$pid)

      if (nrow(selected_parcel) == 1) {
        updateSelectizeInput(
          session,
          inputId = "ecological_priority",
          selected = selected_parcel$priority_ecological_ranking_id
        )

        updateSelectizeInput(
          session,
          inputId = "securement_priority",
          selected = selected_parcel$priority_securement_ranking_id
        )
      }
    })

    ## Output :: Parcels table ----
    output$parcels_table <- renderTable(
      {
        if (!is.null(db_updated)) {
          db_updated()
        }
        req(selected_record())

        # Convert ranking_choices() to a tibble for joining
        ranking_lookup <- tibble(
          id = as.integer(ranking_choices()),
          ranking_label = names(ranking_choices())
        )

        selected_record() |>
          select(
            pid,
            priority_ecological_ranking_id,
            priority_securement_ranking_id
          ) |>
          left_join(
            ranking_lookup,
            by = c("priority_ecological_ranking_id" = "id")
          ) |>
          rename(ecological_label = ranking_label) |>
          left_join(
            ranking_lookup,
            by = c("priority_securement_ranking_id" = "id")
          ) |>
          rename(securement_label = ranking_label) |>
          select(pid, ecological_label, securement_label) |>
          rename(
            PID = pid,
            `Ecological Priority` = ecological_label,
            `Securement Priority` = securement_label
          ) |>
          arrange(PID)
      },
      colnames = TRUE,
      spacing = "s"
    )

    ## Event :: Write changes (property) ----
    observeEvent(input$submit_edit_properties, {
      req(input$property, input$securement_prob)
      req(iv$is_valid())

      or_na <- function(x, na) {
        if (isTruthy(x)) x else na
      }

      df <- tibble(
        property_name = input$property,
        securement_probability_id = input$securement_prob,
        anticipated_closing_year = or_na(input$closing_year, NA_character_),
        anticipated_closing_date = or_na(input$closing_date, as.Date(NA)),
        aps_conditions_date = or_na(input$conditions_date, as.Date(NA)),
        securement_action_description = or_na(
          input$securement_notes,
          NA_character_
        )
      )

      # df <- tibble(
      #   property_name = input$property,
      #   securement_probability_id = input$securement_prob,
      #   anticipated_closing_year = if_else(
      #     !isTruthy(input$closing_year),
      #     NA_character_,
      #     input$closing_year
      #   ),
      #   anticipated_closing_date = if_else(
      #     !isTruthy(input$closing_date),
      #     as.Date(NA),
      #     input$closing_date
      #   ),
      #   aps_conditions_date = if_else(
      #     !isTruthy(input$conditions_date),
      #     as.Date(NA),
      #     input$conditions_date
      #   ),
      #   securement_action_description = if_else(
      #     !isTruthy(input$securement_notes),
      #     NA_character_,
      #     input$securement_notes
      #   )
      # )

      print(df)

      dbx::dbxUpdate(
        db_con,
        table = "properties",
        records = df,
        where_cols = "property_name"
      )

      # Signal update
      if (!is.null(db_updated)) {
        db_updated(db_updated() + 1)
      }

      # Refresh the selected record with updated data
      record <- load_property_record(db_con, input$property)
      selected_record(record)

      shinyalert(
        title = "Success",
        text = str_glue(
          "Table record {input$property} has been successfully updated in Properties table"
        ),
        type = "success",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        timer = 10000
      )
    })
    ## Event :: Write changes (parcels) ----
    observeEvent(input$submit_edit_parcels, {
      req(input$pid, input$ecological_priority)

      df <- tibble(
        pid = input$pid,
        priority_ecological_ranking_id = input$ecological_priority,
        priority_securement_ranking_id = input$securement_priority,
      )

      print(df)

      dbx::dbxUpdate(
        db_con,
        table = "parcels",
        records = df,
        where_cols = "pid"
      )

      # Signal update
      if (!is.null(db_updated)) {
        db_updated(db_updated() + 1)
      }

      # Refresh the selected record with updated data
      record <- load_property_record(db_con, input$property)
      selected_record(record)

      shinyalert(
        title = "Success",
        text = str_glue(
          "Table record {input$pid} has been successfully updated in Parcels table"
        ),
        type = "success",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        timer = 10000
      )
    })

    ## Event :: Setup template action ----
    observeEvent(input$setup_template, {
      req(input$property_setup)

      action_type_ids <- dbGetQuery(db_con, "SELECT id FROM action_item_type")

      action_structure <- expand.grid(
        action_item_type_id = action_type_ids$id,
        property_id = input$property_setup
      )

      append_db_data(
        "securement_action_items",
        data = action_structure,
        con = db_con,
        silent = TRUE
      )

      db_updated(db_updated() + 1)

      shinyalert(
        title = "Success",
        text = glue::glue(
          "Template created with {nrow(action_structure)} action items"
        ),
        type = "success",
        timer = 5000
      )
    })

    ## Event :: Clear inputs ----
    observeEvent(input$clear_inputs, {
      updateSelectizeInput(
        session,
        inputId = "property",
        choices = property_choices(),
        selected = character(0),
        server = TRUE
      )

      updateSelectizeInput(
        session,
        inputId = "securement_prob",
        choices = c("", securement_choices()),
        selected = character(0),
        server = TRUE
      )

      updateTextInput(
        session,
        inputId = "closing_year",
        value = "",
        placeholder = "e.g., 2025/26"
      )

      updateDateInput(
        session,
        inputId = "closing_date",
        value = NA
      )

      updateDateInput(
        session,
        inputId = "conditions_date",
        value = NA
      )

      updateTextAreaInput(
        session,
        inputId = "securement_notes",
        value = ""
      )

      updateSelectizeInput(
        session,
        inputId = "ecological_priority",
        choices = c("", ranking_choices()),
        selected = character(0),
        server = TRUE
      )

      updateSelectizeInput(
        session,
        inputId = "securement_priority",
        choices = c("", ranking_choices()),
        selected = character(0),
        server = TRUE
      )

      updateSelectizeInput(
        session,
        inputId = "pid",
        choices = "",
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
