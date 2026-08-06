# UI ----
# NAV PANEL :: ASSIGN SECUREMENT VALUES
module_assign_securement_values_ui <- function(id) {
  ns <- NS(id)

  div(
    style = "height: 100%; display: flex; flex-direction: column;",
    card(
      full_screen = TRUE,
      height = "100%",
      layout_sidebar(
        # Sidebar ----
        sidebar = sidebar(
          "",
          open = TRUE,
          accordion(
            id = ns("sidebar_accordion"),
            open = "assign_values",
            multiple = FALSE,
            # Accordion Panel 01 ----
            accordion_panel(
              title = "Assign Securement Values",
              value = "assign_values",
              icon = bs_icon("pencil-square"),
              selectizeInput(
                ns("property"),
                "Select Property",
                choices = NULL,
                multiple = FALSE,
                width = "100%"
              ),
              br(),
              actionButton(
                inputId = ns("clear_inputs"),
                label = "Clear Inputs",
                class = "btn-secondary",
                width = "100%"
              )
            ),
            # Accordion Panel 02 ----
            accordion_panel(
              title = "Initiate Action Tracking",
              value = "tracking",
              icon = bs_icon("clipboard-check"),
              selectizeInput(
                ns("property_iat"),
                "Select Property",
                choices = NULL,
                multiple = FALSE,
                width = "100%"
              ),
              textInput(
                inputId = ns("public_name"),
                label = "Assign Public Property Name",
                value = ""
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
                    icon("question-circle"),
                    includeMarkdown("popups/probability_securement_desc.md"),
                    title = "Probability of Securement Categories",
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
                    "Securement action notes",
                    popover(
                      icon("question-circle"),
                      includeMarkdown("popups/securement_desc.md"),
                      title = "Context",
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
                    class = "btn-success"
                  ),
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
                  # popover(
                  #   icon("question-circle"),
                  #   "More information to come",
                  #   title = "Context",
                  #   placement = "right"
                  # )
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
                  tableOutput(ns("parcels_table")),
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
                  layout_columns(
                    col_widths = c(6, 6),
                    textAreaInput(
                      ns("ecological_reason"),
                      label = "Ecological Ranking Reasoning",
                      "",
                      height = "150px",
                      width = "100%"
                    ),
                    textAreaInput(
                      ns("securement_reason"),
                      label = "Securement Ranking Reasoning",
                      "",
                      height = "150px",
                      width = "100%"
                    )
                  ),
                  actionButton(
                    inputId = ns("submit_edit_parcels"),
                    label = "Submit Changes",
                    class = "btn-success"
                  ),
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
module_assign_securement_values_server <- function(id, db_con, db_updated) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Input validation ----
    iv <- InputValidator$new()

    iv$add_rule("closing_year", function(value) {
      sec_prob <- input$securement_prob

      potential_id <- dbGetQuery(
        db_con,
        "SELECT id FROM securement_probability WHERE probability_value = 'Potential'"
      )$id

      # If probability is set to Potential
      if (!is.null(sec_prob) && sec_prob == potential_id) {
        # Allow NULL value
        if (!isTruthy(value)) {
          return(NULL) # Stop validation
        }
      } else {
        # If Expected/Confirmed, then closing_year is required
        if (!isTruthy(value)) {
          return(
            "Valid year is required when securement probability is Confirmed or Expected"
          )
        }
      }

      # If a value is input, check format YYYY/YY format
      if (!str_detect(value, "^[0-9]{4}/[0-9]{2}$")) {
        return("Must be in format YYYY/YY (e.g., 2025/26)")
      }

      start_year <- as.integer(str_sub(value, 1, 4))
      end_year <- as.integer(str_sub(value, 6, 7))

      if ((start_year + 1) %% 100 != end_year) {
        return("Years must be consecutive (e.g., 2025/26, not 2025/27)")
      }

      return(NULL)
    })

    iv$add_rule("securement_prob", sv_required())
    iv$enable()

    # Separate validator so the public_name required rule only gates/highlights
    # the setup_template button, not submit_edit_properties
    iv_template <- InputValidator$new()
    iv_template$add_rule("public_name", sv_required())
    iv_template$enable()

    ## Reactive :: Property List ----
    property_list <- reactive({
      db_updated()
      dbGetQuery(
        db_con,
        "SELECT DISTINCT id, property_name FROM properties 
         ORDER BY property_name;"
      )
    })

    observe({
      updateSelectizeInput(
        session,
        "property",
        choices = setNames(
          property_list()$id,
          property_list()$property_name
        ),
        selected = isolate(input$property),
        server = TRUE
      )
    })

    property_name <- reactiveVal(NULL)

    observe({
      req(input$property)
      name <- property_list() |>
        filter(id == input$property) |>
        pull(property_name)
      property_name(name)
    })

    ## Reactive :: Securement Probability ----
    securement_probability <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, probability_value FROM securement_probability"
      )
    })

    observe({
      updateSelectizeInput(
        session,
        "securement_prob",
        choices = setNames(
          securement_probability()$id,
          securement_probability()$probability_value
        ),
        selected = isolate(input$securement_prob),
        server = TRUE
      )
    })

    ## Reactive :: Ranking ----
    ranking <- reactive({
      dbGetQuery(db_con, "SELECT id, ranking_value FROM ranking")
    })

    observe({
      updateSelectizeInput(
        session,
        "ecological_priority",
        choices = setNames(
          ranking()$id,
          ranking()$ranking_value
        ),
        selected = isolate(input$ecological_priority),
        server = TRUE
      )
    })

    observe({
      updateSelectizeInput(
        session,
        "securement_priority",
        choices = setNames(
          ranking()$id,
          ranking()$ranking_value
        ),
        selected = isolate(input$securement_priority),
        server = TRUE
      )
    })

    ## Reactive :: IAT Property List ----
    property_list_iat <- reactive({
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
        "property_iat",
        choices = setNames(
          property_list_iat()$id,
          property_list_iat()$property_name
        ),
        selected = isolate(input$property_iat),
        server = TRUE
      )
    })

    ## Function :: Load Property Record ----
    load_property_record <- function(db_con, selected_property) {
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
          pa.priority_securement_ranking_id,
          pa.priority_securement_ranking_reason,
          pa.priority_ecological_ranking_reason
        FROM properties p
        LEFT JOIN parcels pa ON p.id = pa.property_id
        WHERE p.id = {selected_property}",
        .con = db_con
      )
      dbGetQuery(db_con, query)
    }

    ## Observer :: Update non-lookup value inputs ----
    observe({
      updateTextInput(
        session,
        "closing_year",
        value = isolate(input$closing_year)
      )

      updateDateInput(
        session,
        "closing_date",
        value = isolate(input$closing_date)
      )

      updateDateInput(
        session,
        "conditions_date",
        value = isolate(input$conditions_date)
      )

      updateTextInput(
        session,
        "securement_notes",
        value = isolate(input$securement_notes)
      )
    })

    ## Reactive Value :: Selected Record ----
    selected_record <- reactiveVal(NULL)
    original_securement_notes <- reactiveVal(NULL)

    ## Event :: Load Property Record  ----
    observeEvent(input$property, {
      req(input$property)
      record <- load_property_record(db_con, input$property) |>
        arrange(pid)

      if (nrow(record) >= 1) {
        selected_record(record)
        original_securement_notes(unique(record$securement_action_description))

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
    })

    ## Event :: PID selected ----
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

        updateTextInput(
          session,
          inputId = "ecological_reason",
          value = selected_parcel$priority_ecological_ranking_reason
        )

        updateTextInput(
          session,
          inputId = "securement_reason",
          value = selected_parcel$priority_securement_ranking_reason
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

        # Convert ranking() to a tibble for joining
        ranking_lookup <- tibble(
          id = ranking()$id,
          ranking_label = ranking()$ranking_value
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
      db_id <- as.integer(input$property_name)
      #' This function checks if an input is truthy and
      #' returns the correct type of missing value for
      #' the corresponding database field.
      valid_or_na <- function(x, na) {
        if (isTruthy(x)) x else na
      }

      df <- tibble(
        id = input$property,
        securement_probability_id = input$securement_prob,
        anticipated_closing_year = valid_or_na(
          input$closing_year,
          NA_character_
        ),
        anticipated_closing_date = valid_or_na(
          input$closing_date,
          as.Date(NA)
        ),
        aps_conditions_date = valid_or_na(
          input$conditions_date,
          as.Date(NA)
        ),
        securement_action_description = valid_or_na(
          input$securement_notes,
          NA_character_
        )
      )

      dbx::dbxUpdate(
        db_con,
        table = "properties",
        records = df,
        where_cols = "id"
      )

      # Update date_securement_description if notes changed
      new_notes <- valid_or_na(input$securement_notes, NA_character_)
      if (
        !identical(
          as.character(original_securement_notes()),
          as.character(new_notes)
        )
      ) {
        dbExecute(
          db_con,
          glue_sql(
            "UPDATE properties SET date_securement_description = {Sys.Date()} WHERE id = {input$property}",
            .con = db_con
          )
        )
        original_securement_notes(new_notes)
      }

      update_property_timestamp(con = db_con, property_id = db_id)

      if (!is.null(db_updated)) {
        db_updated(db_updated() + 1)
      }

      # Refresh the selected record with updated data
      record <- load_property_record(db_con, input$property)
      selected_record(record)

      shinyalert(
        title = "Success",
        text = str_glue(
          "The record for {property_name()} has been successfully updated in Properties table"
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
        priority_securement_ranking_reason = input$securement_reason,
        priority_ecological_ranking_reason = input$ecological_reason
      )

      dbx::dbxUpdate(
        db_con,
        table = "parcels",
        records = df,
        where_cols = "pid"
      )

      if (!is.null(db_updated)) {
        db_updated(db_updated() + 1)
      }

      # Refresh the selected record with updated data
      record <- load_property_record(db_con, input$property)
      selected_record(record)

      shinyalert(
        title = "Success",
        text = str_glue(
          "The record for PID {input$pid} has been successfully updated in Parcels table"
        ),
        type = "success",
        closeOnClickOutside = TRUE,
        timer = 10000
      )
    })

    ## Reactive Value :: IAT Property Name ----
    property_name_iat <- reactiveVal(NULL)

    observe({
      req(input$property_iat)
      name <- property_list_iat() |>
        filter(id == input$property_iat) |>
        pull(property_name)
      property_name_iat(name)
    })

    ## Event :: Setup template action ----
    observeEvent(input$setup_template, {
      req(input$property_iat)
      req(iv_template$is_valid())

      action_type_ids <- dbGetQuery(db_con, "SELECT id FROM action_item_type")

      action_structure <- expand.grid(
        action_item_type_id = action_type_ids$id,
        property_id = input$property_iat
      )

      append_db_data(
        "securement_action_items",
        data = action_structure,
        con = db_con,
        silent = TRUE
      )

      dbx::dbxUpdate(
        db_con,
        table = "properties",
        records = tibble(
          id = input$property_iat,
          property_name_public = input$public_name
        ),
        where_cols = "id"
      )

      db_updated(db_updated() + 1)

      updateTextInput(
        session,
        inputId = "public_name",
        value = ""
      )

      shinyalert(
        title = "Success",
        text = glue::glue(
          "Template for {property_name_iat()} has been created with {nrow(action_structure)} action items"
        ),
        type = "success",
        closeOnClickOutside = TRUE,
        timer = 10000
      )
    })

    ## Event :: Clear inputs ----
    observeEvent(input$clear_inputs, {
      selected_record(NULL)
      original_securement_notes(NULL)

      updateSelectizeInput(
        session,
        inputId = "property",
        choices = setNames(
          property_list()$id,
          property_list()$property_name
        ),
        selected = character(0)
      )

      updateSelectizeInput(
        session,
        inputId = "securement_prob",
        choices = setNames(
          securement_probability()$id,
          securement_probability()$probability_value
        ),
        selected = character(0)
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
        value = as.Date(NA)
      )

      updateDateInput(
        session,
        inputId = "conditions_date",
        value = as.Date(NA)
      )

      updateTextAreaInput(
        session,
        inputId = "securement_notes",
        value = ""
      )

      updateSelectizeInput(
        session,
        inputId = "ecological_priority",
        choices = setNames(
          ranking()$id,
          ranking()$ranking_value
        ),
        selected = character(0)
      )

      updateSelectizeInput(
        session,
        inputId = "securement_priority",
        choices = setNames(
          ranking()$id,
          ranking()$ranking_value
        ),
        selected = character(0)
      )

      updateSelectizeInput(
        session,
        inputId = "pid",
        choices = "",
        selected = character(0)
      )

      updateTextAreaInput(
        session,
        inputId = "ecological_reason",
        value = ""
      )

      updateTextAreaInput(
        session,
        inputId = "securement_reason",
        value = ""
      )
    })
  })
}
