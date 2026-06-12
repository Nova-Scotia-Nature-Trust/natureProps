# UI ----
# NAV PANEL :: ACTIVE PROJECTS REVIEW
module_securement_review_ui <- function(id) {
  ns <- NS(id)

  div(
    style = "height: 100%; display: flex; flex-direction: column;",
    card(
      full_screen = TRUE,
      height = "100%",
      layout_sidebar(
        sidebar = sidebar(
          title = "Edit Securement Details",
          open = TRUE,
          width = 300,
          accordion(
            open = FALSE,
            multiple = FALSE,
            accordion_panel(
              title = "Select Properties",
              value = "properties_panel",
              selectizeInput(
                ns("selected_properties"),
                "Select Properties",
                choices = NULL,
                multiple = TRUE,
                width = "100%",
                options = list(
                  placeholder = "Select one or more properties"
                )
              ),
              actionButton(
                inputId = ns("clear_inputs_properties"),
                label = "Clear Inputs",
                class = "btn-secondary",
                width = "100%"
              )
            ),
            accordion_panel(
              title = "Dates & Securement Probability",
              value = "edit_outreach_panel",
              selectizeInput(
                ns("closing_year"),
                "Closing Year",
                choices = NULL,
                multiple = FALSE,
                width = "100%"
              ),
              dateInput(
                ns("closing_date"),
                "Closing Date",
                value = as.Date(NA),
                width = "100%"
              ),
              selectizeInput(
                ns("securement_probability"),
                "Securement Probability",
                choices = NULL,
                multiple = FALSE,
                width = "100%"
              ),
              actionButton(
                inputId = ns("submit_edit"),
                label = "Submit Edit",
                class = "btn-primary",
                width = "100%"
              ),
              actionButton(
                inputId = ns("clear_inputs_dates"),
                label = "Clear Inputs",
                class = "btn-secondary",
                width = "100%"
              )
            ),
            accordion_panel(
              title = "Action Items",
              value = "action_items_panel",
              selectizeInput(
                ns("action_item_type"),
                "Select Action Item Type",
                choices = NULL,
                multiple = TRUE,
                width = "100%"
              ),
              selectizeInput(
                ns("action_item_status"),
                "Select Action Status",
                choices = NULL,
                multiple = FALSE,
                width = "100%"
              ),
              actionButton(
                inputId = ns("submit_action_items"),
                label = "Submit Edit",
                class = "btn-primary",
                width = "100%"
              ),
              actionButton(
                inputId = ns("clear_inputs_actions"),
                label = "Clear Inputs",
                class = "btn-secondary",
                width = "100%"
              )
            )
          )
        ),
        # Main layout - results card
        card(
          height = "100%",
          card_header(
            class = "d-flex justify-content-between align-items-center",
            h5("Securement Review"),
            downloadButton(
              outputId = ns("download_data"),
              label = "Download",
              class = "btn-sm"
            )
          ),
          card_body(
            DTOutput(outputId = ns("view_df"), height = "100%")
          )
        )
      )
    )
  )
}

# Server ----
module_securement_review_server <- function(id, db_con, db_updated = NULL) {
  moduleServer(id, function(input, output, session) {
    ## Input validation ----
    iv <- InputValidator$new()
    iv$add_rule("selected_properties", sv_required())
    # iv$add_rule("closing_year", sv_required())
    # iv$add_rule("closing_date", sv_required())
    # iv$add_rule("securement_probability", sv_required())
    iv$enable()

    ## Reactive values ----
    table_data <- reactiveVal(NULL)

    ## Properties reactive ----
    properties_reactive <- reactive({
      if (!is.null(db_updated)) {
        db_updated()
      }

      dbGetQuery(
        db_con,
        "SELECT DISTINCT sai.property_id AS id, 
                         pr.property_name,
                         pr.property_name_public,
                         ph.phase_value 
        FROM securement_action_items AS sai
        JOIN properties pr ON sai.property_id = pr.id
        JOIN phase ph ON pr.phase_id = ph.id 
        WHERE ph.phase_value = 'Active - Securement'
        ORDER BY pr.property_name;"
      )
    })

    observe({
      updateSelectizeInput(
        session,
        inputId = "selected_properties",
        choices = setNames(
          properties_reactive()$id,
          properties_reactive()$property_name_public
        ),
        selected = isolate(input$selected_properties),
        server = TRUE
      )
    })

    ## Closing year reactive ----
    closing_year_reactive <- reactive({
      if (!is.null(db_updated)) {
        db_updated()
      }

      dbGetQuery(
        conn = db_con,
        statement = "SELECT DISTINCT anticipated_closing_year FROM properties;"
      ) |>
        pull() |>
        sort()
    })

    observe({
      updateSelectizeInput(
        session,
        inputId = "closing_year",
        choices = closing_year_reactive(),
        selected = isolate(input$closing_year),
        server = TRUE
      )
    })

    ## Securement probability reactive ----
    securement_probability_reactive <- reactive({
      dbGetQuery(
        conn = db_con,
        statement = "SELECT id, probability_value 
                     FROM securement_probability 
                     ORDER BY probability_value;"
      )
    })

    observe({
      updateSelectizeInput(
        session,
        inputId = "securement_probability",
        choices = setNames(
          securement_probability_reactive()$id,
          securement_probability_reactive()$probability_value
        ),
        selected = isolate(input$securement_probability),
        server = TRUE
      )
    })

    ## Action item types ----
    action_item_types <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, type_value FROM action_item_type ORDER BY id;"
      )
    })

    observe({
      updateSelectizeInput(
        session,
        "action_item_type",
        choices = setNames(
          action_item_types()$id,
          action_item_types()$type_value
        ),
        selected = isolate(input$action_item_type),
        server = TRUE
      )
    })

    ## Action item statuses ----
    action_item_statuses <- reactive({
      db_updated()
      dbGetQuery(
        db_con,
        "SELECT id, status_value FROM action_item_status ORDER BY status_value;"
      )
    })

    observe({
      updateSelectizeInput(
        session,
        "action_item_status",
        choices = setNames(
          action_item_statuses()$id,
          action_item_statuses()$status_value
        ),
        selected = isolate(input$action_item_status),
        server = TRUE
      )
    })

    ## Load data ----
    observe({
      if (!is.null(db_updated)) {
        db_updated()
      }

      # Query securement action items
      data <- dbGetQuery(
        db_con,
        "SELECT * FROM view_securement_action_items;"
      )

      data <- data |>
        select("Property Name", "Action Item", "Status") |>
        pivot_wider(
          id_cols = "Property Name",
          names_from = "Action Item",
          values_from = "Status"
        )

      # Query additional securement data
      additional_data <- dbGetQuery(
        conn = db_con,
        statement = '
          SELECT pr.property_name AS "Property Name", 
                 pr.property_name_public AS "Property Name Public",
                pr.anticipated_closing_year AS "Closing Year",
                pr.anticipated_closing_date AS "Closing Date",
                pr.securement_action_description AS "Securement Status",
                pr.aps_conditions_date AS "APS Date",
                se.probability_value AS "Securement Probability",
                ph.phase_value AS "Phase"
          FROM properties pr
          LEFT JOIN securement_probability se ON pr.securement_probability_id = se.id
          LEFT JOIN phase ph ON pr.phase_id = ph.id;'
      )

      data <- data |>
        left_join(additional_data, join_by("Property Name")) |>
        select(-"Property Name") |>
        relocate(
          "Property Name Public",
          "Closing Year",
          "Closing Date",
          "Securement Probability",
          "Phase",
          "Securement Status",
          "APS Date"
        ) |>
        arrange(`Property Name Public`) |>
        filter(Phase == "Active - Securement")

      table_data(data)
    })

    ## Submit edit ----
    observeEvent(input$submit_edit, {
      req(iv$is_valid())

      # Start with base required fields
      update_records <- tibble(
        id = input$selected_properties
      )

      # Add optional fields only if they have values
      if (isTruthy(input$closing_year)) {
        update_records <- update_records |>
          mutate(anticipated_closing_year = input$closing_year)
      }

      if (isTruthy(input$closing_date)) {
        update_records <- update_records |>
          mutate(anticipated_closing_date = input$closing_date)
      }

      if (isTruthy(input$securement_probability)) {
        update_records <- update_records |>
          mutate(
            securement_probability_id = as.integer(input$securement_probability)
          )
      }

      # Only update if there are fields to update beyond just the id
      if (ncol(update_records) > 1) {
        dbx::dbxUpdate(
          db_con,
          table = "properties",
          records = update_records,
          where_cols = "id"
        )

        if (!is.null(db_updated)) {
          db_updated(db_updated() + 1)
        }

        # Get property names for confirmation message
        selected_props <- properties_reactive() |>
          filter(id %in% input$selected_properties) |>
          pull(property_name)

        # Count how many fields were updated
        fields_updated <- ncol(update_records) - 1

        shinyalert(
          title = "Success",
          text = glue::glue(
            "Updated {fields_updated} field{ifelse(fields_updated == 1, '', 's')} for {length(selected_props)} propert{ifelse(length(selected_props) == 1, 'y', 'ies')}"
          ),
          type = "success",
          timer = 5000
        )
      } else {
        shinyalert(
          title = "No Changes",
          text = "Please select at least one field to update",
          type = "warning",
          timer = 3000
        )
      }
    })

    ## Submit action items ----
    observeEvent(input$submit_action_items, {
      req(input$selected_properties)
      req(isTruthy(input$action_item_type))
      req(isTruthy(input$action_item_status))

      # Create records for each property and action item type combination
      upsert_records <- expand.grid(
        property_id = input$selected_properties,
        action_item_type_id = input$action_item_type,
        stringsAsFactors = FALSE
      ) |>
        as_tibble() |>
        mutate(action_item_status_id = as.integer(input$action_item_status))

      dbx::dbxUpsert(
        db_con,
        table = "securement_action_items",
        records = upsert_records,
        where_cols = c("property_id", "action_item_type_id")
      )

      db_updated(db_updated() + 1)

      # Get property names for confirmation message
      selected_props <- properties_reactive() |>
        filter(id %in% input$selected_properties) |>
        pull(property_name)

      shinyalert(
        title = "Success",
        text = glue::glue(
          "Updated {nrow(upsert_records)} action item{ifelse(nrow(upsert_records) == 1, '', 's')} for {length(selected_props)} propert{ifelse(length(selected_props) == 1, 'y', 'ies')}"
        ),
        type = "success",
        timer = 5000
      )
    })

    ## Clear properties inputs ----
    observeEvent(input$clear_inputs_properties, {
      updateSelectizeInput(
        session,
        inputId = "selected_properties",
        selected = character(0)
      )
    })

    ## Clear inputs ----
    observeEvent(input$clear_inputs_dates, {
      updateSelectizeInput(
        session,
        inputId = "closing_year",
        selected = character(0)
      )
      updateDateInput(
        session,
        inputId = "closing_date",
        value = NA
      )
      updateSelectizeInput(
        session,
        inputId = "securement_probability",
        selected = character(0)
      )
    })

    ## Clear action inputs ----
    observeEvent(input$clear_inputs_actions, {
      updateSelectizeInput(
        session,
        inputId = "action_item_type",
        selected = character(0)
      )
      updateSelectizeInput(
        session,
        inputId = "action_item_status",
        selected = character(0)
      )
    })

    ## Render data table ----
    output$view_df <- renderDT({
      req(table_data())

      # Convert character columns to factors for select inputs
      data_for_display <- table_data() |>
        mutate(across(where(is.character), as.factor))

      DT::datatable(
        data_for_display,
        options = list(
          pageLength = 50,
          lengthMenu = list(
            c(10, 25, 50, 100, -1),
            c("10", "25", "50", "100", "All")
          ),
          scrollX = TRUE,
          scrollY = "400px",
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
        extensions = c("Buttons", "FixedHeader")
      )
    })

    ## Download handler ----
    output$download_data <- downloadHandler(
      filename = function() {
        glue("securement_review_{format(Sys.Date(), '%Y%m%d')}.csv")
      },
      content = function(file) {
        data_to_download <- table_data()

        if (!is.null(data_to_download) && nrow(data_to_download) > 0) {
          write_csv(data_to_download, file)
        } else {
          write_csv(data.frame(), file)
        }
      }
    )
  })
}
