# UI ----
module_edit_property_contact_communication_ui <- function(id) {
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
            inputId = ns("property_name"),
            label = "Select Property",
            choices = NULL,
            selected = NULL,
            options = list(
              create = FALSE,
              placeholder = "Select a property"
            )
          ),
          selectizeInput(
            inputId = ns("record_id"),
            label = "Select Communication",
            choices = NULL,
            selected = NULL,
            multiple = FALSE,
            options = list(
              create = FALSE,
              placeholder = "Search or select communication"
            )
          ),
          hr(),
          actionButton(
            inputId = ns("submit_edit"),
            label = "Submit Changes",
            class = "btn-success"
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
              h5("Edit Property Contact Communication")
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
module_edit_property_contact_communication_server <- function(
  id,
  db_con,
  db_updated = NULL
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Input validation ----
    iv <- InputValidator$new()
    iv$add_rule("edit_contact", sv_required())
    iv$add_rule("edit_communication_purpose_id", sv_required())
    iv$add_rule("edit_communication_method_id", sv_required())
    iv$add_rule("edit_date_contacted", sv_required())
    iv$add_rule("edit_communication_description", sv_required())
    iv$enable()

    ## Lookup tables ----
    method <- dbGetQuery(
      db_con,
      "SELECT * FROM communication_method ORDER BY method_value"
    )

    purpose <- dbGetQuery(
      db_con,
      "SELECT * FROM communication_purpose ORDER BY purpose_value"
    )

    ## Reactive :: Properties list (only properties with communications) ----
    properties_list <- reactive({
      db_updated()
      dbGetQuery(
        db_con,
        "
        SELECT DISTINCT p.id, p.property_name
        FROM properties p
        JOIN property_contact_communication pcc ON pcc.property_id = p.id
        ORDER BY p.property_name;
        "
      )
    })

    ## Update property dropdown ----
    observe({
      updateSelectizeInput(
        session,
        inputId = "property_name",
        choices = c(
          "",
          setNames(properties_list()$id, properties_list()$property_name)
        ),
        selected = isolate(input$property_name),
        server = TRUE
      )
    })

    ## Reactive :: Contacts for selected property ----
    contacts <- reactive({
      db_updated()
      req(input$property_name)
      dbGetQuery(
        db_con,
        glue_sql(
          "
          SELECT pcd.*
          FROM property_contact_details pcd
          INNER JOIN properties_contact pc ON pc.property_contact_id = pcd.id
          WHERE pc.property_id = {input$property_name}
          ",
          .con = db_con
        )
      ) |>
        mutate(display_label = glue("{name_first} {name_last} (ID:{id})")) |>
        arrange(name_last, name_first)
    })

    ## Reactive :: Communication choices for selected property ----
    communication_choices <- reactive({
      req(input$property_name)
      db_updated()

      query <- glue_sql(
        "SELECT
          pcc.id,
          pcc.date_contacted,
          pcd.name_first,
          pcd.name_last
        FROM property_contact_communication pcc
        JOIN property_contact_details pcd ON pcc.property_contact_id = pcd.id
        WHERE pcc.property_id = {input$property_name}
        ORDER BY pcc.date_contacted DESC",
        .con = db_con
      )

      results <- dbGetQuery(db_con, query) |>
        mutate(date_contacted = as.Date(date_contacted))

      # Format "First Last - Date - (ID)", most recent first
      set_names(
        results$id,
        paste0(
          results$name_first,
          " ",
          results$name_last,
          " - ",
          format(results$date_contacted, "%Y-%m-%d"),
          " (ID:",
          results$id,
          ")"
        )
      )
    })

    ## Update communication dropdown ----
    observe({
      updateSelectizeInput(
        session,
        inputId = "record_id",
        choices = c("", communication_choices()),
        selected = isolate(input$record_id),
        server = TRUE
      )
    })

    ## Reactive value :: Selected record ----
    selected_record <- reactiveVal(NULL)

    ## Clear selected record when property changes ----
    observeEvent(input$property_name, {
      selected_record(NULL)
    })

    ## Event :: Load record ----
    observeEvent(input$record_id, {
      record_id <- input$record_id

      if (!isTruthy(record_id)) {
        selected_record(NULL)
        return()
      }

      query <- glue_sql(
        "SELECT
          id,
          property_contact_id,
          communication_purpose_id,
          communication_method_id,
          date_contacted,
          communication_description,
          date_follow_up
        FROM property_contact_communication
        WHERE id = {record_id}",
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
        selectizeInput(
          inputId = ns("edit_contact"),
          label = "Property Contact",
          choices = c(
            "",
            setNames(contacts()$id, contacts()$display_label)
          ),
          selected = if (
            !is.null(record) && !is.na(record$property_contact_id)
          ) {
            record$property_contact_id
          } else {
            ""
          }
        ),
        layout_columns(
          col_widths = c(6, 6),
          selectInput(
            inputId = ns("edit_communication_purpose_id"),
            label = "Communication Purpose",
            choices = c("", setNames(purpose$id, purpose$purpose_value)),
            selected = if (
              !is.null(record) && !is.na(record$communication_purpose_id)
            ) {
              record$communication_purpose_id
            } else {
              ""
            }
          ),
          selectInput(
            inputId = ns("edit_communication_method_id"),
            label = "Communication Method",
            choices = c("", setNames(method$id, method$method_value)),
            selected = if (
              !is.null(record) && !is.na(record$communication_method_id)
            ) {
              record$communication_method_id
            } else {
              ""
            }
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          dateInput(
            inputId = ns("edit_date_contacted"),
            label = "Date Contacted",
            value = if (!is.null(record) && !is.na(record$date_contacted)) {
              record$date_contacted
            } else {
              ""
            }
          ),
          dateInput(
            inputId = ns("edit_date_follow_up"),
            label = "Date Follow Up",
            value = if (!is.null(record) && !is.na(record$date_follow_up)) {
              record$date_follow_up
            } else {
              ""
            }
          )
        ),
        textAreaInput(
          inputId = ns("edit_communication_description"),
          label = "Description",
          value = if (
            !is.null(record) && !is.na(record$communication_description)
          ) {
            record$communication_description
          } else {
            ""
          },
          height = "200px",
          width = "100%"
        )
      )
    })

    ## Event :: Write changes ----
    observeEvent(input$submit_edit, {
      req(input$record_id)
      req(iv$is_valid())
      db_id <- as.integer(input$record_id)

      # Build update tibble
      update_tibble <- tibble(
        id = db_id,
        property_contact_id = as.integer(input$edit_contact),
        communication_purpose_id = as.integer(
          input$edit_communication_purpose_id
        ),
        communication_method_id = as.integer(
          input$edit_communication_method_id
        ),
        date_contacted = format(input$edit_date_contacted, "%Y-%m-%d"),
        communication_description = input$edit_communication_description,
        date_follow_up = if (isTruthy(input$edit_date_follow_up)) {
          format(input$edit_date_follow_up, "%Y-%m-%d")
        } else {
          as.Date(NA)
        }
      )

      # Update the record
      dbx::dbxUpdate(
        db_con,
        table = "property_contact_communication",
        records = update_tibble,
        where_cols = "id"
      )

      # Signal update
      if (!is.null(db_updated)) {
        db_updated(db_updated() + 1)
      }

      shinyalert(
        title = "Success",
        text = str_glue(
          "Communication record {db_id} has been successfully updated"
        ),
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
        inputId = "property_name",
        selected = character(0),
        choices = c(
          "",
          setNames(properties_list()$id, properties_list()$property_name)
        ),
        server = TRUE
      )

      updateSelectizeInput(session, "edit_contact", selected = character(0))
      updateSelectInput(
        session,
        "edit_communication_purpose_id",
        selected = character(0)
      )
      updateSelectInput(
        session,
        "edit_communication_method_id",
        selected = character(0)
      )
      updateDateInput(session, "edit_date_contacted", value = "")
      updateDateInput(session, "edit_date_follow_up", value = "")
      updateTextAreaInput(
        session,
        "edit_communication_description",
        value = ""
      )

      updateSelectizeInput(
        session,
        inputId = "record_id",
        choices = c("", communication_choices()),
        selected = character(0),
        server = TRUE
      )
    })
  })
}
