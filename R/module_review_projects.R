# UI ----
# NAV PANEL :: PROJECT OVERVIEW
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
          radioButtons(
            ns("date_filter"),
            "Properties added in:",
            choices = c(
              "Last week" = "7",
              "Last 2 weeks" = "14",
              "Last month" = "30",
              "All" = "all"
            ),
            selected = "all"
          ),
          selectizeInput(
            ns("property"),
            "Select Property",
            choices = NULL,
            multiple = FALSE,
            width = "80%"
          ),
          actionButton(
            inputId = ns("clear_inputs"),
            label = "Clear Inputs",
            class = "btn-secondary"
          ),
          actionButton(
            inputId = ns("refresh_data"),
            label = "Refresh Data",
            icon = icon("arrows-rotate"),
            class = "btn-primary"
          )
        ),
        div(
          style = "height: 100%; display: flex; flex-direction: column;",
          layout_columns(
            height = "100%",
            col_widths = c(12, -1),
            ## Card :: Projects ----
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
      if (!is.null(db_updated)) {
        db_updated()
      }

      all_properties <- dbGetQuery(
        db_con,
        "SELECT property_name, date_added FROM properties;"
      )

      date_filter <- input$date_filter

      if (!is.null(date_filter) && date_filter != "all") {
        cutoff <- Sys.Date() - as.integer(date_filter)
        all_properties <- all_properties |>
          filter(date_added >= cutoff)
      }

      all_properties |>
        pull(property_name) |>
        sort()
    })

    ## Update select input with record IDs based on table
    observe({
      updateSelectizeInput(
        session,
        inputId = "property",
        choices = c("", property_choices()),
        selected = isolate(input$property),
        server = TRUE
      )
    })

    ## Event :: Manual refresh ----
    observeEvent(input$refresh_data, {
      db_updated(db_updated() + 1L)
    })

    ## Reactive value :: Selected record ----
    selected_record <- reactiveVal(NULL)

    ## Event :: Load record on property selection ----
    observeEvent(
      input$property,
      {
        # If property is empty or NULL, clear the record and bail
        if (is.null(input$property) || input$property == "") {
          selected_record(NULL)
          return()
        }

        db_updated()

        prop_name <- input$property

        query_01 <- glue_sql(
          "
        SELECT p.property_description, 
               p.phase_id_description, 
               p.phase_id_followup,
               p.securement_action_description,
               p.date_securement_description,
               p.date_added,
               tl.team_value as team_lead, 
               ph.phase_value as phase, 
               p.stewardship_concerns
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

        # Add date to securement action description if available
        record_01 <- record_01 |>
          mutate(
            securement_action_description = case_when(
              !is.na(date_securement_description) ~
                paste0(
                  format(as.Date(date_securement_description), "%B %d, %Y"),
                  " - ",
                  securement_action_description
                ),
              TRUE ~ securement_action_description
            )
          )

        query_02 <- glue_sql(
          "   
        SELECT pa.pid,                
               con.property_contact_description, 
               con.name_last, 
               con.name_first
        FROM parcels pa
        LEFT JOIN properties pr ON pa.property_id = pr.id
        LEFT JOIN parcel_property_contact ppc ON pa.id = ppc.parcel_id
        LEFT JOIN property_contact_details con ON ppc.property_contact_id = con.id  
        WHERE pr.property_name = {prop_name};
        ",
          .con = db_con
        )

        record_02 <- dbGetQuery(db_con, query_02) |>
          mutate(
            contact_desc = if_else(
              !is.na(property_contact_description),
              str_glue(" - {property_contact_description}"),
              ""
            ),
            contact_pair = str_glue("{name_first} {name_last}{contact_desc}")
          ) |>
          summarise(
            pids = paste(unique(pid), collapse = ", "),
            property_contacts = paste(unique(contact_pair), collapse = "<br>")
          ) |>
          mutate(
            across(
              everything(),
              ~ str_remove_all(.x, "^NA(?:\\s+NA)?") |> str_squish()
            )
          )

        query_03 <- glue_sql(
          "   
        SELECT ic.date, ic.communication_description
        FROM internal_communications ic
        LEFT JOIN properties prop ON ic.property_id = prop.id
        WHERE prop.property_name = {prop_name}
        ORDER BY ic.date DESC;
        ",
          .con = db_con
        )

        record_03 <- dbGetQuery(db_con, query_03) |>
          mutate(
            formatted_comm = str_glue("{date}: {communication_description}")
          ) |>
          summarise(
            internal_communications = paste(formatted_comm, collapse = "<br>")
          )

        query_04 <- glue_sql(
          "   
       SELECT
          tl.team_value,
          tla.action_item_description,
          tla.due_date,
          tla.action_complete
        FROM
          team_lead_actions tla
          LEFT JOIN properties p ON tla.property_id = p.id
          LEFT JOIN team_lead tl ON tla.team_lead_id = tl.id
        WHERE
          p.property_name = {prop_name}
        ORDER BY
          tla.due_date;
        ",
          .con = db_con
        )

        record_04 <- dbGetQuery(db_con, query_04) |>
          mutate(
            formatted_comm = str_glue(
              "{team_value} - {due_date}: {action_item_description} Complete: {action_complete}"
            )
          ) |>
          summarise(
            action_items = paste(formatted_comm, collapse = "<br>")
          )

        record <- bind_cols(record_01, record_02, record_03, record_04)

        if (nrow(record) == 1) {
          selected_record(record)
        } else {
          selected_record(NULL)
        }
      },
      ignoreNULL = FALSE
    )

    ## Create UI for database fields ----
    output$project_summary_ui <- renderUI({
      req(selected_record())

      df <- selected_record()

      tagList(
        # First row: 3 columns for first 3 fields
        layout_columns(
          col_widths = c(2, 2, 2, 2, 2, 2),
          div(
            strong("Project Name:"),
            br(),
            input$property
          ),
          div(
            strong("PIDs:"),
            br(),
            df$pids
          ),
          div(
            strong("Date Added:"),
            br(),
            format(as.Date(df$date_added), "%B %d, %Y")
          ),
          div(
            strong("Team Lead:"),
            br(),
            df$team_lead
          ),
          div(
            strong("Project Phase:"),
            br(),
            df$phase
          ),
          div(
            strong("Phase Followup:"),
            br(),
            format(as.Date(df$phase_id_followup), "%B %d, %Y")
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
                includeMarkdown("popups/prop_opp_overview.md"),
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
                includeMarkdown("popups/phase_desc.md"),
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
                includeMarkdown("popups/prop_contact_desc.md"),
                title = "Property Contact Help",
                placement = "top"
              )
            ),
            br(),
            HTML(df$property_contacts)
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
                includeMarkdown("popups/securement_desc.md"),
                title = "Securement Status Help",
                placement = "top"
              )
            ),
            br(),
            df$securement_action_description
          )
        ),
        hr(),
        div(
          strong("Internal Communications:"),
          div(
            style = "margin-top: 5px;",
            HTML(df$internal_communications)
          )
        ),
        hr(),
        div(
          strong("Action Items:"),
          div(
            style = "margin-top: 5px;",
            HTML(df$action_items)
          )
        ),
        hr(),
        div(
          strong("Stewardship Concerns:"),
          div(
            style = "margin-top: 5px;",
            HTML(df$stewardship_concerns)
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

    ## Clear property selection when date filter changes ----
    observeEvent(input$date_filter, {
      updateSelectizeInput(
        session,
        "property",
        choices = c("", property_choices()),
        selected = character(0),
        server = TRUE
      )
      selected_record(NULL)
    })
  })
}
