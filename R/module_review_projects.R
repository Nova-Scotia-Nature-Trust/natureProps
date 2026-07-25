# UI ----
# NAV PANEL :: PROJECT OVERVIEW
module_review_projects_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(
      HTML(
        "
        .indicator-grid {
          display: grid;
          grid-template-columns: repeat(auto-fill, minmax(200px, 1fr));
          gap: 18px;
        }

        .indicator-card {
          display: flex;
          align-items: center;
          gap: 16px;
          padding: 18px 20px;
          border-radius: 16px;
          background: linear-gradient(135deg, #f7f9fa, #ffffff);
          box-shadow: 0 1px 4px rgba(0,0,0,0.08);
          transition: transform 0.15s ease, box-shadow 0.15s ease;
        }
        .indicator-card:hover {
          transform: translateY(-3px);
          box-shadow: 0 4px 16px rgba(0,0,0,0.15);
        }

        .indicator-card .indicator-icon {
          font-size: 28px;
          opacity: 0.8;
        }

        .indicator-card .indicator-value {
          font-size: 1.6em;
          font-weight: 600;
          margin: 0;
          line-height: 1.1;
        }
        .indicator-card .indicator-value.text {
          font-size: 1.1em;
          white-space: normal;
          word-break: break-word;
        }

        .indicator-card .indicator-title {
          margin: 0;
          color: #607080;
          font-size: 0.9em;
          font-weight: 500;
          letter-spacing: 0.3px;
        }

        .indicator-card.success .indicator-icon { color: #198754; }
        .indicator-card.primary .indicator-icon { color: #0d6efd; }
        .indicator-card.warning .indicator-icon { color: #ffc107; }
        .indicator-card.danger .indicator-icon { color: #dc3545; }

        [data-bs-theme='dark'] .indicator-card .indicator-value {
          color: #495057;
        }
        [data-bs-theme='dark'] .indicator-card .indicator-title {
          color: #6c757d;
        }

        .record-row {
          padding: 10px 0;
          border-bottom: 1px solid #e0e0e0;
        }
        .record-row:last-child {
          border-bottom: none;
        }
        .record-row-title {
          margin: 0;
          font-weight: 600;
        }
        .record-row-subtitle {
          margin: 2px 0 0 0;
          color: #666;
          font-size: 1.05rem;
        }

        .info-box {
          background: #f7f9fa;
          border-radius: 8px;
          padding: 12px 14px;
          margin-top: 6px;
        }
        [data-bs-theme='dark'] .info-box {
          background: rgba(255, 255, 255, 0.05);
        }

        .section-divider {
          border-top: 3px solid #adb5bd;
          opacity: 1;
        }
        "
      )
    ),
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

      choices <- all_properties |>
        pull(property_name) |>
        sort()

      if (length(choices) == 0) {
        "No properties"
      } else {
        choices
      }
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
        if (
          is.null(input$property) ||
            input$property == "" ||
            input$property == "No properties"
        ) {
          selected_record(NULL)
          return()
        }

        db_updated()

        prop_name <- input$property

        query_01 <- glue_sql(
          "
        SELECT p.property_description, 
               p.phase_id_description, 
               p.phase_id_change,
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

        query_02 <- glue_sql(
          "   
        SELECT pr.property_name,                
               con.property_contact_description, 
               con.name_last, 
               con.name_first
        FROM properties pr 
        LEFT JOIN properties_contact pc ON pr.id = pc.property_id
        LEFT JOIN property_contact_details con ON pc.property_contact_id = con.id  
        WHERE pr.property_name = {prop_name};
        ",
          .con = db_con
        )

        query_02_result <- dbGetQuery(db_con, query_02)

        pids_string <- dbGetQuery(
          db_con,
          glue_sql(
            "
            SELECT pa.pid 
            FROM properties pr
            LEFT JOIN parcels pa ON pr.id = pa.property_id
            WHERE pr.property_name = {prop_name}",
            .con = db_con
          )
        ) |>
          pull(pid) |>
          unique() |>
          paste(collapse = ", ")

        # One row per distinct contact, used for the Property Contact table
        contacts_df <- query_02_result |>
          filter(!is.na(name_first) | !is.na(name_last)) |>
          distinct(name_first, name_last, property_contact_description) |>
          arrange(name_last, name_first)

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

        comms_df <- dbGetQuery(db_con, query_03)

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

        actions_df <- dbGetQuery(db_con, query_04)

        if (nrow(record_01) == 1) {
          selected_record(list(
            info = record_01,
            pids = pids_string,
            contacts = contacts_df,
            comms = comms_df,
            actions = actions_df
          ))
        } else {
          selected_record(NULL)
        }
      },
      ignoreNULL = FALSE
    )

    ## Helper :: row-1 value box ----
    info_value_box <- function(title, value, icon, theme = "primary") {
      display_value <- if (
        is.null(value) || length(value) == 0 || is.na(value) || value == ""
      ) {
        "\u2014"
      } else {
        as.character(value)
      }

      div(
        class = paste("indicator-card", theme),
        div(class = "indicator-icon", bs_icon(icon)),
        div(
          h3(class = "indicator-value text", display_value),
          p(class = "indicator-title", title)
        )
      )
    }

    ## Helper :: table-style row (bold title + muted subtitle) ----
    record_row <- function(title, subtitle = NULL) {
      div(
        class = "record-row",
        p(class = "record-row-title", title),
        if (!is.null(subtitle) && !is.na(subtitle) && subtitle != "") {
          p(class = "record-row-subtitle", subtitle)
        }
      )
    }

    ## Create UI for database fields ----
    output$project_summary_ui <- renderUI({
      req(selected_record())

      rec <- selected_record()
      info <- rec$info

      phase_followup <- if (is.na(info$phase_id_change)) {
        NA
      } else {
        format(as.Date(info$phase_id_change), "%B %d, %Y")
      }

      tagList(
        # Row 1: value boxes
        div(
          class = "indicator-grid",
          info_value_box("Project Name", input$property, "signpost", "primary"),
          info_value_box("PIDs", rec$pids, "geo-alt", "primary"),
          info_value_box(
            "Date Added",
            format(as.Date(info$date_added), "%B %d, %Y"),
            "calendar-event",
            "warning"
          ),
          info_value_box("Team Lead", info$team_lead, "person", "primary"),
          info_value_box("Project Phase", info$phase, "flag", "success"),
          info_value_box(
            "Phase Set",
            phase_followup,
            "calendar-check",
            "warning"
          )
        ),

        hr(class = "section-divider"),

        # Row 2: nicer formatting, same 2-column layout
        layout_columns(
          col_widths = c(6, 6),
          div(
            div(
              style = "display: flex; align-items: center; gap: 8px;",
              strong("Property & Opportunity Overview"),
              popover(
                icon("question-circle"),
                includeMarkdown("popups/prop_opp_overview.md"),
                title = "Context",
                placement = "top"
              )
            ),
            div(class = "info-box", info$property_description)
          ),
          div(
            div(
              style = "display: flex; align-items: center; gap: 8px;",
              strong("Phase Description"),
              popover(
                icon("question-circle"),
                includeMarkdown("popups/phase_desc.md"),
                title = "Context",
                placement = "top"
              )
            ),
            div(class = "info-box", info$phase_id_description)
          )
        ),

        # Row 3: nicer formatting, matching row 2 (Property Contact Description / Securement Status)
        layout_columns(
          col_widths = c(6, 6),
          div(
            div(
              style = "display: flex; align-items: center; gap: 8px;",
              strong("Property Contact(s) Description"),
              popover(
                icon("question-circle"),
                includeMarkdown("popups/prop_contact_desc.md"),
                title = "Context",
                placement = "top"
              )
            ),
            div(
              class = "info-box",
              if (nrow(rec$contacts) == 0) {
                "No contacts on file."
              } else {
                rec$contacts |>
                  mutate(
                    # Build "First Last" from whichever name parts are present
                    contact_name = case_when(
                      !is.na(name_first) & !is.na(name_last) ~
                        paste(name_first, name_last),
                      !is.na(name_first) ~ name_first,
                      !is.na(name_last) ~ name_last,
                      TRUE ~ "Unknown contact"
                    ),
                    contact_line = if_else(
                      !is.na(property_contact_description) &
                        property_contact_description != "",
                      paste0(contact_name, " - ", property_contact_description),
                      contact_name
                    )
                  ) |>
                  pull(contact_line) |>
                  paste(collapse = "<br>") |>
                  HTML()
              }
            )
          ),
          div(
            div(
              style = "display: flex; align-items: center; gap: 8px;",
              strong("Securement Status"),
              popover(
                icon("question-circle"),
                includeMarkdown("popups/securement_desc.md"),
                title = "Context",
                placement = "top"
              )
            ),
            div(
              class = "info-box",
              if (is.na(info$securement_action_description)) {
                "No securement status recorded."
              } else if (is.na(info$date_securement_description)) {
                info$securement_action_description
              } else {
                paste0(
                  format(
                    as.Date(info$date_securement_description),
                    "%B %d, %Y"
                  ),
                  ": ",
                  info$securement_action_description
                )
              }
            )
          )
        ),

        hr(class = "section-divider"),

        # Row 4: table (Internal Communications)
        div(
          strong("Internal Communications"),
          if (nrow(rec$comms) == 0) {
            p(class = "text-muted", "No internal communications logged.")
          } else {
            lapply(seq_len(nrow(rec$comms)), function(i) {
              comm <- rec$comms[i, ]
              record_row(
                title = format(as.Date(comm$date), "%B %d, %Y"),
                subtitle = comm$communication_description
              )
            })
          }
        ),

        hr(class = "section-divider"),

        # Row 5: nicer formatting (Action Items, no borders)
        div(
          strong("Action Items"),
          if (nrow(rec$actions) == 0) {
            p(class = "text-muted", "No action items.")
          } else {
            lapply(seq_len(nrow(rec$actions)), function(i) {
              action <- rec$actions[i, ]
              complete <- isTRUE(action$action_complete)

              div(
                style = "padding: 6px 0;",
                span(
                  class = if (complete) {
                    "badge bg-success"
                  } else {
                    "badge bg-warning"
                  },
                  if (complete) "Complete" else "Open"
                ),
                span(
                  style = "font-weight: 600; margin-left: 8px;",
                  action$team_value
                ),
                span(
                  style = "color: #666; margin-left: 8px;",
                  format(as.Date(action$due_date), "%B %d, %Y")
                ),
                div(
                  style = "margin-top: 2px; font-size: 1.05rem;",
                  action$action_item_description
                )
              )
            })
          }
        ),

        hr(class = "section-divider"),

        # Row 6: nicer formatting (Stewardship Concerns)
        div(
          strong("Stewardship Concerns"),
          div(
            class = "info-box",
            if (
              is.na(info$stewardship_concerns) ||
                info$stewardship_concerns == ""
            ) {
              "None on file."
            } else {
              info$stewardship_concerns
            }
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
