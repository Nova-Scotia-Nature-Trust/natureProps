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

        .accordion-button {
          font-size: 1.05rem;
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
              inputId = ns("refresh_data"),
              label = "Refresh Data",
              icon = icon("arrows-rotate"),
              class = "btn-primary"
            ),
            downloadButton(
              outputId = ns("generate_report"),
              label = "Generate Report",
              icon = icon("file-word"),
              class = "btn-primary"
            ),
            actionButton(
              inputId = ns("clear_inputs"),
              label = "Clear Inputs",
              class = "btn-secondary"
            ),
          ),
          div(
            style = "height: 100%; display: flex; flex-direction: column;",
            layout_columns(
              height = "100%",
              col_widths = c(8, 4),
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
                    div(style = "flex-grow: 1;")
                  )
                )
              ),
              ## Right column: Internal Communications + Action Items ----
              accordion(
                id = ns("log_accordion"),
                open = FALSE,
                multiple = FALSE,
                # Panel :: Log Internal Communications ----
                accordion_panel(
                  "Log Internal Communications",
                  icon = popover(
                    icon("question-circle"),
                    includeMarkdown("popups/internal_comms.md"),
                    title = "Context",
                    placement = "right"
                  ),
                  div(
                    style = "display: flex; flex-direction: column; gap: 15px;",
                    dateInput(
                      ns("comm_date"),
                      "Date",
                      width = "100%"
                    ),
                    textAreaInput(
                      ns("communication_description"),
                      "Communication Description",
                      value = "",
                      width = "100%",
                      height = "150px",
                      resize = "vertical"
                    ),
                    actionButton(
                      inputId = ns("log_internal"),
                      label = "Log Communication",
                      class = "btn-success"
                    )
                  )
                ),

                # Panel :: Log Action Item ----
                accordion_panel(
                  "Log Action Item",
                  icon = popover(
                    icon("question-circle"),
                    includeMarkdown("popups/log_actions.md"),
                    title = "Context",
                    placement = "right"
                  ),
                  div(
                    style = "display: flex; flex-direction: column; gap: 15px;",
                    selectizeInput(
                      ns("team_lead"),
                      "Team Lead",
                      choices = NULL,
                      multiple = FALSE,
                      width = "100%"
                    ),
                    dateInput(
                      ns("due_date"),
                      "Due Date",
                      value = as.Date(NA),
                      width = "100%"
                    ),
                    textAreaInput(
                      ns("action_item_description"),
                      "Action Item Description",
                      value = "",
                      width = "100%",
                      height = "150px",
                      resize = "vertical"
                    ),
                    actionButton(
                      inputId = ns("log_action"),
                      label = "Log Action Item",
                      class = "btn-success"
                    )
                  )
                ),
                # Panel :: Log Property Contact Communication ----
                accordion_panel(
                  "Log Property Contact Communication",
                  div(
                    style = "display: flex; flex-direction: column; gap: 15px;",
                    selectizeInput(
                      ns("contact_comm_contact_id"),
                      "Select Property Contact",
                      choices = NULL,
                      multiple = FALSE,
                      width = "100%",
                      options = list(
                        create = FALSE,
                        placeholder = "Select a property contact"
                      )
                    ),
                    layout_columns(
                      col_widths = c(6, 6),
                      selectizeInput(
                        ns("contact_comm_purpose_id"),
                        "Communication Purpose",
                        choices = NULL
                      ),
                      selectizeInput(
                        ns("contact_comm_method_id"),
                        "Communication Method",
                        choices = NULL
                      )
                    ),
                    layout_columns(
                      col_widths = c(6, 6),
                      dateInput(
                        ns("contact_comm_date"),
                        "Date Contacted",
                        width = "100%"
                      ),
                      dateInput(
                        ns("contact_comm_follow_up"),
                        "Date Follow Up",
                        value = as.Date(NA),
                        width = "100%"
                      )
                    ),
                    textAreaInput(
                      ns("contact_comm_description"),
                      "Communication Description",
                      value = "",
                      width = "100%",
                      height = "150px",
                      resize = "vertical"
                    ),
                    actionButton(
                      inputId = ns("log_contact_comm"),
                      label = "Log Communication",
                      class = "btn-success"
                    )
                  )
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
module_review_projects_server <- function(id, db_con, db_updated = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    updateDateInput(session, "comm_date", value = Sys.Date())
    updateDateInput(session, "contact_comm_date", value = Sys.Date())

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

    ## Lookup tables :: Property Contact Communication ----
    communication_purpose_lookup <- dbGetQuery(
      db_con,
      "SELECT * FROM communication_purpose ORDER BY purpose_value"
    )

    communication_method_lookup <- dbGetQuery(
      db_con,
      "SELECT * FROM communication_method ORDER BY method_value"
    )

    updateSelectizeInput(
      session,
      "contact_comm_purpose_id",
      choices = setNames(
        communication_purpose_lookup$id,
        communication_purpose_lookup$purpose_value
      ),
      selected = character(0),
      server = TRUE
    )

    updateSelectizeInput(
      session,
      "contact_comm_method_id",
      choices = setNames(
        communication_method_lookup$id,
        communication_method_lookup$method_value
      ),
      selected = character(0),
      server = TRUE
    )

    ## Reactive :: Property contacts for selected property ----
    property_contacts <- reactive({
      if (!is.null(db_updated)) {
        db_updated()
      }
      req(
        input$property,
        input$property != "",
        input$property != "No properties"
      )
      dbGetQuery(
        db_con,
        glue_sql(
          "
          SELECT pcd.id, pcd.name_first, pcd.name_last
          FROM property_contact_details pcd
          INNER JOIN properties_contact pc ON pc.property_contact_id = pcd.id
          INNER JOIN properties pr ON pr.id = pc.property_id
          WHERE pr.property_name = {input$property}
          ",
          .con = db_con
        )
      ) |>
        mutate(display_label = glue("{name_first} {name_last} (ID:{id})")) |>
        arrange(name_last, name_first)
    })

    ## Observer :: Update property contact choices ----
    observe({
      contacts <- property_contacts()
      updateSelectizeInput(
        session,
        "contact_comm_contact_id",
        choices = c(
          "",
          setNames(contacts$id, contacts$display_label)
        ),
        selected = character(0),
        server = TRUE
      )
    })

    ## Reactive :: Team lead choices ----
    team_lead_choices <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, team_value FROM team_lead ORDER BY team_value"
      )
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

    ## Observer :: Update team lead input ----
    observe({
      if (!is.null(db_updated)) {
        db_updated()
      }

      updateSelectizeInput(
        session,
        "team_lead",
        choices = c(
          "",
          setNames(team_lead_choices()$id, team_lead_choices()$team_value)
        ),
        selected = isolate(input$team_lead),
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
               con.name_first,
               con.email,
               con.phone_home,
               con.phone_cell
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

        size_df <- dbGetQuery(
          db_con,
          glue_sql(
            "
            SELECT info.area_ha
            FROM properties pr
            INNER JOIN parcels pa ON pa.property_id = pr.id
            LEFT JOIN parcel_info info ON pa.id = info.parcel_id
            WHERE pr.property_name = {prop_name}",
            .con = db_con
          )
        )

        total_area_ha <- round(sum(size_df$area_ha, na.rm = TRUE), 0)
        total_area_acres <- round(total_area_ha * 2.47105, 0)

        # One row per distinct contact, used for the Property Contact table
        contacts_df <- query_02_result |>
          filter(!is.na(name_first) | !is.na(name_last)) |>
          distinct(
            name_first,
            name_last,
            property_contact_description,
            email,
            phone_home,
            phone_cell
          ) |>
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

        query_03b <- glue_sql(
          "   
        SELECT pcc.id, pcc.date_contacted, cp.purpose_value AS communication_purpose, pcc.communication_description
        FROM property_contact_communication pcc
        LEFT JOIN communication_purpose cp ON pcc.communication_purpose_id = cp.id
        LEFT JOIN properties prop ON pcc.property_id = prop.id
        WHERE prop.property_name = {prop_name}
        ORDER BY pcc.date_contacted DESC, pcc.id DESC;
        ",
          .con = db_con
        )

        contact_comms_df <- dbGetQuery(db_con, query_03b)

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
            size_ha = total_area_ha,
            size_acres = total_area_acres,
            contacts = contacts_df,
            comms = comms_df,
            contact_comms = contact_comms_df,
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
      is_empty <- is.null(value) ||
        length(value) == 0 ||
        (!inherits(value, "html") && (is.na(value) || value == ""))

      display_value <- if (is_empty) {
        "\u2014"
      } else if (inherits(value, "html")) {
        value
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

    ## Helper :: table-style row (bold date - purpose header, description below) ----
    contact_comm_row <- function(date, purpose, description) {
      div(
        class = "record-row",
        p(class = "record-row-title", date, " - ", em(purpose)),
        if (!is.null(description) && !is.na(description) && description != "") {
          p(class = "record-row-subtitle", description)
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
            "Property Size",
            if (is.na(rec$size_ha) || rec$size_ha == 0) {
              NA
            } else {
              HTML(glue(
                "{rec$size_acres} acres<br>{round(rec$size_ha, 2)} hectares"
              ))
            },
            "bounding-box-circles",
            "success"
          ),
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
                "No property contacts in database."
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
                    ),
                    # Combine whichever contact details are available
                    contact_details = pmap_chr(
                      list(email, phone_home, phone_cell),
                      function(email, phone_home, phone_cell) {
                        parts <- c(
                          if (!is.na(email) && email != "") email else NA,
                          if (!is.na(phone_home) && phone_home != "") {
                            paste0("Home: ", phone_home)
                          } else {
                            NA
                          },
                          if (!is.na(phone_cell) && phone_cell != "") {
                            paste0("Cell: ", phone_cell)
                          } else {
                            NA
                          }
                        )
                        parts <- parts[!is.na(parts)]
                        if (length(parts) == 0) {
                          NA_character_
                        } else {
                          paste(parts, collapse = " | ")
                        }
                      }
                    ),
                    contact_line = if_else(
                      !is.na(contact_details),
                      paste0(contact_line, "<br>", contact_details),
                      contact_line
                    )
                  ) |>
                  pull(contact_line) |>
                  paste(collapse = "<br><br>") |>
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
                "No securement status description available."
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

        # Row 4b: table (Property Contact Communications)
        div(
          strong("Property Contact Communications"),
          if (nrow(rec$contact_comms) == 0) {
            p(
              class = "text-muted",
              "No property contact communications logged."
            )
          } else {
            lapply(seq_len(nrow(rec$contact_comms)), function(i) {
              contact_comm <- rec$contact_comms[i, ]
              contact_comm_row(
                date = format(
                  as.Date(contact_comm$date_contacted),
                  "%B %d, %Y"
                ),
                purpose = contact_comm$communication_purpose,
                description = contact_comm$communication_description
              )
            })
          }
        ),

        hr(class = "section-divider"),

        # Row 5: nicer formatting (Action Items, no borders)
        div(
          strong("Action Items"),
          if (nrow(rec$actions) == 0) {
            p(class = "text-muted", "No general action items.")
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
              "None identified at this time."
            } else {
              info$stewardship_concerns
            }
          )
        )
      )
    })

    ## Download :: Generate property report ----
    output$generate_report <- downloadHandler(
      filename = function() {
        paste0(input$property, "_report_", Sys.Date(), ".docx")
      },
      content = function(file) {
        req(selected_record())

        shinyalert(
          title = "Generating Report",
          text = "Report generation in progress. The download will begin shortly.",
          type = "info",
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE,
          timer = 5000
        )

        render_property_report(selected_record(), input$property, file)
      }
    )

    ## Event :: Log internal communication ----
    observeEvent(input$log_internal, {
      if (!isTruthy(input$property)) {
        shinyalert(
          title = "Missing Property Name",
          text = "Please select a property before logging a communication.",
          type = "warning",
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE
        )
        return()
      }

      req(input$comm_date, input$communication_description)

      property_id <- dbGetQuery(
        db_con,
        glue_sql(
          "SELECT id FROM properties WHERE property_name = {input$property};",
          .con = db_con
        )
      ) |>
        pull(id)

      df <- tibble(
        property_id = property_id,
        date = as.character(input$comm_date),
        communication_description = input$communication_description
      )

      append_db_data(
        db_table_name = "internal_communications",
        data = df,
        con = db_con,
        silent = TRUE
      )

      if (!is.null(db_updated)) {
        db_updated(db_updated() + 1)
      }

      shinyalert(
        title = "Success",
        text = str_glue(
          "Internal communication logged successfully for {input$property}"
        ),
        type = "success",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        timer = 10000
      )

      updateTextAreaInput(session, "communication_description", value = "")
      updateDateInput(session, "comm_date", value = Sys.Date())
    })

    ## Event :: Log property contact communication ----
    observeEvent(input$log_contact_comm, {
      if (!isTruthy(input$property)) {
        shinyalert(
          title = "Missing Property Name",
          text = "Please select a property before logging a communication.",
          type = "warning",
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE
        )
        return()
      }

      req(
        input$contact_comm_contact_id,
        input$contact_comm_purpose_id,
        input$contact_comm_method_id,
        input$contact_comm_date,
        input$contact_comm_description
      )

      property_id <- dbGetQuery(
        db_con,
        glue_sql(
          "SELECT id FROM properties WHERE property_name = {input$property};",
          .con = db_con
        )
      ) |>
        pull(id)

      df <- tibble(
        property_contact_id = input$contact_comm_contact_id,
        property_id = property_id,
        communication_purpose_id = input$contact_comm_purpose_id,
        communication_method_id = input$contact_comm_method_id,
        date_contacted = input$contact_comm_date,
        communication_description = input$contact_comm_description,
        date_follow_up = if (isTruthy(input$contact_comm_follow_up)) {
          input$contact_comm_follow_up
        } else {
          as.Date(NA)
        }
      )

      append_db_data(
        db_table_name = "property_contact_communication",
        data = df,
        con = db_con,
        silent = TRUE
      )

      if (!is.null(db_updated)) {
        db_updated(db_updated() + 1)
      }

      shinyalert(
        title = "Success",
        text = str_glue(
          "Property contact communication logged successfully for {input$property}"
        ),
        type = "success",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        timer = 10000
      )

      updateSelectizeInput(
        session,
        "contact_comm_contact_id",
        selected = character(0)
      )
      updateSelectizeInput(
        session,
        "contact_comm_purpose_id",
        selected = character(0)
      )
      updateSelectizeInput(
        session,
        "contact_comm_method_id",
        selected = character(0)
      )
      updateTextAreaInput(session, "contact_comm_description", value = "")
      updateDateInput(session, "contact_comm_date", value = Sys.Date())
      updateDateInput(session, "contact_comm_follow_up", value = as.Date(NA))
    })

    ## Event :: Log action item ----
    observeEvent(input$log_action, {
      if (!isTruthy(input$property)) {
        shinyalert(
          title = "Missing Property Name",
          text = "Please select a property before logging an action item.",
          type = "warning",
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE
        )
        return()
      }

      req(input$team_lead, input$action_item_description)

      property_id <- dbGetQuery(
        db_con,
        glue_sql(
          "SELECT id FROM properties WHERE property_name = {input$property};",
          .con = db_con
        )
      ) |>
        pull(id)

      df <- tibble(
        property_id = property_id,
        team_lead_id = as.integer(input$team_lead),
        action_item_description = input$action_item_description,
        due_date = if_else(
          is.null(input$due_date),
          NA_character_,
          as.character(input$due_date)
        )
      )

      append_db_data(
        db_table_name = "team_lead_actions",
        data = df,
        con = db_con,
        silent = TRUE
      )

      if (!is.null(db_updated)) {
        db_updated(db_updated() + 1)
      }

      shinyalert(
        title = "Success",
        text = str_glue(
          "Action item logged successfully for {input$property}"
        ),
        type = "success",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        timer = 10000
      )

      updateSelectizeInput(
        session,
        inputId = "team_lead",
        choices = c(
          "",
          setNames(team_lead_choices()$id, team_lead_choices()$team_value)
        ),
        selected = character(0),
        server = TRUE
      )

      updateTextAreaInput(session, "action_item_description", value = "")
      updateDateInput(session, "due_date", value = NA)
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

      updateSelectizeInput(
        session,
        inputId = "team_lead",
        choices = c(
          "",
          setNames(team_lead_choices()$id, team_lead_choices()$team_value)
        ),
        selected = character(0),
        server = TRUE
      )

      updateTextAreaInput(session, "communication_description", value = "")
      updateTextAreaInput(session, "action_item_description", value = "")
      updateDateInput(session, "comm_date", value = Sys.Date())
      updateDateInput(session, "contact_comm_date", value = Sys.Date())
      updateDateInput(session, "due_date", value = NA)
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
