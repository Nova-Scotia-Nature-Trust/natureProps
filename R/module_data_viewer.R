# Shared choices lists (used by both UI and server for labels) ----
choices_outreach <- list(
  "Select a view from the list" = "",
  "PIDs" = "pid_view",
  "Property Contact Details" = "property_contact_details_view",
  "Communication History" = "communication_data_view",
  "Outreach" = "outreach_view",
  "Land & Securement History" = "land_secure_comms",
  "Property Descriptions" = "property_descriptions",
  "Landowner & Address" = "landowner_address"
)

choices_securement <- list(
  "Select a view from the list" = "",
  "Secured Property Details" = "secured_props_view",
  "Action Items (long)" = "action_items_view",
  "Action Items (wide)" = "action_items_view_wide",
  "Appraisals" = "appraisals",
  "Property Sizes" = "property_sizes",
  "Insurance View" = "insurance",
  "LLT Projects" = "llt_projects",
  "Securement Communication" = "securement_communication",
  "Property Contact Details" = "property_contact_details_view",
  "Property Pricing" = "property_pricing"
)

choices_action_item <- list(
  "Action Items (long)" = "action_items_view",
  "Action Items (wide)" = "action_items_view_wide"
)

choices_cons_lands <- list(
  "Conservation Lands" = "cons_lands_view_grouped",
  "Conservation Lands (PIDs)" = "cons_lands_view"
)

# Build a flat key -> label lookup for download filenames
all_choices <- c(
  choices_outreach,
  choices_securement,
  choices_action_item,
  choices_cons_lands
)
view_labels <- setNames(names(all_choices), unlist(all_choices))

# UI ----
module_data_viewer_ui <- function(id, panel_id) {
  ns <- NS(id)

  choices_list <- switch(
    panel_id,
    "outreach_panel" = choices_outreach,
    "securement_panel" = choices_securement,
    "action_item_panel" = choices_action_item,
    "cons_lands_panel" = choices_cons_lands
  )

  ## Card :: Data viewer ----
  nav_panel(
    title = NULL,
    card(
      full_screen = TRUE,
      height = "100%",
      card_header(
        class = "d-flex justify-content-between align-items-center",
        div(
          selectInput(
            inputId = ns("data_view"),
            label = NULL,
            choices = choices_list,
            selected = ifelse(
              panel_id == "action_item_panel",
              "Action Items",
              ""
            ),
            width = "250px"
          ),
          if (panel_id == "outreach_panel") {
            div(
              style = "margin-top: 0.5rem;",
              input_switch(
                id = ns("filter_toggle"),
                label = "Filter by query results",
                value = FALSE
              )
            )
          }
        ),
        downloadButton(
          outputId = ns("download_data"),
          label = "Download",
          class = "btn-sm"
        )
      ),
      card_body(
        style = "padding: 0.5rem 1rem;",
        min_height = "300px",
        DTOutput(outputId = ns("view_df"), height = "100%")
      )
    )
  )
}

# Server ----
module_data_viewer_server <- function(
  id,
  db_con,
  db_updated = NULL,
  prop_filter = NULL,
  focal_pid_rv = NULL,
  panel_id = NULL,
  cons_lands_data = NULL
) {
  moduleServer(id, function(input, output, session) {
    ## View dispatch table ----
    # Each entry: list(fetch = <fn(db_con)>, order_col = <int>, order_dir = <chr>)
    view_config <- list(
      pid_view = list(
        fetch = function(db_con) dbGetQuery(db_con, "SELECT * FROM view_pid;"),
        order_col = 1,
        order_dir = "asc"
      ),
      property_contact_details_view = list(
        fetch = function(db_con) {
          dbGetQuery(db_con, "SELECT * FROM view_property_contacts;")
        },
        order_col = 3,
        order_dir = "asc"
      ),
      communication_data_view = list(
        fetch = function(db_con) {
          dbGetQuery(db_con, "SELECT * FROM view_communication_history;")
        },
        order_col = 1,
        order_dir = "asc"
      ),
      outreach_view = list(
        fetch = function(db_con) {
          dbGetQuery(db_con, "SELECT * FROM view_outreach;")
        },
        order_col = 4,
        order_dir = "desc"
      ),
      land_secure_comms = list(
        fetch = function(db_con) {
          dbGetQuery(db_con, "SELECT * FROM view_historical_communications;")
        },
        order_col = 1,
        order_dir = "asc"
      ),
      property_descriptions = list(
        fetch = function(db_con) {
          dbGetQuery(db_con, "SELECT * FROM view_property_descriptions;")
        },
        order_col = 0,
        order_dir = "asc"
      ),
      landowner_address = list(
        fetch = function(db_con) prep_view_landowner_address(db_con),
        order_col = 0,
        order_dir = "asc"
      ),
      action_items_view = list(
        fetch = function(db_con) {
          dbGetQuery(db_con, "SELECT * FROM view_securement_action_items;") |>
            select(-"Property Name")
        },
        order_col = 0,
        order_dir = "asc"
      ),
      action_items_view_wide = list(
        fetch = function(db_con) {
          dbGetQuery(db_con, "SELECT * FROM view_securement_action_items;") |>
            select("Property Name Public", "Action Item", "Status") |>
            pivot_wider(
              id_cols = "Property Name Public",
              names_from = "Action Item",
              values_from = "Status"
            )
        },
        order_col = 0,
        order_dir = "asc"
      ),
      secured_props_view = list(
        fetch = function(db_con) prep_view_secured_properties(db_con, gis_con),
        order_col = 0,
        order_dir = "asc"
      ),
      appraisals = list(
        fetch = function(db_con) {
          dbGetQuery(db_con, "SELECT * FROM view_appraisals;")
        },
        order_col = 0,
        order_dir = "asc"
      ),
      property_sizes = list(
        fetch = function(db_con) {
          dbGetQuery(db_con, "SELECT * FROM view_property_sizes;")
        },
        order_col = 0,
        order_dir = "asc"
      ),
      insurance = list(
        fetch = function(db_con) {
          dbGetQuery(db_con, "SELECT * FROM view_insurance;")
        },
        order_col = 0,
        order_dir = "asc"
      ),
      llt_projects = list(
        fetch = function(db_con) {
          dbGetQuery(db_con, "SELECT * FROM view_llt_projects;")
        },
        order_col = 3,
        order_dir = "asc"
      ),
      securement_communication = list(
        fetch = function(db_con) {
          dbGetQuery(
            db_con,
            "SELECT * FROM view_securement_communication_history;"
          )
        },
        order_col = 5,
        order_dir = "desc"
      ),
      property_pricing = list(
        fetch = function(db_con) {
          dbGetQuery(db_con, "SELECT * FROM view_property_pricing;")
        },
        order_col = 0,
        order_dir = "asc"
      ),
      cons_lands_view_grouped = list(
        fetch = function(db_con) {
          prep_view_cons_lands(cons_lands_data(), "grouped")
        },
        order_col = 0,
        order_dir = "asc"
      ),
      cons_lands_view = list(
        fetch = function(db_con) {
          prep_view_cons_lands(cons_lands_data(), "ungrouped")
        },
        order_col = 0,
        order_dir = "asc"
      )
    )

    ## Combined reactive: data + sort order ----
    combined_rv <- reactive({
      if (!is.null(db_updated)) {
        db_updated()
      }

      selected_view <- input$data_view
      apply_filter <- isTRUE(input$filter_toggle)

      if (selected_view == "") {
        return(list(data = NULL, order = list(list(0, "asc"))))
      }

      cfg <- view_config[[selected_view]]

      # Conservation lands views require data to be available
      if (selected_view %in% c("cons_lands_view_grouped", "cons_lands_view")) {
        req(cons_lands_data())
      }

      data <- cfg$fetch(db_con)
      order <- list(list(cfg$order_col, cfg$order_dir))

      # Apply property filter (securement / action items)
      if (!is.null(prop_filter) && !is.null(prop_filter())) {
        data <- data |> filter(`Property Name Public` == prop_filter())
      }

      # Apply PID filter (outreach views)
      if (apply_filter) {
        pid_vals <- if (!is.null(focal_pid_rv)) focal_pid_rv() else NULL

        if (
          selected_view %in%
            c(
              "pid_view",
              "outreach_view",
              "land_secure_comms",
              "landowner_address"
            )
        ) {
          data <- if (!is.null(pid_vals)) {
            data |> filter(PID %in% pid_vals)
          } else {
            data |> filter(FALSE)
          }
        } else if (
          selected_view %in%
            c("communication_data_view", "property_descriptions")
        ) {
          data <- if (!is.null(pid_vals)) {
            data |> filter(str_detect(PIDs, str_c(pid_vals, collapse = "|")))
          } else {
            data |> filter(FALSE)
          }
        } else if (
          selected_view == "property_contact_details_view" &&
            panel_id == "outreach_panel"
        ) {
          data <- if (!is.null(pid_vals)) {
            data |>
              filter(str_detect(
                `Property Contact PIDs`,
                str_c(pid_vals, collapse = "|")
              ))
          } else {
            data |> filter(FALSE)
          }
        }
      }

      list(data = data, order = order)
    })

    ## Render datatable ----
    output$view_df <- renderDT({
      rv <- combined_rv()

      if (
        input$data_view %in%
          c("cons_lands_view_grouped", "cons_lands_view") &&
          is.null(cons_lands_data())
      ) {
        return(datatable(
          data.frame(Status = "Loading conservation lands data..."),
          options = list(dom = "t"),
          rownames = FALSE
        ))
      }

      if (is.null(rv$data) || nrow(rv$data) == 0) {
        return(datatable(data.frame()))
      }

      # Convert character columns to factors to enable column filter dropdowns
      data_for_display <- rv$data |>
        mutate(across(where(is.character), as.factor))

      datatable(
        data_for_display,
        escape = FALSE,
        options = list(
          pageLength = 50,
          lengthMenu = list(
            c(10, 25, 50, 100, -1),
            c("10", "25", "50", "100", "All")
          ),
          scrollX = TRUE,
          fixedHeader = TRUE,
          order = rv$order,
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
    })

    ## Download Data View ----
    output$download_data <- downloadHandler(
      filename = function() {
        view_key <- input$data_view
        label <- if (view_key != "" && view_key %in% names(view_labels)) {
          view_labels[[view_key]]
        } else {
          "data"
        }
        # Replace spaces with underscores for safe filenames
        label <- gsub(" ", "_", label)
        glue("{label}_{format(Sys.Date(), '%Y%m%d')}.csv")
      },
      content = function(file) {
        data_to_download <- combined_rv()$data
        req(data_to_download, nrow(data_to_download) > 0)
        write_csv(data_to_download, file)
      }
    )
  })
}
