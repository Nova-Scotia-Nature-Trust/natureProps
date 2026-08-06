# UI ----
module_edit_funding_ui <- function(id) {
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
            multiple = FALSE,
            options = list(
              create = FALSE,
              placeholder = "Search or select property"
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
              h5("Edit Property Funding")
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
module_edit_funding_server <- function(id, db_con, db_updated = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Reactive :: Property choices ----
    property_choices <- reactive({
      choices <- dbGetQuery(
        db_con,
        "SELECT pr.id, 
              pr.property_name,
              pr.property_name_public
        FROM properties pr 
        JOIN ownership ow ON pr.ownership_id = ow.id
        WHERE ownership_value IS NOT NULL
        ORDER BY property_name_public;"
      )

      name_dupes <- choices |>
        get_dupes(property_name_public) |>
        distinct(property_name_public) |>
        pull()

      choices |>
        mutate(
          property_name_view = if_else(
            property_name_public %in% name_dupes,
            str_glue("{property_name_public} || {property_name}"),
            property_name_public
          )
        )
    })

    ## Reactive :: Federal funding choices ----
    fund_federal_choices <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, federal_value FROM fund_federal ORDER BY federal_value;"
      )
    })

    ## Reactive :: Campaign choices ----
    campaign_choices <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, campaign_value FROM campaign ORDER BY campaign_value;"
      )
    })

    ## Update property dropdown ----
    observe({
      updateSelectizeInput(
        session,
        inputId = "property_name",
        choices = c(
          "",
          setNames(
            property_choices()$id,
            property_choices()$property_name_view
          )
        ),
        selected = "",
        server = TRUE
      )
    })

    ## Reactive value :: Selected record ----
    selected_record <- reactiveVal(tibble(
      id = NA_integer_,
      property_name = "",
      llt_funding_secured = FALSE,
      fund_federal_ids = list(integer(0)),
      campaign_id = NA_integer_
    ))

    ## Event :: Load record ----
    observeEvent(input$property_name, {
      property_id <- input$property_name

      if (!isTruthy(property_id)) {
        selected_record(NULL)
        return()
      }

      # Get property details
      property_query <- glue_sql(
        "SELECT 
          id,
          property_name,
          llt_funding_secured,
          campaign_id
        FROM properties 
        WHERE id = {property_id}",
        .con = db_con
      )

      property_record <- dbGetQuery(db_con, property_query)

      # Get associated federal funds from junction table
      funds_query <- glue_sql(
        "SELECT fund_federal_id 
        FROM property_fund_federal 
        WHERE property_id = {property_id}",
        .con = db_con
      )

      funds_record <- dbGetQuery(db_con, funds_query)

      if (nrow(property_record) == 1) {
        property_record$fund_federal_ids <- list(funds_record$fund_federal_id)
        selected_record(property_record)
      }
    })

    ## Create UI for database fields ----
    output$edit_fields_ui <- renderUI({
      record <- selected_record()

      # Return empty if no record selected
      if (is.null(record) || is.na(record$id)) {
        return(NULL)
      }

      property_name_text <- if (isTruthy(record$property_name)) {
        paste0("Editing: ", record$property_name)
      } else {
        "No property selected"
      }

      tagList(
        h6(
          class = "text-muted",
          property_name_text
        ),

        layout_columns(
          col_widths = c(6, 6),
          selectizeInput(
            inputId = ns("edit_fund_federal_ids"),
            label = "Federal Funding",
            choices = setNames(
              fund_federal_choices()$id,
              fund_federal_choices()$federal_value
            ),
            selected = if (length(record$fund_federal_ids[[1]]) > 0) {
              record$fund_federal_ids[[1]]
            } else {
              NULL
            },
            multiple = TRUE,
            options = list(
              create = FALSE,
              placeholder = "Select one or more federal funds"
            )
          ),
          selectizeInput(
            inputId = ns("edit_campaign_id"),
            label = "Campaign",
            choices = c(
              "",
              setNames(
                campaign_choices()$id,
                campaign_choices()$campaign_value
              )
            ),
            selected = if (!is.na(record$campaign_id)) {
              record$campaign_id
            } else {
              ""
            },
            multiple = FALSE,
            options = list(
              create = FALSE,
              placeholder = "Select campaign"
            )
          ),
          checkboxInput(
            inputId = ns("edit_llt_funding_secured"),
            label = "LLT Funding Secured",
            value = if (!is.na(record$llt_funding_secured)) {
              record$llt_funding_secured
            } else {
              FALSE
            }
          )
        )
      )
    })

    ## Event :: Write changes ----
    observeEvent(input$submit_edit, {
      req(input$property_name)

      property_id <- as.integer(input$property_name)

      # Update properties table (campaign and llt funding)
      property_update <- tibble(
        id = property_id,
        llt_funding_secured = as.logical(input$edit_llt_funding_secured),
        campaign_id = if (isTruthy(input$edit_campaign_id)) {
          as.integer(input$edit_campaign_id)
        } else {
          NA_integer_
        }
      )

      dbx::dbxUpdate(
        db_con,
        table = "properties",
        records = property_update,
        where_cols = "id"
      )

      # --- Replace federal fund associations ---
      selected_funds <- if (!is.null(input$edit_fund_federal_ids)) {
        as.integer(input$edit_fund_federal_ids)
      } else {
        integer(0)
      }

      # Remove all current associations for this property
      dbExecute(
        db_con,
        glue_sql(
          "DELETE FROM property_fund_federal
           WHERE property_id = {property_id}",
          .con = db_con
        )
      )

      # Insert new associations if any
      if (length(selected_funds) > 0) {
        dbx::dbxInsert(
          db_con,
          table = "property_fund_federal",
          records = tibble(
            property_id = property_id,
            fund_federal_id = selected_funds
          )
        )
      }

      update_property_timestamp(con = db_con, property_id = property_id)

      # Signal update
      if (!is.null(db_updated)) {
        db_updated(db_updated() + 1)
      }

      shinyalert(
        title = "Success",
        text = str_glue(
          "Funding details for {selected_record()$property_name} have been successfully updated"
        ),
        type = "success",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        timer = 10000
      )
    })

    ## Event :: Clear inputs ----
    observeEvent(input$clear_edit, {
      # Reset to empty template
      selected_record(tibble(
        id = NA_integer_,
        property_name = "",
        llt_funding_secured = FALSE,
        fund_federal_ids = list(integer(0)),
        campaign_id = NA_integer_
      ))

      # Clear the sidebar filter
      updateSelectizeInput(
        session,
        inputId = "property_name",
        selected = character(0),
        choices = c(
          "",
          setNames(
            property_choices()$id,
            property_choices()$property_name_view
          )
        ),
        server = TRUE
      )
    })
  })
}
