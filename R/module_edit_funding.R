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
            label = "Property Name",
            choices = NULL,
            selected = NULL,
            multiple = FALSE,
            options = list(
              create = FALSE,
              placeholder = "Search or select property"
            )
          ),
          actionButton(
            inputId = ns("load_record"),
            label = "Load Property",
            class = "btn-success"
          ),
          hr(),
          actionButton(
            inputId = ns("submit_edit"),
            label = "Submit Changes",
            class = "btn-primary"
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
      dbGetQuery(
        db_con,
        "SELECT id, property_name FROM properties ORDER BY property_name;"
      ) |>
        select(property_name, id) |>
        deframe()
    })

    ## Reactive :: Federal funding choices ----
    fund_federal_choices <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, federal_value FROM fund_federal ORDER BY federal_value;"
      ) |>
        select(federal_value, id) |>
        deframe()
    })

    ## Reactive :: Campaign choices ----
    campaign_choices <- reactive({
      dbGetQuery(
        db_con,
        "SELECT id, campaign_value FROM campaign ORDER BY campaign_value;"
      ) |>
        select(campaign_value, id) |>
        deframe()
    })

    ## Update property dropdown ----
    observe({
      updateSelectizeInput(
        session,
        inputId = "property_name",
        choices = c("", property_choices()),
        selected = "",
        server = TRUE
      )
    })

    ## Reactive value :: Selected record ----
    # Initialize with default empty values
    selected_record <- reactiveVal(tibble(
      id = NA_integer_,
      property_name = "",
      llt_funding_secured = FALSE,
      fund_federal_id = NA_integer_,
      campaign_id = NA_integer_
    ))

    ## Event :: Load record ----
    observeEvent(input$load_record, {
      req(input$property_name)

      property_id <- input$property_name

      query <- glue_sql(
        "SELECT 
          id,
          property_name,
          llt_funding_secured,
          fund_federal_id,
          campaign_id
        FROM properties 
        WHERE id = {property_id}",
        .con = db_con
      )

      record <- dbGetQuery(db_con, query)

      if (nrow(record) == 1) {
        selected_record(record)
      }
    })

    ## Create UI for database fields ----
    output$edit_fields_ui <- renderUI({
      record <- selected_record()

      # Extract values if record exists, otherwise NULL
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
            inputId = ns("edit_fund_federal_id"),
            label = "Federal Funding",
            choices = c("", fund_federal_choices()),
            selected = if (!is.na(record$fund_federal_id)) {
              record$fund_federal_id
            } else {
              ""
            },
            multiple = FALSE,
            options = list(
              create = FALSE,
              placeholder = "Select federal funding"
            )
          ),
          selectizeInput(
            inputId = ns("edit_campaign_id"),
            label = "Campaign",
            choices = c("", campaign_choices()),
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

      db_id <- as.integer(input$property_name)

      # Build update tibble
      update_tibble <- tibble(
        id = db_id,
        llt_funding_secured = as.logical(input$edit_llt_funding_secured),
        fund_federal_id = if (
          is.null(input$edit_fund_federal_id) ||
            input$edit_fund_federal_id == ""
        ) {
          NA_integer_
        } else {
          as.integer(input$edit_fund_federal_id)
        },
        campaign_id = if (
          is.null(input$edit_campaign_id) ||
            input$edit_campaign_id == ""
        ) {
          NA_integer_
        } else {
          as.integer(input$edit_campaign_id)
        }
      )

      # Update the record
      dbx::dbxUpdate(
        db_con,
        table = "properties",
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
      selected_record(tibble(
        id = NA_integer_,
        property_name = "",
        llt_funding_secured = FALSE,
        fund_federal_id = NA_integer_,
        campaign_id = NA_integer_
      ))

      # Clear the sidebar filter
      updateSelectizeInput(
        session,
        inputId = "property_name",
        selected = "",
        choices = c("", property_choices()),
        server = TRUE
      )
    })
  })
}
