# UI ----
module_pol_viewer_ui <- function(id) {
  ns <- NS(id)

  div(
    selectizeInput(
      ns("property"),
      "Select Property",
      choices = NULL,
      multiple = FALSE,
      width = "100%"
    ),
    selectizeInput(
      ns("parcel_pid"),
      "Select PID",
      choices = NULL,
      multiple = FALSE,
      width = "100%"
    ),
    div(style = "margin-top: 10px;"),
    actionButton(
      inputId = ns("open_pol_map"),
      label = "View POL Map",
      class = "btn-primary",
      icon = icon("map"),
      width = "100%"
    ),
    div(style = "margin-top: 8px;"),
    actionButton(
      inputId = ns("open_pol_detail"),
      label = "View POL Details",
      class = "btn-primary",
      icon = icon("file-lines"),
      width = "100%"
    ),
    div(style = "margin-top: 8px;"),
    actionButton(
      inputId = ns("clear_inputs"),
      label = "Clear Inputs",
      class = "btn-secondary",
      width = "100%"
    )
  )
}

# Server ----
module_pol_viewer_server <- function(id, db_con, db_updated = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Reactive :: Property choices ----
    property_choices <- reactive({
      if (!is.null(db_updated)) {
        db_updated()
      }

      dbGetQuery(db_con, glue("SELECT property_name FROM properties;")) |>
        pull(property_name) |>
        sort()
    })

    ## Update select input with property choices
    observe({
      updateSelectizeInput(
        session,
        inputId = "property",
        choices = c("", property_choices()),
        selected = "",
        server = TRUE
      )
    })

    ## Event :: Update PID choices when property is selected ----
    observeEvent(input$property, {
      req(input$property)

      prop_name <- input$property

      query <- glue_sql(
        "
        SELECT par.pid 
        FROM parcels par
        LEFT JOIN properties prop ON par.property_id = prop.id
        WHERE prop.property_name = {prop_name}
        ORDER BY par.pid;
        ",
        .con = db_con
      )

      parcel_pids <- dbGetQuery(db_con, query) |>
        pull(pid) |>
        sort()

      if (length(parcel_pids) > 0) {
        updateSelectizeInput(
          session,
          inputId = "parcel_pid",
          choices = c("", parcel_pids),
          selected = parcel_pids[1],
          server = TRUE
        )
      } else {
        updateSelectizeInput(
          session,
          inputId = "parcel_pid",
          choices = c("No PIDs available" = ""),
          selected = "",
          server = TRUE
        )
      }
    })

    ## Event :: Open POL map in new browser tab ----
    observeEvent(input$open_pol_map, {
      req(input$parcel_pid, input$parcel_pid != "")

      pol_url <- glue(
        "https://pol.novascotia.ca/POL/Map/Index?pid={input$parcel_pid}"
      )
      browseURL(pol_url)
    })

    ## Event :: Open POL property detail in new browser tab ----
    observeEvent(input$open_pol_detail, {
      req(input$parcel_pid, input$parcel_pid != "")

      pol_url <- glue(
        "https://pol.novascotia.ca/POL/PropertyDetail/Index?pid={input$parcel_pid}&returnView=PropertySearch"
      )
      browseURL(pol_url)
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
        "parcel_pid",
        choices = character(0),
        selected = character(0),
        server = TRUE
      )
    })
  })
}
