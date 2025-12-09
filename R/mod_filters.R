# R/mod_filters.R ----------------------------------------------------
mod_filters_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h4("Filtros"),
    # div(
    #   class = "mb-3",
    #   span(
    #     class = "farm-chip",
    #     span(class = "dot"),
    #     "Filtros em tempo real"
    #   )
    # ),
    pickerInput(
      inputId = ns("season"),
      label   = "Season",
      choices = NULL,
      multiple = TRUE,
      options = pickerOptions(
        actionsBox = TRUE,
        liveSearch = TRUE,
        size = 8
      )
    ),
    pickerInput(
      inputId = ns("coo"),
      label   = "Country of Origin (COO)",
      choices = NULL,
      multiple = TRUE,
      options = pickerOptions(
        actionsBox = TRUE,
        liveSearch = TRUE,
        size = 8
      )
    ),
    pickerInput(
      inputId = ns("supplier"),
      label   = "Supplier",
      choices = NULL,
      multiple = TRUE,
      options = pickerOptions(
        actionsBox = TRUE,
        liveSearch = TRUE,
        size = 8
      )
    ),
    pickerInput(
      inputId = ns("category"),
      label   = "Category",
      choices = NULL,
      multiple = TRUE,
      options = pickerOptions(
        actionsBox = TRUE,
        liveSearch = TRUE,
        size = 8
      )
    ),
    pickerInput(
      inputId = ns("squad"),
      label   = "Squad",
      choices = NULL,
      multiple = TRUE,
      options = pickerOptions(
        actionsBox = TRUE,
        liveSearch = TRUE,
        size = 8
      )
    ),
    pickerInput(
      inputId = ns("on_floor_month"),
      label   = "On Floor Month",
      choices = NULL,
      multiple = TRUE,
      options = pickerOptions(
        actionsBox = TRUE,
        liveSearch = TRUE,
        size = 8
      )
    ),
    hr(),
    actionButton(
      ns("reset"),
      label = "Limpar filtros",
      icon = icon("broom"),
      class = "btn-sm btn-outline-primary"
    )
  )
}

mod_filters_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    # Inicializa choices com o dataset completo
    observe({
      updatePickerInput(
        session, "season",
        choices = sort(unique(data$season)),
        selected = character(0)
      )
      updatePickerInput(
        session, "coo",
        choices = sort(unique(data$coo)),
        selected = character(0)
      )
      updatePickerInput(
        session, "supplier",
        choices = sort(unique(data$final_supplier_production)),
        selected = character(0)
      )
      updatePickerInput(
        session, "category",
        choices = sort(unique(data$category)),
        selected = character(0)
      )
      updatePickerInput(
        session, "squad",
        choices = sort(unique(data$squad)),
        selected = character(0)
      )
      updatePickerInput(
        session, "on_floor_month",
        choices = sort(unique(data$on_floor_month)),
        selected = character(0)
      )
    })

    # Reactive com dados filtrados
    dados_filtrados <- reactive({
      df <- data

      if (!is.null(input$season) && length(input$season) > 0) {
        df <- df %>% dplyr::filter(season %in% input$season)
      }
      if (!is.null(input$coo) && length(input$coo) > 0) {
        df <- df %>% dplyr::filter(coo %in% input$coo)
      }
      if (!is.null(input$supplier) && length(input$supplier) > 0) {
        df <- df %>% dplyr::filter(final_supplier_production %in% input$supplier)
      }
      if (!is.null(input$category) && length(input$category) > 0) {
        df <- df %>% dplyr::filter(category %in% input$category)
      }
      if (!is.null(input$squad) && length(input$squad) > 0) {
        df <- df %>% dplyr::filter(squad %in% input$squad)
      }
      if (!is.null(input$on_floor_month) && length(input$on_floor_month) > 0) {
        df <- df %>% dplyr::filter(on_floor_month %in% input$on_floor_month)
      }

      df
    })

    # # Filtros reagindo entre si
    # observeEvent(dados_filtrados(), {
    #   df <- dados_filtrados()
    # 
    #   updatePickerInput(
    #     session, "season",
    #     choices  = sort(unique(df$season)),
    #     selected = intersect(input$season, unique(df$season))
    #   )
    #   updatePickerInput(
    #     session, "coo",
    #     choices  = sort(unique(df$coo)),
    #     selected = intersect(input$coo, unique(df$coo))
    #   )
    #   updatePickerInput(
    #     session, "supplier",
    #     choices  = sort(unique(df$final_supplier_production)),
    #     selected = intersect(input$supplier, unique(df$final_supplier_production))
    #   )
    #   updatePickerInput(
    #     session, "category",
    #     choices  = sort(unique(df$category)),
    #     selected = intersect(input$category, unique(df$category))
    #   )
    #   updatePickerInput(
    #     session, "squad",
    #     choices  = sort(unique(df$squad)),
    #     selected = intersect(input$squad, unique(df$squad))
    #   )
    #   updatePickerInput(
    #     session, "on_floor_month",
    #     choices  = sort(unique(df$on_floor_month)),
    #     selected = intersect(input$on_floor_month, unique(df$on_floor_month))
    #   )
    # }, ignoreInit = TRUE)

    # Reset
    observeEvent(input$reset, {
      updatePickerInput(session, "season", selected = character(0))
      updatePickerInput(session, "coo", selected = character(0))
      updatePickerInput(session, "supplier", selected = character(0))
      updatePickerInput(session, "category", selected = character(0))
      updatePickerInput(session, "squad", selected = character(0))
      updatePickerInput(session, "on_floor_month", selected = character(0))

      updatePickerInput(session, "season",   choices = sort(unique(data$season)))
      updatePickerInput(session, "coo",      choices = sort(unique(data$coo)))
      updatePickerInput(session, "supplier", choices = sort(unique(data$final_supplier_production)))
      updatePickerInput(session, "category", choices = sort(unique(data$category)))
      updatePickerInput(session, "squad",    choices = sort(unique(data$squad)))
      updatePickerInput(session, "on_floor_month", choices = sort(unique(data$on_floor_month)))
    })

    return(dados_filtrados)
  })
}
