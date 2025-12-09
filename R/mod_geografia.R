
# R/mod_geografia.R --------------------------------------------------
mod_geografia_ui <- function(id) {
  ns <- NS(id)

  layout_columns(
    col_widths = c(6, 6),
    card(
      full_screen = TRUE,
      card_header(
        "Custo total por COO",
        tags$small("Quais origens concentram mais custo?")
      ),
      card_body(
        highchartOutput(ns("hc_coo_custo"), height = "380px")
      )
    ),
    card(
      full_screen = TRUE,
      card_header(
        "Verba total por COO",
        tags$small("Distribuição da verba por país de origem")
      ),
      card_body(
        highchartOutput(ns("hc_coo_verba"), height = "380px")
      )
    )
  )
}

mod_geografia_server <- function(id, dados_filtrados) {
  moduleServer(id, function(input, output, session) {

    output$hc_coo_custo <- renderHighchart({
      df <- dados_filtrados()
      req(nrow(df) > 0)

      df_agg <- df %>%
        group_by(coo) %>%
        summarise(
          total_custo = sum(total_custo, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(total_custo))

      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_xAxis(categories = df_agg$coo, title = list(text = NULL)) %>%
        hc_yAxis(title = list(text = "Total Custo (US$)")) %>%
        hc_add_series(
          name  = "Total Custo",
          data  = round(df_agg$total_custo, 2),
          color = "#9A0405"
        ) %>%
        hc_plotOptions(bar = list(borderRadius = 4)) %>%
        hc_tooltip(valueDecimals = 0, valuePrefix = "$")
    })

    output$hc_coo_verba <- renderHighchart({
      df <- dados_filtrados()
      req(nrow(df) > 0)

      df_agg <- df %>%
        group_by(coo) %>%
        summarise(
          total_verba = sum(total_verba, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(total_verba))

      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_xAxis(categories = df_agg$coo, title = list(text = NULL)) %>%
        hc_yAxis(title = list(text = "Total Verba (US$)")) %>%
        hc_add_series(
          name  = "Total Verba",
          data  = round(df_agg$total_verba, 2),
          color = "#1EAD74"
        ) %>%
        hc_plotOptions(bar = list(borderRadius = 4)) %>%
        hc_tooltip(valueDecimals = 0, valuePrefix = "$")
    })
  })
}
