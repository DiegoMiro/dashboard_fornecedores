
# R/mod_markup.R ----------------------------------------------
mod_markup_ui <- function(id) {
  ns <- NS(id)
  
  card(
    full_screen = TRUE,
    card_header(
      "Tabela de Fornecedores",
      tags$small("Resumo de desempenho por fornecedor dentro dos filtros atuais")
    ),
    card_body(
      reactableOutput(ns("tbl_markup"))
    )
  )
}

mod_markup_server <- function(id, dados_filtrados) {
  moduleServer(id, function(input, output, session) {
    
    output$tbl_markup <- renderReactable({
      df <- dados_filtrados()
      req(nrow(df) > 0)
      
      tab <- df %>%
        distinct(season_id, season) %>%
        mutate(season_factor = fct(season)) %>%
        select(season_id, season_factor) %>%
        left_join(
          df %>%
            select(-season_factor)
        ) %>%
        group_by(
          season = season_factor,
          supplier = final_supplier_production,
          fabric = final_fabric
        ) %>%
        summarise(
          n_styles        = n_distinct(style_number),
          total_verba     = sum(total_verba, na.rm = TRUE),
          total_custo     = sum(total_custo, na.rm = TRUE),
          markup          = ifelse(total_custo > 0, total_verba / total_custo, NA_real_),
          .groups = "drop"
        ) %>%
        arrange(desc(total_custo))
      
      reactable(
        tab,
        sortable = TRUE,
        searchable = TRUE,
        resizable = TRUE,
        defaultPageSize = 15,
        borderless = TRUE,
        highlight = TRUE,
        striped = TRUE,
        defaultColDef = colDef(
          headerClass = "header",
          class = "cell",
          align = "center"
        ),
        columns = list(
          final_supplier_production = colDef(
            name = "Supplier",
            minWidth = 180,
            align = "left"
          ),
          coo = colDef(name = "COO", minWidth = 60),
          n_styles = colDef(
            name = "# Styles",
            cell = reactablefmtr::data_bars(
              tab,
              fill_color    = "#FEC85D",
              text_position = "outside-end",
              number_fmt = function(x) formatC(x, big.mark = ".", format = "f", digits = 0)
            )
          ),
          total_custo = colDef(
            name = "Total Custo (US$)",
            format = colFormat(
              prefix   = "$",
              separators = TRUE,
              # big.mark = ",",
              digits   = 0
            )
          ),
          total_verba = colDef(
            name = "Total Verba (US$)",
            format = colFormat(
              prefix   = "$",
              separators = TRUE,
              # big.mark = ",",
              digits   = 0
            )
          ),
          total_retail_us = colDef(
            name = "Total Retail US (US$)",
            format = colFormat(
              prefix   = "$",
              separators = TRUE,
              # big.mark = ",",
              digits   = 0
            )
          ),
          markup_us = colDef(
            name = "Markup US (x)",
            format = colFormat(digits = 2),
            cell = reactablefmtr::color_tiles(
              tab,
              colors = c("#FFE6D6", "#9A0405"),
              bias    = 2
            )
          )
        )
      )
    })
  })
}
