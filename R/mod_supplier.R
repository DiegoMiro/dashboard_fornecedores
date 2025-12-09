
# R/mod_supplier.R --------------------------------------------------
mod_supplier_ui <- function(id) {
  ns <- NS(id)
  
  layout_columns(
    col_widths = c(12),
    card(
      full_screen = TRUE,
      card_header(
        "Supplier Performance by Season"
      ),
      card_body(
        reactableOutput(ns("rt_supplier_season"))
      )
    )
  )
}

mod_supplier_server <- function(id, dados_filtrados) {
  moduleServer(id, function(input, output, session) {
    
    output$rt_supplier_season <- renderReactable({
      df <- dados_filtrados()
      req(nrow(df) > 0)
      
      flags <- tribble(
        ~"COO", ~"FLAG",
        "BRAZIL"   , "🇧🇷",
        "CHINA"    , "🇨🇳",
        "INDIA"    , "🇮🇳",
        "ITALY"    , "🇮🇹",
        "MOROCCO"  , "🇲🇦",
        "TURKEY"   , "🇹🇷"
      )
      
      
      tbl_1 <- df %>%
        distinct(season_id, season) %>%
        arrange(season_id) %>%
        mutate(season_factor = fct(season)) %>%
        select(-season_id) %>%
        left_join(df %>% select(-season_factor)) %>%
        select(
          season_factor,
          COO = coo,
          `FINAL SUPPLIER PRODUCTION` = final_supplier_production,
          `TOTAL VERBA $` = total_verba,
          `STYLE NUMBER` = style_number
        ) %>%
        group_by(
          SEASON = season_factor,
          COO,
          SUPPLIER = `FINAL SUPPLIER PRODUCTION`
        ) %>%
        summarise(
          SKU = n_distinct(`STYLE NUMBER`),
          VERBA = sum(`TOTAL VERBA $`)
        ) %>%
        ungroup() %>%
        pivot_wider(
          id_cols = c(COO, SUPPLIER),
          names_from = SEASON,
          values_from = c(SKU, VERBA),
          values_fill = 0
        ) %>%
        arrange(COO, SUPPLIER) %>%
        left_join(flags) %>%
        select(-COO) %>%
        select(FLAG, everything())
      
      fun_table(tbl_1)
    })
    
  })
}
