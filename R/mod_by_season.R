
# R/mod_by_season.R --------------------------------------------------
mod_by_season_ui <- function(id) {
  ns <- NS(id)
  
  # layout_columns(
  #   col_widths = c(12),
  #   card(
  #     full_screen = TRUE,
  #     card_header(
  #       "By Season"
  #     ),
  #     card_body(
  #       reactableOutput(ns("rt_supplier_season"))
  #     )
  #   )
  # )
  
  navset_card_pill(
    title = "Season Table",
    nav_panel(
      title = "Supplier",
      card(
        reactableOutput(ns("rt_supplier_season"))
      )
    ),
    nav_panel(
      title = "Final Fabric",
      card(
        reactableOutput(ns("rt_final_fabric_season"))
      )
    )
  )
  
  
  
}

mod_by_season_server <- function(id, dados_filtrados) {
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
      
      # flags <- tribble(
      #   ~"COO", ~"FLAG_old",
      #   "BRAZIL"   , "🇧🇷",
      #   "CHINA"    , "🇨🇳",
      #   "INDIA"    , "🇮🇳",
      #   "ITALY"    , "🇮🇹",
      #   "MOROCCO"  , "🇲🇦",
      #   "TURKEY"   , "🇹🇷"
      # ) %>%
      #   mutate(
      #     iso2   = countrycode(COO, "country.name", "iso2c"),
      #     FLAG = vapply(iso2, iso2_to_flag, character(1))#,
      #     # 3) Coluna HTML: bandeira + país
      #     # pais_flag_html = sprintf("%s&nbsp;%s", bandeira, COO)
      #   ) %>%
      #   select(COO, FLAG)
      
      tbl_1 <- df %>%
        distinct(season_id, season) %>%
        arrange(season_id) %>%
        mutate(season_factor = fct(season)) %>%
        select(-season_id) %>%
        left_join(df %>% select(-season_factor), "season") %>%
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
    
    output$rt_final_fabric_season <- renderReactable({
      df <- dados_filtrados()
      req(nrow(df) > 0)
      
      tbl_2 <- df %>%
        distinct(season_id, season) %>%
        arrange(season_id) %>%
        mutate(season_factor = fct(season)) %>%
        select(-season_id) %>%
        left_join(df %>% select(-season_factor), "season") %>%
        select(
          season_factor,
          `FINAL FABRIC` = final_fabric,
          `TOTAL VERBA $` = total_verba,
          `STYLE NUMBER` = style_number
        ) %>%
        group_by(
          SEASON = season_factor,
          `FINAL FABRIC`
        ) %>%
        summarise(
          SKU = n_distinct(`STYLE NUMBER`),
          VERBA = sum(`TOTAL VERBA $`)
        ) %>%
        ungroup() %>%
        pivot_wider(
          id_cols = `FINAL FABRIC`,
          names_from = SEASON,
          values_from = c(SKU, VERBA),
          values_fill = 0
        ) %>%
        arrange(`FINAL FABRIC`)
      
      fun_table(tbl_2)
    })
    
  })
}
