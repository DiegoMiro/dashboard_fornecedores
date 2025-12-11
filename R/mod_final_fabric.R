
# R/mod_final_fabric.R --------------------------------------------------
mod_final_fabric_ui <- function(id) {
  ns <- NS(id)
  
  layout_columns(
    col_widths = c(12),
    card(
      full_screen = TRUE,
      card_header(
        "Final Fabric Performance by Season"
      ),
      card_body(
        reactableOutput(ns("rt_final_fabric_season"))
      )
    )
  )
}

mod_final_fabric_server <- function(id, dados_filtrados) {
  moduleServer(id, function(input, output, session) {
    
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
