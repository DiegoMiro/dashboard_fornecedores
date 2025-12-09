
# R/mod_fabric.R --------------------------------------------------
mod_fabric_ui <- function(id) {
  ns <- NS(id)
  
  layout_columns(
    col_widths = c(12),
    card(
      full_screen = TRUE,
      card_header(
        "Final Fabric Performance by Season"
      ),
      card_body(
        reactableOutput(ns("rt_fabric_season"))
      )
    )
  )
}

mod_fabric_server <- function(id, dados_filtrados) {
  moduleServer(id, function(input, output, session) {
    
    output$rt_fabric_season <- renderReactable({
      df <- dados_filtrados()
      req(nrow(df) > 0)
      
      tbl_2 <- df %>%
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
          VERBA = sum(`TOTAL VERBA $`),
          SKU = n_distinct(`STYLE NUMBER`)
        ) %>%
        ungroup() %>%
        pivot_wider(
          id_cols = `FINAL FABRIC`,
          names_from = SEASON,
          values_from = c(VERBA, SKU),
          values_fill = 0
        ) %>%
        arrange(`FINAL FABRIC`)
      
      fun_table(tbl_2)
    })
    
  })
}
