# R/mod_visao_geral.R -----------------------------------------------
mod_visao_geral_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    layout_columns(
      col_widths = c(3, 3, 3, 3, 4, 4, 4),
      row_heights = c(1, 2),
      card(
        card_body(
          highchartOutput(ns("hc_sku_season"))
        ),
        full_screen = TRUE
      ),
      card(
        card_body(
          highchartOutput(ns("hc_verba_seson"))
        ),
        full_screen = TRUE
      ),
      card(
        card_body(
          highchartOutput(ns("hc_custo_season"))
        ),
        full_screen = TRUE
      ),
      card(
        card_body(
          highchartOutput(ns("hc_markup_season"))
        ),
        full_screen = TRUE
      ),
      
      card(
        card_body(
          reactableOutput(ns("rt_supplier"))
        ),
        full_screen = TRUE
      ),
      card(
        card_body(
          reactableOutput(ns("rt_final_fabric"))
        ),
        full_screen = TRUE
      ),
      card(
        card_body(
          reactableOutput(ns("rt_category"))
        ),
        full_screen = TRUE
      )
      
    )
  )
}

mod_visao_geral_server <- function(id, dados_filtrados) {
  moduleServer(id, function(input, output, session) {
    
    output$kpi_title <- renderText({
      df <- dados_filtrados()
      n_season <- length(unique(df$season))
      n_month  <- length(unique(df$on_floor_month))
      glue("{n_season} season(s) · {n_month} mês(es) no piso")
    })
    
    output$kpi_styles <- renderText({
      df <- dados_filtrados()
      dplyr::n_distinct(df$style_number)
    })
    
    # output$kpi_custo_total <- renderText({
    #   df <- dados_filtrados()
    #   scales::dollar(sum(df$total_custo, na.rm = TRUE),
    #                  big.mark = ".", decimal.mark = ",")
    # })
    # 
    # output$kpi_total_custo <- renderText({
    #   df <- dados_filtrados()
    #   scales::dollar(sum(df$total_custo, na.rm = TRUE),
    #                  big.mark = ".", decimal.mark = ",")
    # })
    # 
    # output$kpi_total_custo_per_style <- renderText({
    #   df <- dados_filtrados()
    #   n_estilos <- dplyr::n_distinct(df$style_number)
    #   if (n_estilos == 0) return("Sem estilos nos filtros atuais")
    #   val <- sum(df$total_custo, na.rm = TRUE) / n_estilos
    #   paste("por estilo:",
    #         scales::dollar(val, big.mark = ".", decimal.mark = ","))
    # })
    # 
    # output$kpi_total_verba <- renderText({
    #   df <- dados_filtrados()
    #   scales::dollar(sum(df$total_verba, na.rm = TRUE),
    #                  big.mark = ".", decimal.mark = ",")
    # })
    # 
    # output$kpi_total_verba_per_style <- renderText({
    #   df <- dados_filtrados()
    #   n_estilos <- dplyr::n_distinct(df$style_number)
    #   if (n_estilos == 0) return("Sem estilos nos filtros atuais")
    #   val <- sum(df$total_verba, na.rm = TRUE) / n_estilos
    #   paste("por estilo:",
    #         scales::dollar(val, big.mark = ".", decimal.mark = ","))
    # })
    # 
    # output$kpi_markup_us <- renderText({
    #   df <- dados_filtrados()
    #   total_custo     <- sum(df$total_custo, na.rm = TRUE)
    #   total_retail_us <- sum(df$total_retail_us, na.rm = TRUE)
    #   if (total_custo == 0) return("–")
    #   mk <- total_retail_us / total_custo
    #   paste0(round(mk, 2), "x")
    # })
    # 
    # output$kpi_total_retail_us <- renderText({
    #   df <- dados_filtrados()
    #   scales::dollar(sum(df$total_retail_us, na.rm = TRUE),
    #                  big.mark = ".", decimal.mark = ",")
    # })
    
    output$hc_sku_season <- renderHighchart({
      df <- dados_filtrados()
      req(nrow(df) > 0)
      
      df_season <- df %>%
        arrange(season_id) %>%
        distinct(season) %>%
        mutate(season_factor = fct(season))
      
      df_agg <- df %>%
        select(-season_factor) %>%
        left_join(df_season) %>%
        group_by(season_factor) %>%
        summarise(
          total_sku = n_distinct(style_number),
          .groups = "drop"
        ) %>%
        arrange(season_factor)
      
      agg_total <- df_agg$total_sku %>%
        sum() %>%
        gt:::vec_fmt_integer(sep_mark = ".")
      
      highchart() %>%
        hc_chart(type = "line") %>%
        hc_xAxis(categories = df_agg$season_factor) %>%
        hc_yAxis(title = list(text = NA), min = 0) %>%
        hc_title(
          text = "SKUs",
          align = "left",
          style = list(
            color = "#111827",
            fontSize = "16px",
            fontWeight = "bold"
          )
        ) %>%
        hc_subtitle(
          text = agg_total,
          align = "left",
          style = list(
            color = "#1F5FBF",
            fontSize = "24px",
            fontWeight = "bold"
          )
        ) %>%
        hc_add_series(
          name  = "SKUs",
          data  = df_agg$total_sku,
          color = "#1F5FBF"
        ) %>%
        hc_legend(enabled = FALSE) %>%
        hc_tooltip(
          shared = TRUE,
          pointFormat = "<span style='color: #111827; font-weight: bold;'>{series.name}: </span><span style='color:{series.color}; font-weight: bold;'>{point.y:,.0f}</span>",
          valueDecimals = 0
        ) %>%
        hc_plotOptions(
          column = list(
            borderRadius = 4,
            pointPadding = 0.1,
            groupPadding = 0.05
          )
        )
    })
    
    
    output$hc_verba_seson <- renderHighchart({
      df <- dados_filtrados()
      req(nrow(df) > 0)
      
      df_season <- df %>%
        arrange(season_id) %>%
        distinct(season) %>%
        mutate(season_factor = fct(season))
      
      df_agg <- df %>%
        select(-season_factor) %>%
        left_join(df_season) %>%
        group_by(season_factor) %>%
        summarise(
          total_verba = sum(total_verba, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(season_factor)
      
      agg_total <- df_agg$total_verba %>%
        sum() %>%
        gt:::vec_fmt_currency(decimals = 0, sep_mark = ".", dec_mark = ",")
      
      highchart() %>%
        hc_chart(type = "line") %>%
        hc_xAxis(categories = df_agg$season_factor) %>%
        hc_yAxis(title = list(text = NA), min = 0) %>%
        hc_title(
          text = "Verba",
          align = "left",
          style = list(
            color = "#111827",
            fontSize = "16px",
            fontWeight = "bold"
          )
        ) %>%
        hc_subtitle(
          text = agg_total,
          align = "left",
          style = list(
            color = "#1EAD74",
            fontSize = "24px",
            fontWeight = "bold"
          )
        ) %>%
        hc_add_series(
          name  = "Verba",
          data  = round(df_agg$total_verba, 2),
          color = "#1EAD74"
        ) %>%
        hc_legend(enabled = FALSE) %>%
        hc_tooltip(
          shared = TRUE,
          pointFormat = "<span style='color: #111827; font-weight: bold;'>{series.name}: </span><span style='color:{series.color}; font-weight: bold;'>${point.y:,.0f}</span>",
          valueDecimals = 0
        ) %>%
        hc_plotOptions(
          column = list(
            borderRadius = 4,
            pointPadding = 0.1,
            groupPadding = 0.05
          )
        )
    })
    
    output$hc_custo_season <- renderHighchart({
      df <- dados_filtrados()
      req(nrow(df) > 0)
      
      df_season <- df %>%
        select(-season_factor) %>%
        arrange(season_id) %>%
        distinct(season) %>%
        mutate(season_factor = fct(season))
      
      df_agg <- df %>%
        left_join(df_season) %>%
        group_by(season_factor) %>%
        summarise(
          total_custo = sum(total_custo, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(season_factor)
      
      agg_total <- df_agg$total_custo %>%
        sum() %>%
        gt:::vec_fmt_currency(decimals = 0, sep_mark = ".", dec_mark = ",")
      
      highchart() %>%
        hc_chart(type = "line") %>%
        hc_xAxis(categories = df_agg$season_factor) %>%
        hc_yAxis(title = list(text = NA), min = 0) %>%
        hc_title(
          text = "Custo",
          align = "left",
          style = list(
            color = "#111827",
            fontSize = "16px",
            fontWeight = "bold"
          )
        ) %>%
        hc_subtitle(
          text = agg_total,
          align = "left",
          style = list(
            color = "#9A0405",
            fontSize = "24px",
            fontWeight = "bold"
          )
        ) %>%
        hc_add_series(
          name  = "Custo",
          data  = round(df_agg$total_custo, 2),
          color = "#9A0405"
        ) %>%
        hc_legend(enabled = FALSE) %>%
        hc_tooltip(
          shared = TRUE,
          pointFormat = "<span style='color: #111827; font-weight: bold;'>{series.name}: </span><span style='color:{series.color}; font-weight: bold;'>${point.y:,.0f}</span>",
          valueDecimals = 0
        ) %>%
        hc_plotOptions(
          column = list(
            borderRadius = 4,
            pointPadding = 0.1,
            groupPadding = 0.05
          )
        )
    })
    
    output$hc_markup_season <- renderHighchart({
      df <- dados_filtrados()
      req(nrow(df) > 0)
      
      df_season <- df %>%
        arrange(season_id) %>%
        distinct(season) %>%
        mutate(season_factor = fct(season))
      
      df_agg <- df %>%
        select(-season_factor) %>%
        left_join(df_season) %>%
        group_by(season_factor) %>%
        summarise(
          total_verba = sum(total_verba, na.rm = TRUE),
          total_custo = sum(total_custo, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          markup = ifelse(total_custo > 0, total_verba / total_custo, NA_real_),
        ) %>%
        arrange(season_factor)
      
      agg_total <- (sum(df_agg$total_verba) / sum(df_agg$total_custo)) %>%
        gt:::vec_fmt_number(decimals = 2, sep_mark = ".", dec_mark = ",")
      
      highchart() %>%
        hc_chart(type = "line") %>%
        hc_xAxis(categories = df_agg$season_factor) %>%
        hc_yAxis(title = list(text = NA)) %>%
        hc_title(
          text = "Markup",
          align = "left",
          style = list(
            color = "#111827",
            fontSize = "16px",
            fontWeight = "bold"
          )
        ) %>%
        hc_subtitle(
          text = agg_total,
          align = "left",
          style = list(
            color = "#F18F01",
            fontSize = "24px",
            fontWeight = "bold"
          )
        ) %>%
        hc_add_series(
          name  = "Markup",
          data  = round(df_agg$markup, 2),
          color = "#F18F01"
        ) %>%
        hc_legend(enabled = FALSE) %>%
        hc_tooltip(
          shared = TRUE,
          pointFormat = "<span style='color: #111827; font-weight: bold;'>{series.name}: </span><span style='color:{series.color}; font-weight: bold;'>{point.y:,.2f}</span>",
          valueDecimals = 0
        ) %>%
        hc_plotOptions(
          column = list(
            borderRadius = 4,
            pointPadding = 0.1,
            groupPadding = 0.05
          )
        )
    })
    
    output$rt_supplier <- renderReactable({
      df <- dados_filtrados()
      req(nrow(df) > 0)
      
      df_agg <- df %>%
        # count(COO = coo, Supplier = final_supplier_production, sort = TRUE) %>%
        count(Supplier = final_supplier_production, sort = TRUE) %>%
        mutate(
          p = n / sum(n),
          a = cumsum(p)
        )
      
      df_agg %>%
        reactable(
          pagination = FALSE,
          sortable = FALSE,
          compact = TRUE,
          striped = TRUE,
          defaultColDef = colDef(
            headerStyle = list(fontWeight = "bold")
          ),
          columns = list(
            n = colDef(
              name = "Cont.",
              width = 60,
              format = colFormat(
                digits = 0
              )
            ),
            p = colDef(
              name = "Perc.",
              width = 60,
              format = colFormat(
                percent = TRUE,
                digits = 0
              )
            ),
            a = colDef(
              name = "Acum.",
              width = 60,
              format = colFormat(
                percent = TRUE,
                digits = 0
              )
            )
          ),
          theme = reactableTheme(
            backgroundColor = "#FFF9F0"
          )
        )
      
    })
    
    output$rt_category <- renderReactable({
      df <- dados_filtrados()
      req(nrow(df) > 0)
      
      df_agg <- df %>%
        count(Category = category, sort = TRUE) %>%
        mutate(
          p = n / sum(n),
          a = cumsum(p)
        )
      
      df_agg %>%
        reactable(
          pagination = FALSE,
          sortable = FALSE,
          compact = TRUE,
          striped = TRUE,
          defaultColDef = colDef(
            headerStyle = list(fontWeight = "bold")
          ),
          columns = list(
            n = colDef(
              name = "Cont.",
              width = 60,
              format = colFormat(
                digits = 0
              )
            ),
            p = colDef(
              name = "Perc.",
              width = 60,
              format = colFormat(
                percent = TRUE,
                digits = 0
              )
            ),
            a = colDef(
              name = "Acum.",
              width = 60,
              format = colFormat(
                percent = TRUE,
                digits = 0
              )
            )
          ),
          theme = reactableTheme(
            backgroundColor = "#FFF9F0"
          )
        )
      
    })
    
    output$rt_final_fabric <- renderReactable({
      df <- dados_filtrados()
      req(nrow(df) > 0)
      
      df_agg <- df %>%
        count(Fabric = final_fabric, sort = TRUE) %>%
        mutate(
          p = n / sum(n),
          a = cumsum(p)
        )
      
      df_agg %>%
        reactable(
          pagination = FALSE,
          sortable = FALSE,
          compact = TRUE,
          striped = TRUE,
          defaultColDef = colDef(
            headerStyle = list(fontWeight = "bold")
          ),
          columns = list(
            n = colDef(
              name = "Cont.",
              width = 60,
              format = colFormat(
                digits = 0
              )
            ),
            p = colDef(
              name = "Perc.",
              width = 60,
              format = colFormat(
                percent = TRUE,
                digits = 0
              )
            ),
            a = colDef(
              name = "Acum.",
              width = 60,
              format = colFormat(
                percent = TRUE,
                digits = 0
              )
            )
          ),
          theme = reactableTheme(
            backgroundColor = "#FFF9F0"
          )
        )
      
    })
    
  })
}
