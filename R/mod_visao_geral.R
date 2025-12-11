# R/mod_visao_geral.R -----------------------------------------------
mod_visao_geral_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    layout_columns(
      col_widths = c(3, 3, 3, 3, 12),
      row_heights = c(2, 3),
      card(
        style = "border: 1px solid #1F5FBF;",
        card_body(
          highchartOutput(ns("hc_sku_season"))
        ),
        full_screen = TRUE
      ),
      card(
        style = "border: 1px solid #1EAD74;",
        card_body(
          highchartOutput(ns("hc_verba_seson"))
        ),
        full_screen = TRUE
      ),
      card(
        style = "border: 1px solid #C1121F;",
        card_body(
          highchartOutput(ns("hc_custo_season"))
        ),
        full_screen = TRUE
      ),
      card(
        style = "border: 1px solid #F18F01;",
        card_body(
          highchartOutput(ns("hc_markup_season"))
        ),
        full_screen = TRUE
      ),
      
      # Abas dentro do card
      navset_card_pill(
        title = "Ranking SKUs | Verba",
        nav_panel(
          title = "COO",
          
          layout_columns(
            col_widths = c(6, 6),
            card(
              style = "border: 2px solid #28465e1a;",
              reactableOutput(ns("rt_coo"))
            ),
            card(
              style = "border: 2px solid #28465e1a;",
              reactableOutput(ns("rt_verba_coo"))
            )
          )
        ),
        nav_panel(
          title = "Supplier",
          
          layout_columns(
            col_widths = c(6, 6),
            card(
              style = "border: 2px solid #28465e1a;",
              reactableOutput(ns("rt_supplier"))
            ),
            card(
              style = "border: 2px solid #28465e1a;",
              reactableOutput(ns("rt_verba_supplier"))
            )
          )
        ),
        nav_panel(
          title = "Final Fabric",
          layout_columns(
            col_widths = c(6, 6),
            card(
              style = "border: 2px solid #28465e1a;",
              reactableOutput(ns("rt_final_fabric"))
            ),
            card(
              style = "border: 2px solid #28465e1a;",
              reactableOutput(ns("rt_verba_final_fabric"))
            )
          )
        ),
        nav_panel(
          title = "Category",
          layout_columns(
            col_widths = c(6, 6),
            card(
              style = "border: 2px solid #28465e1a;",
              reactableOutput(ns("rt_category"))
            ),
            card(
              style = "border: 2px solid #28465e1a;",
              reactableOutput(ns("rt_verba_category"))
            )
          )
        ),
        nav_panel(
          title = "Fabric Type",
          layout_columns(
            col_widths = c(6, 6),
            card(
              style = "border: 2px solid #28465e1a;",
              reactableOutput(ns("rt_fabric_type"))
            ),
            card(
              style = "border: 2px solid #28465e1a;",
              reactableOutput(ns("rt_verba_fabric_type"))
            )
          )
        ),
        nav_panel(
          title = "Squad",
          layout_columns(
            col_widths = c(6, 6),
            card(
              style = "border: 2px solid #28465e1a;",
              reactableOutput(ns("rt_squad"))
            ),
            card(
              style = "border: 2px solid #28465e1a;",
              reactableOutput(ns("rt_verba_squad"))
            )
          )
        )
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
        left_join(df_season, "season") %>%
        group_by(season_factor) %>%
        summarise(
          total_sku = n_distinct(style_number),
        ) %>%
        ungroup() %>%
        arrange(season_factor)
      
      agg_total <- df_agg$total_sku %>%
        sum() %>%
        gt:::vec_fmt_integer(sep_mark = ".")
      
      highchart() %>%
        hc_chart(type = "line") %>%
        hc_xAxis(
          categories = df_agg$season_factor,
          labels = list(
            style = list(
              fontSize = "8px"
            ),
            useHTML = TRUE#,
      #       formatter = htmlwidgets::JS("
      #   function () {
      #     var label = this.value;
      # 
      #     // separa tipo e ano
      #     var tipo = label.replace(/[0-9]/g, '');
      #     var ano  = parseInt(label.match(/[0-9]+/)[0]); // 24, 25, 26...
      # 
      #     // converte para ano completo
      #     var anoFull = 2000 + ano; // 2024, 2025, 2026
      # 
      #     // verifica o label anterior
      #     var prev = this.axis.categories[this.pos - 1];
      #     var prevAno = prev ? 2000 + parseInt(prev.match(/[0-9]+/)[0]) : null;
      # 
      #     // só mostra o ano quando ele muda
      #     var showYear = (!prevAno || prevAno !== anoFull);
      # 
      #     return '<div style=\"text-align:center;line-height:1.2\">' +
      #              '<div>' + tipo + '</div>' +
      #              (showYear
      #                ? '<div style=\"font-size:10px;color:#666\">' + anoFull + '</div>'
      #                : '<div>&nbsp;</div>') +
      #            '</div>';
      #   }
      # ")
          )
          ) %>%
        hc_yAxis(
          title = list(text = NA),
          labels = list(
            style = list(
              fontSize = "8px"
            )
          ),
          min = 0
        ) %>%
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
        left_join(df_season, "season") %>%
        group_by(season_factor) %>%
        summarise(
          total_verba = sum(total_verba, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        arrange(season_factor)
      
      agg_total <- df_agg$total_verba %>%
        sum() %>%
        gt:::vec_fmt_currency(decimals = 0, sep_mark = ".", dec_mark = ",")
      
      highchart() %>%
        hc_chart(type = "line") %>%
        hc_xAxis(
          categories = df_agg$season_factor,
          labels = list(
            style = list(
              fontSize = "8px"
            )
          )
        ) %>%
        hc_yAxis(
          title = list(text = NA),
          labels = list(
            style = list(
              fontSize = "8px"
            )
          ),
          min = 0
        ) %>%
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
        arrange(season_id) %>%
        distinct(season) %>%
        mutate(season_factor = fct(season))
      
      df_agg <- df %>%
        select(-season_factor) %>%
        left_join(df_season, "season") %>%
        group_by(season_factor) %>%
        summarise(
          total_custo = sum(total_custo, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        arrange(season_factor)
      
      agg_total <- df_agg$total_custo %>%
        sum() %>%
        gt:::vec_fmt_currency(decimals = 0, sep_mark = ".", dec_mark = ",")
      
      highchart() %>%
        hc_chart(type = "line") %>%
        hc_xAxis(
          categories = df_agg$season_factor,
          labels = list(
            style = list(
              fontSize = "8px"
            )
          )
        ) %>%
        hc_yAxis(
          title = list(text = NA),
          labels = list(
            style = list(
              fontSize = "8px"
            )
          ),
          min = 0
        ) %>%
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
            color = "#C1121F",
            fontSize = "24px",
            fontWeight = "bold"
          )
        ) %>%
        hc_add_series(
          name  = "Custo",
          data  = round(df_agg$total_custo, 2),
          color = "#C1121F"
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
        left_join(df_season, "season") %>%
        group_by(season_factor) %>%
        summarise(
          total_verba = sum(total_verba, na.rm = TRUE),
          total_custo = sum(total_custo, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        mutate(
          markup = ifelse(total_custo > 0, total_verba / total_custo, NA_real_),
        ) %>%
        arrange(season_factor)
      
      agg_total <- (sum(df_agg$total_verba) / sum(df_agg$total_custo)) %>%
        gt:::vec_fmt_number(decimals = 2, sep_mark = ".", dec_mark = ",")
      
      highchart() %>%
        hc_chart(type = "line") %>%
        hc_xAxis(
          categories = df_agg$season_factor,
          labels = list(
            style = list(
              fontSize = "8px"
            )
          )
        ) %>%
        hc_yAxis(
          title = list(text = NA),
          labels = list(
            style = list(
              fontSize = "8px"
            )
          )
        ) %>%
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
    
    
    output$rt_coo <- renderReactable({
      df <- dados_filtrados()
      req(nrow(df) > 0)
      
      df_agg <- df %>%
        # count(COO = coo, Supplier = final_supplier_production, sort = TRUE) %>%
        count(COO = coo, sort = TRUE) %>%
        mutate(
          p = n / sum(n),
          a = cumsum(p)
        ) %>%
        rowid_to_column(var ="r")
      
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
            r = colDef(
              name = "Rank",
              width = 60,
              format = colFormat(
                digits = 0
              )
            ),
            n = colDef(
              name = "SKUs",
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
    
    output$rt_verba_coo <- renderReactable({
      df <- dados_filtrados()
      req(nrow(df) > 0)
      
      df_agg <- df %>%
        group_by(COO = coo) %>%
        summarise(verba = sum(total_verba)) %>%
        ungroup() %>%
        arrange(desc(verba)) %>%
        mutate(
          p = verba / sum(verba),
          a = cumsum(p)
        ) %>%
        rowid_to_column(var ="r")
      
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
            r = colDef(
              name = "Rank",
              width = 60,
              format = colFormat(
                digits = 0
              )
            ),
            verba = colDef(
              name = "Verba",
              width = 120,
              format = colFormat(
                digits = 0,
                separators = TRUE
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
    
    
    
    output$rt_supplier <- renderReactable({
      df <- dados_filtrados()
      req(nrow(df) > 0)
      
      df_agg <- df %>%
        # count(COO = coo, Supplier = final_supplier_production, sort = TRUE) %>%
        count(Supplier = final_supplier_production, sort = TRUE) %>%
        mutate(
          p = n / sum(n),
          a = cumsum(p)
        ) %>%
        rowid_to_column(var ="r")
      
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
            r = colDef(
              name = "Rank",
              width = 60,
              format = colFormat(
                digits = 0
              )
            ),
            n = colDef(
              name = "SKUs",
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
    
    output$rt_verba_supplier <- renderReactable({
      df <- dados_filtrados()
      req(nrow(df) > 0)
      
      df_agg <- df %>%
        group_by(Supplier = final_supplier_production) %>%
        summarise(verba = sum(total_verba)) %>%
        ungroup() %>%
        arrange(desc(verba)) %>%
        mutate(
          p = verba / sum(verba),
          a = cumsum(p)
        ) %>%
        rowid_to_column(var ="r")
      
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
            r = colDef(
              name = "Rank",
              width = 60,
              format = colFormat(
                digits = 0
              )
            ),
            verba = colDef(
              name = "Verba",
              width = 120,
              format = colFormat(
                digits = 0,
                separators = TRUE
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
        ) %>%
        rowid_to_column(var ="r")
      
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
            r = colDef(
              name = "Rank",
              width = 60,
              format = colFormat(
                digits = 0
              )
            ),
            n = colDef(
              name = "SKUs",
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
    
    output$rt_verba_category <- renderReactable({
      df <- dados_filtrados()
      req(nrow(df) > 0)
      
      df_agg <- df %>%
        group_by(Category = category) %>%
        summarise(verba = sum(total_verba)) %>%
        ungroup() %>%
        arrange(desc(verba)) %>%
        mutate(
          p = verba / sum(verba),
          a = cumsum(p)
        ) %>%
        rowid_to_column(var ="r")
      
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
            r = colDef(
              name = "Rank",
              width = 60,
              format = colFormat(
                digits = 0
              )
            ),
            verba = colDef(
              name = "Verba",
              width = 120,
              format = colFormat(
                digits = 0,
                separators = TRUE
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
        count(`Final Fabric` = final_fabric, sort = TRUE) %>%
        mutate(
          p = n / sum(n),
          a = cumsum(p)
        ) %>%
        rowid_to_column(var ="r")
      
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
            r = colDef(
              name = "Rank",
              width = 60,
              format = colFormat(
                digits = 0
              )
            ),
            n = colDef(
              name = "SKUs",
              width = 120,
              format = colFormat(
                digits = 0,
                separators = TRUE
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
    
    output$rt_verba_final_fabric <- renderReactable({
      df <- dados_filtrados()
      req(nrow(df) > 0)
      
      df_agg <- df %>%
        group_by(`Final Fabric` = final_fabric) %>%
        summarise(verba = sum(total_verba)) %>%
        ungroup() %>%
        arrange(desc(verba)) %>%
        mutate(
          p = verba / sum(verba),
          a = cumsum(p)
        ) %>%
        rowid_to_column(var ="r")
      
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
            r = colDef(
              name = "Rank",
              width = 60,
              format = colFormat(
                digits = 0
              )
            ),
            verba = colDef(
              name = "Verba",
              width = 120,
              format = colFormat(
                digits = 0,
                separators = TRUE
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
    
    
    
    output$rt_fabric_type <- renderReactable({
      df <- dados_filtrados()
      req(nrow(df) > 0)
      
      df_agg <- df %>%
        count(`Fabric Type` = fabric_type, sort = TRUE) %>%
        mutate(
          p = n / sum(n),
          a = cumsum(p)
        ) %>%
        rowid_to_column(var ="r")
      
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
            r = colDef(
              name = "Rank",
              width = 60,
              format = colFormat(
                digits = 0
              )
            ),
            n = colDef(
              name = "SKUs",
              width = 120,
              format = colFormat(
                digits = 0,
                separators = TRUE
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
    
    output$rt_verba_fabric_type <- renderReactable({
      df <- dados_filtrados()
      req(nrow(df) > 0)
      
      df_agg <- df %>%
        group_by(`Fabric Type` = fabric_type) %>%
        summarise(verba = sum(total_verba)) %>%
        ungroup() %>%
        arrange(desc(verba)) %>%
        mutate(
          p = verba / sum(verba),
          a = cumsum(p)
        ) %>%
        rowid_to_column(var ="r")
      
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
            r = colDef(
              name = "Rank",
              width = 60,
              format = colFormat(
                digits = 0
              )
            ),
            verba = colDef(
              name = "Verba",
              width = 120,
              format = colFormat(
                digits = 0,
                separators = TRUE
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
    
    
    
    output$rt_squad <- renderReactable({
      df <- dados_filtrados()
      req(nrow(df) > 0)
      
      df_agg <- df %>%
        count(Squad = squad, sort = TRUE) %>%
        mutate(
          p = n / sum(n),
          a = cumsum(p)
        ) %>%
        rowid_to_column(var ="r")
      
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
            r = colDef(
              name = "Rank",
              width = 60,
              format = colFormat(
                digits = 0
              )
            ),
            n = colDef(
              name = "SKUs",
              width = 120,
              format = colFormat(
                digits = 0,
                separators = TRUE
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
    
    output$rt_verba_squad <- renderReactable({
      df <- dados_filtrados()
      req(nrow(df) > 0)
      
      df_agg <- df %>%
        group_by(Squad = squad) %>%
        summarise(verba = sum(total_verba)) %>%
        ungroup() %>%
        arrange(desc(verba)) %>%
        mutate(
          p = verba / sum(verba),
          a = cumsum(p)
        ) %>%
        rowid_to_column(var ="r")
      
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
            r = colDef(
              name = "Rank",
              width = 60,
              format = colFormat(
                digits = 0
              )
            ),
            verba = colDef(
              name = "Verba",
              width = 120,
              format = colFormat(
                digits = 0,
                separators = TRUE
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
