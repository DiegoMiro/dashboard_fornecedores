
# R/mod_markup.R ----------------------------------------------
mod_markup_ui <- function(id) {
  ns <- NS(id)
  
  card(
    full_screen = TRUE,
    
    card_header(
      bslib::tooltip(
        span(
          span("Markup by Supplier", style = "font-weight:bold; font-family: 'Helvetica', sans-serif; font-size:16px;"),
          span(bsicons::bs_icon("info-circle", width = "14px"))
        ),
        "Bubble Chart: o diâmetro da bolha representa o total da verba"
      )
    ),
    card_body(
      highchartOutput(ns("hc_markup"))
    )
  )
}

mod_markup_server <- function(id, dados_filtrados) {
  moduleServer(id, function(input, output, session) {
    
    output$hc_markup <- renderHighchart({
      df <- dados_filtrados()
      req(nrow(df) > 0)
      
      df_raw <- df %>%
        distinct(season_id, season) %>%
        mutate(season_factor = fct(season)) %>%
        select(season_id, season_factor) %>%
        left_join(
          df %>% select(-season_factor), "season_id"
        ) %>%
        group_by(
          supplier = final_supplier_production,
          date = season_factor
        ) %>%
        summarise(
          verba = sum(total_verba),
          custo = sum(total_custo),
          styles = n_distinct(style_number)
        ) %>%
        ungroup() %>%
        mutate(
          markup = verba / custo
        )
      
      
      # --------------------------------------------------
      # 2. Agregar por supplier para gerar os pontos da bolha
      #    + guardar série temporal de markup para o sparkline
      # --------------------------------------------------
      bubble_data <- df_raw %>%
        arrange(supplier, date) %>%
        group_by(supplier) %>%
        summarise(
          markup_med   = mean(markup),
          total_verba  = sum(verba),
          n_styles     = sum(styles),       # ou n_distinct(style_id) se tiver um ID
          n_seasons    = n_distinct(date),  # ou n_distinct(style_id) se tiver um ID
          markup_ts    = list(markup)       # série de markup ao longo do tempo
        ) %>%
        ungroup() %>%
        arrange(markup_med)
      
      # categorias do eixo Y
      suppliers <- bubble_data$supplier
      
      # Highcharts precisa de valores numéricos para Y quando usamos categories
      bubble_data <- bubble_data %>%
        mutate(
          y_index = seq_along(supplier) - 1  # 0,1,2,... para usar como índice das categorias
        )
      
      # Transformar em lista de pontos com campos extras p/ tooltip
      bubble_points <- pmap(
        list(
          x         = bubble_data$markup_med,
          y         = bubble_data$y_index,
          z         = bubble_data$total_verba,
          name      = bubble_data$supplier,
          spark     = bubble_data$markup_ts,
          styles_n  = bubble_data$n_styles,
          seasons_n = bubble_data$n_seasons
        ),
        function(x, y, z, name, spark, styles_n, seasons_n) {
          list(
            x       = x,
            y       = y,
            z       = z,
            name    = name,
            spark   = spark,      # série de markup ao longo do tempo
            styles  = styles_n,   # número de styles
            seasons = seasons_n   # número de styles
          )
        }
      )
      
      # --------------------------------------------------
      # 3. Gráfico de bolha com tooltip contendo sparkline
      # --------------------------------------------------
      highchart() |>
        hc_chart(type = "bubble") |>
        hc_title(text = NA) |>
        hc_xAxis(
          title = list(text = "Markup médio"),
          min = floor(min(bubble_data$markup_med) * 0.9),
          max = ceiling(max(bubble_data$markup_med) * 1.1)
        ) |>
        hc_yAxis(
          title = list(text = NA),
          categories = suppliers,
          labels = list(enabled = FALSE),
          tickInterval = 1,
          min = 0,
          max = length(suppliers) - 1,
          gridLineWidth = 0
        ) |>
        hc_add_series(
          data = bubble_points,
          # name = name,
          color = "#F18F01",
          maxSize = "4%",
          minSize = "1%",
          dataLabels = list(
            enabled = TRUE,
            format = "{point.name}",  # estilo geom_text
            align = "left",           # alinhado à esquerda do ponto
            x = 10,                    # desloca um pouco pra direita da bolha
            y = 0,
            style = list(
              textOutline = "none",
              fontSize = "10px",
              color = "#666666"
            ),
            allowOverlap = TRUE       # geom_text = pode sobrepor
          )
        ) |>
        hc_legend(enabled = FALSE) |>
        hc_chart(
          events = list(
            click = JS("
              function() { 
                if (this.tooltip) {
                  this.tooltip.hide();
                }
              }"
            )
          )
        ) |>
        hc_tooltip(
          useHTML = TRUE,
          followPointer = TRUE,
          formatter = htmlwidgets::JS("
      function() {
        var point = this.point;
        var markupSeries = point.spark || [];
        
        // ID único para o container do sparkline
        var containerId = 'spark-' + point.index + '-' + (new Date()).getTime();

        var html = '<b>' + point.name + '</b><br/>' +
                   'Total verba: ' + Highcharts.numberFormat(point.z, 0, ',', '.') + '<br/>' +
                   'Styles: ' + point.styles + '<br/>' +
                   'Seasons: ' + point.seasons + '<br/>' + '<br/>' +
                   '<div style=\"color:#F18F01; font-weight:bold;\">Markup médio: ' + Highcharts.numberFormat(point.x, 2, ',', '.') + '</div>' + '<br/>' +
                   '<div id=\"' + containerId + '\" ' +
                   'style=\"height:60px; width:160px; margin-top:4px;\"></div>';

        // desenha o sparkline dentro do tooltip
        setTimeout(function () {
          var el = document.getElementById(containerId);
          if (!el) return;

          Highcharts.chart(containerId, {
            chart: {
              backgroundColor: 'transparent',
              margin: [2, 0, 2, 0],
              height: 60
            },
            title: { text: '' },
            credits: { enabled: false },
            xAxis: {
              visible: false,
              minPadding: 0.05,
              maxPadding: 0.05 
            },
            yAxis: {
              visible: false
            },
            legend: {
              enabled: false
            },
            tooltip: {
              enabled: false
            },
            exporting: {
              enabled: false
            },
            plotOptions: {
              series: {
                label: {
                  enabled: false
                }
              }
            },
            series: [{
              type: 'line',
              color: '#F18F01',
              data: markupSeries,
              marker: { enabled: true }
            }]
          });
        }, 0);

        return html;
      }
    ")
        )
      
    })
  })
}
