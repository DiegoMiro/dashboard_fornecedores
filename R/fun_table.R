fun_table <- function(tbl) {
  
  # Pacotes
  library(tidyverse)
  library(readr)
  library(reactable)
  library(scales)
  library(htmltools)
  
  # ---------------------------------------------------------------------------- #
  # 1. DATA SET                                                                  #
  # ---------------------------------------------------------------------------- #
  # tbl <- readr::read_csv("tbl.csv")
  
  # Substituir NA por 0 em todas as colunas numéricas
  tbl <- tbl %>%
    mutate(across(where(is.numeric), ~ replace_na(., 0)))
  
  if (all(c("SUPPLIER", "FLAG") %in% names(tbl))) {
    tbl <- tbl %>%
      mutate(
        SUPPLIER = map2_chr(
          SUPPLIER, FLAG,
          function(x, y) {
            htmltools::HTML(
              str_c(
                x,
                "<br>",
                "<span style='font-size:1.7em;'>",
                y,
                "</span>"
              )
            )
          } 
        )
      )
  }
  
  
  
  
  # Identificar colunas numéricas
  num_cols <- tbl %>%
    select(where(is.numeric)) %>%
    names()
  
  
  tbl <- tbl %>%
    mutate(
      .before = num_cols[1],
      SKU_TOTAL_SEASON = rowSums(
        across(starts_with("SKU")),
        na.rm = TRUE
      )
    ) %>%
    mutate(
      .before = num_cols[1],
      VERBA_TOTAL_SEASON = rowSums(
        across(starts_with("VERBA")),
        na.rm = TRUE
      )
    )
    
  
  num_cols <- tbl %>%
    select(where(is.numeric)) %>%
    names()
  
  
  # Totais por coluna (para %)
  totals <- sapply(num_cols, function(col) sum(tbl[[col]], na.rm = TRUE))
  
  
  
  # ---------------------------------------------------------------------------- #
  # 2.1 VERBA COLS                                                               #
  # ---------------------------------------------------------------------------- #
  # Função da barrinha verde (%)
  make_progress_bar_dollar <- function(pct) {
    width <- round(pct * 100, 1)
    
    htmltools::HTML(
      paste0(
        "<div style='
        width:100%;
        background:#e5e7eb;
        height:6px;
        border-radius:4px;
        margin-top:4px;
        display:flex;
        justify-content:flex-start;
        flex-direction:row-reverse;'>
        
        <div style='
          width:", width, "%;
          background:#22c55e;
          height:6px;
          border-radius:4px;
        '></div>
        
      </div>"
      )
    )
  }
  
  # Função célula US$ + percentual + barra
  make_cell_dollar_pct_bar <- function(value, total_col) {
    pct <- if (total_col == 0) 0 else value / total_col
    
    htmltools::HTML(
      paste0(
        "$", scales::comma(value, accuracy = 1, big.mark = ".", decimal.mark = ","),
        "<br><span style='font-size:0.85em; color:#6b7280;'>",
        scales::percent(pct, accuracy = 0.1),
        "</span>",
        make_progress_bar_dollar(pct)
      )
    )
  }
  
  # Footer US$ + 100%
  make_footer_dollar <- function(values) {
    total <- sum(values, na.rm = TRUE)
    htmltools::HTML(
      paste0(
        "$", scales::comma(total, accuracy = 1, big.mark = ".", decimal.mark = ","),
        "<br><span style='font-size:0.85em; color:#6b7280;'>100%</span>"
      )
    )
  }
  
  # ---------------------------------------------------------------------------- #
  # 2.2 SKU COLS                                                                 #
  # ---------------------------------------------------------------------------- #
  # Função da barrinha azul (%)
  make_progress_bar_qtd <- function(pct) {
    width <- round(pct * 100, 1)
    
    htmltools::HTML(
      paste0(
        "<div style='
        width:100%;
        background:#e5e7eb;
        height:6px;
        border-radius:4px;
        margin-top:4px;
        display:flex;
        justify-content:flex-start;
        flex-direction:row-reverse;'>
        
        <div style='
          width:", width, "%;
          background:#3b82f6;
          height:6px;
          border-radius:4px;
        '></div>
        
      </div>"
      )
    )
  }
  
  # Função qtd + percentual + barra
  make_cell_qtd_pct_bar <- function(value, total_col) {
    pct <- if (total_col == 0) 0 else value / total_col
    
    htmltools::HTML(
      paste0(
        scales::comma(value, accuracy = 1, big.mark = ".", decimal.mark = ","),
        "<br><span style='font-size:0.85em; color:#6b7280;'>",
        scales::percent(pct, accuracy = 0.1),
        "</span>",
        make_progress_bar_qtd(pct)
      )
    )
  }
  
  # Footer qtd + 100%
  make_footer_qtd <- function(values) {
    total <- sum(values, na.rm = TRUE)
    htmltools::HTML(
      paste0(
        scales::comma(total, accuracy = 1, big.mark = ".", decimal.mark = ","),
        "<br><span style='font-size:0.85em; color:#6b7280;'>100%</span>"
      )
    )
  }
  
  # ---------------------------------------------------------------------------- #
  # 2.3 GRUPOS                                                                   #
  # ---------------------------------------------------------------------------- #
  sku_cols   <- names(tbl)[str_detect(names(tbl), "^SKU_")]
  verba_cols <- names(tbl)[str_detect(names(tbl), "^VERBA_")]
  
  grupos <- str_remove(sku_cols, "^SKU_")
  
  column_groups <- lapply(grupos, function(g) {
    colGroup(
      name = str_replace_all(g, "_", " "),
      columns = c(paste0("SKU_", g), paste0("VERBA_", g))
    )
  })
  
  
  if (all(c("SUPPLIER", "FLAG") %in% names(tbl))) {
    cols_text <- list(
      FLAG = colDef(
        show = FALSE
      ),
      SUPPLIER = colDef(
        name = "Supplier",
        align = "left",
        html = TRUE,
        minWidth = 150,
        sticky = "left",
        footer = "Total"
      )
    )
  } else {
    cols_text <- list(
      `FINAL FABRIC` = colDef(
        name = "FINAL FABRIC",
        align = "left",
        html = TRUE,
        minWidth = 150,
        sticky = "left",
        footer = "Total"
      )
    )
  }
  
  
  col_defs <- c(
    
    cols_text,
    
    setNames(
      lapply(sku_cols, function(col) {
        total_col <- totals[[col]]
        colDef(
          name = "SKU",
          align = "right",
          html = TRUE,
          width = 120,
          style = list(
            borderLeft = "1px solid #cecfd3",
            background = if_else(str_detect(col, "TOTAL"), "#f9fafb", "white")
          ),
          cell = function(value, index) {
            make_cell_qtd_pct_bar(value, total_col)
          },
          footer = make_footer_qtd
        )
      }),
      sku_cols
    ),
    
    setNames(
      lapply(verba_cols, function(col) {
        total_col <- totals[[col]]
        colDef(
          name = "Verba",
          align = "right",
          html = TRUE,
          width = 120,
          style = list(
            background = if_else(str_detect(col, "TOTAL"), "#f9fafb", "white")
          ),
          cell = function(value, index) {
            make_cell_dollar_pct_bar(value, total_col)
          },
          footer = make_footer_dollar
        )
      }),
      verba_cols
    )
  )
  
  # ---------------------------------------------------------------------------- #
  # 3 TABLE                                                                      #
  # ---------------------------------------------------------------------------- #
  reactable(
    tbl,
    defaultPageSize = 30,
    searchable = TRUE,
    highlight = TRUE,
    compact = TRUE,
    bordered = FALSE,
    resizable = TRUE,
    defaultSorted = "VERBA_TOTAL_SEASON",
    defaultSortOrder = "desc",
    defaultColDef = colDef(
      align = "right",
      headerClass = "table-header",
      class = "table-cell",
      minWidth = 80
    ),
    columns = col_defs,
    
    columnGroups = column_groups,
    
    theme = reactableTheme(
      backgroundColor = "#FFF9F0",
      borderColor = "#e5e7eb",
      stripedColor = "#f9fafb",
      highlightColor = "#eef2ff",
      cellPadding = "8px 10px",
      style = list(
        fontFamily = "-apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif",
        fontSize = "0.85rem"
        # fontSize = "1rem"
      ),
      headerStyle = list(
        # background = "#111827",
        # color = "#ffffff",
        fontWeight = "600"#,
        # borderTop = "3px #4b5563"#,
        # borderBottom = "2px solid #4b5563"
      ),
      footerStyle = list(
        fontWeight = "600",
        borderTop = "2px solid #4b5563",
        background = "#f9fafb"
      )
    )
  )
  
}
