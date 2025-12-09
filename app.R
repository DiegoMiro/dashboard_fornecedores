
# app.R ---------------------------------------------------------------
library(shiny)
library(bslib)
library(dplyr)
library(tidyverse)
library(highcharter)
library(reactable)
library(reactablefmtr)
library(scales)
library(shinyWidgets)
library(janitor)

# módulos
source("R/fun_table.R")
source("R/theme_farm_rio.R")
source("R/mod_filters.R")
source("R/mod_visao_geral.R")
source("R/mod_supplier.R")
source("R/mod_fabric.R")

source("R/mod_markup.R")

options(
  highcharter.lang = list(
    decimalPoint = ",",
    thousandsSep = "."
  )
)


#-------------------------------------------------------------------
#   Carregar dados
#-------------------------------------------------------------------

dados <- read_csv(
  "analise_forncedores.csv",
  show_col_types = FALSE
) %>%
  clean_names()

#-------------------------------------------------------------------
#   UI
#-------------------------------------------------------------------

ui <- page_navbar(
  title = div(
      div("🌺", "Farm Rio", class = "farm-brand-title")#,
      # tags$small("Fornecedores")
  ),
  theme  = farm_theme(),
  header = farm_css(),

  sidebar = sidebar(
    open = TRUE,
    position = "left",
    class = "farm-sidebar",
    width = 320,
    mod_filters_ui("filtros")
  ),

  nav_panel("Visão geral", mod_visao_geral_ui("visao_geral")),
  nav_panel("Supplier",    mod_supplier_ui("supplier")),
  nav_panel("Fabric",      mod_fabric_ui("fabric")),
  nav_panel("Markup",      mod_markup_ui("markup")),
)

#-------------------------------------------------------------------
#   SERVER
#-------------------------------------------------------------------

server <- function(input, output, session) {

  # reactive com dados filtrados via pickerGroup
  dados_filtrados <- mod_filters_server("filtros", dados)

  # módulos de conteúdo
  mod_visao_geral_server("visao_geral", dados_filtrados)
  mod_supplier_server("supplier",       dados_filtrados)
  mod_fabric_server("fabric",           dados_filtrados)
  mod_markup_server("markup",           dados_filtrados)
}

shinyApp(ui, server)
