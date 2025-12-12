# R/theme_farm_rio.R -------------------------------------------------
farm_theme <- function() {
  bs_theme(
    version = 5,
    base_font    = font_google("Nunito"),
    heading_font = font_google("Playfair Display"),
    # primary   = "#9A0405",
    primary   = "#8B2C1F",
    secondary = "#FEC85D",
    success   = "#1EAD74",
    info      = "#00A0C6",
      bg        = "#FFF9F0",
    # bg        = "white",
    fg        = "#351E1C"
  )
}

farm_css <- function() {
  tags$style(HTML("
  :root {
    --farm-primary: #8B2C1F;
    --farm-secondary: #FEC85D;
    --farm-green: #1EAD74;
    --farm-blue: #00A0C6;
    --farm-bg: #FFF9F0;
  }

  body {
    background: radial-gradient(circle at top left, #FFE9D0 0, #FFF9F0 45%, #FFF 80%);
  }

  .farm-brand-title {
    font-weight: 700;
    letter-spacing: 0.08em;
    text-transform: uppercase;
  }

  .farm-badge {
    background: linear-gradient(90deg, #FEC85D, #FF8A5C);
    color: #351E1C;
    border-radius: 999px;
    padding: 4px 12px;
    font-size: 0.75rem;
    font-weight: 600;
  }

  .farm-sidebar {
    background: linear-gradient(180deg, #FFF4E4, #FFE3E3);
    border-right: 0;
    box-shadow: 4px 0 18px rgba(0,0,0,0.06);
  }
  
  .farm-sidebar-title {
    color: #8B2C1F;

    font-weight: 600;
    font-size: 1.8rem;
  }
  
  .bslib-sidebar-layout .sidebar .sidebar-content {
    padding-top: 0px !important;
  }

  .farm-sidebar h4 {
    font-weight: 700;
  }

  .farm-kpi-card {
    border-radius: 18px;
    background: #FFFFFFEE;
    box-shadow: 0 14px 30px rgba(0,0,0,0.06);
    border: 0;
  }

  .farm-kpi-title {
    font-size: 0.8rem;
    text-transform: uppercase;
    letter-spacing: 0.08em;
    color: #8B5C51;
    font-weight: 700;
  }

  .farm-kpi-value {
    font-size: 1.6rem;
    font-weight: 800;
    color: #351E1C;
  }

  .farm-kpi-sub {
    font-size: 0.85rem;
    color: #7C5E5B;
  }

  .nav-link.active {
    font-weight: 700 !important;
  }

  .navbar {
    border-bottom: 0;
    box-shadow: 0 6px 20px rgba(0,0,0,0.05);
    background: linear-gradient(90deg, #FFE9D0, #FFD6D9);
  }

  .navbar-brand {
    display: flex;
    align-items: center;
    gap: 10px;
  }

  .navbar-brand .emoji {
    font-size: 1.5rem;
  }

  .farm-chip {
    display: inline-flex;
    align-items: center;
    gap: 6px;
    padding: 4px 10px;
    border-radius: 999px;
    background-color: rgba(255,255,255,0.9);
    border: 1px solid rgba(0,0,0,0.04);
    font-size: 0.8rem;
  }

  .farm-chip .dot {
    width: 8px; height: 8px;
    border-radius: 999px;
    background: #1EAD74;
  }

  .bslib-sidebar-layout>.sidebar {
    padding-top: 1rem;
    padding-bottom: 1rem;
  }

  .form-label {
    font-weight: 600;
    color: #5C3B34;
  }

  .farm-kpi-mini-card {
    height: 300px !important;
    max-height: 300px !important;
    padding: 0.7rem 0.9rem !important;
    border-radius: 14px;
    background: #FFFFFFEE;
    box-shadow: 0 8px 16px rgba(0,0,0,0.05);
    border: 0;
    display: flex;
    flex-direction: column;
  }

  .farm-kpi-mini-title {
    font-size: 0.75rem;
    text-transform: uppercase;
    letter-spacing: 0.08em;
    color: #8B5C51;
    font-weight: 700;
  }

  .farm-kpi-mini-value {
    font-size: 1.4rem;
    font-weight: 800;
    color: #351E1C;
    margin-bottom: 0.25rem;
  }
  
    /* Impede que o pickerInput ultrapasse o container */
  .bootstrap-select {
    max-width: 100% !important;
  }

  /* Permite quebra de linha no texto exibido quando o picker está FECHADO */
  .bootstrap-select .filter-option-inner-inner {
    white-space: normal !important;
    word-wrap: break-word !important;
  }

  /* Permite quebra de linha nos itens do dropdown (picker ABERTO) */
  .bootstrap-select .dropdown-menu.inner li a span.text {
    white-space: normal !important;
    word-wrap: break-word !important;
  }
  
  "))
}
