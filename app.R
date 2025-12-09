library(shiny) 
library(shinyjs) 
library(sf) 
library(dplyr) 
library(leaflet) 
library(promises) 
library(future) 
library(readr)
library(scales)
library(ggplot2)
library(tidyr)    
library(rlang)
library(qs)

Sys.setlocale(category = "LC_ALL", locale = "pt_BR.UTF-8")

# arquivos de anos 
arquivos_anos_onca <- list.files(
  path   = "dados",
  pattern = "^dados_onca_\\d{4}\\.qs$",
  full.names = TRUE
)

arquivos_anos_jag  <- list.files(
  path   = "dados",
  pattern = "^dados_jaguatirica_\\d{4}\\.qs$",
  full.names = TRUE
)

anos_onca   <- sort(as.numeric(gsub("\\D", "", basename(arquivos_anos_onca))))
anos_jag    <- sort(as.numeric(gsub("\\D", "", basename(arquivos_anos_jag))))
anos_unicos <- sort(unique(c(anos_onca, anos_jag)))
if (length(anos_unicos) == 0) anos_unicos <- NA_integer_

# dados de onças afetadas por ano
caminho_csv <- file.path("dados", "soma_onca_ano.csv")
oncas_afetadas_ano <- if (file.exists(caminho_csv)) {
  read_csv(caminho_csv, show_col_types = FALSE)
} else {
  data.frame(ano = integer(), total_oncas = numeric())
}
oncas_afetadas_ano$total_oncas <- suppressWarnings(as.numeric(oncas_afetadas_ano$total_oncas))
total_oncas_afetadas <- sum(oncas_afetadas_ano$total_oncas, na.rm = TRUE)

# Municípios Amazônia Legal
mun <- qread("dados/amazonia_mun_mapa_ultra.qs")

nm_col <- "NM_MUN"
if (!(nm_col %in% names(mun))) {
  stop(paste0("Coluna ", nm_col, " não existe no shapefile!"))
}

# carregamento pré-calculado por município
precalc <- qread("dados/mun_precalc.qs")

# UI 

ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
      body {
        margin: 0; padding: 0;
        background-color: #222222; color: white;
        width:100vw; overflow-x: hidden; overflow-y: hidden;
      }
      .title-container {
        display: flex; 
        align-items: center; 
        justify-content: space-between; 
        padding: 8px; 
        background-color: #222222; 
        box-shadow: none; 
        color: #E6E6E6;
        width: 110%;
        border: none;
      }
      .loading-overlay {
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        z-index: 999;
      }
      .spinner {
        border: 4px solid rgba(255, 255, 255, 0.3);
        width: 30px;
        height: 30px;
        border-radius: 50%;
        border-left-color: white;
        animation: spin 1s linear infinite;
      }
      @keyframes spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
      .map-container {
        position: relative;
        width: 100%;
        height: calc(100vh - 120px);
        margin: 0;
        padding: 0;
        overflow: visible;
      }
      #mapa {width: 100%; height: 100%;}
      .species-selector {
        display: flex;
        justify-content: center;
        gap: 15px;
        margin: 0;
      }
      .species-box {
        cursor: pointer;
        padding: 8px 15px;
        border: 2px solid #FF4C4C;
        border-radius: 8px;
        background-color: transparent;
        color: #FF4C4C;
        font-weight: bold;
        transition: background-color 0.3s, color 0.3s;
      }
      .species-box:hover {
        background-color: #FF4C4C;
        color: white;
      }
      .species-box.active {
        background-color: #FF4C4C !important;
        border-color: #FF4C4C !important;
        color: white !important;
      }
      .info-cards {
        position: absolute;
        top: 10px;
        right: 10px;
        display: flex;
        flex-direction: column;
        gap: 10px;
        z-index: 100;
        background: transparent;
      }
      .info-card {
        background: rgba(0, 0, 0, 0.65) !important;
        padding: 12px 15px;
        border-radius: 12px;
        border: 1px solid rgba(255,255,255,0.15) !important;
        box-shadow: 0 4px 12px rgba(0,0,0,0.5);
      }
      .irs--shiny .irs-bar {
        background: #CC4444;
        border-top: 1px solid #BB3333;
        border-bottom: 1px solid #BB3333;
      }
      .irs--shiny .irs-single {
        background: #CC4444;
      }
      .view-selector-floating{
        position: absolute;
        top: 10px;                 
        left: 50%;                
        transform: translateX(-50%);
        z-index: 500;             
        display: flex;
        gap: 8px;
        background: rgba(0,0,0,0.65);
        backdrop-filter: blur(2px);
        padding: 8px 10px;
        border-radius: 10px;
        border: 1px solid rgba(255,255,255,0.15);
      }
      .view-box{
        cursor: pointer;
        padding: 8px 14px;
        border: 2px solid #32C2A5;
        border-radius: 8px;
        background: transparent;
        color: #32C2A5;
        font-weight: bold;
        transition: background-color .3s, color .3s, border-color .3s;
      }
      .view-box:hover{
        background:#32C2A5; color:#fff;
      }
      .view-box.active{
        background:#32C2A5 !important;
        color:#fff !important;
        border-color:#32C2A5 !important;
      }

      #top_municipios table,
      #top_municipios table thead tr th,
      #top_municipios table tbody tr td {
        background-color: transparent !important;
        color: #ffffff !important;
      }
      #top_municipios table {
        border-collapse: collapse;
        width: 100%;
      }
      #top_municipios table tbody tr {
        border-bottom: 1px solid #444444;
      }
      #top_municipios table thead tr {
        border-bottom: 1px solid #777777;
      }
      #top_municipios .table,
      #top_municipios .table-striped > tbody > tr:nth-of-type(odd),
      #top_municipios .table-hover > tbody > tr:hover {
        background-color: transparent !important;
      }
      #top_municipios table tbody tr:hover td {
        background-color: rgba(255,255,255,0.08) !important;
      }

      /* Botão de informação (i) */
      .info-button {
        background-color: rgba(0,0,0,0.7);
        border: 1px solid #ffffff;
        color: #ffffff;
        border-radius: 50%;
        width: 32px;
        height: 32px;
        padding: 0;
        text-align: center;
        font-weight: bold;
      }

      /* Modal claro com texto preto */
      .modal-content {
        background-color: #ffffff !important;
        color: #000000 !important;
      }
      .modal-title {
        color: #000000 !important;
      }
      .modal-body, .modal-body p, .modal-body b {
        color: #000000 !important;
      }
    "))
  ),
  
  # Controles superiores 
  div(
    class = "controls-container",
    style = "padding: 8px 4px 0 4px;",
    
    div(
      style = "display:flex; justify-content:space-between; align-items:flex-start; gap:16px;",
      
      # título + subtítulo 
      div(
        style = "display:flex; flex-direction:column; align-items:flex-start; gap:2px;",
        h1(
          "Felinos potencialmente afetados pelo desmatamento",
          style = "
            font-weight:500;
            color:white;
            margin-top:4px;
            margin-bottom:0;
            text-align:left;
          "
        ),
        div(
          "Estimativas modeladas de indivíduos de onça-pintada e jaguatirica potencialmente afetados pelo desmatamento anual na Amazônia Legal brasileira.",
          style = "
            font-size:13px;
            color:#dddddd;
            margin-top:2px;
            margin-bottom:0;
            text-align:left;
          "
        )
      ),
      
      # seleção de espécie
      div(
        class = "species-selector",
        actionButton(
          "btn_onca",
          HTML("Onça-pintada<br><span style='font-size: 12px; font-style: italic;'>Panthera onca</span>"),
          class = "species-box active"
        ),
        actionButton(
          "btn_jaguatirica",
          HTML("Jaguatirica<br><span style='font-size: 12px; font-style: italic;'>Leopardus pardalis</span>"),
          class = "species-box"
        )
      )
    ),
    
    # slider de ano
    sliderInput(
      "ano", "Ano:", 
      min    = min(anos_unicos, na.rm = TRUE), 
      max    = max(anos_unicos, na.rm = TRUE), 
      value  = max(anos_unicos, na.rm = TRUE), 
      step   = 1, ticks = TRUE, sep = "", animate = FALSE, width = "100%"
    )
  ),
  
  tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
  
  div(
    class = "view-selector-bar",
    actionButton("btn_geral",      "Geral",      class = "view-box active"),
    actionButton("btn_municipios", "Municípios", class = "view-box")
  ),
  
  # UM ÚNICO MAPA 
  div(
    class = "map-container",
    
    # botão (i) de informação
    div(
      id = "info_btn_container",
      style = "position:absolute; top:10px; left:90px; z-index:600;",
      actionButton("btn_info_geral", "i", class = "info-button")
    ),
    
    # Cards da visão GERAL
    div(
      id = "cards_geral",
      class = "info-cards",
      div(id = "card_ano", class = "info-card", uiOutput("oncas_ano")),
      tags$style(HTML("
        #card_ano, #card_grafico {
          width: 300px !important;
          text-align: center;
        }
      ")),
      div(
        id = "card_grafico", class = "info-card",
        div(
          style = "font-weight:700; margin-bottom:6px; text-align:center;",
          textOutput("titulo_serie")
        ),
        plotOutput("grafico_serie", height = "160px")
      )
    ),
    
    # Cards da visão MUNICÍPIOS
    div(
      id = "cards_mun",
      class = "info-cards",
      div(
        id = "card_top_mun", class = "info-card",
        div(
          "Top 5 municípios",
          style = "font-weight:700; margin-bottom:6px; text-align:center;"
        ),
        tableOutput("top_municipios")
      )
    ),
    
    # Spinner único
    div(id = "loading-overlay", class = "loading-overlay", div(class = "spinner")),
    
    # Mapa único
    leafletOutput("mapa", width = "100%", height = "100%")
  )
)

# SERVER 

future::plan(future::sequential)
options(future.rng.onMisuse = "ignore")

.cache <- new.env(parent = emptyenv())

readQS_cached <- function(path) {
  if (!file.exists(path)) return(NULL)
  mtime_num <- as.numeric(file.info(path)$mtime)
  key <- paste0("qs:", normalizePath(path, winslash="/", mustWork=FALSE), "_", mtime_num)
  if (!exists(key, envir = .cache, inherits = FALSE)) {
    assign(key, qread(path), envir = .cache)
  }
  get(key, envir = .cache, inherits = FALSE)
}

read_csv_cached <- function(path) {
  if (!file.exists(path)) return(NULL)
  key <- paste0("csv:", normalizePath(path, winslash="/", mustWork=FALSE),
                "_", file.info(path)$mtime)
  if (!exists(key, envir = .cache, inherits = FALSE)) {
    assign(key, readr::read_csv(path, show_col_types = FALSE), envir = .cache)
  }
  get(key, envir = .cache, inherits = FALSE)
}

server <- function(input, output, session) {
  
  especie_selecionada <- reactiveVal("Onça-pintada")
  dados_cache         <- reactiveVal(NULL)
  forcar_recarregar   <- reactiveVal(TRUE)
  visao_selecionada   <- reactiveVal("Geral")
  
  mun_pts_data <- reactiveVal(NULL)
  
  # debounce único para o ano
  ano_debounced <- shiny::debounce(reactive(input$ano), 400)
  
  # esconder cards de municípios no início
  observe({
    shinyjs::hide("cards_mun")
    shinyjs::show("cards_geral")
    shinyjs::show("info_btn_container")  # estamos na visão Geral
  })
  
  # Botões de visão 
  observeEvent(input$btn_geral, {
    visao_selecionada("Geral")
    shinyjs::show("cards_geral")
    shinyjs::hide("cards_mun")
    shinyjs::show("info_btn_container")
    
    leafletProxy("mapa") %>%
      clearShapes() %>% clearMarkers() %>% clearMarkerClusters() %>% clearControls()
    forcar_recarregar(TRUE)
    shinyjs::runjs("$('#btn_geral').addClass('active'); $('#btn_municipios').removeClass('active');")
  })
  
  observeEvent(input$btn_municipios, {
    visao_selecionada("Municípios")
    shinyjs::hide("cards_geral")
    shinyjs::show("cards_mun")
    shinyjs::hide("info_btn_container")
    
    leafletProxy("mapa") %>%
      clearShapes() %>% clearMarkers() %>% clearMarkerClusters() %>% clearControls()
    forcar_recarregar(TRUE)
    shinyjs::runjs("$('#btn_municipios').addClass('active'); $('#btn_geral').removeClass('active');")
  })
  
  # anos disponíveis por espécie 
  anos_disponiveis <- reactive({
    esp <- especie_selecionada()
    padrao <- if (esp == "Onça-pintada") "^dados_onca_\\d{4}\\.qs$" else "^dados_jaguatirica_\\d{4}\\.qs$"
    arqs <- list.files(path = "dados", pattern = padrao, full.names = TRUE)
    anos <- sort(as.numeric(gsub("\\D", "", basename(arqs))))
    if (length(anos) == 0) return(NA_integer_)
    anos
  })
  
  # nome do objeto dentro de precalc 
  nome_precalc <- reactive({
    ano <- ano_debounced()
    req(ano)
    if (especie_selecionada() == "Onça-pintada") {
      paste0("onca_", ano)
    } else {
      paste0("jag_", ano)
    }
  })
  
  # seleção de espécie 
  observeEvent(input$btn_onca, {
    especie_selecionada("Onça-pintada")
    forcar_recarregar(TRUE)
    shinyjs::runjs("$('#btn_onca').addClass('active'); $('#btn_jaguatirica').removeClass('active');")
  })
  
  observeEvent(input$btn_jaguatirica, {
    especie_selecionada("Jaguatirica")
    forcar_recarregar(TRUE)
    shinyjs::runjs("$('#btn_jaguatirica').addClass('active'); $('#btn_onca').removeClass('active');")
  })
  
  # título do gráfico 
  output$titulo_serie <- renderText({
    if (especie_selecionada() == "Onça-pintada") {
      "Onças-pintadas afetadas por ano"
    } else {
      "Jaguatiricas afetadas por ano "
    }
  })
  
  # ajuste do slider de anos 
  observeEvent(anos_disponiveis(), {
    anos <- anos_disponiveis()
    req(length(anos) && !all(is.na(anos)))
    
    ano_sel <- isolate(input$ano)
    if (!is.null(ano_sel) && !is.na(ano_sel) && (ano_sel %in% anos)) {
      novo_valor <- ano_sel
    } else if (!is.null(ano_sel) && !is.na(ano_sel)) {
      novo_valor <- anos[which.min(abs(anos - ano_sel))]
    } else {
      novo_valor <- max(anos)
    }
    
    updateSliderInput(
      session, "ano",
      min = min(anos),
      max = max(anos),
      value = novo_valor
    )
  })
  
  # card: total no ano 
  output$oncas_ano <- renderUI({
    anos <- anos_disponiveis(); req(!all(is.na(anos)))
    ano_selecionado <- input$ano; req(ano_selecionado)
    
    esp <- especie_selecionada()
    csv <- if (esp == "Onça-pintada") "dados/soma_onca_ano.csv" else "dados/soma_jaguatirica_ano.csv"
    
    if (!file.exists(csv)) {
      return(HTML("<b>Nenhum dado disponível</b>"))
    }
    
    df <- read_csv_cached(csv)
    col_val <- if (esp == "Onça-pintada") "total_oncas" else "total_jaguatiricas"
    soma_ano <- df %>% filter(ano == ano_selecionado) %>% pull(col_val)
    
    rotulo <- if (esp == "Onça-pintada") "Onças-pintadas afetadas"
    else "Jaguatiricas afetadas"
    
    HTML(paste0("<b>", rotulo, " em ", ano_selecionado, ": ", round(soma_ano, 2), "</b>"))
  })
  
  # série temporal 
  serie_csv <- reactive({
    esp <- especie_selecionada()
    arquivo <- if (esp == "Onça-pintada") "dados/soma_onca_ano.csv" else "dados/soma_jaguatirica_ano.csv"
    if (!file.exists(arquivo)) return(NULL)
    
    df <- readr::read_csv(arquivo, show_col_types = FALSE)
    col_val <- if (esp == "Onça-pintada") "total_oncas" else "total_jaguatiricas"
    if (!("ano" %in% names(df)) || !(col_val %in% names(df))) return(NULL)
    
    df |>
      mutate(
        ano   = suppressWarnings(as.integer(ano)),
        valor = suppressWarnings(as.numeric(.data[[col_val]]))
      ) |>
      filter(!is.na(ano), !is.na(valor)) |>
      arrange(ano)
  })
  
  output$grafico_serie <- renderPlot({
    df <- serie_csv()
    validate(need(!is.null(df) && nrow(df) > 0, "Sem dados para plotar."))
    
    rotulo_y <- if (especie_selecionada() == "Onça-pintada") "Onças-pintadas"
    else "Jaguatiricas"
    
    ggplot(df, aes(x = ano, y = valor)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = pretty(df$ano)) +
      scale_y_continuous(labels = scales::label_number(accuracy = 0.1)) +
      labs(x = "Ano", y = rotulo_y) +
      theme_minimal(base_size = 12) +
      theme(
        plot.background  = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        text             = element_text(colour = "black"),
        axis.text        = element_text(colour = "black"),
        axis.title       = element_text(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey30")
      )
  })
  
  # MAPA ÚNICO 
  output$mapa <- renderLeaflet({
    bb <- sf::st_bbox(mun)
    x1 <- unname(bb['xmin']); y1 <- unname(bb['ymin'])
    x2 <- unname(bb['xmax']); y2 <- unname(bb['ymax'])
    
    leaflet(options = leafletOptions(worldCopyJump = FALSE, preferCanvas = TRUE)) %>%
      addProviderTiles(
        providers$CartoDB.DarkMatter, 
        options = providerTileOptions(noWrap = TRUE, minZoom = 2)
      ) %>%
      fitBounds(x1, y1, x2, y2)
  })
  
  # VISÃO GERAL
  observeEvent(
    list(ano_debounced(), especie_selecionada(), visao_selecionada()),
    {
      req(visao_selecionada() == "Geral")
      
      ano <- ano_debounced()
      req(ano)
      
      shinyjs::show("loading-overlay")
      
      esp <- especie_selecionada()
      arquivo <- if (esp == "Onça-pintada") {
        file.path("dados", paste0("dados_onca_", ano, ".qs"))
      } else {
        file.path("dados", paste0("dados_jaguatirica_", ano, ".qs"))
      }
      
      if (!file.exists(arquivo)) {
        leafletProxy("mapa") %>%
          clearShapes() %>% clearMarkers() %>% clearMarkerClusters() %>% clearControls()
        shinyjs::hide("loading-overlay")
        return()
      }
      
      key <- paste(esp, ano, sep = "_")
      if (!is.null(dados_cache()) && identical(dados_cache()$key, key) && !forcar_recarregar()) {
        dados <- dados_cache()$dados
        
        cor <- if (especie_selecionada() == "Onça-pintada") "red" else "blue"
        cor_cluster <- if (especie_selecionada() == "Onça-pintada") "#ff4c4c" else "#4c6cff"
        
        icons <- awesomeIcons(
          icon = 'ios-close',
          iconColor = 'black',
          library = 'ion',
          markerColor = cor
        )
        
        cluster_js <- sprintf("
          function(cluster) {
            var markers = cluster.getAllChildMarkers();
            var total = 0;
            markers.forEach(function(m){
              if (m.options && m.options.ind_medio){ total += m.options.ind_medio; }
            });
            var totalFmt = (total < 1 && total > 0) ? total.toExponential(2) : total.toFixed(0);
            var size = Math.max(30, Math.min(50, total * 2));
            return L.divIcon({
              html: '<div style=\"background-color:%s; width:' + size + 'px; height:' + size + 'px; border-radius:50%%; display:flex; align-items:center; justify-content:center; font-size:8px; font-weight:bold; color:black;\">' + totalFmt + '</div>',
              className: 'custom-cluster',
              iconSize: L.point(size, size)
            });
          }
        ", cor_cluster)
        
        leafletProxy("mapa") %>%
          clearShapes() %>% clearMarkers() %>% clearMarkerClusters() %>% clearControls() %>%
          addAwesomeMarkers(
            data = dados,
            lng = ~lon, lat = ~lat, icon = icons,
            clusterOptions = markerClusterOptions(
              maxClusterRadius = 200,
              chunkedLoading   = TRUE, 
              iconCreateFunction = JS(cluster_js)
            ),
            popup = ~paste0(
              'Área: ', round(area_km2, 4), ' km²',
              '<br>', (if (especie_selecionada() == "Onça-pintada") 'Onças' else 'Jaguatiricas'),
              ' estimadas: ', round(ind_medio, 4)
            ),
            options = markerOptions(ind_medio = ~ind_medio)
          )
        
        shinyjs::hide("loading-overlay")
        return()
      }
      
      future({
        dados <- qread(arquivo)
        col_val <- if (esp == "Onça-pintada") "oncas_medio" else "jaguatiricas_medio"
        dados <- dplyr::select(dados, lon, lat, area_km2, !!sym(col_val)) %>%
          dplyr::rename(ind_medio = !!sym(col_val))
        
        max_pts <- 20000
        if (nrow(dados) > max_pts) {
          set.seed(123)
          dados <- dplyr::sample_n(dados, max_pts)
        }
        
        dados
      }, seed = TRUE) %...>% (function(dados) {
        if (!is.null(dados) && nrow(dados) > 0) {
          dados_cache(list(key = paste(especie_selecionada(), ano_debounced(), sep = "_"), dados = dados))
          
          cor <- if (especie_selecionada() == "Onça-pintada") "red" else "blue"
          cor_cluster <- if (especie_selecionada() == "Onça-pintada") "#ff4c4c" else "#4c6cff"
          
          icons <- awesomeIcons(
            icon = 'ios-close',
            iconColor = 'black',
            library = 'ion',
            markerColor = cor
          )
          
          cluster_js <- sprintf("
            function(cluster) {
              var markers = cluster.getAllChildMarkers();
              var total = 0;
              markers.forEach(function(m){
                if (m.options && m.options.ind_medio){ total += m.options.ind_medio; }
              });
              var totalFmt = (total < 1 && total > 0) ? total.toExponential(2) : total.toFixed(0);
              var size = Math.max(30, Math.min(50, total * 2));
              return L.divIcon({
                html: '<div style=\"background-color:%s; width:' + size + 'px; height:' + size + 'px; border-radius:50%%; display:flex; align-items:center; justify-content:center; font-size:8px; font-weight:bold; color:black;\">' + totalFmt + '</div>',
                className: 'custom-cluster',
                iconSize: L.point(size, size)
              });
            }
          ", cor_cluster)
          
          leafletProxy("mapa") %>%
            clearShapes() %>% clearMarkers() %>% clearMarkerClusters() %>% clearControls() %>%
            addAwesomeMarkers(
              data = dados,
              lng = ~lon, lat = ~lat, icon = icons,
              clusterOptions = markerClusterOptions(
                maxClusterRadius = 200,
                chunkedLoading   = TRUE, 
                iconCreateFunction = JS(cluster_js)
              ),
              popup = ~paste0(
                'Área: ', round(area_km2, 4), ' km²',
                '<br>', (if (especie_selecionada() == "Onça-pintada") 'Onças' else 'Jaguatiricas'),
                ' estimadas: ', round(ind_medio, 4)
              ),
              options = markerOptions(ind_medio = ~ind_medio)
            )
          
          forcar_recarregar(FALSE)
        } else {
          leafletProxy("mapa") %>%
            clearShapes() %>% clearMarkers() %>% clearMarkerClusters() %>% clearControls()
        }
        shinyjs::hide("loading-overlay")
      }) %...!% (function(err) {
        print(paste("[Erro no future]", err$message))
        shinyjs::hide("loading-overlay")
      })
    }
  )
  
  # VISÃO MUNICÍPIOS 
  observeEvent(
    list(visao_selecionada(), especie_selecionada(), ano_debounced()),
    {
      req(visao_selecionada() == "Municípios")
      
      shinyjs::show("loading-overlay")
      t0 <- Sys.time()
      message(">>> [MUN] início em ", t0)
      
      nome <- nome_precalc()
      pts  <- precalc[[nome]]
      
      if (is.null(pts) || nrow(pts) == 0) {
        leafletProxy("mapa") %>%
          clearShapes() %>% clearMarkers() %>% clearMarkerClusters() %>% clearControls()
        mun_pts_data(NULL)
        shinyjs::hide("loading-overlay")
        t1 <- Sys.time()
        message(">>> [MUN] sem dados | tempo: ",
                round(as.numeric(difftime(t1, t0, units = "secs")), 3), " s")
        return()
      }
      
      if (especie_selecionada() == "Onça-pintada") {
        col_val    <- "total_oncas"
        titulo_leg <- "Onças-pintadas afetadas"
        pal_nome   <- "Reds"
      } else {
        col_val    <- "total_jaguatiricas"
        titulo_leg <- "Jaguatiricas afetadas"
        pal_nome   <- "Blues"
      }
      
      pts <- pts %>%
        mutate(
          valor = .data[[col_val]],
          label = sprintf(
            "<b>%s</b><br>%s: <b>%.2f</b>",
            MUN_DOM, titulo_leg, valor
          ),
          raio  = ifelse(
            valor <= 0, 3,
            pmin(12, 4 + log10(valor + 1) * 4)
          )
        )
      
      mun_pts_data(pts)
      
      vals <- pts$valor
      if (all(is.na(vals))) vals <- rep(0, length(vals))
      
      rng <- range(vals, na.rm = TRUE)
      if (!all(rng > 0)) {
        rng <- c(0, max(vals, na.rm = TRUE) + 1e-6)
      }
      
      pal <- colorNumeric(
        palette = pal_nome,
        domain  = rng,
        na.color = "#999999"
      )
      
      leafletProxy("mapa") %>%
        clearShapes() %>% clearMarkers() %>% clearMarkerClusters() %>% clearControls() %>%
        addCircleMarkers(
          data        = pts,
          lng         = ~lon,
          lat         = ~lat,
          radius      = ~raio,
          stroke      = TRUE,
          weight      = 0.8,
          color       = "#f0f0f0",
          opacity     = 0.8,
          fillColor   = ~pal(valor),
          fillOpacity = 0.85,
          label       = ~lapply(label, HTML),
          popup       = ~lapply(label, HTML)
        ) %>%
        addLegend(
          position = "topleft",
          pal      = pal,
          values   = rng,
          title    = titulo_leg,
          opacity  = 0.7
        )
      
      shinyjs::hide("loading-overlay")
      t1 <- Sys.time()
      message(">>> [MUN] fim em ", t1,
              " | tempo: ", round(as.numeric(difftime(t1, t0, units = "secs")), 3), " s")
    }
  )
  
  # Tabela Top 5 
  output$top_municipios <- renderTable({
    req(visao_selecionada() == "Municípios")
    pts <- mun_pts_data()
    req(!is.null(pts), nrow(pts) > 0)
    
    pts %>%
      arrange(desc(valor)) %>%
      slice_head(n = 5) %>%
      transmute(
        Município             = MUN_DOM,
        `Indivíduos afetados` = round(valor, 2)
      )
  }, striped = FALSE, hover = FALSE, bordered = FALSE, align = "c")
  
  # Modal de informação para o mapa geral 
  observeEvent(input$btn_info_geral, {
    req(visao_selecionada() == "Geral")
    showModal(
      modalDialog(
        title = HTML("<b>Como interpretar o mapa geral</b>"),
        easyClose = TRUE,
        footer = modalButton("Fechar"),
        HTML(paste0(
          "<p>Na visão <b>Geral</b>, cada ponto representa o centro de um polígono ",
          "de desmatamento mapeado no ano selecionado.</p>",
          
          "<p>O número exibido nos círculos maiores (clusters) corresponde ao ",
          "<b>total estimado de indivíduos</b> de onça-pintada ou jaguatirica ",
          "potencialmente afetados pelos polígonos agrupados naquela região.</p>",
          
          "<p>Ou seja, não significa que os animais estejam exatamente naquele ponto, ",
          "mas sim que <b>a soma dos impactos dos polígonos próximos</b> aparece ",
          "agrupada ali para facilitar a visualização.</p>"
        ))
      )
    )
  })
}

shinyApp(ui = ui, server = server)
