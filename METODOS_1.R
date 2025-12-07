# pacotes ----
# install.packages(c(
#  "bslib",
#  "bsicons",
#  "DT",
#  "dplyr",
#  "genderBR",
#  "ggplot2",
#  "purrr",
#  "scales",
#  "shiny",
#  "shinyAce",
#  "shinyjs",
#  "shinyWidgets",
#  "tidyr",
#  "tidyverse",
#  "wordcloud2",
#  "worrrd"
# ))

library(bslib)
library(bsicons)
library(DT)
library(dplyr)
library(genderBR)
library(ggplot2)
library(purrr)
library(scales)
library(shiny)
library(shinyAce)
library(shinyjs)
library(shinyWidgets)
library(tidyr)
library(tidyverse)
library(wordcloud2)
library(worrrd)



# tratamento ----

# Solução Temporaria para Leitura 
if (file.exists("nomes_lista.rds")) {
  df_nomes <- readRDS("nomes_lista.rds") |>
    bind_rows() |>
    as_tibble()
} else {
  df_nomes <- readRDS("nomes_lista") |>
    bind_rows() |>
    as_tibble()
}

sujo_nomes <- df_nomes |>
  unnest_auto(res)

limpo_nomes <- sujo_nomes |>
  mutate(periodo_limpo = gsub("[^0-9,]", "", periodo)) |>
  separate(
    periodo_limpo,
    into = c("ano_inicio", "ano_fim"),
    sep = ",",
    convert = TRUE
  ) |>
  mutate(
    Começo = ifelse(ano_inicio == 1930 & is.na(ano_fim), 1929, ano_inicio),
    Fim = ifelse(is.na(ano_fim) & ano_inicio < 1940, 1930, ano_fim),
    
    Período = case_when(
      Começo < 1930 ~ "Antes de 1930",
      TRUE ~ paste(Começo, "a", Fim)),
    
    Período = fct_reorder(Período, Começo)
  ) |>
  mutate(Sexo = get_gender(nome),
         Sexo = if_else(Sexo == "Male", "Masculino", "Feminino")) |>
  mutate(Inicial = toupper(substr(nome, 1, 1))) |>
  mutate(Comprimento = nchar(nome)) |>
  select(Nome = nome,
         Sexo,
         Localidade = localidade,
         Frequência = frequencia,
         Período,
         Começo,
         Fim,
         Inicial,
         Comprimento)



# valores ----
periodos_ordenados <- levels(limpo_nomes$Período)

cor_bg_escuro <- "#222222"

tema_escuro_ggplot <- theme(
  panel.background = element_rect(fill = cor_bg_escuro, color = cor_bg_escuro),
  plot.background = element_rect(fill = cor_bg_escuro, color = cor_bg_escuro),
  panel.grid.major = element_line(color = "gray35"),
  panel.grid.minor = element_line(color = "gray30"),
  axis.text = element_text(color = "white"),
  axis.title = element_text(color = "white"),
  plot.title = element_text(color = "white", hjust = 0.5),
  plot.subtitle = element_text(color = "white", hjust = 0.5),
  plot.caption = element_text(color = "white"),
  legend.background = element_rect(fill = cor_bg_escuro),
  legend.text = element_text(color = "white"),
  legend.title = element_text(color = "white")
)



# funções ----
ativar_dblclick <- function(id_conteiner, aba, isTabset = T) {
  id_input <- paste0(aba, "_dblclick")
  
  if (isTabset) {
    tags$script(
      sprintf("
      $('#%s a[data-value=\"%s\"]').on('dblclick', function(e) {
         e.preventDefault();
         Shiny.setInputValue('%s', Date.now(), {priority:'event'});
      });
    ", id_conteiner, aba, id_input
      )
    )
  } else {
    tags$script(
      sprintf("
      $('#%s li a[data-value=\"%s\"]').on('dblclick', function(e) {
         e.preventDefault();
         Shiny.setInputValue('%s', Date.now(), {priority:'event'});
      });
    ", id_conteiner, aba, id_input
      )
    )
  }
}

mostrar_modal_codigo <- function(
    input,
    titulo,
    id,
    codigo = '',
    codigo_ui = '',
    codigo_server = '',
    isUiServer = T
) {
  tema_ace <- ifelse(input$tema_atual == "dark", "monokai", "github")
  
  showModal(
    modalDialog(
      title = titulo,
      footer = modalButton("Fechar"),
      easyClose = TRUE,
      size = "xl",
      
      if (isUiServer) {
        tabsetPanel(
          tabPanel(
            title = "ui",
            aceEditor(
              outputId = paste0("display_ui_", id),
              mode = "r",
              theme = tema_ace,
              readOnly = TRUE,
              value = codigo_ui
            )
          ),
          
          tabPanel(
            title = "server",
            aceEditor(
              outputId = paste0("display_server_", id),
              mode = "r",
              theme = tema_ace,
              readOnly = TRUE,
              value = codigo_server
            )
          )
        )
      } else {
        aceEditor(
          outputId = paste0("display_", id),
          mode = "r",
          theme = tema_ace,
          readOnly = TRUE,
          value = codigo
        )
      }
    )
  )
}



# CODIGO_UI_LINHAS ----
CODIGO_UI_LINHAS <-
  'tabPanel("Evolução (Linhas)",
          value = "aba_linhas",
          br(),
          div(
            id = "log_checkbox_linhas",
            checkboxInput("logaritmica_linhas", "Utilizar escala logarítmica", value = FALSE)
          ),
          uiOutput("grafico_evolucao_conteiner"))'



# CODIGO_SERVER_LINHAS ----
CODIGO_SERVER_LINHAS <-
  'output$grafico_evolucao_conteiner <- renderUI({
    if (length(input$nome_selecionado) == 0) {
      tags$div(
        style = "height: 60vh; display: flex; align-items: center; justify-content: center; flex-direction: column;",
        tags$div(
          tags$p("Selecione um nome na aba lateral", 
                 style = "color: #cce8e0; font-weight: 600; font-size: 3rem; padding: 20px; text-align: center;"),
        )
      )
    } else {
      tagList(
        plotOutput("grafico_evolucao"),
        br(),
        hr(),
        br(),
        plotOutput("grafico_regressao")
      )
    }
  })
  
  output$grafico_evolucao <- renderPlot({
    req(dados_filtrados())
    
    ggplot(dados_filtrados(), aes(x = Período, y = Frequência, 
                                  color = Nome, group = Nome)) +
      geom_line(size = 1.2) +
      geom_point(size = 4) +
      geom_label(aes(label = scales::comma(Frequência, big.mark = ".", decimal.mark = ",")), 
                 vjust = -0.7, 
                 show.legend = FALSE,
                 size = 3) +
      labs(
        title = "Evolução do Número de Nascimentos",
        subtitle = paste("Nomes:", paste(input$nome_selecionado, collapse = ", ")),
        x = "Período",
        y = "Número de Nascimentos",
        color = "Nome"
      ) +
      theme_bw() +
      theme(
        axis.title.x = element_text(margin = ggplot2::margin(t = 10), size = 14),
        axis.title.y = element_text(margin = ggplot2::margin(r = 10), size = 14)
      ) +
      {if (input$tema_atual == "dark") tema_escuro_ggplot} +
      scale_y_continuous(
        labels = scales::comma_format(big.mark = ".", decimal.mark = ","),
        expand = expansion(mult = c(0.05, 0.2)),
        trans = if(input$logaritmica_linhas) "log10" else "identity"
      ) 
  })
  
  output$grafico_regressao <- renderPlot({
    req(dados_filtrados())
    
    ggplot(dados_filtrados(), aes(x = Período, y = Frequência,
                                  color = Nome, group = Nome)) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 1.4) +
      labs(
        title = "Linhas de Regressão por Nome",
        subtitle = paste("Nomes:", paste(input$nome_selecionado, collapse = ", ")),
        x = "Período",
        y = "Número de Nascimentos",
        color = "Nome"
      ) +
      theme_bw() +
      theme(
        axis.title.x = element_text(margin = ggplot2::margin(t = 10), size = 14),
        axis.title.y = element_text(margin = ggplot2::margin(r = 10), size = 14)
      ) +
      {if (input$tema_atual == "dark") tema_escuro_ggplot} +
      scale_y_continuous(
        labels = scales::comma_format(big.mark = ".", decimal.mark = ","),
        expand = expansion(mult = c(0.05, 0.2)),
        trans = if(input$logaritmica_linhas) "log10" else "identity"
      )
  })'



# CODIGO_UI_TABELA ----
CODIGO_UI_TABELA <-
  ''



# CODIGO_SERVER_TABELA ----
CODIGO_SERVER_TABELA <-
  ''



# CODIGO_UI_HEATMAP ----
CODIGO_UI_HEATMAP <-
  ''



# CODIGO_SERVER_HEATMAP ----
CODIGO_SERVER_HEATMAP <-
  ''



# CODIGO_UI_HISTOGRAMA ----
CODIGO_UI_HISTOGRAMA <-
  ''



# CODIGO_SERVER_HISTOGRAMA ----
CODIGO_SERVER_HISTOGRAMA <-
  ''



# CODIGO_UI_NUVEM ----
CODIGO_UI_NUVEM <-
  ''



# CODIGO_SERVER_NUVEM ----
CODIGO_SERVER_NUVEM <-
  ''



# CODIGO_UI_CACA_PALAVRAS ----
CODIGO_UI_CACA_PALAVRAS <-
  ''



# CODIGO_SERVER_CACA_PALAVRAS ----
CODIGO_SERVER_CACA_PALAVRAS <-
  ''



# CODIGO_PACOTES ----
CODIGO_PACOTES <-
  '# install.packages(c(
  #  "shiny",
  #  "tidyverse",
  #  "DT",
  #  "genderBR",
  #  "dplyr",
  #  "ggplot2",
  #  "tidyr",
  #  "purrr",
  #  "wordcloud2",
  #  "scales",
  #  "shinyWidgets",
  #  "bslib"
  #  "shinyjs"
  #  "shinyAce"
  #  "bsicons"
  # ))
  
  library(bslib)
  library(shiny)
  library(tidyverse)
  library(DT)
  library(genderBR)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(purrr)
  library(wordcloud2)
  library(scales)
  library(shinyWidgets)
  library(shinyjs)
  library(shinyAce)
  library(bsicons)'



# CODIGO_TRATAMENTO ----
CODIGO_TRATAMENTO <-
  ''



# CODIGO_VALORES ----
CODIGO_VALORES <-
  ''



# CODIGO_FUNCOES ----
CODIGO_FUNCOES <-
  ''



# ui ----
ui <- navbarPage(
  id = "pagina_navbar",
  fillable = TRUE,
  theme = bs_theme(bootswatch = "minty", secondary = "#7fd1ae",),
  title = "Dashboard de Nomes (IBGE)",
  nav_item(input_dark_mode(id = "tema_atual", mode = "light")),
  
  useShinyjs(),
  
  tags$style(HTML("
      /* 1. Força a bolinha e a barra a usarem a cor do tema */
      .irs--shiny .irs-bar { border-top-color: var(--bs-secondary); border-bottom-color: var(--bs-secondary); background: var(--bs-secondary); }
      .irs--shiny .irs-handle { border: 1px solid var(--bs-secondary); background-color: var(--bs-secondary); }
      .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background-color: var(--bs-secondary); }
      
      /* 2. CORREÇÃO DE ALINHAMENTO (O bug da 'pola' acima da linha) */
      /* O Minty muda a altura da linha, então precisamos empurrar o slider para o lugar */
      
      .irs-line { top: 25px !important; }
      .irs-bar { top: 25px !important; }
      .irs-bar-edge { top: 25px !important; }
      .irs-grid-pol { top: -8px !important; }
      /* Centraliza a bolinha (handle) na linha */
      .irs-handle { top: 17px !important; } 
      
      /* Empurra o texto dos anos para baixo para não bater na linha */
      .irs-grid-text { bottom: 5px !important; font-size: 0.8rem !important; }
      
      /* Aumenta a altura total do container para caber tudo sem cortar */
      .irs { height: 70px !important; margin-top: 10px; }
    ")),
  
  # animações pop up
  tags$head(
    tags$style(HTML("
    .modal.fade .modal-dialog {
      transition: transform 0.3s ease-out, opacity 0.3s ease-out;
      transform: translateY(-20px);
      opacity: 0;
    }

    .modal.fade.show .modal-dialog {
      transform: translateY(0);
      opacity: 1;
    }
  "))
  ),
  
  # Aba Principal com Sidebar Moderna ----
  tabPanel("Análise Geral",
           page_sidebar(
             sidebar = sidebar(
               width = 300,
               bg = "var(--bs-tertiary-bg)", 
               title = div(
                 class = "d-flex align-items-center",
                 bs_icon("bar-chart-fill", size = "1.5rem", class = "me-2 text-primary"),
                 h4("Painel", class = "m-0")
               ),
               
               p("Explore a evolução dos nomes no Brasil.", class = "text-muted small mb-4"),
               
               accordion(
                 open = "Seleção de Nomes",
                 accordion_panel(
                   "Seleção de Nomes",
                   icon = bs_icon("people-fill"),
                   selectInput("nome_selecionado", 
                               "Escolha um ou mais Nomes:", 
                               choices = NULL,    
                               selected = NULL,
                               multiple = TRUE)
                 ),
                 
                 accordion_panel(
                   "Configurações",
                   icon = bs_icon("gear-fill"),
                   
                   selectInput(
                     inputId = "tema_plots",          
                     label = "Escolha um dos Tipos de Tema:",
                     choices = c("Base", "Elegante"),  
                     selected = "Elegante"            
                   ),
                   
                   hr(),
                   materialSwitch(
                     inputId = "mostrar_notificacoes",
                     label = "Exibir notificações",
                     value = T,
                     status = "primary",
                     inline = TRUE
                   ),
                   
                   hr(),
                   sliderInput(
                     inputId = "tamanho_fonte_base",
                     label = "Tamanho da fonte",
                     min = 10,
                     max = 20,
                     step = 2,
                     value = 16, # tamanho default
                     ticks = FALSE
                   )
                 ),
                 
                 accordion_panel(
                   "Códigos",
                   icon = bs_icon("code-slash"),
                   p("Para mais códigos, clique duas vezes em uma aba.", class = "small text-muted"),
                   div(class = "list-group",
                       actionLink("codigo_pacotes", "Pacotes",
                                  class = "list-group-item list-group-item-action"),
                       actionLink("codigo_tratamento", "Tratamento",
                                  class = "list-group-item list-group-item-action"),
                       actionLink("codigo_valores", "Valores",
                                  class = "list-group-item list-group-item-action"),
                       actionLink("codigo_funcoes", "Funções",
                                  class = "list-group-item list-group-item-action"),
                       actionLink("ola", "ui? server?",
                                  class = "list-group-item list-group-item-action")
                   )
                 )
               ),
               
               div(class = "mt-auto pt-4 border-top"),
               div(
                 class = "d-flex justify-content-between align-items-center small text-muted",
                 span("Fonte: IBGE (API)"),
                 span(bs_icon("database"))
               )
             ),
             
             tabsetPanel(
               id = "painel_tabset",
               
               tabPanel("Evolução (Linhas)",
                        value = "aba_linhas",
                        br(),
                        div(
                          id = "log_checkbox_linhas",
                          checkboxInput("logaritmica_linhas", "Utilizar escala logarítmica", value = FALSE)
                        ),
                        uiOutput("grafico_evolucao_conteiner")),
               
               tabPanel("Tabela Detalhada",
                        value = "aba_tabela",
                        br(),
                        uiOutput("tabela_dados_conteiner")),
               
               tabPanel("Mapa de Calor",
                        value = "aba_heatmap",
                        br(),
                        div(
                          id = "card_heatmap",
                          tags$style(HTML("
                            #tipo_heatmap .radio-inline {
                              margin-right: 40px;
                            }
                          ")),
                          
                          card(
                            card_header("Configuração do Mapa de Calor"),
                            materialSwitch(
                              inputId = "ver_todos_heatmap",
                              label = "Visualizar TODOS os nomes da base (Ignora seleção lateral)", 
                              status = "primary",
                              inline = TRUE,
                              right = TRUE,
                              value = FALSE
                            ),
                            hr(),
                            
                            radioButtons(
                              inputId = "tipo_heatmap",
                              label = "O que você quer comparar?",
                              choices = c(
                                "Pico do próprio nome (Quando ele foi mais famoso?)" = "pico",
                                "Proporção Real (Considerando o tamanho da população)" = "proporcao"
                              ),
                              selected = "pico",
                              inline = TRUE
                            )
                          )
                        ),
                        uiOutput("heatmap_iniciais_conteiner", height = "600px")),
               
               tabPanel("Histograma (Tamanho)",
                        value = "aba_histograma",
                        br(),
                        fluidRow(
                          column(6, selectInput("periodo_hist", "Escolha o Período:", choices = periodos_ordenados)),
                          column(6, checkboxInput("dividir_sexo", "Dividir por sexo", value = FALSE))
                        ),
                        plotOutput("histograma_comprimento"),
                        tags$style(HTML("
                          #numero_letras .radio-inline {
                            margin-right: 20px;
                          }
                        ")),
                        br(),
                        hr(),
                        br(),
                        radioButtons(
                          inputId = "numero_letras",
                          label = "Selecione o número de letras:",
                          choices = 3:9,
                          selected = 3,
                          inline = TRUE
                        ),
                        plotOutput("barras_comprimento")),
             )
           )
  ),
  
  # Aba Nuvem de Palavras (SEPARADA, sem sidebar) ----
  tabPanel("Nuvem de Palavras",
           value = "aba_nuvem",
           br(),
           tags$style(HTML("
             .irs--shiny .irs-bar { border-top-color: var(--bs-primary); border-bottom-color: var(--bs-primary); background: var(--bs-primary); }
             .irs--shiny .irs-handle { border: 1px solid var(--bs-primary); background-color: var(--bs-primary); }
             .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background-color: var(--bs-primary); }
           ")),
           
           fluidRow(
             column(12, align = "center",
                    div(style = "max-width: 800px; margin: 0 auto;",
                        sliderTextInput(
                          inputId = "periodo_nuvem_slider", 
                          label = "Período de Referência:", 
                          choices = periodos_ordenados,
                          selected = periodos_ordenados[1],
                          animate = animationOptions(interval = 1000),
                          grid = TRUE,
                          width = "100%"
                        )
                    ),
                    br(),
                    div(
                      checkboxInput("usar_iniciais", "Agrupar por iniciais", value = FALSE)
                    )
             )
           ),
           hr(),
           wordcloud2Output("nuvem_nomes", height = "600px")
  ),
  
  # Aba Caça-Palavras ----
  tabPanel("Caça-Palavras",
           value = "aba_caça_palavras",
           br(),
           fluidRow(
             column(4,
                    h4("Configurações"),
                    
                    # Slider de tamanho
                    sliderInput("tamanho", "Tamanho do Grid:", 
                                min = 8, max = 20, value = 12),
                    
                    # Botão de gerar
                    actionButton("gerar", "Gerar Novo Caça-Palavras", 
                                 class = "btn-primary"),
                    br(), br(),
                    
                    # Checkbox para ver a resposta (controla o solution = TRUE)
                    checkboxInput("ver_resposta", "Mostrar Resposta/Gabarito", value = FALSE),
                    
                    hr(),
                    h4("Palavras para encontrar:"),
                    uiOutput("lista_palavras") # Certifique-se de renderizar isso no server se quiser a lista em texto
             ),
             column(8, plotOutput("caca_palavras", height = "600px"))),
  ),
  
  # ativa double click nas abas ----
  ativar_dblclick("painel_tabset", "aba_linhas"),
  ativar_dblclick("painel_tabset", "aba_tabela"),
  ativar_dblclick("painel_tabset", "aba_heatmap"),
  ativar_dblclick("painel_tabset", "aba_histograma"),
  ativar_dblclick("pagina_navbar", "aba_nuvem", F),
  ativar_dblclick("pagina_navbar", "aba_caça_palavras", F)
  )



# server ----
server <- function(input, output, session) {
  
  updateSelectInput(session, "nome_selecionado", choices = sort(unique(limpo_nomes$Nome)))
  
  dados_filtrados <- reactive({
    req(input$nome_selecionado)
    limpo_nomes |> filter(Nome %in% input$nome_selecionado) |> arrange(Começo)
  })
  
  observe({
    if (length(input$nome_selecionado) == 0) {
      shinyjs::hide("log_checkbox_linhas")
    } else {
      shinyjs::show("log_checkbox_linhas")
      shinyjs::show("card_heatmap")
    }
  })
  
  cor_bg_reativa <- reactive({
    if (input$tema_atual == "dark") {
      return(cor_bg_escuro)
    } else {
      return("white")
    }
  })
  
  tamanho_fonte_base <- reactive({
    # 1. Defina o tamanho BASE da fonte no slider
    tamanho_fonte_base <- input$tamanho_fonte_base
  })
  
  my_pretty_theme <- reactive({
    # 2. Seu tema personalizado (atualizado com o base_size)
    my_pretty_theme <- theme_minimal(base_family = "Roboto Condensed", base_size = tamanho_fonte_base()) +
      theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = rel(1.4)), 
        plot.subtitle = element_text(face = "plain", size = rel(1.1), color = "grey40"),
        plot.caption = element_text(face = "italic", size = rel(0.7), color = "grey70", hjust = 0),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = rel(1.1), hjust = 0),
        axis.title = element_text(face = "bold"),
        # Margens ajustadas para não cortar texto grande
        axis.title.x = element_text(margin = ggplot2::margin(t = 15)),
        axis.title.y = element_text(margin = ggplot2::margin(r = 15)),
        strip.background = element_rect(fill = "grey90", color = NA),
        panel.border = element_rect(color = "grey90", fill = NA)
      )
  })
  
  # 1. Gráfico ----
  output$grafico_evolucao_conteiner <- renderUI({
    if (length(input$nome_selecionado) == 0) {
      tags$div(
        style = "height: 60vh; display: flex; align-items: center; justify-content: center; flex-direction: column;",
        tags$div(
          tags$p("Selecione um nome na aba lateral", 
                 style = "color: #cce8e0; font-weight: 600; font-size: 3rem; padding: 20px; text-align: center;"),
        )
      )
    } else {
      tagList(
        plotOutput("grafico_evolucao"),
        br(),
        hr(),
        br(),
        plotOutput("grafico_regressao")
      )
    }
  })
  
  output$grafico_evolucao <- renderPlot({
    req(dados_filtrados())
    
    tema_escolhido <- if (input$tema_plots == "Elegante") {
      my_pretty_theme()
    } else {
      theme_bw(base_size = tamanho_fonte_base())
    }
    
    p <- ggplot(dados_filtrados(), aes(x = Período, y = Frequência, color = Nome, group = Nome, linetype = Nome )) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 4) +
      geom_label(aes(label = scales::comma(Frequência, big.mark = ".", decimal.mark = ",")), 
                 vjust = -0.7, 
                 show.legend = FALSE,
                 size = 3) +
      labs(
        title = "Evolução do Número de Nascimentos",
        subtitle = paste("Nomes:", paste(input$nome_selecionado, collapse = ", ")),
        x = "Período",
        y = "Número de Nascimentos",
        color = "Nome"
      ) + 
      tema_escolhido +
      scale_y_continuous(
        labels = scales::comma_format(big.mark = ".", decimal.mark = ","),
        expand = expansion(mult = c(0.05, 0.2)),
        trans = if(input$logaritmica_linhas) "log10" else "identity"
      )
    
    # 4. Lógica do tema Escuro (Sobrescreve qualquer escolha anterior se estiver ativo)
    if (input$tema_atual == "dark") {
      p <- p + tema_escuro_ggplot + 
        theme(text = element_text(size = tamanho_fonte_base(), color = "white")) 
    }
    
    p
  })
  output$grafico_regressao <- renderPlot({
    req(dados_filtrados())
    
    tema_escolhido <- if (input$tema_plots == "Elegante") {
      my_pretty_theme()
    } else {
      theme_bw(base_size = tamanho_fonte_base())
    }
    
    p <- ggplot(dados_filtrados(), aes(x = Período, y = Frequência, color = Nome, group = Nome, linetype = Nome )) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 1.4) +
      labs(
        title = "Linhas de Regressão por Nome",
        subtitle = paste("Nomes:", paste(input$nome_selecionado, collapse = ", ")),
        x = "Período",
        y = "Número de Nascimentos",
        color = "Nome"
      ) +
      tema_escolhido +
      scale_y_continuous(
        labels = scales::comma_format(big.mark = ".", decimal.mark = ","),
        expand = expansion(mult = c(0.05, 0.2)),
        trans = if(input$logaritmica_linhas) "log10" else "identity"
      )
    
    if (input$tema_atual == "dark") {
      p <- p + tema_escuro_ggplot + 
        theme(text = element_text(size = tamanho_fonte_base(), color = "white"))
    }
    
    p
  })
  
  
  
  # 2. Tabela ----
  output$tabela_dados_conteiner <- renderUI({
    if (length(input$nome_selecionado) == 0) {
      tags$div(
        style = "height: 60vh; display: flex; align-items: center; justify-content: center; flex-direction: column;",
        tags$div(
          tags$p("Selecione um nome na aba lateral", 
                 style = "color: #cce8e0; font-weight: 600; font-size: 3rem; padding: 20px; text-align: center;"),
        )
      )
    } else {
      tagList(
        DTOutput("tabela_dados")
      )
    }
  })
  
  output$tabela_dados <- renderDT({
    req(dados_filtrados())
    
    df <- dados_filtrados() 
    
    df_normalizado <- df |> 
      mutate(Total_Filtro = sum(Frequência)) |> 
      mutate('Frequência Relativa' = (Frequência / Total_Filtro)) |> 
      ungroup() |>
      select(Nome, Localidade, Frequência, 'Frequência Relativa', Período)
    
    datatable(
      df_normalizado,
      filter = "top",
      rownames = FALSE,
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        language = list(
          search = 'Pesquisar', 
          lengthMenu = 'Exibir _MENU_ registros por página',
          info = 'Mostrando _START_ até _END_ de _TOTAL_ registro(s)',
          infoEmpty = 'Nenhum registro disponível',
          infoFiltered = '(filtrado de _MAX_ registro(s))',
          zeroRecords = 'Nenhum registro correspondente encontrado',
          paginate = list(
            first = 'Primeiro', 
            previous = 'Anterior', 
            `next` = 'Próximo', 
            last = 'Último'
          ),
          buttons = list(
            copyTitle = 'Copiado para a área de transferência',
            copySuccess = list(
              `1` = '1 linha copiada com sucesso',
              `_` = '%d linhas copiadas com sucesso'
            ),
            copy = 'Copiar',
            csv = 'Baixar como CSV',
            excel = 'Baixar como Excel'
          )
        )
      ),
      extensions = 'Buttons'
    )|> 
      formatPercentage('Frequência Relativa', digits = 1) |> 
      formatStyle(
        'Frequência',
        background = styleColorBar(range(df_normalizado$Frequência), 'lightblue')
      )
  })
  
  
  
  # 3. Heatmap (Com Lógica de Proporção) ----
  output$heatmap_iniciais_conteiner <- renderUI({
    # A lógica muda: se NÃO tiver seleção E o botão de "ver todos" estiver DESLIGADO, mostra aviso.
    if (length(input$nome_selecionado) == 0 && !isTRUE(input$ver_todos_heatmap)) {
      tags$div(
        style = "height: 60vh; display: flex; align-items: center; justify-content: center; flex-direction: column;",
        tags$div(
          tags$p("Selecione um nome na aba lateral", 
                 style = "color: #cce8e0; font-weight: 600; font-size: 3rem; padding: 20px; text-align: center;"),
          tags$p("Ou ative a opção 'Visualizar TODOS' acima", 
                 style = "color: #999; font-size: 1.2rem; text-align: center;")
        )
      )
    } else {
      tagList(
        br(),
        # Aumentei a altura dinamicamente dependendo da quantidade de dados se for "todos"
        plotOutput("heatmap_iniciais", height = if(isTRUE(input$ver_todos_heatmap)) "1200px" else "800px")
      )
    }
  })
  
  output$heatmap_iniciais <- renderPlot({
    # Removemos o req(dados_filtrados()) padrão e fazemos uma lógica manual
    req(length(input$nome_selecionado) > 0 || isTRUE(input$ver_todos_heatmap))
    
    # Lógica para decidir qual base usar
    df_heatmap <- if (isTRUE(input$ver_todos_heatmap)) {
      limpo_nomes # Usa a base COMPLETA
    } else {
      dados_filtrados() # Usa só a seleção lateral
    }
    
    # Lógica condicional baseada no RadioButton
    if (input$tipo_heatmap == "pico") {
      # Comparar consigo mesmo (0 a 100% do máximo histórico do nome)
      df_heatmap <- df_heatmap %>%
        group_by(Nome) %>%
        mutate(Valor_Plot = Frequência / max(Frequência)) %>%
        ungroup()
      
      titulo_legenda <- "% do Pico"
      subtitulo_plot <- "100% = Momento de maior auge daquele nome específico"
      
    } else {
      # Comparar com o total da população (Frequência / Total de Nascimentos na década)
      # PLACEHOLDER: Para normalização por população da década, seria necessário:
      # 1. Obter dados populacionais do IBGE por década
      # 2. Criar um vetor/tabela com: populacao_decada <- c("1930-1940" = 41236315, ...)
      # 3. Fazer join com df_heatmap baseado no período
      # 4. Calcular: Valor_Plot = (Frequência / populacao_decada) * 100000
      #    (para expressar como "por 100 mil habitantes")
      # 
      # Implementação temporária (NOTA: Total_Nascimentos_Periodo precisa ser criado):
      # Calcular total de nascimentos por período para normalização:
      total_por_periodo <- limpo_nomes %>%
        group_by(Período) %>%
        summarise(Total_Periodo = sum(Frequência), .groups = "drop")
      
      df_heatmap <- df_heatmap %>%
        left_join(total_por_periodo, by = "Período") %>%
        mutate(Valor_Plot = (Frequência / Total_Periodo))  # Já é uma fração, scale_fill_viridis_c formatará como %
      
      titulo_legenda <- "% da População"
      subtitulo_plot <- "Porcentagem de crianças nascidas naquele período com este nome (Corrige o aumento populacional)"
    }
    
    # Escolha do tema base
    tema_escolhido <- if (input$tema_plots == "Elegante") {
      my_pretty_theme()
    } else {
      theme_minimal(base_size = tamanho_fonte_base())
    }
    
    p <- ggplot(df_heatmap, aes(x = Período, y = Nome, fill = Valor_Plot)) +
      geom_tile(color = "white", linewidth = 1.2) +  # Aumentado o espaçamento entre tiles
      scale_fill_viridis_c(option = "plasma", labels = scales::percent, name = titulo_legenda) +
      labs(
        title = "Mapa de Calor de Popularidade",
        subtitle = subtitulo_plot,
        x = "Período",
        y = "Nome"
      ) +
      tema_escolhido +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(1)),  # Melhor legibilidade
        axis.text.y = element_text(size = rel(1.1), face = "bold"),  # Nomes mais legíveis
        panel.grid = element_blank(),
        axis.title.x = element_text(margin = ggplot2::margin(t = 15)),  # Mais espaçamento
        axis.title.y = element_text(margin = ggplot2::margin(r = 15)),  # Mais espaçamento
        legend.position = "right",
        legend.key.height = unit(1.5, "cm"),  # Legenda mais alta para melhor visualização
        plot.margin = ggplot2::margin(10, 10, 10, 10)  # Margem ao redor do plot
      )
    
    # Aplicar tema escuro se ativo
    if (!is.null(input$tema_atual) && input$tema_atual == "dark") {
      p <- p + tema_escuro_ggplot + 
        theme(text = element_text(size = tamanho_fonte_base(), color = "white"))
    }
    
    p
  })
  
  
  
  # 4. Histograma ----
  output$histograma_comprimento <- renderPlot({
    req(input$periodo_hist)
    df_periodo <- limpo_nomes %>% 
      filter(Período == input$periodo_hist)
    
    tema_escolhido <- if (input$tema_plots == "Elegante") {
      my_pretty_theme()
    } else {
      theme_bw(base_size = tamanho_fonte_base())
    }
    
    p <- ggplot(df_periodo, aes(x = Comprimento, weight = Frequência))
    
    if (input$dividir_sexo) {
      p <- ggplot(df_periodo, aes(x = Comprimento, weight = Frequência, fill = Sexo)) +
        geom_histogram(binwidth = 1, position = "dodge",
                       color = cor_bg_reativa(),
                       alpha = 0.8)
    } else {
      p <- p + geom_histogram(binwidth = 1, fill = "skyblue",
                              color = cor_bg_reativa(),
                              alpha = 0.8)
    }
    
    p <- p + 
      labs(
        title = "Distribuição do Tamanho dos Nomes",
        subtitle = paste("Período:", input$periodo_hist),
        x = "Número de Letras",
        y = "Total de Nascimentos"
      ) +
      tema_escolhido +
      scale_y_continuous(
        labels = scales::comma_format(big.mark = ".", decimal.mark = ","),
        expand = expansion(mult = c(0, 0.1))
      ) +
      scale_x_continuous(breaks = seq(0, max(df_periodo$Comprimento), by = 1))
    
    if (input$tema_atual == "dark") {
      p <- p + tema_escuro_ggplot + 
        theme(text = element_text(size = tamanho_fonte_base(), color = "white"))
    }
    
    p
  })
  
  output$barras_comprimento <- renderPlot({
    req(input$periodo_hist)
    df_periodo <- limpo_nomes %>% 
      filter(Período == input$periodo_hist, Comprimento == input$numero_letras)
    
    tema_escolhido <- if (input$tema_plots == "Elegante") {
      my_pretty_theme()
    } else {
      theme_bw(base_size = tamanho_fonte_base())
    }
    
    p <- ggplot(df_periodo, aes(x = reorder(Nome, Frequência), y = Frequência))
    
    if (input$dividir_sexo) {
      df_periodo <- df_periodo %>%
        mutate(Frequência = ifelse(Sexo == "Masculino", -Frequência, Frequência))
      p <- ggplot(df_periodo, aes(x = reorder(Nome, Frequência), y = Frequência, fill = Sexo)) +
        geom_col(alpha = 0.8) +
        coord_flip()
    } else {
      p <- p + geom_col(fill = "skyblue", alpha = 0.8) +
        coord_flip()
    }
    
    p <- p + 
      labs(
        title = "Distribuição dos Nomes por Comprimento",
        subtitle = paste("Período:", input$periodo_hist, "| Letras:", input$numero_letras),
        x = "Nome",
        y = "Total de Nascimentos"
      ) +
      tema_escolhido +
      scale_y_continuous(
        labels = function(x) scales::comma_format(big.mark = ".", decimal.mark = ",")(abs(x)),
        expand = expansion(mult = c(0, 0.1))
      ) + 
      scale_fill_manual(
        values = c(
          "Feminino" = "#f8766d",
          "Masculino" = "#00bfc4"
        )
      )
    
    if (input$tema_atual == "dark") {
      p <- p + tema_escuro_ggplot + 
        theme(text = element_text(size = tamanho_fonte_base(), color = "white"))
    }
    
    p
  })
  
  
  
  # 5. Nuvem de Palavras ----
  output$nuvem_nomes <- renderWordcloud2({
    req(input$periodo_nuvem_slider)
    
    df_periodo <- limpo_nomes %>%
      filter(Período == input$periodo_nuvem_slider) %>% 
      select(Nome, Frequência, Inicial) %>%
      filter(Frequência > 0)
    
    if (input$usar_iniciais) {
      df_periodo <- df_periodo %>%
        group_by(Inicial) %>% 
        summarise(Frequência = sum(Frequência), .groups = "drop")
    }
    
    df_periodo <- df_periodo %>% 
      arrange(desc(Frequência)) %>% 
      head(200)
    
    wordcloud2(
      df_periodo, size = 1,
      color = ifelse(input$tema_atual == "dark", "random-light", "random-dark"),
      backgroundColor = cor_bg_reativa()
    )
  })
  
  
  
  # 6. Caça-Palavras ----
  
  # Assegure-se de que suas palavras estão limpas e disponíveis
  minhas_palavras <- unique(limpo_nomes$Nome)
  
  ws_data <- reactiveVal(NULL)
  
  # 2. Gera o jogo (ao iniciar ou clicar no botão)
  observe({
    # Garante que existem palavras para usar
    req(minhas_palavras) 
    
    # Cria o objeto do pacote worrrd
    ws <- wordsearch(words = minhas_palavras, r = input$tamanho, c = input$tamanho)
    ws_data(ws)
  }) %>% bindEvent(input$gerar, ignoreNULL = FALSE)
  
  # 3. Renderiza o Gráfico (SEM a lista lateral)
  output$caca_palavras <- renderPlot({
    req(ws_data())
    
    df_letras <- tibble::tibble(
      row = rep(seq_len(nrow(ws_data()$search)), times = ncol(ws_data()$search)),
      col = rep(seq_len(ncol(ws_data()$search)), each = nrow(ws_data()$search)),
      value = as.vector(ws_data()$search),
      word  = as.vector(!is.na(ws_data()$solution))
    ) %>% filter(!is.na(value))
    
    p <- ggplot(df_letras, aes(col, row, label = value)) +
      geom_text(size = 18) +
      scale_y_reverse() +
      coord_fixed() +
      theme_void()
    
    if (input$tema_atual == "dark") {
      p <- p +
        geom_text(size = 18, color = "white")
    }
    
    # Lógica segura para mostrar ou não a resposta
    mostrar_solucao <- if (isTRUE(input$ver_resposta)) TRUE else FALSE
    
    if (mostrar_solucao) {
      positions <- attr(ws_data()$search, "positions")
      p <- p + geom_line(
        data = positions,
        aes(x = j, y = i, group = word),
        color = "red",
        size = 2,
        inherit.aes = FALSE
      )
    }
    
    print(p)
  }, bg = cor_bg_reativa)
  
  # 4. Renderiza a Lista de Palavras (TEXTO na barra lateral)
  output$lista_palavras <- renderUI({
    req(ws_data())
    
    # Pega as palavras do jogo atual
    palavras_no_jogo <- sort(unique(ws_data()$words))
    
    # Cria o grupo de botões
    checkboxGroupButtons(
      inputId = "palavras_encontradas", # ID para saber quais foram clicados
      label = "Marque as palavras que você encontrou:",
      choices = palavras_no_jogo,
      
      # Configuração Visual
      status = "success",       # "success" deixa VERDE quando clicado. Use "primary" para azul.
      size = "sm",              # Tamanho dos botões (sm = pequeno)
      direction = "horizontal", # Botões um ao lado do outro
      individual = TRUE,        # Botões separados visualmente
      
      # Ícones (opcional: check quando clicado, nada quando não)
      checkIcon = list(
        yes = icon("check"), 
        no = icon("magnifying-glass") # Ou NULL se não quiser ícone
      )
    )
  })
  
  
  
  # 7. Notificações Nascimentos ----
  intervalo <- (1000 * 60 * 60 * 24 * 365 * 10) /
    sum(limpo_nomes[limpo_nomes$Período == "2000 a 2010", ]$Frequência)
  
  observe({
    invalidateLater(intervalo, session)
    
    if (input$mostrar_notificacoes) {
      showNotification("Nasceu alguém com um nome da sala!", type = "message", duration = 8)
    }
  })
  
  
  
  # a) Código: Gráfico ----
  observeEvent(input$aba_linhas_dblclick, {
    mostrar_modal_codigo(
      input = input,
      titulo = "Código: Evolução (Linhas)",
      id = "linhas",
      codigo_ui = CODIGO_UI_LINHAS,
      codigo_server = CODIGO_SERVER_LINHAS
    )
  }, ignoreInit = TRUE)
  
  # b) Código: Tabela ----
  observeEvent(input$aba_tabela_dblclick, {
    mostrar_modal_codigo(
      input = input,
      titulo = "Código: Tabela Detalhada",
      id = "tabela",
      codigo_ui = CODIGO_UI_TABELA,
      codigo_server = CODIGO_SERVER_TABELA
    )
  }, ignoreInit = TRUE)
  
  # c) Código: Heatmap (Com Lógica de Proporção) ----
  observeEvent(input$aba_heatmap_dblclick, {
    mostrar_modal_codigo(
      input = input,
      titulo = "Código: Mapa de Calor",
      id = "heatmap",
      codigo_ui = CODIGO_UI_HEATMAP,
      codigo_server = CODIGO_SERVER_HEATMAP
    )
  }, ignoreInit = TRUE)
  
  # d) Código: Histograma ----
  observeEvent(input$aba_histograma_dblclick, {
    mostrar_modal_codigo(
      input = input,
      titulo = "Código: Histograma (Tamanho)",
      id = "histograma",
      codigo_ui = CODIGO_UI_HISTOGRAMA,
      codigo_server = CODIGO_SERVER_HISTOGRAMA
    )
  }, ignoreInit = TRUE)
  
  # e) Código: Nuvem de Palavras ----
  observeEvent(input$aba_nuvem_dblclick, {
    mostrar_modal_codigo(
      input = input,
      titulo = "Código: Nuvem de Palavras",
      id = "nuvem",
      codigo_ui = CODIGO_UI_NUVEM,
      codigo_server = CODIGO_SERVER_NUVEM
    )
  }, ignoreInit = TRUE)
  
  # f) Código: Caça-Palavras ----
  observeEvent(input$aba_caça_palavras_dblclick, {
    mostrar_modal_codigo(
      input = input,
      titulo = "Código: Caça-Palavras",
      id = "caca_palavras",
      codigo_ui = CODIGO_UI_CACA_PALAVRAS,
      codigo_server = CODIGO_SERVER_CACA_PALAVRAS
    )
  }, ignoreInit = TRUE)
  
  # g) Código: Pacotes ----
  observeEvent(input$codigo_pacotes, {
    mostrar_modal_codigo(
      input = input,
      titulo = "Código: Pacotes",
      id = "pacotes",
      codigo = CODIGO_PACOTES,
      isUiServer = F
    )
  }, ignoreInit = TRUE)
  
  # h) Código: Tratamento ----
  observeEvent(input$codigo_tratamento, {
    mostrar_modal_codigo(
      input = input,
      titulo = "Código: Tratamento",
      id = "tratamento",
      codigo = CODIGO_TRATAMENTO,
      isUiServer = F
    )
  }, ignoreInit = TRUE)
  
  # i) Código: Valores ----
  observeEvent(input$codigo_valores, {
    mostrar_modal_codigo(
      input = input,
      titulo = "Código: Valores",
      id = "valores",
      codigo = CODIGO_VALORES,
      isUiServer = F
    )
  }, ignoreInit = TRUE)
  
  # j) Código: Funções ----
  observeEvent(input$codigo_funcoes, {
    mostrar_modal_codigo(
      input = input,
      titulo = "Código: Funções",
      id = "funcoes",
      codigo = CODIGO_FUNCOES,
      isUiServer = F
    )
  }, ignoreInit = TRUE)
}



# shinyApp ----
shinyApp(ui, server)
