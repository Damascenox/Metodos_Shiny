# pacotes ----
# install.packages(c(
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
library(bsicons)

# tratamento ----
df_nomes <- readRDS("nomes_lista") |>
  bind_rows() |>
  as_tibble()

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
adicionar_tags_script <- function(painel, aba) {
  input_id <- paste0(aba, "_dblclick")
  
  tags$script(
    sprintf(
      "$('#%s a[data-value=\"%s\"]').on('dblclick', function(e) {
                     e.preventDefault();
                     Shiny.setInputValue('%s', new Date().getTime(), {priority: 'event'});
                   });", 
      painel, aba, input_id
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

# ui ----
ui <- navbarPage(
  fillable = TRUE,
  theme = bs_theme(bootswatch = "minty",
                   secondary = "#7fd1ae",),
  title = "Dashboard de Nomes (IBGE)",
  nav_item(input_dark_mode(id = "tema_atual", mode = "light")),
  
  useShinyjs(), # pra controlar quando o checkbox etc aparecem, preservando o estado
  
  # Aba Principal com Sidebar Moderna ----
  tabPanel("Análise Geral",
           page_sidebar(
             sidebar = sidebar(
               width = 300,
               # Estilização da barra
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
                               multiple = TRUE),
                 ),
                 
                 accordion_panel(
                   "Configurações",
                   icon = bs_icon("gear-fill"),
                   p("Filtros adicionais aparecerão aqui no futuro.", class = "small text-muted")
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
               id = "painel",
               
               # 1. Gráfico ----
               tabPanel("Evolução (Linhas)",
                        value = "aba_linhas",
                        br(),
                        div(
                          id = "log_checkbox_linhas",
                          checkboxInput("logaritmica_linhas", "Utilizar escala logarítmica", value = FALSE)
                        ),
                        uiOutput("grafico_evolucao_conteiner")),
               
               # 2. Tabela ----
               tabPanel("Tabela Detalhada",
                        value = "aba_tabela",
                        br(),
                        uiOutput("tabela_dados_conteiner")),
               
               # 3. Heatmap (Com Lógica de Proporção) ----
               tabPanel("Mapa de Calor",
                        br(),
                        tags$style(HTML("
                          #tipo_heatmap .radio-inline {
                            margin-right: 40px;
                          }
                        ")),
                        div(
                          id = "card_heatmap",
                          card(
                            card_header("Configuração do Mapa de Calor"),
                            radioButtons(
                              "tipo_heatmap",
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
               
               # 4. Histograma ----
               tabPanel("Histograma (Tamanho)",
                        br(),
                        fluidRow(
                          column(6, selectInput("periodo_hist", "Escolha o Período:", choices = periodos_ordenados)),
                          column(6, checkboxInput("dividir_sexo", "Dividir por sexo", value = FALSE))
                        ),
                        plotOutput("histograma_comprimento"),
                        br(),
                        hr(),
                        br(),
                        tags$style(HTML("
                          #numero_letras .radio-inline {
                            margin-right: 20px;
                          }
                        ")),
                        radioButtons(
                          inputId = "numero_letras",
                          label = "Selecione o número de letras:",
                          choices = 3:9,
                          selected = 3,
                          inline = TRUE
                        ),
                        plotOutput("barras_comprimento")),
               
               # 5. Nuvem de Palavras ----
               tabPanel("Nuvem de Palavras",
                        br(),
                        # CSS Personalizado para forçar o slider a usar as cores do tema
                        tags$style(HTML("
                          .irs--shiny .irs-bar { border-top-color: var(--bs-primary); border-bottom-color: var(--bs-primary); background: var(--bs-primary); }
                          .irs--shiny .irs-handle { border: 1px solid var(--bs-primary); background-color: var(--bs-primary); }
                          .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background-color: var(--bs-primary); }
                        ")),
                        
                        # Layout Centralizado
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
               
               # Códigos ----
               adicionar_tags_script("painel", "aba_linhas"),
               adicionar_tags_script("painel", "aba_tabela")
             )
           )
  ))

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
      shinyjs::hide("card_heatmap")
    } else {
      shinyjs::show("log_checkbox_linhas")
      shinyjs::show("card_heatmap")
    }
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
        br(),
        plotOutput("heatmap_iniciais", height = "600px")
      )
    }
  })
  
  output$heatmap_iniciais <- renderPlot({
    req(dados_filtrados())
    
    df_heatmap <- dados_filtrados()
    
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
      df_heatmap <- df_heatmap %>%
        mutate(Valor_Plot = Frequência / Total_Nascimentos_Periodo)
      
      titulo_legenda <- "% da População"
      subtitulo_plot <- "Porcentagem de crianças nascidas naquele período com este nome (Corrige o aumento populacional)"
    }
    
    ggplot(df_heatmap, aes(x = Período, y = Nome, fill = Valor_Plot)) +
      geom_tile(color = "white", size = 0.5) +
      scale_fill_viridis_c(option = "plasma", labels = scales::percent, name = titulo_legenda) +
      labs(title = "Mapa de Calor de Popularidade",
           subtitle = subtitulo_plot) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        axis.text.y = element_text(size = 12, face = "bold")
      ) +
      {if (!is.null(input$tema_atual) && input$tema_atual == "dark") tema_escuro_ggplot}
  })
  
  # 4. Histograma ----
  output$histograma_comprimento <- renderPlot({
    req(input$periodo_hist)
    df_periodo <- limpo_nomes %>% 
      filter(Período == input$periodo_hist)
    
    p <- ggplot(df_periodo, aes(x = Comprimento, weight = Frequência))
    if (input$dividir_sexo) {
      p <- ggplot(df_periodo, aes(x = Comprimento, weight = Frequência, fill = Sexo)) +
        geom_histogram(binwidth = 1, position = "dodge",
                       color = ifelse(input$tema_atual == "dark", cor_bg_escuro, "white"),
                       alpha = 0.8)
    } else {
      p <- p + geom_histogram(binwidth = 1, fill = "skyblue",
                              color = ifelse(input$tema_atual == "dark", cor_bg_escuro, "white"),
                              alpha = 0.8)
    }
    p + 
      labs(
        title = "Distribuição do Tamanho dos Nomes",
        subtitle = paste("Período:", input$periodo_hist),
        x = "Número de Letras",
        y = "Total de Nascimentos"
      ) +
      theme_bw() +
      theme(
        axis.title.x = element_text(margin = ggplot2::margin(t = 10), size = 14),
        axis.title.y = element_text(margin = ggplot2::margin(r = 10), size = 14),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12)
      ) +
      {if (input$tema_atual == "dark") tema_escuro_ggplot} +
      scale_y_continuous(
        labels = scales::comma_format(big.mark = ".", decimal.mark = ","),
        expand = expansion(mult = c(0, 0.1))
      ) +
      scale_x_continuous(breaks = seq(0, max(df_periodo$Comprimento), by = 1))
  })
  
  output$barras_comprimento <- renderPlot({
    req(input$periodo_hist)
    df_periodo <- limpo_nomes %>% 
      filter(Período == input$periodo_hist, Comprimento == input$numero_letras)
    
    p <- ggplot(df_periodo, aes(x = reorder(Nome, Frequência), y = Frequência))
    if (input$dividir_sexo) {
      df_periodo <- df_periodo %>%
        mutate(Frequência = ifelse(Sexo == "Masculino", -Frequência, Frequência))
      p <- ggplot(df_periodo, aes(x = reorder(Nome, Frequência), y = Frequência, fill = Sexo)) +
        geom_col(alpha = 0.8) +
        coord_flip() +
        # sem isso, se tivesse só um nome masculino, ele seria rosa
        scale_fill_manual(
          values = c(
            "Feminino" = "#f8766d",
            "Masculino" = "#00bfc4"
          )
        )
    } else {
      p <- p + geom_col(fill = "skyblue", alpha = 0.8) +
        coord_flip()
    }
    p + 
      labs(
        title = "Distribuição dos Nomes por Comprimento",
        subtitle = paste("Período:", input$periodo_hist, "| Letras:", input$numero_letras),
        x = "Nome",
        y = "Total de Nascimentos"
      ) +
      theme_bw() +
      theme(
        axis.title.x = element_text(margin = ggplot2::margin(t = 10), size = 14),
        axis.title.y = element_text(margin = ggplot2::margin(r = 10), size = 14),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12)
      ) +
      {if (input$tema_atual == "dark") tema_escuro_ggplot} +
      scale_y_continuous(
        labels = function(x) scales::comma_format(big.mark = ".", decimal.mark = ",")(abs(x)),
        expand = expansion(mult = c(0, 0.1))
      )
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
      backgroundColor = ifelse(input$tema_atual == "dark", cor_bg_escuro, "white")
    )
  })
  
  # 6. Notificações Nascimentos ----
  intervalo <- (1000 * 60 * 60 * 24 * 365 * 10) /
    sum(limpo_nomes[limpo_nomes$Período == "2000 a 2010", ]$Frequência)
  
  observe({
    invalidateLater(intervalo, session)
    showNotification("Nasceu alguém com um nome da sala!", type = "message", duration = 8)
  })
  
  # a) Código: Gráfico ----
  observeEvent(input$aba_linhas_dblclick, {
    showModal(
      modalDialog(
        title = "Código: Gráfico de Linhas",
        footer = modalButton("Fechar"),
        easyClose = TRUE,
        size = "xl",
        
        tabsetPanel(
          tabPanel(
            title = "ui",
            aceEditor(
              outputId = "display_ui_linhas",
              mode = "r",
              theme = ifelse(input$tema_atual == "dark", "monokai", "github"),
              readOnly = TRUE,
              value = CODIGO_UI_LINHAS
            )
          ),
          
          tabPanel(
            title = "server",
            aceEditor(
              outputId = "display_server_linhas",
              mode = "r",
              theme = ifelse(input$tema_atual == "dark", "monokai", "github"),
              readOnly = TRUE,
              value = CODIGO_SERVER_LINHAS
            )
          )
        )
      )
    )
  }, ignoreInit = TRUE)
  
  # b) Código: Tabela ----
  observeEvent(input$aba_tabela_dblclick, {
    showModal(
      modalDialog(
        title = "código",
        footer = modalButton("Fechar"),
        easyClose = TRUE
      )
    )
  }, ignoreInit = TRUE)
  
  # e) Código: Pacotes ----
  observeEvent(input$codigo_pacotes, {
    showModal(
      modalDialog(
        title = "Código: Pacotes",
        footer = modalButton("Fechar"),
        easyClose = TRUE,
        size = "xl",
        
        aceEditor(
          outputId = "display_pacotes",
          mode = "r",
          theme = ifelse(input$tema_atual == "dark", "monokai", "github"),
          readOnly = TRUE,
          value = CODIGO_PACOTES
        )
      )
    )
  }, ignoreInit = TRUE)
}

# shinyApp ----
shinyApp(ui, server)
