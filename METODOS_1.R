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

# tratamento ----
df_nomes <- readRDS("nomes_lista.rds") |>
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
  panel.grid.major = element_line(color = "gray60"),
  panel.grid.minor = element_line(color = "gray80"),
  axis.text = element_text(color = "white"),
  axis.title = element_text(color = "white"),
  plot.title = element_text(color = "white", hjust = 0.5),
  plot.subtitle = element_text(color = "white", hjust = 0.5),
  plot.caption = element_text(color = "white"),
  legend.background = element_rect(fill = cor_bg_escuro),
  legend.text = element_text(color = "white"),
  legend.title = element_text(color = "white")
)

# ui ----
ui <- navbarPage(
  theme = bs_theme(bootswatch = "minty"),
  title = "Dashboard de Nomes (IBGE)",
  nav_item(input_dark_mode(id = "tema_atual", mode = "light")),
  
  # Aba Principal com Sidebar
  tabPanel("Análise Geral",
           page_sidebar(
             sidebar = sidebar(
               h4("Filtros Principais"),
               selectInput("nome_selecionado", 
                           "Escolha um ou mais Nomes:", 
                           choices = NULL,    
                           selected = NULL,
                           multiple = TRUE)
             ),
             
             tabsetPanel(
               tabPanel("Evolução (Linhas)",
                        uiOutput("grafico_evolucao_conteiner")),
               
               tabPanel("Tabela Detalhada", 
                        uiOutput("tabela_dados_conteiner")),
               
               tabPanel("Distribuição (Boxplot)",
                        uiOutput("boxplot_nomes_conteiner")),
               
               tabPanel("Histograma (Tamanho)",
                        br(),
                        fluidRow(
                          column(6, selectInput("periodo_hist", "Escolha o Período:", choices = periodos_ordenados)),
                          column(6, checkboxInput("dividir_sexo", "Dividir por sexo", value = FALSE))
                        ),
                        plotOutput("histograma_comprimento")),
               
               tabPanel("Nuvem de Palavras",
                        br(),
                        fluidRow(
                          column(8,
                                 sliderTextInput(
                                   inputId = "periodo_nuvem_slider", 
                                   label = "Período de Referência:", 
                                   choices = periodos_ordenados,
                                   selected = periodos_ordenados[1],
                                   animate = animationOptions(interval = 1000),
                                   grid = TRUE
                                 )
                          ),
                          column(4,
                                 checkboxInput("usar_iniciais", "Agrupar por iniciais", value = FALSE)
                          )
                        ),
                        wordcloud2Output("nuvem_nomes", height = "600px")
               )
             )
           )
  )
)

# server ----
server <- function(input, output, session) {
  
  updateSelectInput(session, "nome_selecionado", choices = sort(unique(limpo_nomes$Nome)))
  
  dados_filtrados <- reactive({
    req(input$nome_selecionado)
    limpo_nomes |> filter(Nome %in% input$nome_selecionado) |> arrange(Começo)
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
        br(),
        checkboxInput("logaritmica_linhas", "Utilizar escala logarítmica", value = FALSE),
        plotOutput("grafico_evolucao") 
      )
    }
  })
  
  output$grafico_evolucao <- renderPlot({
    req(dados_filtrados())
    
    ggplot(dados_filtrados(), aes(x = Período, y = Frequência, 
                                  color = Nome, group = Nome)) +
      geom_line(size = 1.2) +
      geom_point(size = 4) +
      geom_label(aes(label = scales::comma(Frequência, big.mark = ".")), 
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
        labels = scales::comma_format(big.mark = "."),
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
        br(),
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
  
  # 3. Boxplot ----
  output$boxplot_nomes_conteiner <- renderUI({
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
        checkboxInput("logaritmica_boxplot", "Utilizar escala logarítmica", value = FALSE),
        h5("Distribuição da frequência dos nomes selecionados"),
        plotOutput("boxplot_nomes")
      )
    }
  })
  
  output$boxplot_nomes <- renderPlot({
    req(dados_filtrados())
    ggplot(dados_filtrados(), aes(x = Nome, y = Frequência, fill = Nome)) +
      geom_boxplot(alpha = 0.7, show.legend = FALSE) + 
      geom_jitter(color = ifelse(input$tema_atual == "dark", "white", "black"), width = 0.2, alpha = 0.5) +
      labs(x = "Nome", y = "Frequência", title = "Variabilidade da Frequência") +
      theme_bw() + 
      theme(text = element_text(size = 14)) +
      {if (input$tema_atual == "dark") tema_escuro_ggplot} +
      {if (input$logaritmica_boxplot) scale_y_log10()}
  })
  
  # 4. Histograma ----
  output$histograma_comprimento <- renderPlot({
    req(input$periodo_hist)
    df_periodo <- limpo_nomes %>% 
      filter(Período == input$periodo_hist)
    
    p <- ggplot(df_periodo, aes(x = Comprimento, weight = Frequência))
    if (input$dividir_sexo) {
      p <- ggplot(df_periodo, aes(x = Comprimento, weight = Frequência, fill = Sexo)) +
        geom_histogram(binwidth = 1, position = "dodge", color = "white", alpha = 0.8)
    } else {
      p <- p + geom_histogram(binwidth = 1, fill = "skyblue", color = "white", alpha = 0.8)
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
        labels = scales::comma_format(big.mark = "."),
        expand = expansion(mult = c(0, 0.1))
      ) +
      scale_x_continuous(breaks = seq(0, max(df_periodo$Comprimento), by = 1))
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
}

# shinyApp ----
shinyApp(ui, server)
