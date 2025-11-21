library(shiny)
library(tidyverse)
library(DT)


lista_nomes <- bind_rows(lista_nomes) # Ele a lista de tabelas e coloca uma embaixo da outra

sujo_nomes <- df_nomes |> 
  unnest_auto(res) # na lista o res tinha uma lista dentro, o unest abriu o conteúdo detalhado (anos e frequências) que estava compactado dentro de cada nome

limpo_nomes <- sujo_nomes |> 
  mutate(periodo_limpo = gsub("[^0-9,]", "", periodo)) |>
  separate(
    periodo_limpo, 
    into = c("ano_inicio", "ano_fim"), 
    sep = ",",
    convert = TRUE # Converte para numero 
  ) |>
  mutate(
    Começo = ifelse(ano_inicio == 1930 & is.na(ano_fim), 1929, ano_inicio),  # tem que revisar a forma que eu to fazendo, mas ta funcionando
    Fim = ifelse(is.na(ano_fim) & ano_inicio < 1940, 1930, ano_fim),
    
    Periodo = case_when(
      Começo < 1930 ~ "Antes de 1930",             #faz as legendas do período 
      TRUE ~ paste(Começo, "a", Fim)),
    
    Periodo = fct_reorder(Periodo, Começo) # Transforma em fator ordenado pelo Começo
  ) |>
  select(Nome = nome, 
         Localidade = localidade, 
         Frequência = frequencia, 
         Periodo,
         Começo,
         Fim)


ui <- fluidPage(
  theme = NULL, 
  
  titlePanel("Dashboard de Nomes (IBGE)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filtros"),
      selectInput("nome_selecionado", 
                  "Escolha um ou mais Nomes:", 
                  choices = NULL,    
                  selected = NULL,
                  multiple = TRUE),  # Ativa seleção múltipla
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Gráfico", 
                 plotOutput("grafico_evolucao")
        ),
        tabPanel("Tabela", 
                 DTOutput("tabela_dados")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  updateSelectInput(
    inputId = "nome_selecionado", 
    choices = sort(unique(limpo_nomes$Nome))
  )
  
  dados_filtrados <- reactive({
    req(input$nome_selecionado)
    
    limpo_nomes |> 
      filter(Nome %in% input$nome_selecionado) |>  
      arrange(Começo)                        #ordena pelo ano inicial
  })
  
  output$grafico_evolucao <- renderPlot({
    req(dados_filtrados())
    
    # Define paleta de cores para múltiplos nomes IA contribuition
    cores <- scales::hue_pal()(length(input$nome_selecionado))
    
    ggplot(dados_filtrados(), aes(x = Periodo, y = Frequência, 
                                  color = Nome, group = Nome)) +
      geom_line(size = 1.2) +
      geom_point(size = 4) +
      geom_label(aes(label = scales::comma(Frequência, big.mark = ".")), 
                 vjust = -0.7, 
                 show.legend = FALSE,
                 size = 3) +
      labs(
        title = paste("Evolução do Número de Nascimentos"),
        subtitle = paste("Nomes:", paste(input$nome_selecionado, collapse = ", ")),  #isso aqui foi IA porque n tava sabendo como fazer
        x = "Período",
        y = "Número de Nascimentos",
        color = "Nome" # associando uma cor a cada nome IA contribution 
      ) +
      theme_bw() +
      scale_y_continuous(
        labels = scales::comma_format(big.mark = "."),
        expand = expansion(mult = c(0.05, 0.2))    # aumentar o plot pq tava cordando a label 
      ) +
      theme(
        axis.title.x = element_text(margin = margin(t = 10), size = 14), # aumentando os espaço entre o titulo do eixo e o plot
        axis.title.y = element_text(margin = margin(r = 10), size = 14),
      )
  })
  # DT e um pacote para tabelas interativas
  output$tabela_dados <- renderDT({
    
    
    df <- dados_filtrados() 
    
    df_normalizado <- df |> 
      mutate('Frequência Relativa' = (Frequência / sum(Frequência))) |> 
      select(Nome, Localidade, Frequência, 'Frequência Relativa', -Começo, -Fim)
    
    datatable(
      df_normalizado,
      filter = "top",
      rownames = FALSE,
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',   # isso aqui e pata definir a ordem que os elementos UI da tabela vao aparecer, n sei exatamente o porque mas a IA recomendou
        buttons = c('copy', 'csv', 'excel')
      ),
      extensions = 'Buttons'
    )|> 
      # Formata a coluna de porcentagem
      formatPercentage('Frequência Relativa', digits = 1) |> 
      formatStyle(
        'Frequência',
        background = styleColorBar(df$Frequência, 'lightblue') # Barras na frequência isso daqui tmb pedi ajuda da IA
      )
  })
}

shinyApp(ui, server)