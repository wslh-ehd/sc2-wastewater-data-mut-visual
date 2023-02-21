library(shiny)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(viridis)
library(tidyr)
library(lubridate)
library(shinycssloaders)



load("plot.RData")


ui <- fluidPage(
  
  titlePanel("SNP that are NOT VoC-associated (but observed in previous described variants)"),
  
  sidebarLayout(
    
    sidebarPanel(
      sliderInput("choice_sampleoccurence", "Highlight SNPs occuring in at least X samples within last 3 months:", min = 0, max = 10, value = 3),
      sliderInput("choice_mutpercent", "With a minimum frequency (%) of:", min = 0, max = 50, value = 5),
      pickerInput("choice_city", "Select a city:", choices = levels(as.factor(data.snp.notvoc$sites)), multiple = TRUE, options = list(`actions-box` = TRUE), selected = levels(as.factor(data.snp.notvoc$sites))),
      pickerInput("choice_run", "Select a run:", choices = levels(as.factor(data.snp.notvoc$run)), multiple = TRUE, options = list(`actions-box` = TRUE), selected = levels(as.factor(data.snp.notvoc$run))),
      #textInput("choice_mut.nt", "Select a mutation (nt):", value = "Enter nt mutation"),
      #textInput("choice_mut.aa", "Select a mutation (aa):", value = "Enter aa mutation"),
      radioButtons("choice_n1n2", "Highlight N1/N2 primer/probe sites", choices = list("All" = 1, "N1" = 2, "N2" = 3), selected = 1),
      radioButtons("choice_display", "Samples ordered by:", choices = list("wwtp name" = 1, "date" = 2), selected = 2)
      ),
    
    mainPanel(
      withSpinner(plotlyOutput("graph.KNOWN"), color="#c5050c"),
      #hr(),
      #withSpinner(plotlyOutput("graph.UNKNOWN"), color="#c5050c")
    )
  )
)



server <- function(input, output, session){
  
  SNP.list <- reactive({
    req(input$choice_mutpercent)
    req(input$choice_sampleoccurence)
  
    data.snp.notvoc %>% 
      filter(ALT_FREQ >= input$choice_mutpercent) %>% 
      group_by(mutation.nt) %>% 
      summarise(count = n()) %>%
      filter(count >= input$choice_sampleoccurence)
  })
  
  
  selectedData <- reactive({
    req(input$choice_city)
    req(input$choice_run)
    #req(input$choice_mut.nt)
    #req(input$choice_mut.aa)
    req(input$choice_n1n2)
    req(input$choice_display)


    data.snp.notvoc %>%
      filter(mutation.nt %in% SNP.list()$mutation.nt) %>%
      filter(sites %in% input$choice_city) %>%
      filter(run %in% input$choice_run) %>%
      #filter(mutation.nt %in% input$choice_mut.nt) %>%
      #filter(mutation.aa %in% input$choice_mut.aa) %>%
      filter(case_when(input$choice_n1n2==1 ~ POS %in% c(1:30000),
                       input$choice_n1n2==2 ~ POS %in% c(28287:28306, 28309:28332, 28335:28358),
                       input$choice_n1n2==3 ~ POS %in% c(29164:29183, 29188:29210, 29213:29230)))
  })

  
  
  selectedData.KNOWN <- reactive({
    selectedData() %>% filter(!is.na(lineage)) %>%
      mutate(
        Samples = forcats::fct_reorder(
          Samples,
          if(input$choice_display == "1"){-desc(sites)}else{-desc(Date)}))
  })
  
  
  # selectedData.UNKNOWN <- reactive({
  #   selectedData() %>% filter(is.na(lineage)) %>%
  #     mutate(
  #       Samples = forcats::fct_reorder(
  #         Samples,
  #         if(input$choice_display == "1"){-desc(Date)}else{desc(sites)}))
  # })
  

  
  output$graph.KNOWN <- renderPlotly({
    
    plot_ly(height = length(unique(selectedData.KNOWN()$mutation.nt))*10,
            width = length(unique(selectedData.KNOWN()$Samples))*8) %>%
      add_trace(data=selectedData.KNOWN(), 
                x = ~Samples, 
                y = ~reorder(display, -POS),
                showlegend = TRUE,
                text =~paste0( "</br>", Samples, "</br>Mut_nt: ", mutation.nt, "</br>Mut_aa: ", mutation.aa, "</br>Frequency(%): ", ALT_FREQ,  "</br>Depth: ", TOTAL_DP, "</br>Lineages ID: ", lineage, "</br>Run: ", run), 
                type = 'scatter', 
                mode = 'markers',
                marker = list(color = ~ALT_FREQ,
                              colorscale = 'Viridis',
                              reversescale = T,
                              opacity = ~(TOTAL_DP/100))
    ) %>%
      layout(
        xaxis = list(titlefont = list(size = 3), 
                     tickfont = list(size = 3)),
                     #categoryorder = "array",
                     #categoryarray = factor(order4_xaxis)),
        yaxis = list(titlefont = list(size = 5), tickfont = list(size = 3)))
    
  })
  
  
  
  
  
  
  # output$graph.UNKNOWN <- renderPlotly({
  #   
  #   plot_ly(height = length(unique(selectedData.UNKNOWN()$mutation.nt))*10,
  #           width = length(unique(selectedData.UNKNOWN()$Samples))*8) %>%
  #     add_trace(data=selectedData.UNKNOWN(), 
  #               x = ~Samples, 
  #               y = ~reorder(display, -POS),
  #               showlegend = TRUE,
  #               text =~paste0( "</br>", Samples, "</br>Mut_nt: ", mutation.nt, "</br>Mut_aa: ", mutation.aa, "</br>Frequency(%): ", ALT_FREQ,  "</br>Depth: ", TOTAL_DP, "</br>Lineages ID: ", lineage, "</br>Run: ", run), 
  #               type = 'scatter', 
  #               mode = 'markers',
  #               marker = list(color = ~ALT_FREQ,
  #                             colorscale = 'Viridis',
  #                             reversescale = T,
  #                             opacity = ~(TOTAL_DP/100))
  #     ) %>%
  #     layout(
  #       xaxis = list(titlefont = list(size = 3), 
  #                    tickfont = list(size = 3),
  #                    categoryorder = "array",
  #                    categoryarray = factor(order4_xaxis)),
  #       yaxis = list(titlefont = list(size = 5), tickfont = list(size = 3)))
  #   
  # })
  
}



shinyApp(ui, server)
