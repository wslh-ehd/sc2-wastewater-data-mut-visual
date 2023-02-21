library(shiny)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(viridis)
library(lubridate)
library(shinycssloaders)



load("plot.RData")


ui <- fluidPage(
  
  titlePanel("SNP-VoC associated"),
  
  sidebarLayout(
    
    sidebarPanel(
      pickerInput("choice_city", "Select a city:", choices = levels(as.factor(data.snp.voc$sites)), multiple = TRUE, options = list(`actions-box` = TRUE), selected = levels(as.factor(data.snp.voc$sites))),
      pickerInput("choice_run", "Select a run:", choices = levels(as.factor(data.snp.voc$run)), multiple = TRUE, options = list(`actions-box` = TRUE), selected = levels(as.factor(data.snp.voc$run))),
      #textInput("choice_mut.nt", "Select a mutation (nt):", value = "Enter nt mutation"),
      #textInput("choice_mut.aa", "Select a mutation (aa):", value = "Enter aa mutation"),
      radioButtons("choice_n1n2", "Highlight N1/N2 primer/probe sites", choices = list("All" = 1, "N1" = 2, "N2" = 3), selected = 1),
      sliderInput("choice_no.snp", "SNP shared among # VoC", min = 1, max = max(data.snp.voc$SNP.lineage.count), value = c(1, max(data.snp.voc$SNP.lineage.count)))
      ),
    
    mainPanel(
      withSpinner(plotlyOutput("graph"), color="#c5050c") 
    )
  )
)



server <- function(input, output, session){
  
  selectedData <- reactive({
    req(input$choice_city)
    req(input$choice_run)
    #req(input$choice_mut.nt)
    #req(input$choice_mut.aa)
    req(input$choice_n1n2)
    req(input$choice_no.snp)

    data.snp.voc %>%
      filter(sites %in% input$choice_city) %>%
      filter(run %in% input$choice_run) %>%
      #filter(mutation.nt %in% input$choice_mut.nt) %>%
      #filter(mutation.aa %in% input$choice_mut.aa) %>%
      filter(case_when(input$choice_n1n2==1 ~ POS %in% c(1:30000),
                       input$choice_n1n2==2 ~ POS %in% c(28287:28306, 28309:28332, 28335:28358),
                       input$choice_n1n2==3 ~ POS %in% c(29164:29183, 29188:29210, 29213:29230))) %>%
      filter(SNP.lineage.count %in% c(input$choice_no.snp[1]:input$choice_no.snp[2]))
    

  })

  
  
  output$graph <- renderPlotly({
   
    #cat()
    
    plot_ly(height = ifelse(input$choice_n1n2==1, length(unique(selectedData()$mutation.nt))*10, 500),
            width = length(unique(selectedData()$Samples))*8) %>%
      add_trace(data=selectedData(), 
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
                     tickfont = list(size = 3),
                     categoryorder = "array",
                     categoryarray = factor(order3_xaxis)),
        yaxis = list(titlefont = list(size = 5), tickfont = list(size = 3)))
    
  })
  
}



shinyApp(ui, server)
