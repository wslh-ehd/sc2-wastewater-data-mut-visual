library(shiny)
library(dplyr)
library(plotly)
library(viridis)
library(tidyr)

load("plot.RData")
data.voc.nextstrain<-data.voc.nextstrain %>% filter(Sample.type == "Sample" & QC_Run == "qc_passed" & QC_Sample == "qc_passed")
#data.mut.voc.nextstrain$Lineages2<-gsub(".* [(]", "", data.mut.voc.nextstrain$Lineages)
#data.mut.voc.nextstrain$Lineages2<-gsub("[)]", "", data.mut.voc.nextstrain$Lineages2)


ui <- fluidPage(
  
  checkboxGroupInput("choice", "Select a group:", choices = levels(as.factor(data.voc.nextstrain$Lineages)), inline = TRUE, selected = levels(as.factor(data.voc.nextstrain$Lineages))),
  plotlyOutput("graph", height = 1000, width = 800)
  
  
)



server <- function(input, output, session){
  

  output$graph <- renderPlotly({
    
    data<-data.voc.nextstrain %>% filter(Lineages %in% c(input$choice))
    #data<-data.mut.voc.nextstrain %>% filter(Lineages %in% c("21K (BA.1)", "22F (XBB)"))
    mut.covariants<-unique(data[, c("Lineages", "mutation.nt")])
    
    count <- mut.covariants %>%
      count(mutation.nt) %>%
      group_by(mutation.nt) %>% 
      filter(row_number()==1)
    count<-as.data.frame(count)

    data<-left_join(data, count, by=c("mutation.nt"))
    data <-data %>% filter(n==1)
    
    
    
    data%>%
      group_by(Lineages) %>%
      group_map(~ plot_ly(data=., 
                          x = ~paste0(Date, "_", sites), 
                          y = ~display,
                          width = length(unique(data$Samples))*8,
                          #color = ~ALT_FREQ,
                          #colors = rev(viridis::viridis(99)),
                          text = ~paste0( "</br>", Samples, "</br>Mut_nt: ", mutation.nt, "</br>Mut_aa: ", mutation.aa, "</br>Frequency(%): ", ALT_FREQ,  "</br>Depth: ", TOTAL_DP, "</br>Lineages: ", Lineages, "</br>Run: ", run), 
                          type = "scatter", 
                          mode="markers", 
                          marker = list(color = ~ALT_FREQ,
                                        colorscale = 'Viridis',
                                        reversescale = T,
                                        opacity = ~(TOTAL_DP/100)),
                          hoverinfo ="text"
      ) %>% layout(showlegend = F) %>%
        
        add_annotations(
          text = ~unique(Lineages),
          x = 0.5,
          y = 1.1,
          yref = "paper",
          xref = "paper",
          xanchor = "left",
          yanchor = "top",
          showarrow = FALSE,
          font = list(size = 15)
        ), 
      
      .keep=TRUE) %>% 
      
      
      
      subplot(nrows = NROW(.), shareX = TRUE, shareY = FALSE, titleX=F,titleY=F) %>% 
      
      
      layout(showlegend=F,
             plot_bgcolor='snow', 
             xaxis = list(title = "",
                          tickangle = -45,
                          tickfont = list(size = 5)))
    
  
    
    })
  
}



shinyApp(ui, server)
