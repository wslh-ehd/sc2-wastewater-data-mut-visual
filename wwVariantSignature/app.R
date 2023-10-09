library(shiny)
library(dplyr)
library(plotly)
library(viridis)


load("plot.RData")



ui <- fluidPage(
  
  checkboxGroupInput("choice", "Select a group:", choices = levels(as.factor(data.voc.force$lineage)), inline = TRUE, selected = levels(as.factor(data.voc.force$lineage))),
  plotlyOutput("graph", height = 4000, width = 1600)
  
)



server <- function(input, output, session){
  

  output$graph <- renderPlotly({
    
    data<-data.voc.force %>% filter(lineage %in% input$choice)
    data<-data.voc.force %>% filter(lineage %in% input$choice)
    count <- mutations.voc.force %>%
      filter(lineage %in% input$choice) %>%
      count(mutation.nt) %>%
      group_by(mutation.nt) %>% 
      filter(row_number()==1)
    count<-as.data.frame(count)

    data<-left_join(data, count, by=c("mutation.nt"))
    data <-data %>% filter(n==1)

    
    data%>%
      group_by(lineage) %>%
      group_map(~ plot_ly(data=., 
                          x = ~paste0(Date, "_", sites), 
                          y = ~display,
                          #width = ~length(unique(Samples))*8),
                          #color = ~ALT_FREQ,
                          #colors = rev(viridis::viridis(99)),
                          text = ~paste0( "</br>", Samples, "</br>Mut_nt: ", mutation.nt, "</br>Mut_aa: ", mutation.aa, "</br>Frequency(%): ", ALT_FREQ,  "</br>Depth: ", TOTAL_DP, "</br>lineage: ", lineage, "</br>Run: ", run), 
                          type = "scatter", 
                          mode="markers", 
                          marker = list(color = ~ALT_FREQ,
                                        colorscale = 'Viridis',
                                        reversescale = T,
                                        opacity = ~(TOTAL_DP/100)),
                          hoverinfo ="text",
                          legendgroup = 'group1',
                          showlegend = F
                          ) %>%
                  
                  add_annotations(
                    text = ~unique(lineage),
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
                          tickfont = list(size = 5))) %>% 
      
      hide_legend()

    })
  
}



shinyApp(ui, server)


