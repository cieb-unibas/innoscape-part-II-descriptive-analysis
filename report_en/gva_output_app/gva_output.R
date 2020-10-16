#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library("ggplot2")
library("RColorBrewer")
library("shinyWidgets")
library("fst")
library("plotly")


# Load data on employment 
gva <- read.fst("gva_data_ch.fst")


# Define UI for application that draws a histogram
ui <- fluidPage(
    htmltools::htmlDependency("jquery", "3.5.1",
                              src = c(href = "https://code.jquery.com/"),
                              script = "jquery-3.5.1.min.js"),
    # Input panels for indicator and industry
    
    radioButtons(inputId = "variable_gva", label = "Choose a variable",
                 choices = list("Share in GDP" = "Share in GDP",
                                "GVA in millions CHF" = "GVA in millions CHF", 
                                "GVA percentage change" = "GVA percentage change"),
                 selected = "Share in GDP", 
                 inline = T),
    
    pickerInput(inputId = "ind.name_gva",
                label = "Choose industries", 
                choices= sort(unique(gva$ind.name)),
                selected = "Pharmaceutical products",
                options = list(`actions-box` = TRUE),
                multiple = TRUE),
    mainPanel(plotlyOutput("gva_plot"))
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # subset the data according to the input:
  d_gva <- reactive({ gva %>%
      filter(variable %in% input$variable_gva,
             ind.name %in% input$ind.name_gva)
  })
    
    
    # make the plot
  output$gva_plot <-   renderPlotly({
    
    # specify y axis title
    Y_AXIS_TITLE <- ifelse(input$variable_gva == "Share in GDP", "Share in GDP (in %)",
                           ifelse(input$variable_gva == "GVA in millions CHF", "GVA (in millions CHF)", "GVA change (in %)"))
    
    if(nrow(d_gva()) != 0){
      
      
      # make the plot
      ggplotly(
        ggplot(d_gva(),aes(x = as.numeric(year), y = value, group = ind.name, colour = ind.name, label = Industry)) +
          geom_point(alpha = 0.7) +
          geom_line()+
          scale_color_manual(values = colorRampPalette(brewer.pal(8, "RdBu")[c(1:3, 6:8)])(length(unique(d_gva()$ind.name)))) +
          xlab("Year")+
          ylab(Y_AXIS_TITLE)+
          guides(color = FALSE)+
          scale_x_continuous(breaks = seq(min(d_gva()$year), max(d_gva()$year), by = 5)) +
          theme(axis.title = element_text(face="bold",size = 10),
                panel.background = element_blank(),
                axis.line = element_line(),
                legend.position="bottom"
          ), dynamicTicks = T,  tooltip = c("label")) %>%
        config(displayModeBar = F) %>%
        layout(xaxis = list(fixedrange=T),
               yaxis = list(fixedrange=T),
               legend = list(orientation = "h", y = -0.25)) 
      
      # plot_ly(d_gva(), x = ~as.numeric(year), y = ~value,
      #                   type = "scatter", mode = 'lines+markers',
      #       color = ~ind.name,
      #       colors = colorRampPalette(brewer.pal(8, "RdBu")[c(1:3, 6:8)])(length(unique(d_gva()$ind.name))),
      #       text = ~Industry,
      #       hoverinfo = 'text') %>%
      #       config(displayModeBar = F) %>%
      #       layout(xaxis = list(title = "<b>Year<b>"), yaxis = list(title = paste0("<b>", Y_AXIS_TITLE, "<b>")), legend = list(x = 0, y = 100, orientation = 'h'))
      
      
      
    } else{}
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
