library(shiny)
library(ggplot2)
library(RColorBrewer)
library(shinyWidgets)
library(fst)
library(plotly)

# Load data 
gva <- read.fst("gva_data_ch.fst")


# Define UI 
ui <- fluidPage(
    tags$head(tags$link(type="text/css",
                            href = "https://innoscape.de/pharma_intro/style.css")), 
    htmltools::htmlDependency("jquery", "3.5.1",
                              src = c(href = "https://code.jquery.com/"),
                              script = "jquery-3.5.1.min.js"),

  
    # GVA
    fluidRow(column = 12,
             id = "gva_id",
             radioButtons(inputId = "variable_gva", label = "Choose a variable",
                          choices = list("Share in GDP" = "Share in GDP",
                                         "GVA in millions CHF" = "GVA in millions CHF" 
                                         # "GVA percentage change" = "GVA percentage change"
                                         ),
                          selected = "Share in GDP", 
                          inline = T),
             
             pickerInput(inputId = "ind.name_gva",
                         label = "Choose industries", 
                         choices= sort(unique(gva$ind.name)),
                         selected = c("Pharmaceutical products", "Machinery and equipment n.e.c.", "Computer, electronic and optical products; watches and clocks"),
                         options = list(`actions-box` = TRUE),
                         multiple = TRUE),
             mainPanel(plotlyOutput("gva_plot")))
)

# Define server 
server <- function(input, output) {
 
    # GVA plot
    d_gva <- reactive({ 
             subset(gva, variable %in% input$variable_gva &
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
