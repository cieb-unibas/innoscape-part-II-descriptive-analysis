library(shiny)
library(ggplot2)
library(RColorBrewer)
library(shinyWidgets)
library(fst)
library(plotly)
library(DT)
library(dplyr)

# Load data 
map_world <- read.fst("map_data.fst")
volumes <- read.fst("trad_data.fst")

# Define UI 
ui <- fluidPage(
    htmltools::htmlDependency("jquery", "3.5.1",
                              src = c(href = "https://code.jquery.com/"),
                              script = "jquery-3.5.1.min.js"),
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),
    # Export
    fluidRow(column = 12,
             id = "exp_id",
    radioButtons(inputId = "variable_exp", label = "Choose a variable",
                 choices = list("in million USD"="val_export", 
                                "% of total Swiss exports to the destination"="share_in_tot",
                                "% of total Swiss pharma exports"="share_in_tot2"),
                 selected = "val_export", inline = T),
    sliderInput(inputId =  "year_exp", label = "Year", min=1990, max=2019, step = 1, value=2000, sep=""),
    mainPanel(plotlyOutput("export_plot"))
)
)

# Define server 
server <- function(input, output) {
    
  
    
    # Export
    # Create world map
    world <- ggplot() +
        geom_polygon(data = map_world,
                     aes(long, lat, group = group, 
                         fill = region), 
                     show.legend = FALSE,
                     alpha = 1,
                     fill="#D1E5F0",
                     color = "white")
    output$export_plot <-  renderPlotly({
        # subset the data accoding to the input:
        d_exp <- reactive({volumes %>%
                subset(variable %in% input$variable_exp &
                           year %in% input$year_exp)}) 
        # make the plot
        p <- ggplotly( world + 
                           geom_point(data = d_exp(),
                                      aes(long_par, lat_par, size = value,  label = Country), color = "#B2182B", alpha = 0.7) +
                           theme_bw() +
                           theme(axis.line = element_blank(),
                                 panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank(),
                                 panel.background = element_blank(),
                                 panel.border = element_blank(),
                                 axis.ticks = element_blank(),
                                 axis.title = element_blank(),
                                 axis.text = element_blank()),
                       dynamicTicks = T, tooltip = c("Country")
        ) %>% config(displaylogo = F, modeBarButtonsToRemove = c('sendDataToCloud',
                                                                 'toImage',
                                                                 'autoScale2d',
                                                                 'resetScale2d',
                                                                 'hoverClosestCartesian',
                                                                 'hoverCompareCartesian',
                                                                 'pan2d','select2d','lasso2d')) %>%
            layout(xaxis = list(fixedrange = F, showspikes = F), yaxis = list(fixedrange = F, showspikes = F))  
        # %>% 
        # partial_bundle() 
        
        p$x$data[[1]]$text <- c(paste0("Move over a point to get information"))
        p$y$data[[1]]$text <- c(paste0("Move over a point to get information"))
        
        p
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
