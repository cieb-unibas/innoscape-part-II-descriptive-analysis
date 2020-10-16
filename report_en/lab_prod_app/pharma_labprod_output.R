library(shiny)
library("ggplot2")
library("RColorBrewer")
library("shinyWidgets")
library("fst")
library("plotly")


# Load data on employment 
labprod <- read.fst("labprod.fst")

# Define UI for application that draws the plot
ui <- fluidPage(
    htmltools::htmlDependency("jquery", "3.5.1",
                              src = c(href = "https://code.jquery.com/"),
                              script = "jquery-3.5.1.min.js"),
    # Input panels for indicator and industry
    checkboxGroupInput(inputId = "variable_prod", label = "Choose a productivity indicator",
                       choices = list("Only domestic workers"="Only domestic workers",
                                      "Total jobs in firms"="Total jobs in firms",
                                      "Domestic and cross-border workers"="Domestic and cross-border workers"),
                       selected = "Only domestic workers",
                       inline = T),
        pickerInput(inputId = "ind.name_prod",
                label = "Choose industry", 
                choices= sort(unique(labprod$ind.name)),
                selected = c("Pharmaceutical products"),
                options = list(`actions-box` = TRUE),
                multiple = TRUE),
    mainPanel(plotlyOutput("prod_plot"))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # subset the data according to the input:
    d_output <- reactive({labprod %>%
            filter(variable %in% input$variable_prod,
                   ind.name %in% input$ind.name_prod)
    })
    
  output$prod_plot <-  renderPlotly({
        
        if(nrow(d_output()) != 0){
            # make the plot
            Y_MAX <- max(d_output()$value, na.rm = TRUE)
            ggplotly(
                ggplot(d_output(),aes(x = as.numeric(year), y = value,
                                      group = interaction(ind.name, variable, sep = ": "),
                                      colour = ind.name, linetype  = variable, label = Industry)) +
                    geom_point(alpha = 0.7) +
                    scale_color_manual(values = colorRampPalette(brewer.pal(8, "RdBu")[c(1:3, 6:8)])(length(unique(d_output()$ind.name)))) +
                    geom_line()+
                    xlab("Year")+
                    ylab(input$variable_prod)+
                    ylim(0, Y_MAX)+
                    guides(color = FALSE)+
                    scale_x_continuous(breaks = seq(min(d_output()$year), max(d_output()$year), by = 5)) +
                    theme(axis.title = element_text(face="bold",size = 10),
                          panel.background = element_blank(),
                          axis.line = element_line()
                    ), dynamicTicks = T,  tooltip = c("label")) %>%
                config(displayModeBar = F) %>%
                layout(xaxis = list(fixedrange=T),
                       yaxis = list(fixedrange=T),
                       legend = list(orientation = "h", y = -0.25)) 
        } else{}  
    })
}

# Run the application 
shinyApp(ui = ui, server = server)