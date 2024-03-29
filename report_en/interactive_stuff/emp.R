library(shiny)
library(ggplot2)
library(RColorBrewer)
library(shinyWidgets)
library(fst)
library(plotly)

# Load data 
ilo <- read.fst("ilo.fst") 



# Define UI 
ui <- fluidPage(
    htmltools::htmlDependency("jquery", "3.5.1",
                              src = c(href = "https://code.jquery.com/"),
                              script = "jquery-3.5.1.min.js"),
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),
    # Employment
    fluidRow(column = 12,
             id = "emp_id",
    radioButtons(inputId = "variable_emp", label = "Choose a variable",
                 choices = list("Share of employed" = "Share of employed",
                                "Number of employed in thousands" = "Number of employed in thousands"),
                 selected = "Share of employed",
                 inline = TRUE),
    
    pickerInput(inputId = "country_emp",
                label = "Choose a country", 
                choices= sort(unique(ilo$country)),
                selected = "Switzerland",
                options = list(`actions-box` = TRUE),
                multiple = TRUE),
    mainPanel(plotlyOutput("emp_plot"))))

# Define server 
server <- function(input, output) {
    
    # Employment plot
    d_emp <- reactive({
            subset(ilo, variable %in% input$variable_emp &
                   country %in% input$country_emp)})
    
    
    # make the plot
    output$emp_plot <-   renderPlotly({  if(nrow(d_emp()) != 0){
        Y_MAX <- max(d_emp()$value, na.rm = TRUE) 
        
        ggplotly(
            ggplot(d_emp(), aes(x = as.numeric(year), y = value,
                                group = country, colour = country, label = Country)) +
                geom_point(alpha = 0.7) +
                geom_line()+
                # scale_colour_brewer(palette = "RdBu") +
                # scale_colour_brewer(palette= pal_new) +
                scale_color_manual(values = colorRampPalette(brewer.pal(8, "RdBu")[c(1:3, 6:8)])(length(unique(d_emp()$country)))) +
                xlab("Year")+
                ylab(ifelse(input$variable_emp == "Share of employed", "Share of employed (in %)",
                            "Number of employed (in thousands)") )+
                guides(color = FALSE)+
                ylim(0, Y_MAX)+
                scale_x_continuous(breaks = seq(min(d_emp()$year, na.rm = T),
                                                max(d_emp()$year, na.rm = T), by = 5)) +
                theme(axis.title = element_text(face="bold",size = 10),
                      panel.background = element_blank(),
                      axis.line = element_line()
                ), dynamicTicks = T,  tooltip = c("label")) %>%
            config(displayModeBar = F) %>%
            layout(xaxis = list(fixedrange=T),
                   yaxis = list(fixedrange=T),
                   legend = list(orientation = "h", y = -0.25))
        #           plot_ly(d_emp(), x = ~as.numeric(year), y = ~value,
        #                       type = "scatter", mode = 'lines+markers',
        #           color = ~country,
        #           colors = colorRampPalette(brewer.pal(8, "RdBu")[c(1:3, 6:8)])(length(unique(d_emp()$country))),
        #           text = ~Country,
        #           hoverinfo = 'text') %>%
        #           config(displayModeBar = F) %>%
        #           layout(xaxis = list(title = "<b>Year<b>"), yaxis = list(title = paste0("<b>", Y_AXIS_TITLE, "<b>")), legend = list(x = 0, y = 100, orientation = 'h'))
        
    } else {}
        
    })
    
   
}

# Run the application 
shinyApp(ui = ui, server = server)
