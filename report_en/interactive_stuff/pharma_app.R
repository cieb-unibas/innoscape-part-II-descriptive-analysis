library(shiny)
library(ggplot2)
library(RColorBrewer)
library(shinyWidgets)
library(fst)
library(plotly)
library(DT)
library(igraph)
library(visNetwork)
library(dplyr)

# Load data 
ilo <- read.fst("ilo.fst") 
gva <- read.fst("gva_data_ch.fst")
labprod <- read.fst("labprod.fst")


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
                label = "Choose a country 1", 
                choices= sort(unique(ilo$country)),
                selected = "Switzerland",
                options = list(`actions-box` = TRUE),
                multiple = TRUE),
    mainPanel(plotlyOutput("emp_plot"))),
    br(),
    br(),
    br(),
    br(),
    br(),
    
    # GVA
    fluidRow(column = 12,
             id = "gva_id",
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
             mainPanel(plotlyOutput("gva_plot"))),
    br(),
    br(),
    br(),
    br(),
    br(),
    
    # Productivity
    fluidRow(column = 12,
             id = "prod_id",
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
    mainPanel(plotlyOutput("prod_plot"))),
    br(),
    br(),
    br(),
    br(),
    br(),
    
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
),


br(),
br(),
br(),
br(),
br()
)

# Define server 
server <- function(input, output) {
    
    # Employment plot
    d_emp <- reactive({ilo %>%
            filter(variable %in% input$variable_emp,
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
    
    
    # GVA plot
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
    
    # Prod plot
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
