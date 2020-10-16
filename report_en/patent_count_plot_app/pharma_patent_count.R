library(shiny)
library("ggplot2")
library("RColorBrewer")
library("shinyWidgets")
library("fst")
library("plotly")
library(DT)

# Load data on patents
numpat_data <- readRDS("numpat_data.rds")

# Define UI for application that draws the plot
ui <- fluidPage(
    htmltools::htmlDependency("jquery", "3.5.1",
                              src = c(href = "https://code.jquery.com/"),
                              script = "jquery-3.5.1.min.js"),
    
    # Input panels 
    mainPanel(
        tabsetPanel(
            tabPanel("Countries", 
                     fluidPage(fluidRow(column = 12,
                                        DT::dataTableOutput('country')
                     )),
                     ## Add clearing of rows
                     actionButton('clear_ctry', 'Clear Selection'),
                     # Input panels for indicator 
                     radioButtons("ind_ctry", "Choose an indicator",
                                  choices = list("Absolute numbers"="Absolute numbers",
                                                 "% of total patents"= "% of total patents", 
                                                 "RCA of pharma patents"="RCA of pharma patents"),
                                  selected = "% of total patents", 
                                  inline = T),
                     fluidPage(fluidRow(column = 12, plotlyOutput("plot_ctry")))),
            tabPanel("Regions", 
                     fluidPage(fluidRow(column = 12,
                                        DT::dataTableOutput('regio')
                     )),
                     ## Add clearing of rows
                     actionButton('clear_regio', 'Clear Selection'),
                     # Input panels for indicator 
                     radioButtons("ind_regio", "Choose an indicator",
                                  choices = list("Absolute numbers"="Absolute numbers",
                                                 "% of total patents"= "% of total patents", 
                                                 "RCA of pharma patents"="RCA of pharma patents"),
                                  selected = "% of total patents", 
                                  inline = T),
                     fluidPage(fluidRow(column = 12, plotlyOutput("plot_regio")))),
            tabPanel("Continents", 
                     fluidPage(fluidRow(column = 12,
                                        DT::dataTableOutput('conti')
                     )),
                     ## Add clearing of rows
                     actionButton('clear_conti', 'Clear Selection'),
                     # Input panels for indicator 
                     radioButtons("ind_conti", "Choose an indicator",
                                  choices = list("Absolute numbers"="Absolute numbers",
                                                 "% of total patents"= "% of total patents", 
                                                 "RCA of pharma patents"="RCA of pharma patents"),
                                  selected = "% of total patents", 
                                  inline = T),
                     fluidPage(fluidRow(column = 12, plotlyOutput("plot_conti"))))
            
        )
    )
)



# })
# 
# fluidPage(fluidRow(column = 12,
#                    DT::dataTableOutput('regio')
# ))






# Define server logic required to draw a histogram
server <- function(input, output) {
   
    
    ###########
    # Country #
    ###########
    select_country <- distinct(numpat_data, Country, `Rank based on year 2000`)  %>% 
    na.omit() %>% arrange(`Rank based on year 2000`)
    
    output$country <- DT::renderDT(select_country, server = F, extensions = 'Scroller',  
                                   filter = "top", # location of column filters
                                   rownames= FALSE,
                                   class = "display nowrap compact", 
                                   selection = list(mode = 'multiple', selected = which(select_country$Country %in% c("Germany", "Switzerland", "United States"))),
                                   options = list(scrollY = 180, scroller = TRUE, dom = 't', scrollX= T,
                                                  columnDefs = list(list(className = 'dt-center', targets = 1))))
    
    selectedRow_ctry <- eventReactive(input$country_rows_selected,{
        select_country[input$country_rows_selected, "Country"]
    })
    
    
    ## Add clearing of rows
    proxy_ctry <- dataTableProxy('country')
    actionButton('clear_ctry', 'Clear Selection')
    
    observeEvent(input$clear_ctry, {
        proxy_ctry %>% selectRows(NULL)
    })
 
    # subset the data according to the input:
    plot02_data_country <- reactive({numpat_data %>%
            filter(geo %in% selectedRow_ctry(),
                   indicator %in% input$ind_ctry)
    }) 
    
       
    ## Create plot
    output$plot_ctry <- renderPlotly({
        ggplotly(
            ggplot(data = plot02_data_country(), aes(x = p_year, y= share_inv, color = Country, shape = geo, label = Geo)) +
                geom_point(alpha = 0.7) +
                geom_line() +
                # geom_text(aes(label = NOGA2digit), size = 4, check_overlap = FALSE, position = position_nudge(y = +0.1))+
                xlab("Priority Year")+
                ylab(input$ind_ctry)+
                guides(color = FALSE)+
                scale_color_manual(values = colorRampPalette(brewer.pal(8, "RdBu")[c(1:3, 6:8)])(length(unique(plot02_data_country()$geo)))) +
                scale_x_continuous(breaks = seq(min(plot02_data_country()$p_year), max(plot02_data_country()$p_year), by = 5)) +
                theme(axis.title = element_text(face="bold",size = 10),
                      panel.background = element_blank(),
                      axis.line = element_line(),
                      legend.title = element_blank()
                ), dynamicTicks = T, tooltip = c("label")) %>%
            config(displayModeBar = F) %>%
            layout(xaxis = list(fixedrange=T
            )) %>%
            layout(yaxis = list(fixedrange=T), legend = list(orientation = "h", y = -0.25))
    })

    ##########
    # Region #
    ##########
    select_regio <- distinct(numpat_data, Region, `Rank based on year 2000`)  %>%
        na.omit() %>% arrange(`Rank based on year 2000`)
    
    


    
    output$regio <-  DT::renderDT(select_regio, server = F, extensions = 'Scroller',  
                                                                 filter = "top", # location of column filters
                                                                 rownames= FALSE,
                                                                 class = "display nowrap compact",
                                                                 selection = list(mode = 'multiple', selected = which(select_regio$Region %in% c("CH - Northwestern Switzerland "))),
                                                                 options = list(scrollY = 180, scroller = TRUE, dom = 't', scrollX= T,
                                                                                columnDefs = list(list(className = 'dt-center', targets = 1))))

    selectedRow_regio <- eventReactive(input$regio_rows_selected,{
            select_regio[input$regio_rows_selected, "Region"]
    })
    
    
    

    ## Add clearing of rows
    proxy_regio <- dataTableProxy('regio')
    actionButton('clear_reg', 'Clear Selection')
    observeEvent(input$clear_reg, {
        proxy %>% selectRows(NULL)
    })
    
    
    # subset the data according to the input:
    plot02_data_regio <- reactive({numpat_data %>%
            filter(geo %in% selectedRow_regio(),
                   indicator %in% input$ind_regio)
    })
    
    ## Create plot
    output$plot_regio <- renderPlotly({
        ggplotly(
            ggplot(data = plot02_data_regio(), aes(x = p_year, y= share_inv, color = Region, shape = geo, label = Geo)) +
                geom_point(alpha = 0.7) +
                geom_line() +
                # geom_text(aes(label = NOGA2digit), size = 4, check_overlap = FALSE, position = position_nudge(y = +0.1))+
                xlab("Priority Year")+
                ylab(input$ind_regio)+
                guides(color = FALSE)+
                scale_color_manual(values = colorRampPalette(brewer.pal(8, "RdBu")[c(1:3, 6:8)])(length(unique(plot02_data_regio()$geo)))) +
                scale_x_continuous(breaks = seq(min(plot02_data_regio()$p_year), max(plot02_data_regio()$p_year), by = 5)) +
                theme(axis.title = element_text(face="bold",size = 10),
                      panel.background = element_blank(),
                      axis.line = element_line(),
                      legend.title = element_blank(),
                      legend.position = 'bottom'
                ),
            dynamicTicks = T, tooltip = "label") %>%
            config(displayModeBar = F) %>%
            layout(xaxis = list(fixedrange=T
            )) %>%
            layout(yaxis = list(fixedrange=T), legend = list(orientation = "h", y = -0.25))
    })

        # plot_ly(plot02_data_regio(), x = ~as.numeric(p_year), y = ~share_inv,
        #                 type = "scatter", mode = 'lines+markers',
        #     color = ~geo,
        #     colors = colorRampPalette(brewer.pal(8, "RdBu")[c(1:3, 6:8)])(length(unique(plot02_data_regio()$geo))),
        #     text = ~Geo,
        #     hoverinfo = 'text') %>%
        #     config(displayModeBar = F) %>%
        #     layout(xaxis = list(title = '<b>Year</b>', fixedrange=T), yaxis = list(title = paste0('<b>', input$ind_regio, '<b>'), fixedrange=T), legend = list(x = 0, y = 100, orientation = 'h'))


    #############
    # Continent #
    #############
    select_conti <- distinct(numpat_data, Continent, `Rank based on year 2000`)  %>% 
        na.omit() %>% arrange(`Rank based on year 2000`)
    
    output$conti <-  DT::renderDT(select_conti, server = F,
                                  class = "display nowrap compact",
                                  filter = "top", # location of column filters
                                  rownames= FALSE,
                                  fillContainer = F,
                                  selection = list(mode = 'multiple', selected = which(select_conti$Continent %in% c("Europe"))),
                                  options = list(scrollY = F, scroller = F, dom = 't', scrollX= F,
                                                 columnDefs = list(list(className = 'dt-center', targets = 1))))

    
    selectedRow_conti <- eventReactive(input$conti_rows_selected,{
        select_conti[input$conti_rows_selected, "Continent"]
    })
    
    ## Add clearing of rows
    proxy_conti <- dataTableProxy('conti')
    actionButton('clear_conti', 'Clear Selection')
    observeEvent(input$clear_conti, {
        proxy_conti %>% selectRows(NULL)
    })
    
    # subset the data according to the input:
    plot02_data_conti <- reactive({numpat_data %>%
            filter(geo %in% selectedRow_conti(),
                   indicator %in% input$ind_conti)
    })
      

  output$plot_conti  <- renderPlotly({
            # make the plot
            ggplotly(
                ggplot(data =   plot02_data_conti(), aes(x = p_year, y= share_inv, color = geo, shape = geo, label = Geo)) +
                    geom_point(alpha = 1) +
                    geom_line(alpha = 1) +
                    # geom_text(aes(label = NOGA2digit), size = 4, check_overlap = FALSE, position = position_nudge(y = +0.1))+
                    xlab("Priority Year")+
                    ylab(input$ind_conti)+
                    guides(color = FALSE)+
                    scale_color_manual(values = colorRampPalette(brewer.pal(8, "RdBu")[c(1:3, 6:8)])(length(unique(plot02_data_conti()$geo)))) +
                    scale_x_continuous(breaks = seq(min(plot02_data_conti()$p_year, na.rm = T), max(plot02_data_conti()$p_year, na.rm = T), by = 5)) +
                    # scale_color_viridis(option="viridis", begin = 0.7, end = 0.4, discrete = FALSE, name = "None")+
                    theme(axis.title = element_text(face="bold",size = 10),
                          panel.background = element_blank(),
                          axis.line = element_line(),
                          legend.title = element_blank()),
                dynamicTicks = FALSE,  tooltip = c("label")) %>%
                config(displayModeBar = F) %>%
                layout(xaxis = list(fixedrange=T
                )) %>%
                layout(yaxis = list(fixedrange=T),
                       legend = list(orientation = "h", y = -0.25))
      })

            # plot_ly(plot02_data_conti(), x = ~as.numeric(p_year), y = ~share_inv,
            #                 type = "scatter", mode = 'lines+markers',
            #     color = ~geo,
            #     colors = colorRampPalette(brewer.pal(8, "RdBu")[c(1:3, 6:8)])(length(unique(plot02_data_conti()$geo))),
            #     text = ~Geo,
            #     hoverinfo = 'text') %>%
            #     config(displayModeBar = F) %>%
            #     layout(xaxis = list(title = '<b>Year</b>', fixedrange=T), yaxis = list(title = paste0('<b>', input$ind_conti, '<b>'), fixedrange=T), legend = list(x = 0, y = 100, orientation = 'h'))

}

# Run the application 
shinyApp(ui = ui, server = server)