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
map_world <- read.fst("map_data.fst")
volumes <- read.fst("trad_data.fst")
numpat_data <- readRDS("numpat_data.rds")
citflow_ctry_back <- read.fst("citflow_ctry_back.fst")
citflow_ctry_forw <- read.fst("citflow_ctry_forw.fst")

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
),


br(),
br(),
br(),
br(),
br(),

# Patent count
fluidPage(htmltools::htmlDependency("jquery", "3.5.1",
                                    src = c(href = "https://code.jquery.com/"),
                                    script = "jquery-3.5.1.min.js"),
         column = 12,
         id = "pat_id", 
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
                                             DT::dataTableOutput('reg')
                          )),
                          ## Add clearing of rows
                          actionButton('clear_reg', 'Clear Selection'),
                          # Input panels for indicator 
                          radioButtons("ind_reg", "Choose an indicator",
                                       choices = list("Absolute numbers"="Absolute numbers",
                                                      "% of total patents"= "% of total patents", 
                                                      "RCA of pharma patents"="RCA of pharma patents"),
                                       selected = "% of total patents", 
                                       inline = T),
                          fluidPage(fluidRow(column = 12, plotlyOutput("plot_reg")))),
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
         )),


br(),
br(),
br(),
br(),
br(),

# Patent network
fluidPage(id = "pat_net_id",
    fluidRow(
        column(6,  
               # (3a) Input panel for investigated country
               pickerInput(inputId = "cit_ctry",
                           label = "Choose a country", 
                           choices= unique(citflow_ctry_back$ctry_cited),
                           selected = c("Switzerland"),
                           options = list(`actions-box` = TRUE),
                           multiple = FALSE)
        ), column(6,
                  # (3b) Input panels flows from and to
                  pickerInput(inputId = "group",
                              label = "Flows from and to", 
                              choices=c("All", "Americas", "Asia", "Europe", "Domestic"),
                              selected = c("All"),
                              options = list(`actions-box` = TRUE),
                              multiple = TRUE)
                  
        )),
mainPanel(visNetworkOutput("net_plot_1")),
fluidRow(column(12, sliderInput(inputId = "p_year_citing", label =  "Year of citation", min=1990, max=2015, value=1990, sep=""))),
mainPanel(visNetworkOutput("net_plot_2"))),
br(),
br(),
br(),
br(),
br()
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
    
    # Patent count
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
                                   options = list(scrollY = 140, scroller = TRUE, dom = 't', scrollX= T,
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
                   indicator %in% input$ind_ctry)     }) 
    
    ## Create plot
    output$plot_ctry <- renderPlotly({

   
        
        if(nrow(plot02_data_country()) != 0){
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
    } else {}
        })
    
    ##########
    # Region #
    ##########
   select_reg <- distinct(numpat_data, Region, `Rank based on year 2000`)  %>%
        na.omit() %>% arrange(`Rank based on year 2000`)
    
    
    output$reg <-  DT::renderDT(select_reg, server = F, extensions = 'Scroller',  
                                  filter = "top", # location of column filters
                                  rownames= FALSE,
                                  class = "display nowrap compact",
                                  selection = list(mode = 'multiple', selected = which(select_reg$Region %in% c("CH - Northwestern Switzerland "))),
                                  options = list(scrollY = 140, scroller = TRUE, dom = 't', scrollX= T,
                                                 columnDefs = list(list(className = 'dt-center', targets = 1))))
    
    selectedRow_reg <- eventReactive(input$reg_rows_selected,{
        select_reg[input$reg_rows_selected, "Region"]
    })
    
    
    ## Add clearing of rows
    proxy_reg <- dataTableProxy('reg')
    actionButton('clear_reg', 'Clear Selection')
    
    observeEvent(input$clear_reg, {
        proxy_reg %>% selectRows(NULL)
    })
        
    # subset the data according to the input:
    plot02_data_reg <- reactive({numpat_data %>%
            filter(geo %in% selectedRow_reg(),
                   indicator %in% input$ind_reg)
    })
    
    ## Create plot
    output$plot_reg <- renderPlotly({

        if(nrow(plot02_data_reg()) != 0){
        ggplotly(
            ggplot(data = plot02_data_reg(), aes(x = p_year, y= share_inv, color = Region, shape = geo, label = Geo)) +
                geom_point(alpha = 0.7) +
                geom_line() +
                # geom_text(aes(label = NOGA2digit), size = 4, check_overlap = FALSE, position = position_nudge(y = +0.1))+
                xlab("Priority Year")+
                ylab(input$ind_reg)+
                guides(color = FALSE)+
                scale_color_manual(values = colorRampPalette(brewer.pal(8, "RdBu")[c(1:3, 6:8)])(length(unique(plot02_data_reg()$geo)))) +
                scale_x_continuous(breaks = seq(min(plot02_data_reg()$p_year), max(plot02_data_reg()$p_year), by = 5)) +
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
    } else{}
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
        
        if(nrow(plot02_data_conti()) != 0){
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
    } else{}
        })
    
    #Patent network
    # (2) Create function to calculate coordinates for each unique technology that is cited in a c
    # credit: https://stackoverflow.com/users/1465387/sebastian-c on
    # https://stackoverflow.com/questions/40279052/coordinates-of-equally-distanced-n-points-on-a-circle-in-r
    eq_spacing <- function(r = 2, data){
        # data: back or forward citations / use only subset of input picker to better distribute the technologies
        data <- data %>% filter(group %in% input$group, 
                                ctry_cited %in% input$cit_ctry,
                                p_year_citing %in% input$p_year_citing)
        # if(nrow(data) == 0){
        #    data <- data.frame(group = "Domestic", 
        #                       ctry_cited = input$cit_ctry,
        #                       p_year_citing = 2000, 
        #                       tech_field_cited = "Pharmaceuticals",
        #                       tech_field_citing = "Pharmaceuticals",
        #                       tech_name = "Pharmaceuticals",
        #                       share_inv_cited = 0,
        #                       x = as.character(c(0)),
        #                       y = as.character(c(0)))
        #    return(NULL)
        # } else {
        coord <- sort(unique(data$tech_field_cited))
        coord <- data.frame(coord)
        
        # create unique coordinate 
        polypoints <- seq(0, 2*pi, length.out=nrow(coord)+1)
        polypoints <- polypoints[-length(polypoints)]
        circx <- r * sin(polypoints)
        circy <- r * cos(polypoints)
        numbers <- data.frame(x=circx, y=circy)
        
        coord <- cbind(coord, numbers)
        coord <- mutate(coord, coord = as.character(coord))
        coord <- mutate(coord, x = ifelse(coord == 16, 0, x), y = ifelse(coord == 16, 0, y))
        colnames(coord) <- c("tech_field_cited", "x", "y")
        
        citflow_ctry <- merge(data, coord)
        return(citflow_ctry)
    }
    # }
    
    check_back <- reactive({eq_spacing(r = 0.7, data = citflow_ctry_back) })
    check_forw <- reactive({eq_spacing(r = 0.7, data = citflow_ctry_forw) })
    
    # make the plot
    
    output$net_plot_1 <- renderVisNetwork({
        
        # Create coordinates for backward citations
        nodes <- eq_spacing(r = 0.7, data = citflow_ctry_back) 
        edges <- nodes
        
        
        
        # (b) Creating NODES from the reactive (filtered) data
        nodes <- dplyr::select(nodes, tech_field_cited, tech_name, group, x, y, share_inv_cited)
        colnames(nodes) <- c("id", "label", "group", "x", "y", "value_nodes")
        
        nodes <- mutate(nodes, label = case_when(label == "Pharmaceuticals" ~ paste0(input$cit_ctry),
                                                 label == "Cited Pharmaceuticals" ~ "Pharmaceuticals",
                                                 T ~ label))
        
        
        nodes %>% group_by(id, label, x, y, value_nodes) %>%
           dplyr::summarize(group = paste(sort(unique(group)),collapse=", ")) %>%
            ungroup()  -> nodes
        
        mutate(nodes, x = case_when(
            label == paste0(input$cit_ctry, " Pharmaceuticals") ~ 0, 
            TRUE   ~ x )) -> nodes
        
        
        mutate(nodes, y = case_when(
            label == paste0(input$cit_ctry, " Pharmaceuticals") ~ 0, 
            TRUE   ~ y )) -> nodes
        
        
        # (c) Creating EDGES from the reactive (filtered) data    
        edges <- subset(edges, select = c("tech_field_citing", "tech_field_cited" ,"share_inv_cited", "group"))
        colnames(edges) <- c("from", "to", "value", "group")
        edges <- filter(edges, value != 0)
        edges$title <- paste0(round(edges$value*100, 1), c(" %"))
        
        # (d) keep only observations above a min threshold
        edges <- filter(edges, value >= 0.01)
        nodes <- filter(nodes, id %in% edges$to | id == 16)
        
        # (e) Define different colors for different regions
        nodes <- mutate(nodes, value = ifelse(x == 0 & y == 0, 1, value_nodes), color = case_when(group == "All" ~ colorRampPalette(brewer.pal(8, "RdBu"))(8)[1],
                                                                                                  group == "Europe" ~ colorRampPalette(brewer.pal(8, "RdBu"))(8)[2],
                                                                                                  group == "Americas" ~ colorRampPalette(brewer.pal(8, "RdBu"))(8)[4], 
                                                                                                  group == "Asia" ~ colorRampPalette(brewer.pal(8, "RdBu"))(8)[7],
                                                                                                  group == "Domestic" ~ colorRampPalette(brewer.pal(8, "RdBu"))(8)[8]),
                        group = ifelse(group %in% c("All", "Europe", "Asia", "Americas", "Domestic"), group, NA))
        
        # (f) create data frame for legend 
        nodes_temp <- filter(nodes, is.na(group) != T)
        ledges <- data.frame(color = unique(nodes_temp$color), 
                             label = unique(nodes_temp$group), shape = rep("dot", length(unique(nodes_temp$group)))) 
        
        # (g) Creating Network Plot
        visNetwork(nodes, edges, height = "100%",  width = "100%", main = list(text = paste("<b>\nBackward citations of pharma patents<b>"), style = "text-align:left;")) %>%
            visIgraphLayout(layout = "layout_nicely") %>%
            visEvents(type = "once", startStabilizing = "function() {
               this.moveTo({scale:0.5})}") %>%
            visPhysics(stabilization = FALSE) %>%
            visNodes(
                fixed = TRUE,
                shape = "dot",
                color=colorRampPalette(brewer.pal(8, "RdBu"))(8)[6],
                shadow = list(enabled = F, size = 20)
            ) %>%
            visEdges(
                value="value",
                scaling = list(min = min(unique(edges$value), rm.na = T)*8, max = max(unique(edges$value), rm.na = T)*8),
                arrows = "from",
                color="lightgrey",
                selfReferenceSize = F,
                shadow = FALSE,
                smooth = FALSE) %>%
            visInteraction(dragNodes = F, dragView = F, zoomView = T)%>%
            visOptions(highlightNearest = list(enabled = F, degree = 1, hover = T))%>%
            visLegend(position = "right", width = 0.4, stepX = 50, stepY = 100, ncol = 1,  useGroups = F, addNodes = ledges, zoom = F) 
    })
    
    
    output$net_plot_2 <-   
        # make the 2nd plot
        renderVisNetwork({
            
            # Create coordinates for forward citations used in 2nd plot
            nodes_2 <- eq_spacing(r = 0.5, data = citflow_ctry_forw) 
            edges_2 <- nodes_2
            
            # (b) Creating NODES from the reactive (filtered) data
            nodes_2 <- dplyr::select(nodes_2, tech_field_cited, tech_name, group, x, y, share_inv_cited)
            colnames(nodes_2) <- c("id", "label", "group", "x", "y", "value_nodes")
            nodes_2 <- mutate(nodes_2, label = case_when(label == "Pharmaceuticals" ~ paste0(input$cit_ctry),
                                                         label == "Cited Pharmaceuticals" ~ "Pharmaceuticals",
                                                         T ~ label))
            
            nodes_2 %>% group_by(id, label, x, y, value_nodes) %>%
              dplyr::summarize(group = paste(sort(unique(group)),collapse=", ")) %>%
                ungroup()  -> nodes_2
            
            mutate(nodes_2, x = case_when(
                label == paste0(input$cit_ctry, " Pharmaceuticals") ~ 0, 
                TRUE   ~ x )) -> nodes_2
            
            mutate(nodes_2, y = case_when(
                label == paste0(input$cit_ctry, " Pharmaceuticals") ~ 0, 
                TRUE   ~ y )) -> nodes_2
            
            
            # (c) Creating EDGES from the reactive (filtered) data    
            edges_2 <- subset(edges_2, select = c("tech_field_citing", "tech_field_cited" ,"share_inv_cited", "group"))
            colnames(edges_2) <- c("from", "to", "value", "group")
            edges_2 <- filter(edges_2, value != 0)
            edges_2$title <- paste0(round(edges_2$value*100, 1), c(" %"))
            
            # (d) keep only observations above a min threshold
            edges_2 <- filter(edges_2, value >= 0.01)
            nodes_2 <- filter(nodes_2, id %in% edges_2$to | id == 16)
            
            # (e) Define different colors for different regions
            nodes_2 <- mutate(nodes_2, value = ifelse(x == 0 & y == 0, 1, value_nodes), color = case_when(group == "All" ~ colorRampPalette(brewer.pal(8, "RdBu"))(8)[1],
                                                                                                          group == "Europe" ~ colorRampPalette(brewer.pal(8, "RdBu"))(8)[2],
                                                                                                          group == "Americas" ~ colorRampPalette(brewer.pal(8, "RdBu"))(8)[4], 
                                                                                                          group == "Asia" ~ colorRampPalette(brewer.pal(8, "RdBu"))(8)[7],
                                                                                                          group == "Domestic" ~ colorRampPalette(brewer.pal(8, "RdBu"))(8)[8]),
                              group = ifelse(group %in% c("All", "Europe", "Asia", "Americas", "Domestic"), group, NA))
            
            # (f) create data frame for legend 
            nodes_2_temp <- filter(nodes_2, is.na(group) != T)
            ledges_2 <- data.frame(color = unique(nodes_2_temp$color),
                                   label_2 = unique(nodes_2_temp$group), shape = rep("dot", length(unique(nodes_2_temp$group))))
            
            # (g) Creating Network Plot
            visNetwork(nodes_2, edges_2, height = "100%", width = "100%", main = list(text = paste("<b>\nForward citations of pharma patents<b>"), style = "text-align:left;")) %>%
                visIgraphLayout(layout = "layout_nicely") %>%
                visEvents(type = "once", startStabilizing = "function() {
               this.moveTo({scale:0.5})}") %>%
                visPhysics(stabilization = FALSE) %>%
                visNodes(
                    fixed = TRUE,
                    shape = "dot",
                    color=colorRampPalette(brewer.pal(8, "RdBu"))(8)[6],
                    shadow = list(enabled = F, size = 20)
                ) %>%
                visEdges(
                    value="value",
                    scaling = list(min = min(unique(edges_2$value), rm.na = T)*8, max = max(unique(edges_2$value), rm.na = T)*8),
                    arrows = "to",
                    color="lightgrey",
                    selfReferenceSize = F,
                    shadow = FALSE,
                    smooth = FALSE) %>%
                visInteraction(dragNodes = F, dragView = F, zoomView = T)%>%
                visOptions(highlightNearest = TRUE)
            # %>%
            # visOptions(highlightNearest = list(enabled = F, degree = 1, hover = T)) 
        })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
