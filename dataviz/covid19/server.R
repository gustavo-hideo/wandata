#
# covid19 | server
#


library(shiny)
library(shinyWidgets)
library(leaflet)
library(plotly)


server <- function(input, output) {
    
    ## Reactives
    

    
    scatter <- reactive({
        
        covid.sum %>% 
            filter(Country %in% countries) %>% 
            mutate(label = ifelse(Country %in% top8$Country, Country, ''))
        
    })

    
    
    new.country <- reactive({
        
        new %>% 
            filter(Country %in% input$countries)
        
    })
    
    
    
    
    r.timeline.c <- reactive({
        req(input$countries)
        
        covid %>%
            filter(Country %in% input$countries1) %>%
            select(Country, status, Date, Cases) %>%
            group_by(Country, status, Date) %>%
            summarize(Cases = sum(Cases, na.rm = T)) %>%
            # flag last days to put label
            group_by(Country, status) %>%
            mutate(label = ifelse(Cases == max(Cases, na.rm = T), 1, 0)) %>%
            ungroup() %>%
            unique()
        
    })
    
    
    
    
    
    
    
    new.r <- reactive({
        
        new.country() %>% 
            group_by(Date) %>% 
            summarise(Confirmed = sum(Confirmed, na.rm = T),
                      Deaths = sum(Deaths, na.rm = T)) %>% 
            ungroup()
        
    })
    
    
    
    
    new.today <- reactive({
        
        new.r() %>% 
            filter(Date == max(Date))
        
    })
    
    
    
    r.timeline.country <- reactive({
        req(input$countries)

        covid %>%
            filter(Country %in% input$countries) %>%
            select(Country, status, Date, Cases) %>%
            group_by(Country, status, Date) %>%
            summarize(Cases = sum(Cases, na.rm = T)) %>%
            # flag last days to put label
            group_by(Country, status) %>%
            mutate(label = ifelse(Date == max(Date), 1, 0)) %>%
            ungroup() %>%
            unique()

    })
    
    
    r.summary <- reactive({
        req(input$countries)
        
        covid %>% 
            filter(Country %in% input$countries) %>% 
            select(status, Current)
        
    })
    
    
    
    
    
    covid.us.dt.output <- reactive({
        
        covid.us.dt %>% 
            mutate(label = ifelse(State %in% top8.us$State, State, ''))
        
    })
    
    
    
    
    
    
    covid.us.output <- reactive({
        req(input$states)
        
        covid.us %>% 
            filter(State %in% input$states) %>% 
            select(State, status, Date, Cases) %>%
            group_by(State, status, Date) %>%
            summarize(Cases = sum(Cases, na.rm = T)) %>%
            # flag last days to put label
            group_by(State, status) %>%
            mutate(label = ifelse(Cases == max(Cases, na.rm = T), 1, 0))
        
    })
    
    
    

    
    
    
    
    #########################
    ## Outputs
    
    
    # world map
    output$covid.map.sp <- renderLeaflet({
        
        covid.map.sp %>%
            leaflet() %>%
            addProviderTiles("CartoDB.Positron",
                             options = providerTileOptions(
                                 minZoom = 1, maxZoom = 5, noWrap = T)
            ) %>%
            addPolygons(
                fillColor = ~mypal.ref(covid.map.sp$Current),
                stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.5,
                weight = 1,
                popup = paste0("Country: ", as.character(covid.map.sp$Country), "<br>", "Total: ", as.character(covid.map.sp$Current))
            )

    })
    
    

    

    
    output$timeline.p <- renderPlotly({
        
        scatter() %>% 
            plot_ly(x = ~Deaths, y = ~Confirmed,
                    marker = list(color='rgba(255, 182, 193, .9)',
                                  line=list(color='rgba(152, 0, 0, .8)',
                                            width=1)),
                    text = ~label,
                    showlegend=F,
                    hoverinfo="text",  #Removes the "trace 0 trace 1" and the coordinates from the hover
                    hovertext = paste("Country:", scatter()$Country,
                                      "<br> Confirmed:", format(scatter()$Confirmed, big.mark=","),
                                      "<br> Deaths:", format(scatter()$Deaths, big.mark=","))) %>% 
            add_markers() %>% 
            add_text(textposition = "top right",
                     textfont = list(size=10)) %>% 
            layout(xaxis = list(showgrid=F,
                                titlefont=list(size=10),
                                title='Deaths',
                                tickfont=list(size=9)),
                   yaxis = list(showgrid=F,
                                titlefont=list(size=10),
                                title='Confirmed Cases',
                                tickfont=list(size=9)),
                   hoverlabel = list(align="left",
                                     font=list(size=9)))
        
        
    })
    
    
    
    
    
    # World timeline plot
    
    output$r.timeline.country <- renderPlot({
        
        r.timeline.c() %>%
            ggplot(aes(Date, Cases, color=status, group=status)) +
            geom_line(size=1) +
            directlabels::geom_dl(data=r.timeline.c() %>% filter(label==1),
                                  aes(label=format(as.numeric(Cases), big.mark = ",")),
                                  method=list(directlabels::dl.combine("first.points"),
                                              cex=.7, vjust= -.1)) +
            my.colors +
            scale_y_continuous(labels=scales::unit_format(unit='k', scale=0.001)) +
            facet_wrap(~Country) +
            my.theme +
            theme(legend.position = "top",
                  legend.background = element_rect(color = "grey75"),
                  legend.key = element_blank(),
                  legend.key.height = unit(.1, "cm"),
                  legend.title = element_blank(),
                  legend.text = element_text(size=10))
    })
    
    
    
    
    
    
    
    
    
    ####################
    ## New Cases/Deaths
    
    output$new.t <- renderUI({
        
        new.confirmed <- sum(new.today()$Confirmed, na.rm = T)
        new.deaths <- sum(new.today()$Deaths, na.rm = T)
        
        new.conf <- paste("<font size=5>", "New Confirmed:", "</font size>", "<font size=6><font color=#6495ed>", format(as.numeric(new.confirmed), big.mark = ","), "</font color>", "|", "</font size>")
        new.death <- paste("<font size=5>", "New Deaths:", "</font size>", "<font size=6><font color=#CD5C5C>", format(as.numeric(new.deaths), big.mark = ","), "</font color></font size>")
        
        h5(HTML(paste(new.conf, new.death, sep = ' ')), style="text-align:center;")
        
    })
    
    
    
    output$new.conf <- renderPlotly({
        
        # new.r() %>% 
        #     ggplot(aes(Date, Confirmed)) +
        #     geom_line(size=.8, color='cornflowerblue') +
        #     my.theme
        
        new.r() %>% 
            plot_ly(x = ~Date,
                    y = ~Confirmed,
                    type = 'scatter',
                    mode = 'lines',
                    showlegend=F,    #remove 'trace 0, trace 1'
                    hoverinfo="text",    #remove 'trace 0, trace 1'
                    line = list(color='rgb(100,149,237)'),
                    marker = list(color = 'rgb(100,149,237)',
                                  line=list(color='rgb(1,88,238)',
                                            width=1))) %>% 
            layout(xaxis = list(showgrid=F,
                                title=F,
                                tickfont=list(size=9)),
                   yaxis = list(showgrid=F,
                                title=F,
                                tickfont=list(size=9)),
                           title = 'New Confirmed Cases')
        
    })
    
    
    
    output$new.death <- renderPlotly({
        
        new.r() %>% 
            plot_ly(x = ~Date,
                    y = ~Deaths,
                    type = 'scatter',
                    mode = 'lines',
                    line = list(color='rgb(205, 92, 92'),
                    marker = list(color = 'rgb(205, 92, 92)',
                                  line=list(color='rgb(213,0,0)',
                                            width=1))) %>% 
            layout(xaxis = list(showgrid=F,
                                title=F,
                                tickfont=list(size=9)),
                   yaxis = list(showgrid=F,
                                title=F,
                                tickfont=list(size=9)),
                   title = 'New Deaths')
        
    })
    
    
    
    
    ## DATA TABLE
    output$dt.new <- DT::renderDataTable({
        
        new.country() %>% 
            group_by(Country) %>% 
            filter(Date == max(Date)) %>% 
            select(-Date) %>% 
            arrange(desc(Deaths)) %>% 
            rename("New Deaths"=Deaths,
                   "New Confirmed Cases"=Confirmed)
        
    })

    
    
    
    
    
    #### US
    
    ## US MAP
    output$us.map <- renderLeaflet({
        
        covid.map.sp.us.conf %>%
            leaflet() %>%
            setView(lat=37, lng=-97.7129, zoom=3.4) %>% 
            addProviderTiles("CartoDB.Positron"#,
                             #options = providerTileOptions(
                             #minZoom = 1, maxZoom = 12, noWrap = T)
            ) %>%
            addPolygons(
                fillColor = ~mypal.ref.state.conf(covid.map.sp.us.conf$Current),
                stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.5,
                weight = 1,
                popup = paste0("State: ", as.character(covid.map.sp.us.conf$State), "<br>",
                               "Total: ", as.character(covid.map.sp.us.conf$Current))
            )
    })
    
    
    
    
    ## Scatterplot
    output$scatter.us <- renderPlotly({
        
        covid.us.dt.output() %>% 
            plot_ly(x = ~Deaths, y = ~Confirmed,
                    marker = list(color='rgba(255, 182, 193, .9)',
                                  line=list(color='rgba(152, 0, 0, .8)',
                                            width=1)),
                    text = ~label,
                    showlegend=F,
                    hoverinfo="text",  #Removes the "trace 0 trace 1" and the coordinates from the hover
                    hovertext = paste("State:", covid.us.dt$State,
                                      "<br> Confirmed:", format(covid.us.dt$Confirmed, big.mark=","),
                                      "<br> Deaths:", format(covid.us.dt$Deaths, big.mark=","))) %>% 
            add_markers() %>% 
            add_text(textposition = "top right",
                     textfont = list(size=10)) %>% 
            layout(xaxis = list(showgrid=F,
                                titlefont=list(size=10),
                                title='Deaths',
                                tickfont=list(size=9)),
                   yaxis = list(showgrid=F,
                                titlefont=list(size=10),
                                title='Confirmed Cases',
                                tickfont=list(size=9)),
                   hoverlabel = list(align="left",
                                     font=list(size=9)))
        
        
    })
    
    
    
    
    # US timeline plot
    
    output$timeline.us <- renderPlot({

        covid.us.output() %>%
            ggplot(aes(Date, Cases, color=status, group=status)) +
            geom_line(size=1) +
            directlabels::geom_dl(data=covid.us.output() %>% filter(label==1),
                                  aes(label=format(as.numeric(Cases), big.mark = ",")),
                                  method=list(directlabels::dl.combine("first.points"),
                                              cex=.7, vjust= -.1)) +
            my.colors +
            scale_y_continuous(labels=scales::unit_format(unit='k', scale=0.001)) +
            facet_wrap(~State) +
            my.theme +
            theme(legend.position = "top",
                  legend.background = element_rect(color = "grey75"),
                  legend.key = element_blank(),
                  legend.key.height = unit(.1, "cm"),
                  legend.title = element_blank(),
                  legend.text = element_text(size=10))
    })
    
    
    
    
    
    ## DATA TABLE
    output$dt.us <- DT::renderDataTable({
        
        covid.us.dt
        
    })
    
    
    
    

    
    
    
}




