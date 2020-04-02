#
# covid19 | server
#


library(shiny)
library(shinyWidgets)
library(leaflet)



server <- function(input, output) {
    
    ## Reactives
    
    r.timeline <- reactive({
        req(input$countries)
        
        covid %>% 
            filter(Country %in% input$countries) %>% 
            select(status, Date, Cases) %>% 
            group_by(status, Date) %>% 
            summarize(Cases = sum(Cases)) %>% 
            # flag last days to put label
            group_by(status) %>% 
            mutate(label = ifelse(Cases == max(Cases), 1, 0)) %>% 
            ungroup() %>% 
            unique()
    
    })
    
    r.timeline.country <- reactive({
        req(input$countries)
        
        covid %>% 
            filter(Country %in% input$countries) %>% 
            select(Country, status, Date, Cases) %>% 
            group_by(Country, status, Date) %>% 
            summarize(Cases = sum(Cases)) %>% 
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
    
    
    # top8 timeline plot
    output$timeline.8 <- renderPlot({
        
        covid.map.8 %>% 
            ggplot(aes(Date, Cases, color=status, group=status)) +
            geom_line(size=1) +
            #geom_label(data=timeline %>% filter(label==1), 
            #          aes(label=status),
            #         label.size = NA) +
            my.colors +
            scale_y_continuous(labels=scales::unit_format(unit='k', scale=0.001)) +
            my.theme +
            facet_wrap(~Country, nrow=2)
    })
    
    
    # summary
    # output$summary <- renderUI({
    #     
    #     confirmed <- r.summary() %>% 
    #         filter(status=='Confirmed') %>% 
    #         unique() %>% 
    #         summarise(total = sum(Current, na.rm = T))
    #     
        # recovered <- r.summary() %>% 
        #     filter(status=='Recovered') %>% 
        #     unique() %>% 
        #     summarise(total = sum(Current, na.rm = T))

        # deaths <- r.summary() %>% 
        #     filter(status=='Deaths') %>% 
        #     unique() %>% 
        #     summarise(total = sum(Current, na.rm = T))
        # 
        # conf <- paste("<font size=5>", "Confirmed:", "</font size>", "<font size=6><font color=#0074D9>", format(as.numeric(confirmed$total), big.mark = ","), "</font color></font size>")
        # #reco <- paste("<font size=5>", "Recovered:", "</font size>", "<font size=6><font color=#37B701>", format(as.numeric(recovered$total), big.mark = ","), "</font color></font size>")
        # death <- paste("<font size=5>", "Deaths:", "</font size>", "<font size=6><font color=#FF4136>", format(as.numeric(deaths$total), big.mark = ","), "</font color></font size>")
        # 
        # HTML(paste(conf, death, sep = '<br/>'))
    #})
    
    
    # world timeline plot
    output$timeline.p <- renderPlot({
        
        r.timeline.country() %>% 
            ggplot(aes(Date, Cases, color=status, group=status)) +
            geom_line(size=1) +
            # geom_label(data=r.timeline.country() %>% filter(label==1),
            #            aes(label=Cases),
            #            label.size = NA) +
            directlabels::geom_dl(data=r.timeline.country() %>% filter(label==1),
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
    
    
    
}




