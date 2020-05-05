#
# covid19 | ui
#



library(shiny)
library(shinyWidgets)
library(leaflet)
library(plotly)
library(shinycssloaders)



ui <- fluidPage(
    
  theme = shinythemes::shinytheme("simplex"),
  
  titlePanel("", windowTitle = "COVID19"),
  
  mainPanel(
    width = 12,
    
    tabsetPanel(type = "tabs",
                
                tabPanel("General",
                         
                          width = 12,  # 12 is the maximum width (12 columns). It will fill the screen
                          
                          
                         fluidRow(
                         
                            h1(HTML(title)),
                            
                            h5(HTML(paste(conf, death, sep = ' ')), style="text-align:center;"),
                         ),
                                
                          fluidRow(
                            column(6,
                                   leafletOutput(outputId = "covid.map.sp", width = "100%") %>% 
                                     withSpinner(color="#980008")
                                   ),
                            column(6,
                                   plotlyOutput(outputId = "timeline.p", width = "100%")%>% 
                                     withSpinner(color="#980008")
                                   )
                          ),
                         
                         br(),
                         
                         fluidRow(
                           
                           column(3,
                                  pickerInput(inputId = 'countries1',
                                       label = 'Countrie\'s timeline to compare:',
                                       choices = countries,
                                       options = list(`actions-box` = T,                # pickerInput allows this option that incluse SELECT ALL
                                                      `live-search` = T),    # adding search box
                                       multiple = T,
                                       selected = c("United States", "United Kingdom", "Brazil")
                                  )
                           ),
                           column(9,
                                  plotOutput(outputId = "r.timeline.country", width = "100%")%>% 
                                    withSpinner(color="#980008")
                                  )
                           
                           
                           
                         ),
                          
                          br(),
                          
                          h6(HTML('Data source: <a href="https://github.com/CSSEGISandData">John Hopkins</a>'))
                          
                ),
                
                
                
                
                
                ######
                
                
                
                
                
                tabPanel("New Cases/Deaths",
                         
                         width = 12,
                         
                         fluidRow(
                           
                           h1(HTML(title)),
                    
                           htmlOutput(outputId = "new.t"),
                             
                           pickerInput(inputId = 'countries',
                            label = 'Countries',
                            choices = countries,
                            options = list(`actions-box` = T,                # pickerInput allows this option that incluse SELECT ALL
                                           `live-search` = T),    # adding search box
                            multiple = T,
                            selected = countries
                            )
                           
                         ),
                         
                         #br(),
                       
                         
                         fluidRow(
                           column(6,
                                  plotlyOutput(outputId = "new.conf", width = "100%")%>% 
                                    withSpinner(color="#980008")
                           ),
                           column(6,
                                  plotlyOutput(outputId = "new.death", width = "100%")%>% 
                                    withSpinner(color="#980008")
                           )
                         ),
                         
                         
                         br(),
                         
                         
                         fluidRow(
                           DT::dataTableOutput("dt.new")%>% 
                             withSpinner(color="#980008")
                         ),
                         
                         
                         
                         br(),
                         
                         h6(HTML('Data source: <a href="https://github.com/CSSEGISandData">John Hopkins</a>'))
                         
                         
                         ),
                
                
                
                
                
                
                ####### United States
                
                
                
                tabPanel("United States",
                         
                         width = 12,
                         
                         fluidRow(
                           
                           h1(HTML(title.us)),
                           
                           h5(HTML(paste(conf.us, death.us, sep = ' ')), style="text-align:center;"),
                         ),
                         
                         fluidRow(
                           column(6,
                                  leafletOutput(outputId = "us.map", width = "100%")%>% 
                                    withSpinner(color="#980008")
                           ),
                           column(6,
                                  plotlyOutput(outputId = "scatter.us", width = "100%")%>% 
                                    withSpinner(color="#980008")
                                  )
                         ),
                         
                         br(),
                         
                         fluidRow(
                           column(3,
                                  pickerInput(inputId = 'states',
                                              label = 'Choose the states for comparison:',
                                              choices = states,
                                              options = list(`actions-box` = T,                # pickerInput allows this option that incluse SELECT ALL
                                                             `live-search` = T),    # adding search box
                                              multiple = T,
                                              selected = c("New York", "California")
                                              )
                                  ),
                           column(9,
                                  plotOutput(outputId = "timeline.us", width = "100%")%>% 
                                    withSpinner(color="#980008")
                                  )
                         ),
                         
                         br(),
                         
                         fluidRow(
                           column(10,
                                  DT::dataTableOutput("dt.us")%>% 
                                    withSpinner(color="#980008")
                                  )
                           ),
                         
                         br(),
                         
                         h6(HTML('Data source: <a href="https://github.com/CSSEGISandData">John Hopkins</a>'))
                           
                         )
                         
                         
    
    
    
    )
  )
)





