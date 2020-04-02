#
# covid19 | ui
#



library(shiny)
library(shinyWidgets)
library(leaflet)



ui <- fluidPage(theme = shinythemes::shinytheme("simplex"),
                
                titlePanel("", windowTitle = "COVID19"),
                
                mainPanel(width = 12,  # 12 is the maximum width (12 columns). It will fill the screen
                    
                    h1(HTML(title)),
                    
                    h5(HTML(paste(conf, death, sep = ' ')), 
                      style="text-align:center;"),
                          
                    fluidRow(
                        column(6,
                               leafletOutput(outputId = "covid.map.sp", width = "100%")
                               ),
                        column(6,
                               plotOutput(outputId = "timeline.8", width = "100%"
                               )
                    )),
                    
         
                    
                    fluidRow(
                        column(5,
                               br(),
                               br(),
                               pickerInput(inputId = 'countries',
                                           label = 'Countries',
                                           choices = countries,
                                           options = list(`actions-box` = T,                # pickerInput allows this option that incluse SELECT ALL
                                                          `live-search` = T),    # adding search box  
                                           multiple = T,
                                           selected = "US"),
                               br(),
                               htmlOutput(outputId = "summary")
                               ),
                        column(7,
                               plotOutput(outputId = "timeline.p", width = "100%")
                               )
                    ),
                    
                    br(),
                    br()
                )
)
                
                






