## ui.R ##

navbarPage(
  'NYC Property', 
  theme = shinytheme("flatly"),

# Interactive Map-----------------------------------------------------------------------------------
  tabPanel('Manhattan Map', 
           div( class='outer', tags$head(includeCSS('styles.css')),
                leafletOutput('myMap', width = '100%', height = '100%')),                  
                    
# Control Panel -----------------------------------------------------------------------------------
                    
# Control Panel Style: 
  absolutePanel(id = 'controls', 
                class = 'panel panel-default', fixed = TRUE, draggable = TRUE, 
                top = 60, left = 'auto', right = 30, bottom = 'auto', width = 500,  height = 'auto',
  
                # Control Panel-Choose Inputs:
                
                fluidRow( column(6, selectInput(inputId = 'element', 
                                                label = h4('Select'),
                                                choices = element, 
                                                selected = 'Volume')
                                 ),
                          column(6, selectInput(inputId = 'ManhattanNeighborhood', 
                                                label = h4('Select Neighborhood'),
                                                choices = neighborhood, 
                                                selected = 'All')
                                 )
                          ),
                br(),
                
                # Control Panel- Plot Output:
                plotlyOutput("Donut", height = 270), # pie chart for buildingclass breakdown
                
                plotlyOutput("Time", height = 270)   # bar chart for price over time
                )
), # Tab panel end
           
# Tab 2  NYC Borougnwise bubble Map---------------------------------------------------------------------
tabPanel('Borough Map', 
         div( class='outer', tags$head(includeCSS('styles.css')),
              leafletOutput('myMap2', width = '100%', height = '100%'))),                  
         

# Tab 3  Data Table Explorer--------------------------------------------------------------------
tabPanel("Data Explorer",
         fluidRow( column(2, selectInput(inputId = 'boro', "Borough", borough, selected = 'All')),
                   column(2, numericInput('minPrice','Minimum Price',min=500000,max=300000000,value=1000000)),
                   column(2, numericInput('maxPrice', 'Maximum Price', min=1500000, max=300000000,value=2000000)),
 
         DT::dataTableOutput("table"))
         )
           

) # End
           