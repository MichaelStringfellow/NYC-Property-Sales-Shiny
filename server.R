## server.R ##

function(input, output, session) {

  # Map rendering -----------------------------------------------------------------------------
  # Initializzation
  output$myMap = renderLeaflet({ leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -73.95, lat = 40.745, zoom =13)
  })
 
  # Reactively receive instructions from UI----------------------------------------------
  # reactives for the map
  bins <- reactive({
    if (input$element == 'Volume') {
      bins = c(0, 100, 300, 500, 1000, 3000, Inf) # color bin for volume
    } else if (input$element == 'Median Price') {
      bins = c(0, 1000000, 1250000, 1500000, 1750000, 2000000, Inf) # color bin for price
    }
  })
  
  select <- reactive({
    if (input$element == 'Volume') {
      select = Map_data_manhattan@data$n # select column n for map vlaue input
    } else if (input$element == 'Median Price') {
      select = Map_data_manhattan@data$price # select column price for value input
    }
  })
   
  # reactives for the donut chart 
  selectDonutAll <- reactive({
    if (input$element == 'Volume') {
      selectDonutAll = donutAll$n 
    } else if (input$element == 'Median Price') {
      selectDonutAll = donutAll$price
    }  
  })  
  
  selectDonut <- reactive({
      selectDonut = donut %>% filter(district == input$ManhattanNeighborhood) 
  })  
  
  selected <- reactive({
    if (input$element == 'Volume') {
      selected = "n"
    } else if (input$element == 'Median Price') {
      selected = "price"
    }  
  })  
  
  # reactive data frame for bar chart
  selectTime <- reactive({
    selectTime = chartTime %>% filter(district == input$ManhattanNeighborhood) 
  })    
  
  
  
  
  
 # Reactively update map with from received UI instruction----------------------------------------------------------------
  observe({
    # Leaflet Manhattan Map
    pal = colorBin("YlOrRd", domain = select(), bins = bins()) # set fill color
    
    labels <- paste("<strong>",Map_data_manhattan@data$neighborhood,"</strong>",'<br/>',
                     "Total Transaction:", Map_data_manhattan@data$n, '<br/>',
                     "Median Price:", '$',format(Map_data_manhattan@data$price, big.mark = ',')
                       ) %>% lapply(htmltools::HTML)
    
    leafletProxy( 'myMap',
                  data = Map_data_manhattan) %>%
        addPolygons(
        smoothFactor = 0.5, fillOpacity = 0.7, fillColor = ~ pal(select()),
        color = "white", dashArray = "1", weight = 1,
        highlight = highlightOptions(weight = 2, color = "white", 
                                     fillOpacity = 1,bringToFront = TRUE),
        label = ~ labels) %>%
      
        clearControls() %>%
      
        addLegend(
        "bottomleft", pal = pal, values = select(),
        title = input$element,
        labFormat = labelFormat(prefix = " "),
        opacity = 0.75)
    
    
    # Plotly Donut Chart for building class
    output$Donut <- renderPlotly({
      if (input$ManhattanNeighborhood == "All") {
        donutAll %>%
        plot_ly(labels = ~ building.class.category, values = selectDonutAll()) %>% 
        add_pie(hole = 0.3,textposition = 'inside',
                textinfo = 'percent') %>%
        layout(title = paste(input$element ,"By Building Class for All"),  showlegend = F,
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))  
      } else if (selected() == "n"){
        
          plot_ly(data = selectDonut(), labels = ~ building.class.category, values = ~ n) %>% 
          add_pie(hole = 0.3,textposition = 'inside',
                  textinfo = 'percent') %>%
          layout(title = paste('Volumn by Building Class for',input$ManhattanNeighborhood),  showlegend = F,
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))  
        
      } else {
        plot_ly(data = selectDonut(), labels = ~ building.class.category, values = ~ price) %>% 
          add_pie(hole = 0.3,textposition = 'inside',
                  textinfo = 'percent') %>%
          layout(title = paste("Median Price By Building Class for",input$ManhattanNeighborhood),  showlegend = F,
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))  
      }
    })
    
    # Plotly Time Series Chart for building class
    output$Time <- renderPlotly({
      if (input$ManhattanNeighborhood == "All") {
        if(input$element == 'Volume') {
          chartTimeAll %>% plot_ly(x = ~date, y = ~n, type = 'bar', name = 'Median Price', 
                                   marker = list(color = 'rgb(49,130,189)')) %>%
            layout(xaxis = list(title = "", tickangle = -45),
                   yaxis = list(title = ""),
                   title = 'Total Transaction by Month for All',
                   margin = list(b = 50),
                   barmode = 'group')
        } else if (input$element == 'Median Price') {
          chartTimeAll %>% plot_ly(x = ~date, y = ~price, type = 'bar', name = 'Median Price', 
                                   marker = list(color = 'rgb(49,130,189)')) %>%
            add_trace(y = ~avgPrice, name = 'Avg Price', 
                      marker = list(color = 'rgb(204,204,204)')) %>%
            layout(xaxis = list(title = "", tickangle = -45),
                   yaxis = list(title = ""),
                   title = 'Median/Average Price by Month for All',
                   legend = list(orientation = 'h',x = 0, y = 0.9),
                   margin = list(b = 50),
                   barmode = 'group')
        }
      } else if (selected() == "n") {
        selectTime() %>% plot_ly(x = ~date, y = ~n, type = 'bar', name = 'Median Price', 
                                 marker = list(color = 'rgb(49,130,189)')) %>%
          layout(xaxis = list(title = "", tickangle = -45),
                 yaxis = list(title = ""),
                 title = paste('Total Transaction by Month for',input$ManhattanNeighborhood),
                 margin = list(b = 50),
                 barmode = 'group')
        
      } else {
        selectTime() %>% plot_ly(x = ~date, y = ~price, type = 'bar', name = 'Median Price', 
                                 marker = list(color = 'rgb(49,130,189)')) %>%
          add_trace(y = ~avgPrice, name = 'Avg Price', 
                    marker = list(color = 'rgb(204,204,204)')) %>%
          layout(xaxis = list(title = "", tickangle = -45),
                 yaxis = list(title = ""),
                 title = paste('Median/Average Price per Month for',input$ManhattanNeighborhood),
                 legend = list(orientation = 'h',x = 0, y = 0.9),
                 margin = list(b = 50),
                 barmode = 'group')
      }
    })
  }) # end for observe
  
  
  # datatable rendering--------------------------------------------------------------------------------
  output$table <- DT::renderDataTable ({
    if(input$boro=='All') {
      subset1 = subset1 %>% filter(sale.price<=input$maxPrice & sale.price>=input$minPrice)
    } else {
      subset1 = subset1 %>% filter(borough==input$boro,
                                   sale.price<=input$maxPrice & sale.price>=input$minPrice)
    }

   DT::datatable(subset1, rownames = FALSE) %>% 
      formatDate(~sale.date) %>% 
      formatCurrency(~sale.price, currency = "$", interval = 3, mark = ",")
 
  })
  
  # Borough Map rendering-------------------------------------------------------------------------------
  
  # By ordering by sales price, we ensure that the comparatively rare higher prices
  # will be drawn last and thus be easier to see
  sample_geocoded <- sample_geocoded[order(sample_geocoded$sale.price),]
  
  # set color for circles
  pal2 = colorBin( palette="YlOrRd", domain=sample_geocoded$sale.price, 
                   na.color="transparent")
  
  # style popup label
  labels2 = paste(sample_geocoded$formatted_address, '<br/>',
                  tolower(sample_geocoded$building.class.category), '<br/>',
                  '$',format(sample_geocoded$sale.price, big.mark = ',')) %>% lapply(htmltools::HTML)
  
  output$myMap2 = renderLeaflet({ leaflet(sample_geocoded) %>%
      addProviderTiles("CartoDB.DarkMatter") %>%
      setView(lng = -73.9969, lat = 40.7061, zoom = 12) %>%
      addCircleMarkers(~long, ~lat,
                       fillColor = ~pal2(sale.price), fillOpacity = 0.7, color="white", 
                       radius=~sample_geocoded$sale.price/1000000, # set radius for circle
                       stroke=FALSE,
                       label = labels2,
                       labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), 
                                                    textsize = "13px", direction = "auto")
      ) %>%
      addLegend( pal=pal2, values=~sale.price, opacity=0.9, title = "Sale Price", position = "bottomright" )
 
    
  })
 
}
  
