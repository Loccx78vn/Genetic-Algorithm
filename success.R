# Load required libraries
pacman::p_load(shiny, 
               leaflet, 
               dplyr, 
               networkD3)

# Convert GA solution to a matrix
optimal_quantities <- matrix(ga_result@solution, nrow = 5, ncol = 3, byrow = TRUE)

# Define UI
ui <- fluidPage(
  titlePanel("Warehouse Distribution Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "warehouse", 
                         label = "Select Warehouse:", 
                         choices = paste("Warehouse", 1:5), 
                         selected = paste("Warehouse", 1))
      
    ), 
    mainPanel(
      fluidRow(
        column(12, leafletOutput("map", height = "400px")),
        column(4, uiOutput("sankey_ui")),
        column(8, dataTableOutput("cost_table"))
        
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  location <- reactive({
    req(input$warehouse) 
    as.numeric(sub("Warehouse ", "", input$warehouse))
  })
  
  # Create the leaflet map
  output$map <- renderLeaflet({
    req(location())
    
    # Initialize the map
    map <- leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(data = warehouses %>% filter(ID %in% location()), 
                 lat = ~Latitude, 
                 lng = ~Longitude, 
                 label = ~paste0("<strong> ID Warehouse: </strong> ", ID, "<br/> ",
                                 "<strong> Province: </strong> ", Province, "<br/> ",
                                 "<strong> District: </strong> ", District, "<br/> ",
                                 "<strong> Address: </strong> ", Address, "<br/> ") %>% 
                   lapply(htmltools::HTML),
                 icon = warehouse_icon) %>%
      addMarkers(data = distribution_centers, 
                 lat = ~Latitude, 
                 lng = ~Longitude, 
                 label = ~paste0("<strong> ID Distribution Center: </strong> ", ID, "<br/> ",
                                 "<strong> Province: </strong> ", Province, "<br/> ",
                                 "<strong> District: </strong> ", District, "<br/> ",
                                 "<strong> Address: </strong> ", Address, "<br/> ") %>% 
                   lapply(htmltools::HTML),
                 icon = dc_icon)
    
    qty_data <- optimal_quantities[location(), , drop = FALSE]  
    
    # Add routes based on the optimal quantities
    for (i in 1:nrow(qty_data)) {
      for (j in 1:ncol(qty_data)) {
        if (qty_data[i, j] > 0) {
          route_start <- warehouses[warehouses$ID == i, c("Longitude", "Latitude")]
          route_end <- distribution_centers[distribution_centers$ID == j, c("Longitude", "Latitude")]
          
          map <- map %>%
            addPolylines(lat = c(route_start$Latitude, route_end$Latitude),
                         lng = c(route_start$Longitude, route_end$Longitude),
                         color = "black", weight = 2, opacity = 0.5)
        }
      }
    }
    
    map  # Return the modified map
  })
  
  # Render the cost table
  output$cost_table <- renderDataTable({
    req(location())
    
    gt <- result %>% 
      filter(Warehouse %in% paste("WH", location())) %>% 
      select(c(Warehouse, DC, Loading_Cost, Transport_cost))
    
    gt
  })
  
  # Render the Sankey diagram
  output$sankey_ui <- renderUI({
    req(location())
    
    qty_data <- optimal_quantities[location(), , drop = FALSE] 
    req(nrow(qty_data) > 0)  # Ensure there is data to display
    
    # Create links data frame
    links <- data.frame(
      source = rep(0:(nrow(qty_data) - 1), each = ncol(qty_data)),  
      target = as.vector(sapply(0:(ncol(qty_data) - 1), 
                                function(j) rep(nrow(qty_data) + j, nrow(qty_data)))),
      value = as.vector(qty_data)  
    )
    
    # Filter for selected warehouses
    links <- links %>% filter(source %in% (location() - 1))  
    
    # Create nodes data frame
    nodes <- data.frame(name = c(paste("Warehouse", location()), paste("DC", 1:ncol(qty_data))))
    
    # Create the Sankey network
    sankey <- sankeyNetwork(Links = links, 
                            Nodes = nodes, 
                            Source = "source", 
                            Target = "target", 
                            Value = "value",
                            NodeID = "name",
                            height = 500,  
                            width = 400,
                            fontSize = 12,
                            nodeWidth = 30)
    
    sankey  # Output the Sankey diagram
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
