server <- function(input, output, session) {
  
  # Create default map to start with on the app
  output$map <- renderLeaflet({
    
    default_var <- var_inputs[[1]]
    
    pal <- colorNumeric("plasma", rw_tracts[[default_var]], na.color = "#bfbfbf", reverse = T)
    
    rw_tracts %>% 
      leaflet() %>% 
      addMapboxGL(style = "mapbox://styles/mapbox/light-v9") %>% 
      addPolygons(
        fillColor = pal(rw_tracts[[default_var]]),
        fillOpacity = 0.6,
        color = "black",
        weight = 0.5,
        opacity = 1,
        layerId = ~geoid,
        group = "variable"
      )
  })
  
  # Any time a new variable is selected from the dropdown menu the choropleth
  # map is redrawn for that indicator
  observeEvent(input$variable, {

    var_name <- input$variable

    if(is.null(var_name))
      return()
  
    pal <- colorNumeric("plasma", rw_tracts[[var_name]], na.color = "#bfbfbf")
    
    # The default legend has values low/top to high/bottom so need to reverse the palette
    rev_pal <- colorNumeric("plasma", rw_tracts[[var_name]], na.color = "#bfbfbf", reverse = T)

    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(
        data = rw_tracts,
        fillColor = pal(rw_tracts[[var_name]]),
        fillOpacity = 0.6,
        color = "black",
        weight = 0.5,
        opacity = 1,
        layerId = ~geoid,
        group = "variable"
      ) %>% 
      addLegend(
        "topright", 
        pal = rev_pal, 
        values = rw_tracts[[var_name]],
        opacity = 1,
        labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
        layerId = "legend"
      )
  })
  
  # Every time a tract is clicked on, a table is generated in the side pane to
  # display all its indicator values and an outline it added to the map
  observeEvent(input$map_shape_click, {

    event <- input$map_shape_click
    
    if(is.null(event)) 
      return() 
    
    # remove the outline of the previously clicked tract
    if(event$id == "selected") {
      leafletProxy("map") %>% removeShape(layerId = "selected")
    }
    
    # add the outline for the clicked tract
    leafletProxy("map") %>%
      addPolygons(
        data = rw_tracts %>% filter(geoid == event$id),
        fillColor = NA,
        fillOpacity = 0,
        color = "black",
        weight = 2,
        opacity = 1,
        layerId = "selected"
      )
  })
  
  tract_info <- eventReactive(input$map_shape_click, {
    event <- input$map_shape_click
    
    if(is.null(event)) 
      return() 
    
    if(event$id == "selected")
      return()
    
    # Get the row for the selected tract, reshape the data, join in display
    # names and formatting info from the manually created csv file, then build
    # out a simple table for display in the side pane
    rw_tracts %>% 
      st_drop_geometry() %>% 
      filter(geoid == event$id) %>% 
      select(-geoid) %>% 
      pivot_longer(everything()) %>%
      right_join(indic_info, by = c("name" = "var_name")) %>%
      arrange(order) %>% 
      gt(rowname_col = "display_name", groupname_col = "var_group") %>% 
      cols_hide(vars(name, val_fmt, order)) %>% 
      tab_style(cell_text(weight = "bold"), cells_row_groups()) %>% 
      fmt_currency(vars(value), rows = val_fmt == "cur", decimals = 0) %>% 
      fmt_percent(vars(value), rows = val_fmt == "pct", decimals = 1) %>%
      fmt_number(vars(value), rows = val_fmt == "num", decimals = 0) %>%
      fmt_number(vars(value), rows = val_fmt == "rt", decimals = 1) %>% 
      fmt_missing(vars(value)) %>% 
      tab_options(column_labels.hidden = TRUE)
  })
  
  # Output the map for access on the UI side
  output$tract_table <- render_gt(
    tract_info(),
    height = px(700),
    width = px(400)
  )
  
  # Output a UI component for the side pane
  output$sidebar <- renderUI({
    fixedPanel(
      id = "sidebar", class = "panel panel-default",
      style = "overflow-y: scroll",
      top = 80, right = 0, width = 400, height = 650,
      gt_output(outputId = "tract_table")
    )
  })
}
