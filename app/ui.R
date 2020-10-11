ui <- fluidPage(
  tags$style(
    type = "text/css", 
    "
    html, body {
      width: 100%;
      height: 100%
    }
    #map {
      width: calc(100% - 400px + 15px) !important;
      padding: 0
    }
    "
  ),
  img(src='logo.png', align = "right", height="70"),
  selectInput(
    "variable", 
    "Variable:", 
    var_inputs, 
    selected = var_inputs[[1]], 
    width = "calc(100% - 400px + 10px)"
  ),
  leafletOutput("map", width = "100%", height = "650px"),
  uiOutput("sidebar")
)
