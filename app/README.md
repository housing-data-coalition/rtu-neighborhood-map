This very simple app is created using the [shiny](https://shiny.rstudio.com/) package in R, which makes it easy to build interactive web apps straight from R. While the structure of these apps can get more complex, this one is very simple and is composed of these three files:

* `global.R` - loads all the packages used by the app and imports all the data that will be used.
* `server.R` - set up all the server-side operations for the app, such as creating the map and table as "reactive" elements that will be recreated based on specific events triggered on the UI side, such as selecting a variable from the menu or clicking on a census tract from the map.
* `ui.R` - set up the user interface for the app, such as layout for all the different elements like menus and images, as well as outputs created in server.R such as the map and table.
