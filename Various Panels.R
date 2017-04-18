library(shiny)

# Conditional Panel
# You can use conditionalPanel() to either show or hide a UI element based on a simple condition, 
# such as the value of another input.
ui <- fluidPage(
  numericInput("num", "Number", 5, 1, 10),
  conditionalPanel(
    "input.num <5",
    "Can't be smaller than 5"
  )
)
server <- function(input, output) {}
shinyApp(ui = ui, server = server)


# navbarPage() or tabsetPanel()
# To have multiple tabs in the UI
# If your apps requires more than a single "view", you can have separate tabs
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Tab 1", "Hello"),
    tabPanel("Tab 2", "there!")
  )
)
server <- function(input, output) {}
shinyApp(ui = ui, server = server)


# isolate()
# Use isolate() function to remove a dependency on a reactive variable
# Any reactive variables that are inside isolate() will not result in the code re-executing when their value is changed


# update*Input()
# Use update*Input() functions to update input values programmatically
ui <- fluidPage(
  sliderInput("slider", "Move me", value = 5, 1, 10),
  numericInput("num", "Number", value = 5, 1, 10)
)
server <- function(input, output, session) {
  observe({
    updateNumericInput(session, "num", value = input$slider)
  })
}
shinyApp(ui = ui, server = server)

# Scoping rules in Shiny apps

# global.R
# Use global.R to define objects available to both ui.R and server.R
# If there are objects that you want to have available to both ui.R and server.R, you can place them in global.R.

# Add images
# You can add an image to your Shiny app by placing an image under the "www" folder and using the UI function img(src = "image.png"). 
# Shiny will know to automatically look in the "www" folder for the image.


# Add JavaScript/CSS
library(shiny)
ui <- fluidPage(
  tags$head(tags$script("alert('Hello!');")),
  tags$head(tags$style("body{ color: blue; }")),
  "Hello"
)
server <- function(input, output) {
  
}
shinyApp(ui = ui, server = server)

