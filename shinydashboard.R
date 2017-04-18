# Basic skeleton ----------------------------------------------------------
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)

server <- function(input, output) { }

shinyApp(ui, server)


# Row-based box layout -----------------------------------------------------------------
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(
        title = "Histogram", status = "primary", background = "maroon", solidHeader = TRUE,
        collapsible = TRUE, width = 4, height = 300,
        plotOutput("plot3", height = 250)
      ),
      
      box(
        title = "Histogram", status = "primary", background = "maroon", solidHeader = TRUE,
        collapsible = TRUE, width = 4, height = 300,
        plotOutput("plot3", height = 250)
      ),
      
      box(
        title = "Inputs", status = "warning", background = "black", solidHeader = TRUE,
        collapsible = TRUE, width = 4, height = 300,
        "Box content here", br(), "More box content",
        sliderInput("slider", "Slider input:", 1, 100, 50),
        textInput("text", "Text input:")
      )
    )
  )
)

server <- function(input, output) { }

shinyApp(ui, server)


# Use tabBox --------------------------------------------------------------
body <- dashboardBody(
  fluidRow(
    tabBox(
      title = "First tabBox",
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1", height = "250px",
      tabPanel("Tab1", "First tab content"),
      tabPanel("Tab2", "Tab content 2")
    ),
    tabBox(
      side = "right", height = "250px",
      selected = "Tab3",
      tabPanel("Tab1", "Tab content 1"),
      tabPanel("Tab2", "Tab content 2"),
      tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
    )
  ),
  fluidRow(
    tabBox(
      # Title can include an icon
      title = tagList(shiny::icon("gear"), "tabBox status"),
      tabPanel("Tab1",
               "Currently selected tab from first box:",
               verbatimTextOutput("tabset1Selected")
      ),
      tabPanel("Tab2", "Tab content 2")
    )
  )
)

shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "tabBoxes"),
    dashboardSidebar(),
    body
  ),
  server = function(input, output) {
    # The currently selected tab from the first box
    output$tabset1Selected <- renderText({
      input$tabset1
    })
  }
)


# Use infoBox -------------------------------------------------------------
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Info boxes"),
  dashboardSidebar(),
  dashboardBody(
    # infoBoxes with fill=FALSE
    fluidRow(
      # A static infoBox
      infoBox("New Orders", 10 * 2, icon = icon("bell", lib = "font-awesome")),
      # Dynamic infoBoxes
      infoBoxOutput("progressBox"),
      infoBoxOutput("approvalBox")
    ),
    
    # infoBoxes with fill=TRUE
    fluidRow(
      infoBox("New Orders", 10 * 2, icon = icon("tv", lib = "font-awesome"), fill = TRUE),
      infoBoxOutput("progressBox2"),
      infoBoxOutput("approvalBox2")
    ),
    
    fluidRow(
      # Clicking this will increment the progress amount
      box(width = 4, actionButton("count", "Increment progress"))
    )
  )
)

server <- function(input, output) {
  output$progressBox <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = icon("cab", lib = "font-awesome"),
      color = "purple"
    )
  })
  output$approvalBox <- renderInfoBox({
    infoBox(
      "Approval", "80%", icon = icon("ship", lib = "font-awesome"),
      color = "yellow"
    )
  })
  
  # Same as above, but with fill=TRUE
  output$progressBox2 <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = icon("apple", "fa-2x", lib = "font-awesome"),
      color = "purple", fill = TRUE
    )
  })
  output$approvalBox2 <- renderInfoBox({
    infoBox(
      "Approval", "80%", icon = icon("thumbs-up", lib = "font-awesome"),
      color = "yellow", fill = TRUE
    )
  })
}

shinyApp(ui, server)


# valueBox ----------------------------------------------------------------
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Value boxes"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      # A static valueBox
      valueBox(10 * 2, "New Orders", icon = icon("credit-card")),
      
      # Dynamic valueBoxes
      valueBoxOutput("progressBox"),
      
      valueBoxOutput("approvalBox")
    ),
    fluidRow(
      # Clicking this will increment the progress amount
      box(width = 4, actionButton("count", "Increment progress"))
    )
  )
)

server <- function(input, output) {
  output$progressBox <- renderValueBox({
    valueBox(
      paste0(25 + input$count, "%"), "Progress", icon = icon("list"),
      color = "purple"
    )
  })
  
  output$approvalBox <- renderValueBox({
    valueBox(
      "80%", "Approval", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
}

shinyApp(ui, server)


# Column-based box layout -------------------------------------------------
body <- dashboardBody(
  fluidRow(
    column(width = 4,
           box(
             title = "Box title", width = NULL, status = "primary",
             "Box content"
           ),
           box(
             title = "Title 1", width = NULL, solidHeader = TRUE, status = "primary",
             "Box content"
           ),
           box(
             width = NULL, background = "black",
             "A box with a solid black background"
           )
    ),
    
    column(width = 4,
           box(
             status = "warning", width = NULL,
             "Box content"
           ),
           box(
             title = "Title 3", width = NULL, solidHeader = TRUE, status = "warning",
             "Box content"
           ),
           box(
             title = "Title 5", width = NULL, background = "light-blue",
             "A box with a solid light-blue background"
           )
    ),
    
    column(width = 4,
           box(
             title = "Title 2", width = NULL, solidHeader = TRUE,
             "Box content"
           ),
           box(
             title = "Title 6", width = NULL, background = "maroon",
             "A box with a solid maroon background"
           )
    )
  )
)

# We'll save it in a variable `ui` so that we can preview it in the console
ui <- dashboardPage(skin = "yellow",
  dashboardHeader(title = "Column layout"),
  dashboardSidebar(),
  body
)

# Preview the UI in the console
shinyApp(ui = ui, server = function(input, output) { })


# Logout panel ------------------------------------------------------------
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "custom font"),
  dashboardSidebar(
    # Custom CSS to hide the default logout panel
    tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
    
    # The dynamically-generated user panel
    uiOutput("userpanel")
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    )
  )
)

server <- function(input, output, session) {
  output$userpanel <- renderUI({
    # session$user is non-NULL only in authenticated sessions
    if (!is.null(session$user)) {
      sidebarUserPanel(
        span("Logged in as ", session$user),
        subtitle = a(icon("sign-out"), "Logout", href="__logout__"))
    }
  })
}

shinyApp(ui, server)


# How to add custom CSS to your dashboard ---------------------------------
# There are two ways: one is create css file in the "www" folder; 
#                     second is put css code directly into dashboardBody()

library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "custom font"),
  dashboardSidebar(),
  
  # Way 1
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    )
    
  # Way 2
  # dashboardBody(
  # tags$head(tags$style(HTML('
  #     .main-header .logo {
  #                           font-family: "Georgia", Times, "Times New Roman", serif;
  #                           font-weight: bold;
  #                           font-size: 24px;
  #                           }
  #                           ')))
  
  )
)

server <- function(input, output) { }

shinyApp(ui, server)

