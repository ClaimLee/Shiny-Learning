
# Customize your UI with HTML ----------------------------------------------

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Hello Shiny!"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      # adding the new div tag to the sidebar            
      tags$div(class="header", checked=NA,
               tags$p("Ready to take the Shiny tutorial? If so"),
               tags$a(href="shiny.rstudio.com/tutorial", "Click Here!")
               
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server <- function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
  
  output$distPlot <- renderPlot({
    x    <- faithful[, 2]  # Old Faithful Geyser data
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
}

shinyApp(ui = ui, server = server) # this launches your app


# Layout your app using Bootstrap¡¯s grid system manually ------------------

server <- function(input, output, session) {
  
}

ui <- fluidPage(
  
  fluidRow(
    column(6, offset=1,
           h1("Title in one row"), width = 6
    )
    
  ),
  fluidRow(
    column(
      actionButton("button", "Click"), width = 6
      
    ),
    column(
      p("Row 2, Column 2 (button is col 1)"), width = 6
    )
    
  )
)

shinyApp(ui = ui, server = server)


# Using existing theme to reformat your web page --------------------------
library(shiny)
library(shinythemes)
# themes: cerulean, cosmo, flatly, journal, readable, spacelab, united
server <- function(input, output, session) {
  
}

ui <- fluidPage(theme=shinytheme("united"),
                
                titlePanel("Use an existing theme"),
                
                sidebarLayout(
                  
                  sidebarPanel(
                    h3("Note the button is black. This is different from the previous app."),
                    actionButton("button", "A button")
                  ), 
                  
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Plot"), 
                      tabPanel("Summary"), 
                      tabPanel("Table")
                    )
                  )
                )
)

shinyApp(ui = ui, server = server)


# Style yourself with CSS -------------------------------------------------
server <- function(input, output, session) {
  
}

ui <- basicPage(
  # this is your web page header information
  tags$head(
    # here you include your inline styles
    tags$style(HTML("
                    
                    body {
                    background-color: cornflowerblue;
                    color: #6B1413;
                    }
                    
                    "))
    ),
  
  h3("CSS using the HTML tag"),
  p("Some important text")
  
    )

shinyApp(ui = ui, server = server)


# Using shinyjs package to includeCSS -------------------------------------
# Using shinyjs is more convenient
library(shiny)
library(shinyjs)

server <- function(input, output, session) {
  
}

ui <- fluidPage(
  
  # This adds the CSS to the file
  shinyjs::inlineCSS(list(body = "color:DarkBlue")),
  titlePanel("Use the shinyjs package to add styles"),
  
  sidebarLayout(
    
    sidebarPanel(
      h3("Sidebar title")
    ), 
    
    mainPanel(
      "Body text"
    )
  )
)

shinyApp(ui = ui, server = server)


# The use of observe and observeEvent functions --------------------------------------------
server <- function(input, output, session) {
  
# The disadvantage of observe function
  
# Any reactive value inside observer function change will trigger the change of update result
  # observe({
  #   # even though the slider is not involved in a calculation, if
  #   # you change the slider it will run all this code and update the text box
  #   # changes to the mytext box also will trigger the code to run
  #   input$myslider
  #   txt <- paste(input$mytext, sample(1:10000, 1))
  #   updateTextInput(session, inputId = "myresults", value = txt)
  # 
  # })
  
# Please make a comparison with observeEvent function (this one is better)
  observeEvent(input$mytext, {

    # myslider is a reactive but it does not trigger the code to
    # run here because we're using observeEvent and only specified
    # input$mytext
    input$myslider
    txt <- paste(input$mytext, sample(1:10000, 1))
    updateTextInput(session, inputId = "myresults", value = txt)

  })
  
}

ui <-   basicPage(
  h3("The results text box gets updated if you change the other text box OR the slider."),
  sliderInput("myslider", "A slider:", min=0, max=1000, value=500),
  textInput("mytext", "Input goes here", value = "Initial value"),
  textInput("myresults", "Results will be printed here")
)

shinyApp(ui = ui, server = server)


# Using reactive function to generate output ------------------------------
server <- function(input, output, session) {
  
  # this is my reactive function -- I'm using it to
  # isolate reactions related to the text box
  mystring <- reactive({
    paste(input$mytext, " is what the user types")
  })
  
  observe({
    # The reactive will run each time the textbox changes and
    # print results to the console.
    txt <- mystring()                                              # Pay attention to the ()
    updateTextInput(session, inputId = "myresults", value = txt) 
  })
  
}

ui <- basicPage(
  
  h3("The reactive generates a string output which is added to the results text box"),
  textInput("mytext", "Input goes here"),
  textInput("myresults", "Results will be printed here", "")
  
)

shinyApp(ui = ui, server = server)


# eventReactive function --------------------------------------------------
server <- function(input, output, session) {
  
  # since both mytext and myslider are in the reactive
  # they both trigger the code to run
  myresults <- reactive({
    paste(input$mytext, input$myslider)
  })
  
  # eventReactive here tells Shiny only to trigger this code
  # when mytext changes
  myresults_lim <- eventReactive(input$mytext, {
    paste(input$mytext, input$myslider)
  })
  
  observe(updateTextInput(session, "myresults", value = myresults()))
  observe(updateTextInput(session, "myresults_lim", value = myresults_lim()))
  
  
}

ui <- basicPage(
  
  h3("Using eventReactive to limit reactions."),
  sliderInput("myslider", "", min=0, max=1000, value=500),
  textInput("mytext", "Input goes here"),
  textInput("myresults", "Text box + slider (updates when either changes)", "Initial value"),
  textInput("myresults_lim", "Text box + slider (updates when text box changes)", "Initial value")
  
)

shinyApp(ui = ui, server = server)


# Reactive values are kept separate or not --------------------------------
# Please make a comparison
server <- function(input, output, session) {
  
  # Now if you change the slider only the slider result changes
  # and the text box result stays the same. This is because
  # we isolated the reactive values in their own reactive function
  
  txt <- reactive({paste(input$mytext, sample(1:100, 1))})
  val <- reactive({paste(input$myslider, sample(1:100, 1), sep="-")})
  
  
  observe({
    res <- paste0(txt(), " | Slider ", val())
    updateTextInput(session, "myresults", value = res)
  })
  
# Please make a comparison
  # Notice that even if you only change the text box that the 
  # slider code also runs and gets changed. The reverse is also
  # true. You might want to isolate these pieces.
  # observe({
  #   txt <- paste(input$mytext, sample(1:100, 1))
  #   val <- paste(input$myslider,  sample(1:100, 1), sep="-")
  #   
  #   res <- paste0(txt, " | Slider ", val)
  #   updateTextInput(session, "myresults", value = res)
  # })

}

ui <- basicPage(
  
  h3("Changes to the text box and slider are separated so that a change to the text box will not affect the slider part of the results textbox"),
  sliderInput("myslider", "A slider:", min=0, max=1000, value=500),
  textInput("mytext", "Input goes here", "Text"),
  textInput("myresults", "Results will be printed here", "Initial value")
  
)

shinyApp(ui = ui, server = server)


# Use isolate to avoid triggering reactions -------------------------------
server <- function(input, output, session) {
  
  observe({
    updateTextInput(session, "text_output1", value = input$text_input)
  })
  
# Two methods, but observeEvent is better
  # instead of observe and isolate, you could instead use observeEvent
  observe({
    input$updateButton
    updateTextInput(session, "text_output2", value = isolate(input$text_input))
  })
  
  # observeEvent(input$updateButton, {
  #   updateTextInput(session, "text_output2", value = input$text_input)
  # })
  
}

ui <- basicPage(
  
  h3("The value in the text box gets printed to the results text box."),
  textInput("text_input", "Type here"),
  textInput("text_output1", "This box is constantly updating"),
  
  textInput("text_output2", "Updates only with action button click"),
  actionButton("updateButton", "Update list")
  
)
shinyApp(ui = ui, server = server)


# observe, reactive and render* in one app --------------------------------
server <- function(input, output, session) {
  
  # when the slider changes update the text box
  observe({
    updateTextInput(session, "mytext", value=input$myslider)
  })
  
  # when the slider changes update the dataset
  dat <- reactive({
    input$myslider
    cars[1:input$myslider,]
    
  })
  
  # Since dat() is generated from a reactive that 
  # is triggered by input$myslider this table will update
  # any time that input$myslider updates
  output$mytable <- renderTable({
    dat()
  })
  
}

ui <- fluidPage(
  
  titlePanel("An app using an observe, reactive and render"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("myslider", "Number of rows to display", min=1, max=50, value=5),
      textInput(inputId = "mytext", label = "Slider value")
    ), # end sidebar panel
    
    mainPanel(
      tabsetPanel(
        tabPanel("Table", tableOutput("mytable"))
      )
    ) # end main panel
  )
)

shinyApp(ui = ui, server = server)


# Create re-useable UI elements -------------------------------------------
# Just call the CreateSelectRadio function to do the same thing for those three tabs
# This can greatly reduce the block of your code
server <- function(input, output, session) {
  
  # A function to create a block of UI elements this 
  # function can be re-used to create similar blocks
  # of ui elements in different places
  createSelectRadio <- function(id, title){
    
    selectID <- paste0("myselect", id)
    radioID <- paste0("myradio", id)
    checkID <- paste0("mycheck", id)
    
    res <- list(
      h2(title),
      selectInput(inputId = selectID, label="", choices = sample(LETTERS, 3)),
      radioButtons(inputId = radioID, label="", choices = sample(letters,3)),
      checkboxInput(inputId = checkID, label="", value=TRUE)
    )
    
    return(res)
  }
  
  # here we create our blocks of UI elements by running the function
  output$forPlot    <- renderUI({createSelectRadio(1, "In plot tab")})
  output$forSummary <- renderUI({createSelectRadio(2, "In summary tab")})
  output$forTable   <- renderUI({createSelectRadio(3, "In table tab")})
  
}

ui <- basicPage(
  
  h3("All tabs have the same set of components created with a function."),
  
  tabsetPanel(
    tabPanel("Summary", uiOutput("forSummary")), 
    tabPanel("Plot",    uiOutput("forPlot")), 
    tabPanel("Table",    uiOutput("forTable"))
  )
  
)

shinyApp(ui = ui, server = server)


# Using the DT package for an interactive table ---------------------------
library(DT)

server <- function(input, output, session) {
  
  observe({
    updateTextInput(session, "mytext", value=input$myslider)
  })
  
  dat <- reactive({
    input$myslider
    mtcars[1:input$myslider,c("mpg", "cyl", "disp")]
    
  })
  
  # I'm setting paging = FALSE so all rows are shown all the time
  # scrollX adds a scrollbar, filter allows column filtering
  output$mytable <- DT::renderDataTable(dat(), 
                                        options = list(paging=FALSE, scrollX = TRUE), 
                                        rownames=TRUE, 
                                        filter = "top")
}

ui <- basicPage(
  
  h3("Interactive table using the DT data table renderer"),
  sliderInput("myslider", "Number of rows to display", min=1, max = 32, value = 5),
  DT::dataTableOutput("mytable")
  
) 

shinyApp(ui = ui, server = server)


# Use plot.ly to make a ggplot interactive --------------------------------
library(shiny)
library(plotly)

server <- function(input, output, session) {
  
  dat <- reactive(cars[1:input$myslider,])
  
  output$myplot <- renderPlotly({
    p <- ggplot(dat(), aes(speed, dist)) + geom_point(color="purple")
    p <- ggplotly(p)
    p
  })
}

ui <- basicPage(
  
  h3("Example of plot.ly, the plot is interactive"),
  sliderInput("myslider", "A slider:", min=1, max=50, value=10),
  plotlyOutput("myplot")
  
)

shinyApp(ui = ui, server = server)


# Use highcharts to make an interactive plot ------------------------------
library(shiny)
library(highcharter)
library(magrittr) # for the pipe %>%

server <- function(input, output, session) {
  
  dat <- reactive(cars[1:input$myslider,])
  
  output$myplot <- renderHighchart({
    highchart() %>% 
      hc_title(text = "Scatter chart") %>% 
      hc_add_series_scatter(dat()$speed, dat()$dist)
    
    # here is the code if you don't want to use the %>% pipe
    # hc_add_serie_scatter(hc_title(highchart(), 
    # text = "Scatter chart"), dat()$speed, dat()$dist)
  })
}

ui <-  basicPage(
  
  h3("Example of highcharter, the plot is interactive"),
  sliderInput("myslider", "A slider:", min=1, max=50, value=10),
  highchartOutput("myplot")
)

shinyApp(ui = ui, server = server)


# Use leaflet for an interactive map --------------------------------------
library(leaflet)
#### server
server <- function(input, output, session) {
  
  
  # create random points in the US
  dat <- reactive({
    long <- runif(input$myslider,-121, -77 )
    lat <- runif(input$myslider,33, 48)
    vals <- rpois(input$myslider, 5)
    data.frame(latitude = lat, longitude = long, vals)
  })
  
  output$mymap <- renderLeaflet({
    leaflet() %>% addProviderTiles("Stamen.TonerLite") %>%  
      addCircleMarkers(data = dat(), radius = ~vals ) %>% 
      setView(-20, 38.6, zoom=3)
  })
}

#### user interface
ui <- fluidPage(
  
  titlePanel("Example of leaflet interactive map"),
  
  sidebarLayout(
    
    sidebarPanel(
      h3("Slider changes number of map points"),
      sliderInput(inputId = "myslider", label = "Limit the ", min = 0, 
                  max = 50, value = c(10))
    ), #endsidebarpanel
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("mymap"))
        
      )
    )#end mainpanel
  )# end sidebarlayout
)

shinyApp(ui = ui, server = server)


# One practical example using module  -------------------------------------
library(shiny)
library(ggplot2)

# MODULE UI
scatterUI <- function(id) {
  ns <- NS(id)
  
  list(
    div(sliderInput(ns("slider1"), label = "Limit points", min = 0, max = 32, value = 15)),
    div(style="display: inline-block; height:220px;", plotOutput(ns("plot1"))),
    div(style="display: inline-block; height:220px;", plotOutput(ns("plot2")))
  )
}

# MODULE Server
scatter <- function(input, output, session, datname, var1, var2, ptshape, col1, col2) {
  
  dat <- eval(as.name(datname))
  dat <- dat[order(dat[[var1]]),]
  
  resultdata <- reactive({
    dat[1:input$slider1,]
  })
  
  output$plot1 <- renderPlot({
    plot(1:10)
    ggplot(resultdata(), aes_string(var1, var2)) + geom_point(color=col1, shape=ptshape, size=3)+
      ggtitle(paste("Using the", datname, "data.frame"))
  }, width=200, height=200)
  
  output$plot2 <- renderPlot({
    plot(1:10)
    ggplot(resultdata(), aes_string(var1, var2)) + geom_point(color=col2, shape=ptshape, size=3) +
      ggtitle(paste("Using the", datname, "data.frame"))
  }, width=200, height=200)
}


# App ui
ui <- fluidPage(
  h3("The module creates two plots and a slider and is called twice below"),
  scatterUI("prefix"),
  scatterUI("prefix2")
)

# App server
server <- function(input, output,session){
  
  callModule(scatter, "prefix", "cars", "speed", "dist",  1, "red", "blue")
  callModule(scatter, "prefix2", "mtcars", "mpg", "hp", 17, "forestgreen", "purple")
}

shinyApp(ui, server)


# Shiny Dashboard ---------------------------------------------------------
library(ggplot2)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                box(plotOutput("plot2", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("myslider", "Number of observations:", 1, 50, 15)
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)

server <- function(input, output) {
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    dat <- mtcars[1:input$myslider,]
    ggplot(dat, aes(mpg)) + geom_histogram(fill="cadetblue")
  })
  output$plot2 <- renderPlot({
    dat <- mtcars[1:input$myslider,]
    ggplot(dat, aes(mpg, wt)) + geom_point() + stat_smooth()
  })
}

shinyApp(ui, server)



