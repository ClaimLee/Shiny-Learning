#=========================================================================
#                  Most popular HTML elements
#=========================================================================
library(shiny)

server <- function(input, output, session) { } #the server
ui <- basicPage( 
# tags$a()
# Creates a link to a web page
tags$a(href="www.rstudio.com", "Click here!"),

# tags$audio()
# Adds an audio element (e.g., a sound to your app)
# autoplay	A valueless attribute. If present, audio starts playing automatically when loaded
# controls	A valueless attribute. If present, play controls are displayed
# src	The location of the audio file to play
# type	The type of file to play
tags$audio(src = "sound.mp3", type = "audio/mp3", autoplay = NA, controls = NA),
br(),br(),

# tags$b()
# Creates bold text.
tags$b("This text is bold."),
br(),br(),

# tags$blockquote()
# Creates a block of quoted text. Usually it is displayed in a special way.
# cite	the source of the quote
tags$blockquote("Tidy data sets are all the same. Each messy data set is messy in its own way.", cite = "Hadley Wickham"),
br(),br(),

# tags$br()
# Creates a line break
tags$div(
  "Some text followed by a break", 
  tags$br(),
  "Some text following a break"
),
br(),br(),

# tags$code()
# Creates text formatted as computer code
tags$code("This text will be displayed as computer code."),
br(),br(),

# tags$div()
# Creates a section (e.g., "division") of an HTML document. divs provide a useful hook for CSS styling.
# class	The class of the div, a useful way to style the div with CSS
# id	The ID of the div, a useful way to style the div with CSS
# style	CSS styling to apply to the div
# tags$div(... = ),
br(),br(),

# tags$em()
# Creates emphasized (e.g., italicized) text.
tags$em("This text is emphasized."),
br(),br(),

# tags$embed()
# Embed a plug-in or third party application
# src	The source of the file to embed
# type	The MIME type of the embedded content
# height	The height of the embedded content
# width	The width of the embedded content
tags$embed(src = "animation.swf"),
br(),br(),

# h1(),h2(),h3(),h4(),h5(),h6()
# Adds hierarchical headings
tags$div(
  tags$h1("Heading"), 
  tags$h2("Subheading"), 
  tags$h3("Subsubheading"), 
  tags$h4("Subsubsubheading"), 
  tags$h5("Subsubsubsubheading"), 
  tags$h6("Subsubsubsubsubheading") 
),
br(),br(),

# tags$hr()
# Adds a horizontal line (e.g., horizontal rule)
# The line will appear in the UI page
tags$hr(),
br(),br(),

# tags$i()
# Creates italicized text.
tags$i("This text is italicized."),
br(),br(),

# tags$iframe()
# Creates an inline frame to embed an HTML document in.
# src	The URL of the HTML document to embed
# srcdoc	A raw HTML document to embed
# scrolling	Should iframe display scrollbars (yes, no, auto)
# seamless	A valueless attribute. Should the iframe seem like part of the web page?
# height	The height of the iframe
# width	The width of the iframe
# name	The name of the iframe

# tags$img()
# Creates an image
# src	The source of the image to embed
# height	The height of the image
# width	The width of the image
tags$img(src = "www.rstudio.com", width = "100px", height = "100px"),
br(),br(),

# tags$ol()
# Create an ordered list (i.e., a numbered list).
tags$ol(
  tags$li("First list item"), 
  tags$li("Second list item"), 
  tags$li("Third list item")
),
br(),br(),

# tags$p()
# Create a paragraph (a block of text that begins on its own line)
tags$div(
  tags$p("First paragraph"), 
  tags$p("Second paragraph"), 
  tags$p("Third paragraph")
),
br(),br(),

# tags$pre()
# Create pre-formatted text, text that looks like computer code.
tags$pre("This text is preformatted."),
br(),br(),

# tags$script()
# Add a client-side script such as javascript. You must wrap the actual script in HTML to prevent it from being passed as text.
tags$script(HTML("if (window.innerHeight > 400) alert('Screen too big');")),
br(),br(),

# tags$span()
# Create a group of inline elements. Normally used to style a string of text
tags$div(
  HTML(paste("This text is ", tags$span(style="color:red", "LIFEI"), sep = ""))
),
br(),br(),

# tags$strong()
# Create bold text
tags$strong("This text is strongly emphasized."),
br(),br(),

# tags$style()
# Create style specifications. A way to add CSS styles directly to your Shiny App.

# tags$sub(), tags$sup()
# Create subscript or super script.
tags$div(
    HTML(paste("E = mc", tags$sup(2), sep = "")), 
    HTML(paste("H", tags$sub(2), "0", sep = ""))
),
br(),br(),

# tags$ul, tags$li()
# Create an unordered list (i.e., a list of bullet points).
tags$ul(
    tags$li("First list item"), 
    tags$li("Second list item"), 
    tags$li("Third list item")
),
br(),br(),

# tags$video()
# Add a video
# autoplay	A valueless attribute. If present, video starts playing automatically when loaded
# controls	A valueless attribute. If present, Shiny will display play controls.
# src	The location of the video file to play
# height	The height of the video
# width	The width of the video
tags$video(src = "video.mp4", type = "video/mp4", autoplay = NA, controls = NA)

)
shinyApp(ui = ui, server = server) # this launches your app


