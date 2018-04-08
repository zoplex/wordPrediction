#
#       ui.r
#
# Define UI for application that draws a histogram
fluidPage(
        tags$head(tags$style(paste0("#shiny-notification-panel {top: 50% !important;left: "
                        ,"50% !important;margin-top: -100px !important;margin-left: -50px "
                        ,"!important; color: blue;font-size: 20px;font-style: verdana; width: 410px;}"))),
        
        # Application title
        titlePanel("Word prediction"),
        
        # Sidebar with a slider input for number of bins
        sidebarLayout(
                sidebarPanel(
                        textInput("enteredPhrase", "Enter Phrase:", "start content"),
                        verbatimTextOutput("value"),
                        actionButton("do", "Click when done with input"),
                        wellPanel(p(strong("top picks (once clicked, new input is needed, or Click when done input):")),
                        actionButton("pick1", "prediction1"),
                        actionButton("pick2", "prediction2"),
                        actionButton("pick3", "prediction3")),
                        br(),
                        wellPanel(p(strong("model loading options:")),
                        actionButton("loadBase", "Click to use base model (default)"),
                        br(),
                        actionButton("loadAdvance", "Click to load/use Advance model (3-5 min loading time)"),
                        br()), width=8),
        mainPanel(
                         h3("Instructions:")
                        ,h5("    - Text prediction portal -")
                        ,h5("    ")
                        ,span(textOutput("textEntered"), style="color:blue")
                        ,h5("    ")
                        ,span(textOutput("textCleaned"), style="color:blue")
                        ,h5("    ")
                        ,span(textOutput("modelLoading"), style="color:blue")
                        ,h5("    ")
                        ,span(textOutput("n1grams_items"), style="color:purple")
                        ,span(textOutput("n2grams_items"), style="color:purple")
                        ,span(textOutput("n3grams_items"), style="color:purple")
                        ,span(textOutput("profws_words"), style="color:purple")
                        ,plotOutput("distPlot")
                        ,h5("    ")
                )
        )
)
