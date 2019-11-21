library(shiny)

  fluidPage(
    titlePanel("Mark Summarisation Tool"),
    sidebarLayout(
      sidebarPanel(
        tags$ul(
          tags$li('Specify max mark'),
          tags$li('Paste raw marks'),
          tags$li( 'Copy results')),
        textInput("out_of", "Max Mark", value="100", width='100px'),
        textAreaInput("raw_values", "Raw Marks", width = "100px", height='500px'),
        width=2
      ),
      mainPanel(
        h4('Grade Summary Results'),
        tableOutput("gradeSummary")
      )
    )
)
