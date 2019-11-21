library(shiny)

  fluidPage(
    titlePanel("Summarisation tool for Grades"),
    h4('Set the maximum mark, paste in your raw marks, and copy the result'),
    sidebarLayout(
      sidebarPanel(
        textInput("out_of", "Maximum Mark", value="100", width='100px'),
        textAreaInput("raw_values", "Values to Summarise", width = "100px", height='500px')
      ),
      mainPanel(
        h4('Results'),
        tableOutput("rv")
      )
    )
)
