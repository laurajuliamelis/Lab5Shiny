# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title 
  titlePanel("test"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      sliderInput(inputId = "period", 
                  h3("Start of Fiscal Year"), 
                  min = 2002, max = 2018, 
                  value = c(2002, 2018)
                  ),
      
      checkboxGroupInput("party", 
                         h3("Party Selector"), 
                         choices = list("Centerpartier" = "C", 
                                        "Folkpartiet" = "FP", 
                                        "Liberalerna" = "L",
                                        "Kristdemokraterna" = "KD",
                                        "Miljöpartiet" = "MP",
                                        "Moderata Samlingspartiet" = "M",
                                        "Socialdemokraterna" = "S",
                                        "Sverigedemokraterna" = "SD",
                                        "Vänsterpartiet" = "V",
                                        "Others" = "-")
                         ),

            # Input: Selector for choosing dataset ----
            radioButtons(inputId = "vote_result", 
                         h3("Vote"), 
                         choices = list("All" = "",
                                        "Yes" = "Ja", 
                                        "No" = "Nej", 
                                        "Refrain" = "Avstår",
                                        "Absent" = "Frånvarande"),
                         selected = ""
                         ),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "rows",
                   label = "Number of observations to view:",
                   value = 10)
      ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),
      
      # Output: HTML table with requested number of observations ----
      tableOutput("view")
      
    )
  )
)


# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  # By declaring datasetInput as a reactive expression we ensure
  # that:
  #
  # 1. It is only called when the inputs it depends on changes
  # 2. The computation and result are shared by all the callers,
  #    i.e. it only executes a single time
  datasetInput <- reactive({
    GET_votation(period = input$period, span = TRUE, party = input$party, 
                 vote_result = input$vote_result, rows = input$rows)
  })
  
  
  # Generate a summary of the dataset ----
  # The output$summary depends on the datasetInput reactive
  # expression, so will be re-executed whenever datasetInput is
  # invalidated, i.e. whenever the input$dataset changes
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Show the first "n" observations ----
  # The output$view depends on both the databaseInput reactive
  # expression and input$obs, so it will be re-executed whenever
  # input$dataset or input$obs is changed
  output$view <- renderTable({
    head(datasetInput(), n = input$rows)
  })
  
}

shinyApp(ui, server)