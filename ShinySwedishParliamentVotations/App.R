ui <- fluidPage(
  
  # App title 
  titlePanel("Swedish Parliament Votations"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # Input: Selector for choosing year span
      sliderInput(inputId = "period", 
                  h3("Start of Fiscal Year"), 
                  min = 2002, max = 2018, 
                  value = c(2002, 2018),
                  sep = "",
                  step = 1
                  ),
      
      # Input: Selector for choosing parties
      checkboxGroupInput("party", 
                         h3("Party Selector"), 
                         choices = list("Centerpartiet (C)" = "C", 
                                        "Folkpartiet (FP)" = "FP", 
                                        "Liberalerna (L)" = "L",
                                        "Kristdemokraterna (KD)" = "KD",
                                        "Miljöpartiet (MP)" = "MP",
                                        "Moderata Samlingspartiet (M)" = "M",
                                        "Socialdemokraterna (S)" = "S",
                                        "Sverigedemokraterna (SD)" = "SD",
                                        "Vänsterpartiet (V)" = "V",
                                        "Others (-)" = "-")
                         ),

      # Input: Selector for choosing votation result
      radioButtons(inputId = "vote_result", 
                   h3("Vote"), 
                   choices = list("All" = "",
                                  "Yes" = "Yes", 
                                  "No" = "No", 
                                  "Refrain" = "Refrain",
                                  "Absent" = "Absent"),
                   selected = ""
                   ),
    
      # Input: Slider for query size
      sliderInput(inputId = "rows", 
                  h3("Maximum size of query"), 
                  min = 5000, max = 90000, 
                  value = 5000,
                  step = 5000
      ),
      
      # Download dataset button
      h4("Download dataset"), 
      downloadButton("downloadData", "Download")
      
    , width = 3),
    
    # Main panel for displaying outputs
    mainPanel(
      
      # Output text: Number of aquired rows, warning if query size equals aquired rows
      htmlOutput("query_information"),
      
      # Output: Tabset w/ plot, summary, and table
      tabsetPanel(type = "tabs",
                  
                  # Output: Plots
                  tabPanel("Plots", 
                           fluidRow(splitLayout(cellWidths = c("50%", "50%"), plotOutput("barplot", height = "4in"), plotOutput("piechart", height = "4in"))),
                           br(),
                           fluidRow(splitLayout(cellWidths = c("50%", "50%"), plotOutput("histogram", height = "4in"), plotOutput("hbarplot", height = "4in")))
                        
                  ),
                  
                  # Output: Verbatim text for data summary
                  tabPanel("Summary", 
                           h3("Summary tables"),
                           fluidRow(splitLayout(cellWidths = c("25%", "25%", "25%", "25%"), tableOutput("yearstable"), tableOutput("partiestable"), tableOutput("votestable"), tableOutput("genretable"))),
                           br(),
                           h3("Information about the dataset"),
                           verbatimTextOutput("summary")
                           
                  ),
                  
                  # Output: HTML table with requested number of observations
                  tabPanel("Data", 
                           tableOutput("view"))
      )
    )
  )
)


# Define server logic
server <- function(input, output) {
  
  # 0. Return the requested dataset
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
  
  # 1. PLOTS FOR THE "PLOTS PANEL"
  # 1.1. Generate a barplot of the vote results
  output$barplot <- renderPlot({
    votes_count <- table(datasetInput()$vote)
    
    barplot(votes_count, xlab= "Votes", ylab= "Counts", main="Barplot of vote results", col= blues9[-(1:3)])
  })
  
  # 1.2. Generate a piechart of the parties
  output$piechart <- renderPlot({
    parties_count <- table(datasetInput()$party)
    pct <- round(parties_count/sum(parties_count)*100)
    lbls <- paste(names(parties_count),pct,"%", sep=" ")
    
    pie(table(datasetInput()$party), col=blues9, main= "Pie chart of parties", labels = lbls)
  })
  
  # 1.3. Generate an histogram of the birth years
  output$histogram <- renderPlot({
    hist(datasetInput()$birth_year, col= blues9[3], xlab="Birth year", main="Histogram of the birth years")
  })
  
  # 1.4. Generate an horizontal barplot of the genre
  output$hbarplot <- renderPlot({
    sex_count <- table(datasetInput()$sex)
    
    barplot(sex_count, horiz=TRUE, xlab= "Counts", main="Horizontal barplot of genre", col= blues9[-(1:3)])
  })
  
  # 2. TABLES FOR THE "SUMMARY PANEL"
  # 2.1. Generate a summary table of the birth years
  output$yearstable <- renderTable({
    year_summary <- round(as.numeric(summary(datasetInput()$birth_year)),0)
    
    year_table <- data.frame("Statistics" = c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max."),
                             "Birth year" = c(year_summary[1],
                                              year_summary[2],
                                              year_summary[3],
                                              year_summary[4],
                                              year_summary[5],
                                              year_summary[6]),
                             stringsAsFactors = FALSE
    )
  })
  
  # 2.2. Generate a frequency table of votes
  output$votestable <- renderTable({
    votes_count <- as.data.frame(table(datasetInput()$vote), row.names= NULL)
    names(votes_count) <- c("Vote", "Frequency")
    votes_count
  })
  
  # 2.3. Generate a frequency table of genre
  output$genretable <- renderTable({
    genre_count <- as.data.frame(table(datasetInput()$sex), row.names= NULL)
    names(genre_count) <- c("Genre", "Frequency")
    genre_count
  })
  
  # 2.4. Generate a frequency table of parties
  output$partiestable <- renderTable({
    parties_count <- as.data.frame(table(datasetInput()$party), row.names= NULL)
    names(parties_count) <- c("Party", "Frequency")
    parties_count
  })
  
  
  # 2.5. Generate a summary of the dataset
  # The output$summary depends on the datasetInput reactive
  # expression, so will be re-executed whenever datasetInput is
  # invalidated, i.e. whenever the input$dataset changes
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # 2.6. Generate text for query size information
  output$query_information <- renderText({
    row_response <- nrow(datasetInput())
    if(row_response < input$rows){
      paste0('<h4>Acquired results ', row_response, "/", input$rows, 
             '</h4><h4 style="color: #27ae60;">All query results acquired</h4>')
    }else{
      paste0('<h4>Acquired results ', row_response, "/", input$rows, 
             '</h4><h4 style="color: #c0392b;">WARNING: Some results might be left out, try narrowing the query.</h4>')
    }
  })
  
  # 3. DATASET FOR THE "DATA PANEL"
  # Show the first "n" observations
  # The output$view depends on both the databaseInput reactive
  # expression and input$obs, so it will be re-executed whenever
  # input$dataset or input$obs is changed
  output$view <- renderTable({
    head(datasetInput(), n = input$rows)
  })
  
  # 4. Downloadable csv of selected dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("SwedishParliamentVotations", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)