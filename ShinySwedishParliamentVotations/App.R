# Load packages
library(xml2)
library(shiny)
library(ggplot2)
library(dplyr)

# Load data
path <- "http://data.riksdagen.se/voteringlista/?"

# rm = year span ex. 2018/19
# //bet = "betäckning" -> designation
# //punkt = "förslagspunkt" -> proposal point
# parti = party e.g. C for Centerpartiet
# //valkrets = constituency
# rost = vote (Ja, Nej, Avstå, Frånvarande)
# //iid = some personal id
# sz = number of rows to request
# utformat = format of the response

request <- paste0(path, "rm=2017/18", "&sz=50", "&utformat=xml")

#path <- "http://data.riksdagen.se/voteringlista/?rm=2018%2F19&sz=5&utformat=xml"
response <- read_xml(request)

df <- data.frame()
i <- 1
for(child in xml_children(response)){
  values <- vector()
  for(subchild in xml_children(child)){
    values <- append(values, xml_text(subchild))
  }
  df[i,1:length(values)] <- values
  i <- i+1
}
# count(df, "V16")
# count(df, "V14")

# Define UI
ui <- fluidPage(
  titlePanel("Swedish Parliament Votations"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("V2Input", "Year", 2002, 2019, c(2018, 2019)),
      selectInput("V16Input", "Vote", choices= unique(df$V16), selected = "Ja"),
      selectInput(inputId = "V14Input", label = "Genre",
                  choices = unique(df$V14), selected= "kvinna")
    ),
    mainPanel(
      plotOutput(outputId ="coolplot"),
      br(), br(),
      tableOutput("results")
    )
  )
)

# Define server function
server <- function(input, output){
  output$coolplot <- renderPlot({
    filtered <-
      df %>%
      filter(V2 >= input$V2Input[1],
             V2 <= input$V2Input[2],
             V16 == input$V16Input,
             V14 == input$V14Input
      )
    ggplot(filtered, aes(V14)) +
      geom_histogram()
  })
  
  output$results <- renderTable({
    filtered <-
      df %>%
      filter(V2 >= input$V2Input[1],
             V2 <= input$V2Input[2],
             V16 == input$V16Input,
             V14 == input$V14Input
      )
    filtered
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)