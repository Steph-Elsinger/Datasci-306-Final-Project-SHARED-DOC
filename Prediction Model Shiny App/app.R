library(shiny)

ui <- fluidPage(
  titlePanel("What Are Your Odds of Being a Smoker in the US in 2025?")
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "race",
        "What is your race?"
        choices = unique(tobacco_region$Demographic == `Race & Ethnicity`)),
      selectInput(
        "region",
        "Where in the US do you live"
        choices = unique(tobacco_region$region)),
      selectInput(
        "income",
        "How much do you make anually?"
        choices = unique(tobacco_region$Demographic == `Income`)),
      selectInput(
        "employment",
        "How would you describe your employment status?"
        choices = unique(tobacco_region$Demographic == `Employment`)),
      selectInput(
        "age",
        "How old are you?"
        choices = unique(tobacco_region$Demographic == `Age`)),
      selectInput(
        "mentalhealth",
        "How would you describe the state of your mental health?"
        choices = unique(tobacco_region$Demographic == `Mental Health`)),
      actionButton("go", "Calculate My Odds")),
    
    mainPanel(textOutput("predictionText"))
  )
)

server <- function(input, output, session) {
  