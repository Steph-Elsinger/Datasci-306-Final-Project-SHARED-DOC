library(shiny)

tobacco_region <- read_csv("data/tobacco_region.csv")
tobacco_region$`Comparing (Focus group)` <- as.factor(tobacco_region$`Comparing (Focus group)`)
tobacco_region$region <- as.factor(tobacco_region$region)

ui <- fluidPage(
  
  titlePanel("What Are Your Odds of Being a Smoker in the US in 2025?"),
  
  sidebarLayout(
    sidebarPanel(
      
      # RACE
      selectInput(
        "race",
        "What is your race?",
        choices = unique(tobacco_region$`Comparing (Focus group)`[
          tobacco_region$Demographic == "Race and Ethnicity"
        ])
      ),
      
      # REGION
      selectInput(
        "region",
        "Where in the US do you live?",
        choices = unique(tobacco_region$region)
      ),
      
      # INCOME
      selectInput(
        "income",
        "How much do you make annually?",
        choices = unique(tobacco_region$`Comparing (Focus group)`[
          tobacco_region$Demographic == "Income"
        ])
      ),
      
      # EMPLOYMENT
      selectInput(
        "employment",
        "How would you describe your employment status?",
        choices = unique(tobacco_region$`Comparing (Focus group)`[
          tobacco_region$Demographic == "Employment"
        ])
      ),
      
      # AGE
      selectInput(
        "age",
        "How old are you?",
        choices = unique(tobacco_region$`Comparing (Focus group)`[
          tobacco_region$Demographic == "Age"
        ])
      ),
      
      # MENTAL HEALTH
      selectInput(
        "mentalhealth",
        "How would you describe your mental health?",
        choices = unique(tobacco_region$`Comparing (Focus group)`[
          tobacco_region$Demographic == "Mental Health"
        ])
      ),
      
      numericInput(
        "year",
        "Prediction year:",
        value = 2025,
        min = 2011,
        max = 2030
      )
      
    ),
    
    mainPanel(
      h3("Predicted Smoking Prevalence"),
      textOutput("prediction"),
      br(),
      h4("Model Summary (for reference)"),
      verbatimTextOutput("model_summary")
    )
  )
)

server <- function(input, output, session) {
  
  options(scipen = 999)
  
  # MODEL (placeholder)
  model <- reactive({
    lm(`Cigarette Use Prevalence % (Focus group)` ~ Year + `Comparing (Focus group)` + region,
       data = tobacco_region)
  })
  
  output$model_summary <- renderPrint({
    summary(model())
  })
  
  new_case <- reactive({
    data.frame(
      Year = input$year,
      `Comparing (Focus group)` = input$race,
      region = input$region
    )
  })
  
  output$prediction <- renderText({
    pred <- predict(model(), newdata = new_case())
    paste("Estimated smoking prevalence for", input$year, ":", round(pred, 2), "%")
  })
}

shinyApp(ui = ui, server = server)