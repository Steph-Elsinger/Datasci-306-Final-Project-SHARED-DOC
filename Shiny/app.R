library(shiny)
library(dplyr)
library(ggplot2)

# ---- Clean up column names to make more coder friendly ----
tobacco_clean <- tobacco_region %>%
  rename(
    cig_prev    = `Cigarette Use Prevalence % (Focus group)`,
    focus_group = `Comparing (Focus group)`
  )

regions      <- sort(unique(tobacco_clean$region))
focus_groups <- sort(unique(tobacco_clean$focus_group))

# ---- linear model ----
model <- lm(
  cig_prev ~ Year + region + focus_group,
  data = tobacco_clean
)

# Helper function For Later
predict_prev <- function(year, region, focus) {
  newdata <- data.frame(
    Year        = year,
    region      = region,
    focus_group = focus
  )
  as.numeric(predict(model, newdata = newdata))
}

# ---- UI ----
ui <- fluidPage(
  titlePanel("Smoking Risk Comparison Tool"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Group A"),
      selectInput("region_A", "Region", choices = regions),
      selectInput("focus_A", "Focus Group", choices = focus_groups),
      numericInput("year_A", "Year", value = 2030, min = 2023),
      
      hr(),
      
      h3("Group B"),
      selectInput("region_B", "Region", choices = regions),
      selectInput("focus_B", "Focus Group", choices = focus_groups),
      numericInput("year_B", "Year", value = 2030, min = 2023)
    ),
    
    mainPanel(
      h3("Predictions"),
      textOutput("predA"),
      textOutput("predB"),
      hr(),
      
      strong(textOutput("disparitySentence")),
      br(), br(),
      
      h3("Average Smoking Rate Over Time by Region"),
      plotOutput("regionTrendPlot"),
      br(), br(),
      
      # ---- MOVED MODEL EXPLANATION TO THE BOTTOM ----
      tags$em(
        "These predictions are generated from a linear regression model using Year, Region, and Focus Group as predictors."
      )
    )
    
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  output$predA <- renderText({
    predA <- predict_prev(input$year_A, input$region_A, input$focus_A)
    paste("Predicted Prevalence for Group A:", round(predA, 2), "%")
  })
  
  output$predB <- renderText({
    predB <- predict_prev(input$year_B, input$region_B, input$focus_B)
    paste("Predicted Prevalence for Group B:", round(predB, 2), "%")
  })
  
  # ----- Ratio & interpretation -----
  output$disparitySentence <- renderText({
    predA <- predict_prev(input$year_A, input$region_A, input$focus_A)
    predB <- predict_prev(input$year_B, input$region_B, input$focus_B)
    
    if (is.na(predA) || is.na(predB)) {
      return("Cannot compare predicted risks: one of the predictions does not exist.")
    }
    if (predB == 0) {
      return("Cannot compute comparison because Group B's predicted prevalence is 0%.")
    }
    
    ratio <- predA / predB
    
    
    if (abs(ratio - 1) < 0.02) {
      return("Group A and Group B are approximately equally likely to smoke.")
    }
    
    # Ratio > 1: A more likely
    if (ratio > 1) {
      return(
        paste0("Group A is ", round(ratio, 2),
               " times more likely to smoke than Group B.")
      )
    }
    
    # Ratio < 1: A less likely
    paste0("Group A is ", round(ratio, 2),
           " times as likely to smoke as Group B.")
  })
  
  # ---- Region trend graph ----
  output$regionTrendPlot <- renderPlot({
    tobacco_region |> group_by(`Year`, region) |> 
      summarize(avg = mean(`Cigarette Use Prevalence % (Focus group)`, na.rm = T)) |>
      
      ggplot() + geom_line(aes(x = `Year`, 
                               y = avg, color = region)) +
      labs(title = "Average Smoking Rate Over Time by Focus Group's Region",
           y = "Average Cigarette Smoking Rate (as a %)",
           color = "Region")
  })
}

# ---- Run App ----
shinyApp(ui, server)