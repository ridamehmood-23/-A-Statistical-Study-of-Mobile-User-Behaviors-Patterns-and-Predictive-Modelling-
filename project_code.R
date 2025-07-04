# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(MASS)
library(fitdistrplus)

# Load the dataset
data <- read.csv("user_behavior_dataset.csv")

# Convert relevant columns to appropriate data types
data$Device_Model <- as.factor(data$Device_Model)
data$Operating_System <- as.factor(data$Operating_System)
data$Gender <- as.factor(data$Gender)

# UI
ui <- fluidPage(
  # Add custom styling
  tags$style(HTML(
    "
    body {
      background: linear-gradient(to right, #ff7e5f, #feb47b);
      color: #2F2F2F;
      font-size: 20px;
    }
    .container-fluid {
      background: transparent;
    }
    .sidebarPanel {
      background: linear-gradient(to right, #6a11cb, #2575fc);
      color: white;
    }
    .tabsetPanel {
      background: #2c3e50;
      border-radius: 10px;
      padding: 15px;
    }
    .panel-title {
      font-size: 30px;
    }
    .summary-container {
      background-color: #389894;
      padding: 20px;
      border-radius: 10px;
    }
    .summary-text {
      color: #389894;
      font-size: 20px;
    }
    h1, h3 {
      font-weight: bold;
      font-family: Arial, sans-serif;
      font-size: 30px;
      color: #4a4a4a;
      border-bottom: 2px solid #4a4a4a;
      padding-bottom: 10px;
      text-transform: uppercase;
    }
    h4 {
      font-weight: bold;
      font-family: Arial, sans-serif;
      font-size: 24px;
      color: #4a4a4a;
      border-bottom: 1px solid #4a4a4a;
      padding-bottom: 8px;
    }
    "
  )),

  titlePanel("User Behavior Analysis:"),

  sidebarLayout(
    sidebarPanel(
      h3("Exploratory Data Analysis"),
      p("Explore the user behavior dataset with various visualizations and statistical summaries."),
      
      div(class = "summary-container", 
          h4("Data Analysis Summary:"),
          verbatimTextOutput("summaryStats")
      ),

      div(class = "student-names", 
          h4(strong("Student Details:")),
          p(strong("Name: "), "Sadaf Saqlain", " | ", strong("Roll No: "), "23F-0692", " | ", strong("Section: "), "BCS 4F"),
          p(strong("Name: "), "Rida Mehmood", " | ", strong("Roll No: "), "23F-0554", " | ", strong("Section: "), "BCS 4F"),
          p(strong("Name: "), "Faryaal", " | ", strong("Roll No: "), "23F-0606", " | ", strong("Section: "), "BCS 4F"),
          p(strong("Name: "), "Maryam Asif", " | ", strong("Roll No: "), "23F-6030", " | ", strong("Section: "), "BCS 4F"),
          p(strong("Name: "), "Adan Sajid", " | ", strong("Roll No: "), "23F-0691", " | ", strong("Section: "), "BCS 4F")
      )
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Bar Charts", 
                 plotOutput("genderBarChart"), 
                 plotOutput("osBarChart")
        ),
        tabPanel("Histograms", 
                 plotOutput("appUsageHist"),
                 plotOutput("batteryDrainHist")
        ),
        tabPanel("Boxplots", 
                 plotOutput("screenOnBoxplot"),
                 plotOutput("appUsageBoxplot")
        ),
        tabPanel("Normal Distribution Fit", 
                 plotOutput("normFitPlot")
        ),
        tabPanel("Regression Model", 
                 plotOutput("regressionPlot"),
                 verbatimTextOutput("regressionSummary"),
                 verbatimTextOutput("confIntervals")
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output) {
  
  # Summary statistics
  output$summaryStats <- renderPrint({
    summary(data[, c("App_Usage_Time", "Screen_On_Time", "Battery_Drain")])
  })

  # Bar chart of Gender
  output$genderBarChart <- renderPlot({
    barplot(table(data$Gender), main = "Bar Chart of Gender", xlab = "Gender", ylab = "Count", col = c("blue", "pink"))
  })

  # Bar chart of Operating System
  output$osBarChart <- renderPlot({
    barplot(table(data$Operating_System), main = "Bar Chart of Operating Systems", xlab = "Operating System", ylab = "Count", col = rainbow(length(unique(data$Operating_System))))
  })

  # Histogram of App Usage Time
  output$appUsageHist <- renderPlot({
    hist(data$App_Usage_Time, main = "Histogram of App Usage Time", xlab = "App Usage Time (minutes)", col = "blue", border = "black")
  })

  # Histogram of Battery Drain
  output$batteryDrainHist <- renderPlot({
    hist(data$Battery_Drain, main = "Histogram of Battery Drain", xlab = "Battery Drain (%)", col = "green", border = "black")
  })

  # Boxplot of Screen On Time by Operating System
  output$screenOnBoxplot <- renderPlot({
    boxplot(Screen_On_Time ~ Operating_System, data = data, main = "Boxplot of Screen On Time by Operating System", xlab = "Operating System", ylab = "Screen On Time (hours)", col = c("cyan", "yellow"))
  })

  # Boxplot of App Usage Time by Gender
  output$appUsageBoxplot <- renderPlot({
    boxplot(App_Usage_Time ~ Gender, data = data, main = "Boxplot of App Usage Time by Gender", xlab = "Gender", ylab = "App Usage Time (minutes)", col = c("lightpink", "lightblue"))
  })

  # Normal distribution fit
  output$normFitPlot <- renderPlot({
    fit_norm <- fitdist(data$App_Usage_Time, "norm")
    hist(data$App_Usage_Time, prob = TRUE, main = "Normal Distribution Fit", xlab = "App Usage Time", col = "lightgray", border = "white")
    curve(dnorm(x, mean = fit_norm$estimate[1], sd = fit_norm$estimate[2]), add = TRUE, col = "red", lwd = 2)
  })

  # Linear regression model
  reg_model <- lm(Battery_Drain ~ Screen_On_Time + App_Usage_Time, data = data)

  # Regression model summary
  output$regressionSummary <- renderPrint({
    summary(reg_model)
  })

  # Confidence intervals
  output$confIntervals <- renderPrint({
    confint(reg_model)
  })

  # Regression plot: Battery_Drain vs App_Usage_Time
  output$regressionPlot <- renderPlot({
    ggplot(data, aes(x = App_Usage_Time, y = Battery_Drain)) +
      geom_point(color = "blue", size = 2) +
      geom_smooth(method = "lm", color = "red", se = TRUE) +
      labs(
        title = "Regression Plot: App Usage Time vs Battery Drain",
        x = "App Usage Time (minutes)",
        y = "Battery Drain (%)"
      ) +
      theme_minimal()
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
