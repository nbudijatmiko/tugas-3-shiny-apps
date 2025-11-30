# app.R
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(corrplot)
library(plotly)

data = read_excel("D:/kuliah UT/semester 6/tugas3/visualisasi data/Tugas3-weather.xlsx")

# Cek apakah data berhasil dimuat
if (nrow(data) == 0) {
  stop("Data tidak berhasil dimuat. Periksa path file.")
}

print("Data berhasil dimuat!")
print(paste("Jumlah baris:", nrow(data)))
print(paste("Jumlah kolom:", ncol(data)))

# Konversi variabel ke numeric
data$pendapatan = as.numeric(data$pendapatan)
data$kebahagiaan = as.numeric(data$kebahagiaan)

# Sample data cuaca
weather_data <- data.frame(
  MinTemp = c(8, 14),
  MaxTemp = c(24.3, 26.9),
  Rainfall = c(0, 3.6),
  Evaporation = c(3.4, 4.4),
  Sunshine = c(6.3, 9.7),
  WindGustDir = c("NW", "ENE"),
  WindGustSpeed = c(30, 39),
  WindDir9am = c("SW", "E"),
  WindDir3pm = c("NW", "W"),
  WindSpeed9am = c(6, 4),
  WindSpeed3pm = c(20, 17),
  Humidity9am = c(68, 80),
  Humidity3pm = c(29, 36),
  Pressure9am = c(1019.7, 1012.4),
  Pressure3pm = c(1015, 1008.4),
  Cloud9am = c(7, 5),
  Cloud3pm = c(7, 3),
  Temp9am = c(14.4, 17.5),
  Temp3pm = c(23.6, 25.7)
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Analisis Data Cuaca"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data", tabName = "data", icon = icon("table")),
      menuItem("Visualisasi", tabName = "visualization", icon = icon("chart-bar")),
      menuItem("Korelasi", tabName = "correlation", icon = icon("project-diagram")),
      menuItem("Statistik", tabName = "statistics", icon = icon("calculator"))
    )
  ),
  dashboardBody(
    tabItems(
      # Dashboard Tab
      tabItem(tabName = "dashboard",
        fluidRow(
          valueBoxOutput("min_temp_box"),
          valueBoxOutput("max_temp_box"),
          valueBoxOutput("avg_rainfall_box")
        ),
        fluidRow(
          box(
            title = "Distribusi Suhu Maksimum", 
            status = "primary", 
            solidHeader = TRUE,
            plotOutput("temp_plot")
          ),
          box(
            title = "Curah Hujan vs Suhu", 
            status = "warning", 
            solidHeader = TRUE,
            plotOutput("rainfall_plot")
          )
        )
      ),
      
      # Data Tab
      tabItem(tabName = "data",
        box(
          title = "Data Cuaca", 
          status = "primary", 
          solidHeader = TRUE,
          width = 12,
          DTOutput("weather_table")
        )
      ),
      
      # Visualisasi Tab
      tabItem(tabName = "visualization",
        fluidRow(
          box(
            title = "Kontrol Visualisasi",
            status = "info",
            solidHeader = TRUE,
            width = 3,
            selectInput("x_var", "Variabel X:",
                       choices = names(weather_data)[sapply(weather_data, is.numeric)],
                       selected = "MaxTemp"),
            selectInput("y_var", "Variabel Y:",
                       choices = names(weather_data)[sapply(weather_data, is.numeric)],
                       selected = "MinTemp"),
            selectInput("color_var", "Variabel Warna:",
                       choices = c("None", names(weather_data)[sapply(weather_data, is.numeric)]),
                       selected = "Rainfall")
          ),
          box(
            title = "Scatter Plot Interaktif",
            status = "primary",
            solidHeader = TRUE,
            width = 9,
            plotlyOutput("interactive_plot")
          )
        ),
        fluidRow(
          box(
            title = "Distribusi Variabel",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            selectInput("hist_var", "Pilih Variabel:",
                       choices = names(weather_data)[sapply(weather_data, is.numeric)],
                       selected = "MaxTemp"),
            plotOutput("histogram_plot")
          ),
          box(
            title = "Box Plot",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            selectInput("box_var", "Pilih Variabel:",
                       choices = names(weather_data)[sapply(weather_data, is.numeric)],
                       selected = "Humidity9am"),
            plotOutput("box_plot")
          )
        )
      ),
      
      # Korelasi Tab
      tabItem(tabName = "correlation",
        box(
          title = "Matriks Korelasi",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          plotOutput("corr_plot")
        )
      ),
      
      # Statistik Tab
      tabItem(tabName = "statistics",
        fluidRow(
          box(
            title = "Summary Statistik",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("summary_stats")
          ),
          box(
            title = "Uji Normalitas",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            selectInput("normality_var", "Pilih Variabel untuk Uji Normalitas:",
                       choices = names(weather_data)[sapply(weather_data, is.numeric)],
                       selected = "MaxTemp"),
            verbatimTextOutput("normality_test")
          )
        ),
        fluidRow(
          box(
            title = "Analisis Regresi",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            selectInput("regression_y", "Variabel Dependent (Y):",
                       choices = names(weather_data)[sapply(weather_data, is.numeric)],
                       selected = "MaxTemp"),
            selectInput("regression_x", "Variabel Independent (X):",
                       choices = names(weather_data)[sapply(weather_data, is.numeric)],
                       selected = "MinTemp"),
            verbatimTextOutput("regression_output")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Value boxes
  output$min_temp_box <- renderValueBox({
    valueBox(
      round(min(weather_data$MinTemp), 1),
      "Suhu Minimum Terendah",
      icon = icon("temperature-low"),
      color = "blue"
    )
  })
  
  output$max_temp_box <- renderValueBox({
    valueBox(
      round(max(weather_data$MaxTemp), 1),
      "Suhu Maximum Tertinggi",
      icon = icon("temperature-high"),
      color = "red"
    )
  })
  
  output$avg_rainfall_box <- renderValueBox({
    valueBox(
      round(mean(weather_data$Rainfall), 1),
      "Rata-rata Curah Hujan",
      icon = icon("cloud-rain"),
      color = "green"
    )
  })
  
  # Plots
  output$temp_plot <- renderPlot({
    ggplot(weather_data, aes(x = MaxTemp)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
      labs(title = "Distribusi Suhu Maksimum",
           x = "Suhu Maksimum (°C)",
           y = "Frekuensi") +
      theme_minimal()
  })
  
  output$rainfall_plot <- renderPlot({
    ggplot(weather_data, aes(x = MaxTemp, y = Rainfall)) +
      geom_point(size = 3, color = "darkgreen") +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      labs(title = "Hubungan Suhu Maksimum dan Curah Hujan",
           x = "Suhu Maksimum (°C)",
           y = "Curah Hujan (mm)") +
      theme_minimal()
  })
  
  # Data table
  output$weather_table <- renderDT({
    datatable(weather_data, 
              options = list(scrollX = TRUE),
              rownames = FALSE)
  })
  
  # Interactive plot
  output$interactive_plot <- renderPlotly({
    p <- ggplot(weather_data, aes_string(x = input$x_var, y = input$y_var)) +
      geom_point(aes_string(color = if(input$color_var != "None") input$color_var), 
                 size = 3, alpha = 0.7) +
      labs(title = paste("Hubungan antara", input$x_var, "dan", input$y_var),
           x = input$x_var,
           y = input$y_var) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Histogram
  output$histogram_plot <- renderPlot({
    ggplot(weather_data, aes_string(x = input$hist_var)) +
      geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7) +
      labs(title = paste("Distribusi", input$hist_var),
           x = input$hist_var,
           y = "Frekuensi") +
      theme_minimal()
  })
  
  # Box plot
  output$box_plot <- renderPlot({
    ggplot(weather_data, aes_string(y = input$box_var)) +
      geom_boxplot(fill = "lightgreen", alpha = 0.7) +
      labs(title = paste("Box Plot", input$box_var),
           y = input$box_var) +
      theme_minimal()
  })
  
  # Correlation plot
  output$corr_plot <- renderPlot({
    numeric_data <- weather_data[, sapply(weather_data, is.numeric)]
    cor_matrix <- cor(numeric_data)
    
    corrplot(cor_matrix, 
             method = "color",
             type = "upper",
             order = "hclust",
             tl.cex = 0.8,
             tl.col = "black",
             title = "Matriks Korelasi Variabel Cuaca")
  })
  
  # Summary statistics
  output$summary_stats <- renderPrint({
    summary(weather_data[, sapply(weather_data, is.numeric)])
  })
  
  # Normality test
  output$normality_test <- renderPrint({
    var_data <- weather_data[[input$normality_var]]
    shapiro_test <- shapiro.test(var_data)
    cat("Uji Normalitas Shapiro-Wilk untuk", input$normality_var, ":/n/n")
    print(shapiro_test)
    cat("/nInterpretasi:/n")
    if(shapiro_test$p.value > 0.05) {
      cat("Data berdistribusi normal (p-value > 0.05)")
    } else {
      cat("Data TIDAK berdistribusi normal (p-value ≤ 0.05)")
    }
  })
  
  # Regression analysis
  output$regression_output <- renderPrint({
    formula <- as.formula(paste(input$regression_y, "~", input$regression_x))
    model <- lm(formula, data = weather_data)
    
    cat("ANALISIS REGRESI LINEAR/n")
    cat("=======================/n/n")
    cat("Persamaan Regresi:/n")
    cat(input$regression_y, "=", 
        round(coef(model)[1], 3), "+", 
        round(coef(model)[2], 3), "*", input$regression_x, "/n/n")
    
    cat("Hasil Summary Model:/n")
    print(summary(model))
  })
}

# Run the application
shinyApp(ui = ui, server = server)