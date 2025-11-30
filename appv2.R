# app.R

# Install packages jika belum terinstal
if (!require("shiny")) install.packages("shiny")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("DT")) install.packages("DT")
if (!require("corrplot")) install.packages("corrplot")
if (!require("plotly")) install.packages("plotly")
if (!require("readxl")) install.packages("readxl")

# Load packages
# APP.R - VERSI DENGAN AUTO-CLEANING

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(corrplot)
library(plotly)
library(readxl)
library(stringr)

# Fungsi cleaning data
load_and_clean_data = function(file_path) {
  tryCatch({
    # Baca data
    raw_data = read_excel(file_path)
    
    # Jika struktur tidak sesuai, coba alternatif
    if (ncol(raw_data) == 19) {
      names(raw_data) = c("MinTemp", "MaxTemp", "Rainfall", "Evaporation", "Sunshine",
                         "WindGustDir", "WindGustSpeed", "WindDir9am", "WindDir3pm",
                         "WindSpeed9am", "WindSpeed3pm", "Humidity9am", "Humidity3pm",
                         "Pressure9am", "Pressure3pm", "Cloud9am", "Cloud3pm",
                         "Temp9am", "Temp3pm")
    }
    
    # Konversi kolom numeric
    numeric_cols = c("MinTemp", "MaxTemp", "Rainfall", "Evaporation", "Sunshine",
                    "WindGustSpeed", "WindSpeed9am", "WindSpeed3pm", "Humidity9am",
                    "Humidity3pm", "Pressure9am", "Pressure3pm", "Cloud9am",
                    "Cloud3pm", "Temp9am", "Temp3pm")
    
    for(col in numeric_cols) {
      if(col %in% names(raw_data)) {
        # Cleaning: hapus karakter non-numeric
        raw_data[[col]] = as.numeric(gsub("[^0-9.-]", "", as.character(raw_data[[col]])))
      }
    }
    
    # Hapus baris dengan semua NA di kolom numeric
    numeric_data = raw_data[, numeric_cols[numeric_cols %in% names(raw_data)]]
    raw_data = raw_data[complete.cases(numeric_data), ]
    
    return(raw_data)
    
  }, error = function(e) {
    # Fallback: data dummy
    cat("Error loading data, using sample data\n")
    return(data.frame(
      MinTemp = c(8, 14, 12, 10, 9, 11),
      MaxTemp = c(24.3, 26.9, 25.5, 23.8, 22.6, 27.1),
      Rainfall = c(0, 3.6, 1.2, 0.5, 2.1, 0),
      Humidity9am = c(68, 80, 75, 70, 72, 78),
      WindGustSpeed = c(30, 39, 35, 28, 32, 41)
    ))
  })
}

# Load data
weather_data = load_and_clean_data("Tugas3-weather.xlsx")

# Debug info
cat("Data loaded successfully:\n")
cat("Rows:", nrow(weather_data), "Columns:", ncol(weather_data), "\n")
cat("Numeric columns:", names(weather_data)[sapply(weather_data, is.numeric)], "\n")

# Konversi kolom ke numeric jika diperlukan
numeric_columns = c("MinTemp", "MaxTemp", "Rainfall", "Evaporation", "Sunshine", 
                   "WindGustSpeed", "WindSpeed9am", "WindSpeed3pm", "Humidity9am", 
                   "Humidity3pm", "Pressure9am", "Pressure3pm", "Cloud9am", 
                   "Cloud3pm", "Temp9am", "Temp3pm")

for(col in numeric_columns) {
  if(col %in% names(weather_data)) {
    weather_data[[col]] = as.numeric(weather_data[[col]])
  }
}

# Cek struktur data
print("Struktur data yang dimuat:")
str(weather_data)
print(paste("Jumlah baris:", nrow(weather_data)))
print(paste("Jumlah kolom:", ncol(weather_data)))

# UI
ui = dashboardPage(
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
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .plot-container {
          background-color: white;
          border-radius: 5px;
          padding: 10px;
        }
      "))
    ),
    tabItems(
      # Dashboard Tab
      tabItem(tabName = "dashboard",
        fluidRow(
          valueBoxOutput("min_temp_box"),
          valueBoxOutput("max_temp_box"),
          valueBoxOutput("avg_rainfall_box")
        ),
        fluidRow(
          valueBoxOutput("total_days_box"),
          valueBoxOutput("avg_humidity_box"),
          valueBoxOutput("max_wind_box")
        ),
        fluidRow(
          box(
            title = "Distribusi Suhu Maksimum", 
            status = "primary", 
            solidHeader = TRUE,
            width = 6,
            div(class = "plot-container",
                plotOutput("temp_plot", height = "300px")
            )
          ),
          box(
            title = "Curah Hujan vs Suhu", 
            status = "warning", 
            solidHeader = TRUE,
            width = 6,
            div(class = "plot-container",
                plotOutput("rainfall_plot", height = "300px")
            )
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
                       choices = c("Tidak ada" = "None", 
                                  setNames(names(weather_data)[sapply(weather_data, is.numeric)], 
                                          names(weather_data)[sapply(weather_data, is.numeric)])),
                       selected = "None"),
            sliderInput("point_size", "Ukuran Titik:",
                       min = 1, max = 10, value = 3)
          ),
          box(
            title = "Scatter Plot Interaktif",
            status = "primary",
            solidHeader = TRUE,
            width = 9,
            div(class = "plot-container",
                plotlyOutput("interactive_plot", height = "400px")
            )
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
            div(class = "plot-container",
                plotOutput("histogram_plot", height = "300px")
            )
          ),
          box(
            title = "Box Plot",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            selectInput("box_var", "Pilih Variabel:",
                       choices = names(weather_data)[sapply(weather_data, is.numeric)],
                       selected = "Humidity9am"),
            div(class = "plot-container",
                plotOutput("box_plot", height = "300px")
            )
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
          div(class = "plot-container",
              plotOutput("corr_plot", height = "500px")
          )
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
            div(style = "overflow-y: scroll; max-height: 400px;",
                verbatimTextOutput("summary_stats")
            )
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
            fluidRow(
              column(6,
                     selectInput("regression_y", "Variabel Dependent (Y):",
                                 choices = names(weather_data)[sapply(weather_data, is.numeric)],
                                 selected = "MaxTemp")
              ),
              column(6,
                     selectInput("regression_x", "Variabel Independent (X):",
                                 choices = names(weather_data)[sapply(weather_data, is.numeric)],
                                 selected = "MinTemp")
              )
            ),
            verbatimTextOutput("regression_output")
          )
        )
      )
    )
  )
)

# Server
server = function(input, output) {
  
  # Reactive expression untuk data numeric
  numeric_data = reactive({
    weather_data[, sapply(weather_data, is.numeric), drop = FALSE]
  })
  
  # Value boxes
  output$min_temp_box = renderValueBox({
    valueBox(
      round(min(weather_data$MinTemp, na.rm = TRUE), 1),
      "Suhu Minimum Terendah (°C)",
      icon = icon("temperature-low"),
      color = "blue"
    )
  })
  
  output$max_temp_box = renderValueBox({
    valueBox(
      round(max(weather_data$MaxTemp, na.rm = TRUE), 1),
      "Suhu Maximum Tertinggi (°C)",
      icon = icon("temperature-high"),
      color = "red"
    )
  })
  
  output$avg_rainfall_box = renderValueBox({
    valueBox(
      round(mean(weather_data$Rainfall, na.rm = TRUE), 1),
      "Rata-rata Curah Hujan (mm)",
      icon = icon("cloud-rain"),
      color = "green"
    )
  })
  
  output$total_days_box = renderValueBox({
    valueBox(
      nrow(weather_data),
      "Total Hari Data",
      icon = icon("calendar"),
      color = "purple"
    )
  })
  
  output$avg_humidity_box = renderValueBox({
    valueBox(
      round(mean(weather_data$Humidity9am, na.rm = TRUE), 1),
      "Rata-rata Kelembaban 9am (%)",
      icon = icon("tint"),
      color = "aqua"
    )
  })
  
  output$max_wind_box = renderValueBox({
    valueBox(
      max(weather_data$WindGustSpeed, na.rm = TRUE),
      "Kecepatan Angin Tertinggi (km/h)",
      icon = icon("wind"),
      color = "orange"
    )
  })
  
  # Plots dengan error handling
  output$temp_plot = renderPlot({
    tryCatch({
      if (!is.null(weather_data$MaxTemp) && any(!is.na(weather_data$MaxTemp))) {
        ggplot(weather_data, aes(x = MaxTemp)) +
          geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7, na.rm = TRUE) +
          labs(title = "Distribusi Suhu Maksimum",
               x = "Suhu Maksimum (°C)",
               y = "Frekuensi") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5))
      }
    }, error = function(e) {
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Error dalam membuat plot", col = "red", cex = 1.2)
    })
  })
  
  output$rainfall_plot = renderPlot({
    tryCatch({
      if (!is.null(weather_data$MaxTemp) && !is.null(weather_data$Rainfall)) {
        ggplot(weather_data, aes(x = MaxTemp, y = Rainfall)) +
          geom_point(size = 3, color = "darkgreen", alpha = 0.7, na.rm = TRUE) +
          geom_smooth(method = "lm", se = FALSE, color = "red", na.rm = TRUE) +
          labs(title = "Hubungan Suhu Maksimum dan Curah Hujan",
               x = "Suhu Maksimum (°C)",
               y = "Curah Hujan (mm)") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5))
      }
    }, error = function(e) {
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Error dalam membuat plot", col = "red", cex = 1.2)
    })
  })
  
  # Data table
  output$weather_table = renderDT({
    datatable(weather_data, 
              options = list(
                scrollX = TRUE, 
                pageLength = 10,
                autoWidth = TRUE
              ),
              rownames = FALSE)
  })
  
  # Interactive plot dengan error handling
  output$interactive_plot = renderPlotly({
    tryCatch({
      req(input$x_var, input$y_var)
      
      if (input$x_var %in% names(weather_data) && input$y_var %in% names(weather_data)) {
        p = ggplot(weather_data, aes(x = .data[[input$x_var]], y = .data[[input$y_var]]))
        
        if (input$color_var != "None" && input$color_var %in% names(weather_data)) {
          p = p + geom_point(aes(color = .data[[input$color_var]]), 
                             size = input$point_size, alpha = 0.7, na.rm = TRUE) +
            scale_color_gradient(low = "blue", high = "red", name = input$color_var)
        } else {
          p = p + geom_point(size = input$point_size, alpha = 0.7, color = "steelblue", na.rm = TRUE)
        }
        
        p = p +
          labs(title = paste("Hubungan antara", input$x_var, "dan", input$y_var),
               x = input$x_var,
               y = input$y_var) +
          theme_minimal()
        
        ggplotly(p)
      }
    }, error = function(e) {
      empty_plot = ggplot() + 
        annotate("text", x = 1, y = 1, label = "Error dalam membuat plot", size = 6) +
        theme_void()
      ggplotly(empty_plot)
    })
  })
  
  # Histogram dengan error handling
  output$histogram_plot = renderPlot({
    tryCatch({
      req(input$hist_var)
      
      if (input$hist_var %in% names(weather_data)) {
        data_vector = weather_data[[input$hist_var]]
        data_vector = data_vector[!is.na(data_vector)]
        
        if (length(data_vector) > 0) {
          bin_width = if (length(unique(data_vector)) > 1) {
            diff(range(data_vector, na.rm = TRUE)) / 20
          } else {
            1
          }
          
          ggplot(weather_data, aes(x = .data[[input$hist_var]])) +
            geom_histogram(binwidth = bin_width, fill = "lightblue", color = "black", alpha = 0.7, na.rm = TRUE) +
            labs(title = paste("Distribusi", input$hist_var),
                 x = input$hist_var,
                 y = "Frekuensi") +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5))
        }
      }
    }, error = function(e) {
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Error dalam membuat plot", col = "red", cex = 1.2)
    })
  })
  
  # Box plot dengan error handling
  output$box_plot = renderPlot({
    tryCatch({
      req(input$box_var)
      
      if (input$box_var %in% names(weather_data)) {
        ggplot(weather_data, aes(y = .data[[input$box_var]])) +
          geom_boxplot(fill = "lightgreen", alpha = 0.7, na.rm = TRUE) +
          labs(title = paste("Box Plot", input$box_var),
               y = input$box_var) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5))
      }
    }, error = function(e) {
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Error dalam membuat plot", col = "red", cex = 1.2)
    })
  })
  
  # Korelasi plot dengan error handling
  output$corr_plot = renderPlot({
    tryCatch({
      numeric_cols = weather_data[, sapply(weather_data, is.numeric), drop = FALSE]
      numeric_cols = numeric_cols[, colSums(!is.na(numeric_cols)) > 0, drop = FALSE]
      
      if (ncol(numeric_cols) > 1) {
        cor_matrix = cor(numeric_cols, use = "complete.obs")
        
        corrplot(cor_matrix, 
                 method = "color",
                 type = "upper",
                 tl.cex = 0.8,
                 tl.col = "black",
                 title = "Matriks Korelasi Variabel Cuaca",
                 mar = c(0, 0, 1, 0))
      } else {
        plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "Tidak cukup variabel numeric\nuntuk matriks korelasi", cex = 1.2)
      }
    }, error = function(e) {
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Error dalam membuat korelasi plot", col = "red", cex = 1.2)
    })
  })
  
  # Output lainnya tetap sama...
  output$summary_stats = renderPrint({
    numeric_cols = weather_data[, sapply(weather_data, is.numeric), drop = FALSE]
    if (ncol(numeric_cols) > 0) {
      summary(numeric_cols)
    } else {
      cat("Tidak ada variabel numeric dalam dataset")
    }
  })
  
  output$normality_test = renderPrint({
    req(input$normality_var)
    if (input$normality_var %in% names(weather_data)) {
      var_data = weather_data[[input$normality_var]]
      var_data = na.omit(var_data)
      
      if(length(var_data) >= 3 && length(var_data) <= 5000) {
        shapiro_test = shapiro.test(var_data)
        cat("Uji Normalitas Shapiro-Wilk untuk", input$normality_var, ":\n\n")
        print(shapiro_test)
        cat("\nInterpretasi:\n")
        if(shapiro_test$p.value > 0.05) {
          cat("Data berdistribusi normal (p-value > 0.05)")
        } else {
          cat("Data TIDAK berdistribusi normal (p-value ≤ 0.05)")
        }
      } else {
        cat("Data tidak memenuhi syarat untuk uji Shapiro-Wilk\n")
        cat("Jumlah observasi:", length(var_data), "\n")
        cat("Syarat: 3 ≤ n ≤ 5000")
      }
    }
  })
  
  output$regression_output = renderPrint({
    req(input$regression_y, input$regression_x)
    
    if (input$regression_y %in% names(weather_data) && input$regression_x %in% names(weather_data)) {
      clean_data = na.omit(weather_data[c(input$regression_y, input$regression_x)])
      
      if(nrow(clean_data) >= 2) {
        formula = as.formula(paste(input$regression_y, "~", input$regression_x))
        model = lm(formula, data = clean_data)
        
        cat("ANALISIS REGRESI LINEAR\n")
        cat("=======================\n\n")
        cat("Jumlah observasi:", nrow(clean_data), "\n")
        cat("Persamaan Regresi:\n")
        cat(input$regression_y, "=", 
            round(coef(model)[1], 3), "+", 
            round(coef(model)[2], 3), "*", input$regression_x, "\n\n")
        
        cat("Hasil Summary Model:\n")
        print(summary(model))
      } else {
        cat("Data tidak cukup untuk analisis regresi\n")
        cat("Dibutuhkan minimal 2 observasi tanpa missing values")
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)