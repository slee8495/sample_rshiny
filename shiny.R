library(tidyverse)
library(shiny)
library(readr)
library(readxl)
library(writexl)
library(janitor)
library(tidyr)
library(lubridate)
library(DT)
library(rpivotTable)
library(shinyjs)
library(shinyWidgets)

source("data.wrangling.R")

# UI Part
ui <- shiny::navbarPage("Sample Shiny Demonstration",
                        id = "navbarID",
                        tags$head(
                          tags$style(HTML(".navbar {background-color: #001F3F;}"))
                        ),
                        tabPanel("Upload Data",
                                 sidebarLayout(
                                   sidebarPanel(
                                     fileInput("file1", "Choose .xlsx File"),
                                     downloadButton("downloadData", "Download Cleaned .xlsx file")
                                   ),
                                   mainPanel(
                                     img(src = "Sample.png", height = 700, width = 1200),
                                     tags$br(),
                                     tags$br(),
                                     tags$br(),
                                     tags$p(
                                       HTML(paste("<span style='color:red;'>", data_info(), "</span>",
                                                  tags$br(), 
                                                  "Although the default data is displayed throughout this Shiny WebApp,",
                                                  "the uploading function will work with the end user's own data for customized visibility.",
                                                  tags$br(),
                                                  tags$br(),
                                                  "- The images and the logo displayed here were created by AI for this sample Shiny Web App only",
                                                  tags$br(),
                                                  "- The data used in this sample Shiny Web App is not real data")),
                                       style = "font-size: 18px; font-weight: bold;"
                                     ),
                                     absolutePanel(
                                       top = "100%",  
                                       left = "55%",  
                                       img(src = "SampleLogo.png", height = 70, width = 500)
                                     )
                                   )
                                 )
                        ),
                        tabPanel("View Data",
                                 br(),
                                 pickerInput("ackFlagFilter", "Order Acknowledgement Flag:", 
                                             choices = sort(unique(cleaned_default_data$`Order Acknowledgement Flag`)),
                                             selected = unique(cleaned_default_data$`Order Acknowledgement Flag`), 
                                             multiple = TRUE,
                                             options = list(`actions-box` = TRUE)),
                                 fluidRow(
                                   column(width = 12, 
                                          dataTableOutput("viewData"),
                                          downloadButton("downloadViewData", "Download Data"))
                                 )
                        ),
                        tabPanel("Customer Summary",
                                 br(),
                                 fluidRow(
                                   column(width = 3,
                                          pickerInput("customerSummaryPicker", "Customer:", 
                                                      choices = sort(unique(cleaned_default_data$CustomerName)), 
                                                      selected = cleaned_default_data$CustomerName,
                                                      options = list(`actions-box` = TRUE, `live-search` = TRUE), 
                                                      multiple = TRUE),
                                          dateRangeInput("dateRange", "Order Date:",
                                                         start = min(cleaned_default_data$OrderDate, na.rm = TRUE), 
                                                         end = max(cleaned_default_data$OrderDate, na.rm = TRUE))
                                   ),
                                   column(width = 9,
                                          dataTableOutput("pivot")
                                   )
                                 )
                        ),
                        tabPanel("Summary by Profile name",
                                 br(),
                                 tabsetPanel(
                                   tabPanel("Non Compliance to Order Acknowledgement Table",
                                            br(),
                                            fluidRow(
                                              column(width = 3,
                                                     pickerInput("failFilter", "Fail Status:", 
                                                                 choices = sort(unique(cleaned_default_data$`On Time`)),
                                                                 selected = cleaned_default_data$`On Time`[2], multiple = TRUE),
                                                     dateRangeInput("dateRange_2", "Order Date:",
                                                                    start = min(cleaned_default_data$OrderDate, na.rm = TRUE), 
                                                                    end = max(cleaned_default_data$OrderDate, na.rm = TRUE))),
                                              column(width = 9,
                                                     DT::dataTableOutput("pivot1"))
                                            )
                                   ),
                                   tabPanel("Non Compliance to Order Acknowledgement Plot",
                                            br(),
                                            fluidRow(
                                              column(width = 3,
                                                     pickerInput("failFilter_2", "Fail Status:", 
                                                                 choices = sort(unique(cleaned_default_data$`On Time`)),
                                                                 selected = cleaned_default_data$`On Time`[2], multiple = TRUE),
                                                     dateRangeInput("dateRange_3", "Order Date:",
                                                                    start = min(cleaned_default_data$OrderDate, na.rm = TRUE), 
                                                                    end = max(cleaned_default_data$OrderDate, na.rm = TRUE))
                                              ),
                                              column(width = 9,
                                                     plotOutput("barplot")))),
                                   
                                   tabPanel("Fail Status Table",
                                            br(),
                                            fluidRow(
                                              column(width = 3,
                                                     dateRangeInput("dateRange_4", "Order Date:",
                                                                    start = min(cleaned_default_data$OrderDate, na.rm = TRUE), 
                                                                    end = max(cleaned_default_data$OrderDate, na.rm = TRUE))
                                              ),
                                              column(width = 9,
                                                     dataTableOutput("pivot2")))),
                                   
                                   tabPanel("Fail Status Plot",
                                            br(),
                                            fluidRow(
                                              column(width = 3,
                                                     dateRangeInput("dateRange_5", "Order Date:",
                                                                    start = min(cleaned_default_data$OrderDate, na.rm = TRUE), 
                                                                    end = max(cleaned_default_data$OrderDate, na.rm = TRUE))
                                              ),
                                              column(width = 9,
                                                     plotOutput("stackedbarplot")))),
                                 )
                        ),
                        
                        
                        tabPanel("Overall Process Summary",
                                 tabsetPanel(
                                   tabPanel("Fail Distribution - Pie Chart",
                                            br(),
                                            br(),
                                            fluidRow(
                                              column(width = 3,
                                                     dateRangeInput("dateRange_7", "Order Date:",
                                                                    start = min(cleaned_default_data$OrderDate, na.rm = TRUE), 
                                                                    end = max(cleaned_default_data$OrderDate, na.rm = TRUE))
                                              ),
                                              column(width = 9,
                                                     plotOutput("pieChart")))
                                            
                                   ),
                                   tabPanel("Stacked Bar Chart",
                                            br(),
                                            br(),
                                            fluidRow(
                                              column(width = 12,
                                                     pickerInput("weekFilter", "Week:",
                                                                 choices = sort(unique(cleaned_default_data$Week)),
                                                                 selected = unique(cleaned_default_data$Week),
                                                                 options = list(`actions-box` = TRUE), 
                                                                 multiple = TRUE)
                                              )
                                            ),
                                            br(),
                                            br(),
                                            fluidRow(
                                              column(width = 12,
                                                     plotOutput("stackedBar"))
                                            )
                                   )
                                   ,
                                   tabPanel("Line Graph",
                                            br(),
                                            br(),
                                            fluidRow(
                                              column(width = 12,
                                                     dateRangeInput("dateRange_8", "Order Date:",
                                                                    start = min(cleaned_default_data$OrderDate, na.rm = TRUE), 
                                                                    end = max(cleaned_default_data$OrderDate, na.rm = TRUE))
                                              )
                                            ),
                                            br(),
                                            br(),
                                            fluidRow(
                                              column(width = 12,
                                                     plotOutput("lineGraph", height = "800px"))
                                            )
                                   ),
                                   tabPanel("Order Date Average Graph",
                                            br(),
                                            br(),
                                            fluidRow(
                                              column(width = 12,
                                                     pickerInput("Fail", "Select Fail Status:", 
                                                                 choices = unique(cleaned_default_data$`On Time`),
                                                                 selected = unique(cleaned_default_data$`On Time`[2]),
                                                                 multiple = TRUE),
                                                     dateRangeInput("dateRange_9", "Order Date:",
                                                                    start = min(cleaned_default_data$OrderDate, na.rm = TRUE), 
                                                                    end = max(cleaned_default_data$OrderDate, na.rm = TRUE)))
                                            ),
                                            br(),
                                            br(),
                                            fluidRow(
                                              column(width = 12,
                                                     plotOutput("avgGraph", height = "800px"))  
                                            )
                                   )
                                   
                                 )
                        )
                        
                        
)





# Server logic
server <- function(input, output, session) {
  data_to_display <- reactiveVal(cleaned_default_data)
  
  min_date <- reactive({
    min(data_to_display()$OrderDate, na.rm = TRUE)
  })
  
  max_date <- reactive({
    max(data_to_display()$OrderDate, na.rm = TRUE)
  })
  
  # Observe event for file upload
  observeEvent(input$file1, {
    if (!is.null(input$file1)) {
      uploaded_data <- readxl::read_xlsx(input$file1$datapath)
      
      if (identical(names(uploaded_data), names(raw_data_as400))) {
        cleaned_uploaded_data <- clean_data(uploaded_data)
        data_to_display(cleaned_uploaded_data)
        
        # Update the date range inputs
        updateDateRangeInput(session, "dateRange", 
                             start = min(cleaned_uploaded_data$OrderDate, na.rm = TRUE), 
                             end = max(cleaned_uploaded_data$OrderDate, na.rm = TRUE))
        updateDateRangeInput(session, "dateRange_2", 
                             start = min(cleaned_uploaded_data$OrderDate, na.rm = TRUE), 
                             end = max(cleaned_uploaded_data$OrderDate, na.rm = TRUE))
        updateDateRangeInput(session, "dateRange_3", 
                             start = min(cleaned_uploaded_data$OrderDate, na.rm = TRUE), 
                             end = max(cleaned_uploaded_data$OrderDate, na.rm = TRUE))
        updateDateRangeInput(session, "dateRange_4", 
                             start = min(cleaned_uploaded_data$OrderDate, na.rm = TRUE), 
                             end = max(cleaned_uploaded_data$OrderDate, na.rm = TRUE))
        updateDateRangeInput(session, "dateRange_5", 
                             start = min(cleaned_uploaded_data$OrderDate, na.rm = TRUE), 
                             end = max(cleaned_uploaded_data$OrderDate, na.rm = TRUE))
        updateDateRangeInput(session, "dateRange_6", 
                             start = min(cleaned_uploaded_data$OrderDate, na.rm = TRUE), 
                             end = max(cleaned_uploaded_data$OrderDate, na.rm = TRUE))
        updateDateRangeInput(session, "dateRange_7", 
                             start = min(cleaned_uploaded_data$OrderDate, na.rm = TRUE), 
                             end = max(cleaned_uploaded_data$OrderDate, na.rm = TRUE))
        updateDateRangeInput(session, "dateRange_8", 
                             start = min(cleaned_uploaded_data$OrderDate, na.rm = TRUE), 
                             end = max(cleaned_uploaded_data$OrderDate, na.rm = TRUE))
        updateDateRangeInput(session, "dateRange_9", 
                             start = min(cleaned_uploaded_data$OrderDate, na.rm = TRUE), 
                             end = max(cleaned_uploaded_data$OrderDate, na.rm = TRUE))
        
      } else {
        showModal(
          modalDialog(
            title = "Data Structure Error",
            "The structure of the uploaded data does not match the expected structure.",
            footer = NULL
          ))}}
  })
  
  
  
  
  
  customer_names <- reactive({
    unique(data_to_display()$CustomerName)
  })
  
  

  
  
  observe({
    updatePickerInput(session, "customerSummaryPicker", choices = sort(customer_names()))
  })
  
  observeEvent(input$file1, {
    
    if (!is.null(input$file1)) {
      
      uploaded_data <- readxl::read_xlsx(input$file1$datapath)
      
      if (identical(names(uploaded_data), names(raw_data_as400))) {
        
        cleaned_uploaded_data <- clean_data(uploaded_data)
        
        data_to_display(cleaned_uploaded_data)
      } else {
        
        showModal(
          modalDialog(
            title = "Data Structure Error",
            "The structure of the uploaded data does not match the expected structure.",
            footer = NULL
          )
        )
        
      }
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("cleaned_data-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(data_to_display(), file)
    }
  )
  
  output$viewData <- renderDataTable({
    
    view_data_filtered <- data_to_display() %>% 
      filter(`Order Acknowledgement Flag` %in% input$ackFlagFilter) 
    
    datatable(view_data_filtered, extensions = 'Buttons', 
              options = list(
                pageLength = 100,
                scrollX = TRUE,
                scrollY = "600px",
                dom = "Blfrtip",
                buttons = c("copy", "csv", "excel"),
                fixedColumns = list(leftColumns = 2),
                searchDelay = 500
              ),
              filter = 'top',
              rownames = FALSE
    )
  })
  
  
  
  output$pivot <- renderDataTable({
    
    selected_dates <- input$dateRange
    
    pivot_data <- data_to_display() %>%
      filter(OrderDate >= selected_dates[1] & OrderDate <= selected_dates[2]) %>%
      filter(CustomerName %in% input$customerSummaryPicker | length(input$customerSummaryPicker) == 0) %>%
      group_by(CustomerName, `On Time`) %>%
      summarise(Count = n(), .groups = 'drop') %>% 
      pivot_wider(names_from = `On Time`, values_from = Count, values_fill = list(Count = 0)) 
    
    if (!"Not on Time" %in% names(pivot_data)) {
      pivot_data$`Not on Time` <- 0
    }
    if (!"On Time" %in% names(pivot_data)) {
      pivot_data$`On Time` <- 0
    }
    
    column_totals <- colSums(select(pivot_data, -CustomerName), na.rm = TRUE)
    total_row <- data.frame(CustomerName = "Total", t(column_totals)) %>%
      mutate(`Not on Time` = Not.on.Time,
             `On Time` = On.Time) %>%
      select(-Not.on.Time, -On.Time)
    
    pivot_data <- bind_rows(total_row, pivot_data) %>%
      mutate(yes_no = `Not on Time` + `On Time`,
             `Not on Time` = `Not on Time` / yes_no * 1,
             `On Time` = `On Time` / yes_no * 1) %>%
      select(-yes_no)
    
    pivot_data <- pivot_data %>% 
      mutate(`Not on Time` = scales::percent(`Not on Time`, accuracy = 0.01),
             `On Time` = scales::percent(`On Time`, accuracy = 0.01))
    
    datatable(pivot_data, 
              extensions = c("Buttons", "FixedHeader"), 
              options = list(pageLength = 5000,
                             dom = "Blfrtip",
                             buttons = c("copy", "csv", "excel"),
                             scrollX = TRUE,
                             scrollY = "1500px",
                             fixedHeader = TRUE,
                             fixedColumns = list(leftColumns = 2)), 
              rownames = FALSE) %>%
      formatStyle(
        columns = 1:ncol(pivot_data),
        fontWeight = styleEqual("Total", "bold"),
        backgroundColor = styleEqual("Total", "lightyellow"),
        target = "row"
      )
  })
  
  
  
  output$pivot1 <- renderDataTable({
    
    selected_dates_2 <- input$dateRange_2
    
    pivot1_data <- data_to_display() %>%
      filter(OrderDate >= selected_dates_2[1] & OrderDate <= selected_dates_2[2]) %>%
      filter(`On Time` %in% input$failFilter) %>%  
      group_by(`Profile name`) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      arrange(desc(Count))
    
    datatable(pivot1_data, 
              extensions = c("Buttons", "FixedHeader"), 
              options = list(pageLength = 5000,
                             dom = "Blfrtip",
                             buttons = c("copy", "csv", "excel"),
                             scrollX = TRUE,
                             scrollY = "1500px",
                             fixedHeader = TRUE,
                             fixedColumns = list(leftColumns = 2)), 
              rownames = FALSE)
  })
  
  observeEvent(input$file1, {
    updatePickerInput(session, "failFilter", 
                      choices = unique(data_to_display()$`On Time`),
                      selected = unique(data_to_display()$`On Time`[2]))
  })
  
  
  
  
  
  
  
  output$barplot <- renderPlot({
    
    
    
    barplot_data <- data_to_display() %>%
      filter(OrderDate >= input$dateRange_3[1] & OrderDate <= input$dateRange_3[2]) %>%
      filter(`On Time` %in% input$failFilter_2) %>%  
      group_by(`Profile name`) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      arrange(desc(Count))
    
    ggplot(barplot_data, aes(x = reorder(`Profile name`, -Count), y = Count, fill = Count)) +
      geom_bar(stat = "identity") +
      scale_fill_gradient(low = "lightblue", high = "blue") +  
      geom_text(aes(label = Count), position = position_stack(vjust = 0.5), size = 4.5, color = "white", fontface = "bold") + 
      theme_classic() +
      labs(y = "", x = "", title = "Non Compliance to Order Acknowledgement") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none"
      )
  }, width = 1200, height = 800) 
  
  observeEvent(input$file1, {
    updatePickerInput(session, "failFilter_2", 
                      choices = unique(data_to_display()$`On Time`),
                      selected = unique(data_to_display()$`On Time`[2]))
  })
  
  
  
  
  output$pivot2 <- renderDataTable({
    fail_status_data <- data_to_display() %>%
      filter(OrderDate >= input$dateRange_4[1] & OrderDate <= input$dateRange_4[2]) %>%
      group_by(`Profile name`, `On Time`) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      pivot_wider(names_from = `On Time`, values_from = Count, values_fill = list(Count = 0)) %>%
      replace_na(list(`Not on Time` = 0, `On Time` = 0)) 
    
    totals <- fail_status_data %>%
      summarise(across(-`Profile name`, sum, na.rm = TRUE))
    
    totals$`Profile name` <- "Total"
    
    fail_status_data <- bind_rows(totals, fail_status_data)
    fail_status_data <- fail_status_data %>% 
      relocate(`Profile name`)
    
    datatable(fail_status_data, 
              extensions = c("Buttons", "FixedHeader"), 
              options = list(pageLength = 5000,
                             dom = "Blfrtip",
                             buttons = c("copy", "csv", "excel"),
                             scrollX = TRUE,
                             scrollY = "1500px",
                             fixedHeader = TRUE,
                             fixedColumns = list(leftColumns = 2)), 
              rownames = FALSE) %>%
      formatStyle(
        columns = 1:ncol(fail_status_data),
        fontWeight = styleEqual("Total", "bold"),
        backgroundColor = styleEqual("Total", "lightyellow"),
        target = "row"
      )
  })
  
  
  
  
  
  output$stackedbarplot <- renderPlot({
    stacked_bar_data <- data_to_display() %>%
      filter(OrderDate >= input$dateRange_5[1] & OrderDate <= input$dateRange_5[2]) %>%
      group_by(`Profile name`, `On Time`) %>%
      summarise(Count = n(), .groups = 'drop') 
    
    
    
    totals <- stacked_bar_data %>%
      group_by(`Profile name`) %>%
      summarise(Total = sum(Count)) %>%
      arrange(desc(Total))
    
    stacked_bar_data <- stacked_bar_data %>%
      inner_join(totals, by = "Profile name") %>%
      arrange(desc(Total)) 
    
    
    # Create ggplot2 stacked bar plot with labels
    ggplot(stacked_bar_data, aes(x = reorder(`Profile name`, -Total), y = Count, fill = `On Time`)) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(label = Count), position = position_stack(vjust = 0.5), size = 4.5, color = "white", fontface = "bold") +
      theme_classic() +
      labs(x = "", y = "", fill = "Fail Status", 
           title = "Profile Name vs Count by Fail Status") +
      scale_fill_manual(values = c(`On Time` = "#1f77b4", `Not on Time` = "#ff7f0e")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
            axis.text.y = element_text(size = 12, face = "bold"),
            axis.title.x = element_text(size = 14, face = "bold"),
            axis.title.y = element_text(size = 14, face = "bold"),
            plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            legend.text = element_text(size = 12, face = "bold"),
            legend.title = element_text(size = 14, face = "bold")) 
  }, width = 1300, height = 800)
  
  
  
  
  
  
  output$pieChart <- renderPlot({
    pie_data <- data_to_display() %>% 
      filter(OrderDate >= input$dateRange_7[1] & OrderDate <= input$dateRange_7[2]) %>%
      group_by(`On Time`) %>% 
      summarise(Count = n())
    
    
    ggplot(pie_data, aes(x = "", y = Count, fill = `On Time`)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      labs(fill = "On Time", title = expression(bold("Fail Distribution"))) +
      geom_text(aes(label = scales::percent(Count/sum(Count))),
                position = position_stack(vjust = 0.5),
                size = 5, fontface = "bold") + 
      theme_minimal() +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.title = element_text(size = 20, face = "bold", family = "Your_Font_Family"),
            legend.text = element_text(size = 14, face = "bold")) +
      scale_fill_manual(values = c(`Not on Time` = "#FF9999", `On Time` = "#66B2FF")) 
  })
  
  
  
  output$stackedBar <- renderPlot({
    bar_data <- data_to_display() %>%
      filter(Week %in% input$weekFilter) %>%  
      dplyr::filter(!is.na(Week)) %>%
      group_by(Week, `On Time`) %>%
      summarise(Count = n()) %>%
      ungroup()
    
    
    bar_data$Week <- factor(bar_data$Week, levels = unique(bar_data$Week), ordered = TRUE)
    
    
    total_counts <- bar_data %>% group_by(Week) %>% summarise(Total = sum(Count))
    
    
    bar_data <- merge(bar_data, total_counts, by = "Week")
    
    
    bar_data$Percentage <- (bar_data$Count / bar_data$Total) * 100
    
    
    bar_data <- bar_data %>%
      arrange(Week, desc(`On Time`)) %>%
      group_by(Week) %>%
      mutate(CumulativePercentage = cumsum(Percentage) - 0.5 * Percentage)
    
    ggplot2::ggplot(bar_data, ggplot2::aes(x = Week, y = Percentage, fill = `On Time`)) +
      ggplot2::geom_bar(stat = "identity", position = "stack") +
      ggplot2::geom_text(aes(label = sprintf("%.1f%%", Percentage), y = CumulativePercentage), 
                         size = 6, fontface = "bold", color = "black") +
      ggplot2::labs(y = "Percentage (%)", x = "", fill = "On Time",
                    title = "Fail Distribution by Week") +
      ggplot2::theme_classic() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(face = "bold", size = 12),
        axis.text.y = ggplot2::element_text(face = "bold", size = 12),
        axis.title.x = ggplot2::element_text(face = "bold", size = 14),
        axis.title.y = ggplot2::element_text(face = "bold", size = 14),
        legend.text = ggplot2::element_text(face = "bold", size = 16),
        plot.title = ggplot2::element_text(face = "bold", size = 18, family = "Your_Font_Family"),
        legend.title = ggplot2::element_text(face = "bold", size = 16)
      ) +
      ggplot2::scale_fill_manual(values = c(`Not on Time` = "#FF9999", `On Time` = "#66B2FF"))
    
  })
  
  
  
  output$lineGraph <- renderPlot({
    line_data <- data_to_display() %>%
      filter(OrderDate >= input$dateRange_8[1] & OrderDate <= input$dateRange_8[2]) %>%
      group_by(`OrderDate`, `On Time`) %>%
      summarise(Count = n()) %>%
      ungroup()
    
    
    line_data$`OrderDate` <- factor(line_data$`OrderDate`, levels = unique(line_data$`OrderDate`))
    
    p <- ggplot2::ggplot(line_data, ggplot2::aes(x = `OrderDate`, y = Count, color = `On Time`, group = `On Time`)) +
      ggplot2::geom_line(size = 1) +
      ggplot2::geom_text(aes(label = Count), vjust = -0.5, size = 5, fontface = "bold") +  
      ggplot2::theme_classic() +
      ggplot2::labs(title = "Fail Distribution by number of Customers over Time", y = "Number of Customers", x = "", color = "Fail Status") +
      ggplot2::ylim(0, max(line_data$Count, na.rm = TRUE) + 1) +  
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 14, face = "bold"),
        axis.text.y = ggplot2::element_text(size = 14, face = "bold"),
        axis.title.x = ggplot2::element_text(size = 16, face = "bold"),
        axis.title.y = ggplot2::element_text(size = 16, face = "bold"),
        legend.title = ggplot2::element_text(size = 16, face = "bold"),
        legend.text = ggplot2::element_text(size = 14, face = "bold"),
        legend.key.size = unit(1.5, "cm"),
        plot.title = ggplot2::element_text(face = "bold", size = 18, family = "Your_Font_Family")) +
      ggplot2::scale_color_manual(values = c(`Not on Time` = "#FF9999", `On Time` = "#66B2FF"))
    
    print(p)
  })
  
  output$avgGraph <- renderPlot({
    
    avg_data <- data_to_display() %>%
      filter(OrderDate >= input$dateRange_9[1] & OrderDate <= input$dateRange_9[2]) %>%
      dplyr::filter(`On Time` %in% input$Fail) %>%
      dplyr::group_by(`OrderDate`) %>%
      dplyr::mutate(`Days to acknowledge` = as.numeric(`Days to acknowledge`)) %>%
      dplyr::summarise(
        Avg_Days_to_acknowledge = mean(`Days to acknowledge`, na.rm = TRUE)
      ) %>%
      tidyr::gather(key = "Metric", value = "Value", -`OrderDate`)
    
    avg_data$`OrderDate` <- factor(avg_data$`OrderDate`, levels = unique(avg_data$`OrderDate`))
    
    ggplot(avg_data, aes(x = OrderDate, y = Value, color = Metric, group = Metric)) +
      geom_line(size = 1) +
      geom_text(aes(label = sprintf("%d", round(Value))), vjust = -0.5, size = 5, fontface = "bold", color = "black") +
      geom_hline(yintercept = 2, linetype = "dashed", color = "lightgreen", face = "bold", size = 2) +
      annotate("text", x = last(avg_data$OrderDate), y = 2, label = "Target: 2 days", hjust = 1, vjust = 2, color = "black", face = "bold", size = 5) +
      ylim(0, max(avg_data$Value, na.rm = TRUE) + 1) +  
      theme_classic() +
      labs(y = "Days", x = "", color = "Metric") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold"),
        axis.text.y = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14, face = "bold"),
        legend.key.size = unit(1.5, "cm") 
      )
  })
  
  
  
  
  
  
  output$downloadViewData <- downloadHandler(
    filename = function() {
      paste("view_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_to_display(), file, row.names = FALSE)
    })  
  
  
  
  
  
}

shinyApp(ui, server)