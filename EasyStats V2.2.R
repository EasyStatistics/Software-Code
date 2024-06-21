#Test app Updated V2.2
# Load the necessary packages
library(shiny)
library(shinydashboard)
library(readxl)
library(DT)
library(knitr)
library(kableExtra)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(cowplot)
library(ggpubr)
library(shinyjs)
library(survival)
library(survminer)
library(gridExtra)
library(plotly)
library(htmlwidgets)
library(purrr)
library(zip)

# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "Easy Stats"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName = "welcome", icon = icon("home")),
      menuItem("Survival Analysis", tabName = "survival", icon = icon("heartbeat")),
      menuItem("Clinical Data Analysis", tabName = "clinical", icon = icon("dna"),
               menuSubItem("Marker Correlation", tabName = "marker_correlation"),
               menuSubItem("Marker by Variable", tabName = "marker_variable"),
               menuSubItem("Marker Comparison", tabName = "marker_comparison"),
               menuSubItem("Contingency Table", tabName = "contingency_table")
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "welcome",
  h2("Welcome to Easy Stats!"),
  br(),  # Line break
  HTML("
    <p><b>“Easy Stats”</b>, is designed to perform various statistical analyses and visualizations. Here’s a summary of its main functionalities:</p>
    <p><b>Survival Analysis:</b> This section allows users to upload Excel files and perform survival analysis. Users can select markers, choose to plot by p-value or Hazard Ratio, remove outliers, and update the plot. The plot can be of two types: Percentile or Raw Value. There’s also an option to download the plot as PNG or PDF.</p>
    <p><b>Clinical Data Analysis:</b> This section has three sub-sections:</p>
    <ul>
      <li><b>Marker Correlation:</b> Users can upload an Excel file, select two markers and select a test type (Pearson, Spearman or Kendall). The output is a scatter plot with statistical comparison, which can be downloaded as PNG or PDF.</li>
      <li><b>Marker by Variable:</b> Users can upload an Excel file, select variables and markers, choose a color, and select a test type (Student’s t-test, Mann-Whitney U test, Anova). The output is a bar plot with statistical comparison, which can be downloaded as PNG or PDF.</li>
      <li><b>Marker Comparison:</b> Users can upload an Excel file, select two markers, and choose a test type (Student’s t-test, Mann-Whitney U test). The output is a comparison plot.</li>
      <li><b>Contingency Table:</b> Users can upload an Excel file, select variables and markers, and view a contingency table. The table can be downloaded as CSV, PNG, or PDF.</li>
    </ul>
  <p style='text-align: right;'>This code was developed by Miguel Lopez de Rodas Gregorio and Barani Kumar Rajendran at Yale University</p>
  ")
),
      tabItem(tabName = "survival",
              fluidPage(
                useShinyjs(),
                titlePanel("Survival Analysis"),
                tabsetPanel(
                  tabPanel("Survival Analysis", fluid = TRUE,
                           sidebarLayout(
                             sidebarPanel(
                               fileInput("files", "Choose Files", accept = c(".csv", ".xls", ".xlsx"), multiple = TRUE),
                               uiOutput("marker_ui"),
                               selectInput("plot_by", "Plot by", choices = c("p-value", "Hazard Ratio")),
                               checkboxInput("remove_outliers", "Remove Outliers", FALSE),
                               actionButton("go", "Update"),
                                selectInput("plot_type", "Plot Type", choices = c("Percentile", "Raw Value"), selected = "Percentile"),
                                
                            ),
                            mainPanel(
                                plotlyOutput("plot"),
                                verbatimTextOutput("selected"),
                                plotOutput("km_plot", width = "800px", height = "600px")  # Adjust the width and height as needed
                            )
                        )
                    ),
                  tabPanel("Download", fluid = TRUE,
                           sidebarLayout(
                             sidebarPanel(
                               downloadButton("downloadPlotPNG", "Download Plot as PNG"),
                               downloadButton("downloadPlotPDF", "Download Plot as PDF")
                             ),
                             mainPanel()
                           ))))),
        tabItem(tabName = "marker_correlation",
              fluidPage(
                titlePanel("Correlation Analysis"),
                tabsetPanel(
                  tabPanel("Correlation Plots", fluid = TRUE,
                           sidebarLayout(
                             sidebarPanel(
                               fileInput("file_input", "Choose File:", accept = c(".csv", ".xls", ".xlsx")),
                               uiOutput("gene_inputs"),
                               selectInput("cor_method_input", "Correlation Method:", 
                                          choices = c("pearson", "spearman", "kendall"),
                                          selected = "pearson"),
                               checkboxInput("show_p_value_input", "Show Adjusted p-value", value = FALSE),
                               actionButton("plot_button", "Plot Correlation")
                            ),
                            mainPanel(
                              plotOutput("correlation_plot")
                             )
                           )
                  ),
                  tabPanel("Download", fluid = TRUE,
                           sidebarLayout(
                             sidebarPanel(
                               downloadButton("download_png4", "Download as PNG"),
                               downloadButton("download_pdf4", "Download as PDF")
                             ),
                             mainPanel()
                           ))))),
      tabItem(tabName = "marker_variable",
              fluidPage(
                titlePanel("Bar Plot with Statistical Comparison"),
                tabsetPanel(
                  tabPanel("Marker by Variable", fluid = TRUE,
                           sidebarLayout(
                             sidebarPanel(
                               fileInput("file", "Upload File", accept = c(".csv", ".xls", ".xlsx")),
                               uiOutput("variable_selector"),
                               uiOutput("marker_selector"),
                               uiOutput("color_selector"),
                               selectInput("test_type", "Select Test Type", choices = c("Student's t-test", "Mann-Whitney U test", "Anova")),
                               actionButton("edit_labels_2", "Edit Labels"),
                               hidden(div(id = "label_inputs_2",
                                          textInput("plot_title", "Plot Title", "Marker by Variable"),
                                          textInput("y_axis_title", "Y Axis Title", "Marker Value")
                               )),
                             ),
                             mainPanel(
                               plotOutput("barplot")
                             )
                           )
                  ),
                  tabPanel("Download", fluid = TRUE,
                           sidebarLayout(
                             sidebarPanel(
                               downloadButton("download_png1", "Download as PNG"),
                               downloadButton("download_pdf1", "Download as PDF")
                             ),
                             mainPanel()
                           ))))),
    tabItem(tabName = "marker_comparison",
    fluidPage(
      titlePanel("Marker Comparison"),
      tabsetPanel(
        tabPanel("Marker Comparison", fluid = TRUE,
                 sidebarLayout(
                   sidebarPanel( 
                     fileInput("file", "Choose File", accept = c(".csv", ".xls", ".xlsx")),
                 selectInput("marker1", "Select Marker 1", choices = NULL),
                 selectInput("marker2", "Select Marker 2", choices = NULL),
                 selectInput("test_type", "Select Test Type", choices = c("Student's t-test", "Mann-Whitney U test")),
                 actionButton("plotBtn", "Plot"),
                 actionButton("edit_labels", "Edit Labels"),
                 hidden(div(id = "label_inputs",
                            textInput("plot_title", "Plot Title", "Marker Comparison"),
                            textInput("y_axis_title", "Y Axis Title", "Marker Value")
                 )),
                 textOutput("tTestOutput")
                   ),
                   mainPanel(
                     plotOutput("comparisonPlot")
                   )
                 )),
        tabPanel("Download", fluid = TRUE,
                 sidebarLayout(
                   sidebarPanel(
                     downloadButton("download_png3", "Download as PNG"),
                     downloadButton("download_pdf3", "Download as PDF")
                   ),
                   mainPanel()
                 )
        )
      ))),
      tabItem(tabName = "contingency_table",
              fluidPage(
                titlePanel("Excel File Upload"),
                tabsetPanel(
                  tabPanel("Contingency Table", fluid = TRUE,
                           sidebarLayout(
                             sidebarPanel(
                               fileInput("file", "Upload File", accept = c(".csv", ".xls", ".xlsx")),
                               uiOutput("var_select"),
                               uiOutput("marker_select"),
                               actionButton("view", "View Contingency Table"),
                             ),
                             mainPanel(
                               DTOutput("table")  # Change DTOutput to uiOutput
                             )
                           )
                  ),
                  tabPanel("Download", fluid = TRUE,
                           sidebarLayout(
                             sidebarPanel(
                               downloadButton("download_csv", "Download as CSV"),
                               downloadButton("download_png2", "Download as PNG"),
                               downloadButton("download_pdf2", "Download as PDF")
                             ),
                             mainPanel()
                           )))))
    )
  )
)
# Define the server 
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 100*1024^2)  # Increase file upload size limit to 100MB
  # stop the shiny app when the browser window is closed
  
  session$onSessionEnded(function() { stopApp() })   
  
  ####################################################################################
  #Correlation plots
  # Dynamic UI for gene inputs based on selected file
  output$gene_inputs <- renderUI({
    req(input$file_input)
    file <- input$file_input$datapath
    # Check the file extension
    ext <- tools::file_ext(file)
    if (ext == "csv") {
      df <- read.csv(file)
    } else if (ext %in% c("xls", "xlsx")) {
      df <- readxl::read_excel(file)
    } else {
      stop("Invalid file type.")
    }
    gene_names <- colnames(df)
    list(
      selectInput("gene1_input", "Select Marker 1:", choices = gene_names),
      selectInput("gene2_input", "Select Marker 2:", choices = gene_names)
    )
  })
  
 # Define correlation_plot as a reactive expression
  correlation_plot <- reactive({
    req(input$plot_button, input$file_input, input$gene1_input, input$gene2_input)
    # Check the file extension
  ext <- tools::file_ext(input$file_input$datapath)
  if (ext == "csv") {
    data <- read.csv(input$file_input$datapath)
  } else if (ext %in% c("xls", "xlsx")) {
    data <- readxl::read_excel(input$file_input$datapath)
  } else {
    stop("Invalid file type.")
  }
    # Ensure the data is numeric and handle NA values
  gene1 <- as.numeric(data[[input$gene1_input]])
  gene2 <- as.numeric(data[[input$gene2_input]])
  gene1 <- na.omit(gene1)
  gene2 <- na.omit(gene2)
    correlation <- cor.test(gene1, gene2, method = input$cor_method_input)
    cor_coef <- correlation$estimate
    p_value <- if (input$show_p_value_input) correlation$p.value else NA
    ggplot(data, aes_string(x = input$gene1_input, y = input$gene2_input)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      labs(
        title = paste("Correlation Plot (Correlation =", round(cor_coef, 2), ", p-value =", format(p_value, scientific = TRUE), ")"),
        x = input$gene1_input,
        y = input$gene2_input
      ) +
      theme_minimal()
  })
  
  # Render the plot
  output$correlation_plot <- renderPlot({
    correlation_plot()
  })

  # Download the correlation plot as a PNG
  output$download_png4 <- downloadHandler(
    filename = function() {
      paste("Correlation_plot_for_", input$gene1_input, "_and_", input$gene2_input, Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = correlation_plot(), width = 10, height = 6)
    }
  )
  
  # Download the correlation plot as a PDF
  output$download_pdf4 <- downloadHandler(
    filename = function() {paste("Correlation_plot_for_", input$gene1_input, "_and_", input$gene2_input, Sys.Date(), ".pdf", sep = "")    
    },
    content = function(file) {
      ggsave(file, plot = correlation_plot(), width = 10, height = 6)
    }
  )
  
  
####################################################################################
 #Marker comparison
  # Load data from uploaded file
  data <- reactive({
    req(input$file)
    inFile <- input$file
  if (is.null(inFile))
    return(NULL)
  
# Check the file extension
    ext <- tools::file_ext(inFile$datapath)
    if (ext == "csv") {
      df <- read.csv(inFile$datapath)
    } else if (ext %in% c("xls", "xlsx")) {
      df <- readxl::read_excel(inFile$datapath)
    } else {
      stop("Invalid file type.")
    }
    return(df)
  })
  # Update selectInput choices based on uploaded data
  observe({
    if (!is.null(data())) {
      updateSelectInput(session, "marker1", choices = colnames(data()))
      updateSelectInput(session, "marker2", choices = colnames(data()))
    }
  })
  observeEvent(input$edit_labels, {
    toggle("label_inputs")
  })
  # Create a reactive plot object
comparisonPlot <- reactive({
  req(input$plotBtn)
  
  if (!is.null(data())) {
    marker1 <- input$marker1
    marker2 <- input$marker2
    
    # Subset data for selected markers and ensure they are numeric
    comparison_data <- data() %>%
      select({{marker1}}, {{marker2}}) %>%
      mutate(across(everything(), as.numeric))
    
    # Reshape data for plotting
    comparison_data_long <- comparison_data %>%
      pivot_longer(everything(), names_to = "Marker", values_to = "Value")
    
    # Perform selected test
    if (input$test_type == "Student's t-test") {
      test_result <- t.test(comparison_data[[1]], comparison_data[[2]], na.rm = TRUE)
    } else if (input$test_type == "Mann-Whitney U test") {
      test_result <- wilcox.test(comparison_data[[1]], comparison_data[[2]], na.rm = TRUE)
    }
    
    # Check if test was performed correctly and p-value is not NA or NULL
    if (!is.null(test_result$p.value) && !is.na(test_result$p.value)) {
      # If p-value is less than 0.001, display it as "<0.001"
      if (test_result$p.value < 0.001) {
        p_value <- "<0.001"
      } else {
        p_value <- format.pval(test_result$p.value, digits = 3)
      }
    } else {
      p_value <- "NA"
    }
    
    # Calculate mean, SEM, and count for each marker
    summary_data <- comparison_data_long %>%
      group_by(Marker) %>%
      summarise(mean = mean(Value, na.rm = TRUE), 
                sem = sd(Value, na.rm = TRUE) / sqrt(n()), 
                count = sum(!is.na(Value)), 
                .groups = "drop")
    
    # Plot bar graph
    p <- ggplot(summary_data, aes(x = Marker, y = mean, fill = Marker)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.6) +  # Custom color scheme
      geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = 0.2) +  # Add error bars
      labs(x = "", y = input$y_axis_title, title = input$plot_title) +
      theme_minimal() +
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5))  # Center the plot title
    
    # Add count labels to the bars
    p <- p +
      geom_text(aes(label = count, y = 0), 
                vjust = -0.5, 
                size = 4, 
                color = "black", 
                fontface = "bold")
    
    # Calculate x-coordinate for the line
    line_x <- mean(match(c(marker1, marker2), summary_data$Marker))
    
    # Calculate y-position for p-value text
    max_y <- max(summary_data$mean + summary_data$sem, na.rm = TRUE)
    y_position <- max_y * 1.05
    
    # Add p-value annotation
    p <- p +
      geom_text(aes(x = 1.5, y = y_position, label = paste("p =", p_value)), 
                hjust = 0.5, 
                vjust = -0.5, 
                size = 4, 
                fontface = "plain", 
                family = "sans") +
      geom_segment(aes(x = line_x - 0.4, xend = line_x + 0.4, y = y_position - 0.05, yend = y_position - 0.05), 
                   size = 1, color = "black")
    
    return(p)
  }
})
  
  # Use the reactive plot object in the renderPlot function
  output$comparisonPlot <- renderPlot({
    comparisonPlot()
  })
  
  # Download the Marker Plot as a PNG
  output$download_png3 <- downloadHandler(
    filename = function() {
      paste("comparison_plot for", input$marker1, "_and_", input$marker2, Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = comparisonPlot())
    }
  )
  # Download the Marker Plot as a PDF
  output$download_pdf3 <- downloadHandler(
    filename = function() {
      paste("comparison_plot for", input$marker1, "_and_", input$marker2, Sys.Date(), ".pdf", sep = "")    
    },
    content = function(file) {
      ggsave(file, plot = comparisonPlot())
    }
  )
  ####################################################################################  
 # Contingency Table
output$var_select <- renderUI({
    req(data())
    selectizeInput("variable", "Choose variables:", choices = names(data()), multiple = TRUE)
  })
  
  # UI for selecting markers
  output$marker_select <- renderUI({
    req(data())
    selectizeInput("marker", "Choose markers:", choices = names(data()), multiple = TRUE)
  })
  
  # Reactive value to store the contingency table
  contingency_table <- reactiveVal()
  
  observe({
    # Check if the current tab is "contingency_table"
    if (req(input$sidebarItemExpanded) %in% c("contingency_table", "marker_variable", "marker_comparison")) {
      # Update the "marker" select input to have no selections
      updateSelectInput(session, "marker", selected = NULL)
    }
  })
  
  observeEvent(input$view, {
    output$table <- renderDT({
      req(input$variable)
      
      # Create a copy of the data
      df_copy <- data()
      
      # For each selected marker, calculate the median and create a new group column
      for (marker in input$marker) {
        # Check if marker is not empty
        if (!is.null(marker) && marker != "") {
          median_marker <- median(df_copy[[marker]], na.rm = TRUE)
          df_copy[[paste0(marker, "_Group")]] <- ifelse(df_copy[[marker]] >= median_marker, paste(marker, "High"), paste(marker, "Low"))
        }
      }
      
      # Create a contingency table for each selected variable and each marker group
      df_tab_list <- lapply(input$variable, function(var) {
        df_tab <- data.frame(Variable = var)
        for (marker in input$marker) {
          tab <- table(df_copy[[var]], df_copy[[paste0(marker, "_Group")]])
          df_marker <- as.data.frame.matrix(tab)
          
          # Calculate percentages
          df_marker_perc <- round(prop.table(tab, margin = 1) * 100, 1)
          
          # Combine counts and percentages into a single dataframe
          df_marker_combined <- as.data.frame(matrix(ncol = ncol(df_marker), nrow = nrow(df_marker)))
          for (i in 1:ncol(df_marker)) {
            df_marker_combined[, i] <- paste0(df_marker[, i], " (", df_marker_perc[, i], "%)")
          }
          colnames(df_marker_combined) <- colnames(df_marker)
          rownames(df_marker_combined) <- rownames(df_marker)
          
          # Perform Fisher's Exact Test
          fisher_test <- fisher.test(tab)
          
          # Add a new column with the results of the Fisher's Exact Test
          df_marker_combined$Fisher_Test <- c(paste("p-value:", round(fisher_test$p.value, 4)), rep("", nrow(df_marker_combined) - 1))
           
           # Add the marker data to the variable data
          df_tab <- cbind(df_tab, Groups = rownames(df_marker_combined), df_marker_combined)
        }
        df_tab
      })
      
       # Combine all tables into one
      df_combined <- do.call(rbind, df_tab_list)
      
      # Store the contingency table in the reactive object
      contingency_table(df_combined)
      
      # Render the table with DT::datatable instead of kable
      datatable(df_combined, options = list(scrollX = TRUE), rownames = FALSE)
    })
  })
  
  # Generate a plot and store it in a reactive object
  plot <- eventReactive(input$plot, {
    ggplot(mtcars, aes(mpg, disp)) + geom_point()  # Replace this with your actual plot
  })
  
  # Render the plot
  output$plotOutput <- renderPlot({
    plot()
  })
  
  # Download the contingency table as a CSV file
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("contingency_table", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(contingency_table(), file, row.names = FALSE)
    }
  )
  
  # Download the contingency table as a PNG image
  output$download_png2 <- downloadHandler(
    filename = function() {
      paste("contingency_table", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Get the dimensions of the table
      nrows <- nrow(contingency_table())
      ncols <- ncol(contingency_table())
      
      # Calculate the width and height based on the number of rows and columns
      width <- max(600, 100 * ncols)
      height <- max(400, 40 * nrows)
      
      png(file, width = width, height = height)  # Specify the dimensions
      gridExtra::grid.table(contingency_table())  # Use gridExtra to create a table graphic
      dev.off()
    }
  )
  
  # Download the contingency table as a PDF
  output$download_pdf2 <- downloadHandler(
    filename = function() {
      paste("contingency_table", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Get the dimensions of the table
      nrows <- nrow(contingency_table())
      ncols <- ncol(contingency_table())
      
      # Calculate the width and height based on the number of rows and columns
      width <- max(17, 0.4 * ncols)
      height <- max(22, 0.2 * nrows)
      
      pdf(file, width = width, height = height)  # Specify the dimensions
      gridExtra::grid.table(contingency_table())  # Use gridExtra to create a table graphic
      dev.off()
    }
  )
  observe({
    # Check if the current tab is "marker_variable" or "marker_comparison"
    if (req(input$sidebarItemExpanded) %in% c("marker_variable", "marker_comparison")) {
      # Update the "marker" select input to have no selections
      updateSelectInput(session, "marker", selected = "")
    }
  })
  ####################################################################################
  # Survival Analysis
    output$marker_ui <- renderUI({
        req(input$files)
        df <- read_excel(input$files$datapath[1])
        
        selectInput("marker", "Marker Name", choices = names(df))
    })
   # Define percentiles outside of the eventReactive
    percentiles <- NULL

results <- eventReactive(input$go, {
    req(input$files, input$marker)     
    if (input$plot_type == "Percentile") {
        percentiles <- seq(0.01, 0.99, by = 0.01)
    } else {
        # Combine data from all Excel files
        all_data <- lapply(input$files$datapath, function(file_path) {
            read_excel(file_path)
        })
        df <- do.call(rbind, all_data)
        values <- sort(unique(df[[input$marker]]))
        percentiles <- values / max(values)
    }
    
    results <- data.frame()
    
    for (i in seq_along(input$files$datapath)) {
      df <- read_excel(input$files$datapath[i])
      dataset_results <- data.frame(x_value = numeric(), p_value = numeric(), HR = numeric())
      
      for (p in percentiles) {
        if (input$plot_type == "Percentile") {
          cutpoint <- quantile(df[[input$marker]], p, na.rm = TRUE)
          x_value <- p
        } else {
          values <- sort(unique(df[[input$marker]]))
          cutpoint <- max(values[values <= p * max(values)], na.rm = TRUE)
          x_value <- cutpoint
        }
        
        df$group <- ifelse(df[[input$marker]] > cutpoint, "Low", "High")
        
        if (length(unique(df$group)) < 2) next
        
        fit <- coxph(Surv(Time, Status) ~ group, data = df)
        test <- survdiff(Surv(Time, Status) ~ group, data = df)
        dataset_results <- rbind(dataset_results, data.frame(x_value = x_value, p_value = pchisq(test$chisq, length(test$n) - 1, lower.tail = FALSE), HR = exp(coef(fit))))
      }
      
      # Get the filename and remove extension
      filename <- tools::file_path_sans_ext(basename(input$files$name[i]))
      
      dataset_results$dataset <- filename
      results <- rbind(results, dataset_results)
    }
    
    # Add 'shape' variable based on HR values
    results$shape <- ifelse(results$HR < 1, "Good Survival (HR < 1)", "Worse Survival (HR >= 1)")
    
    # Remove outliers if the checkbox is checked
    if (input$remove_outliers) {
      Q1 <- quantile(results$HR, 0.25, na.rm = TRUE)
      Q3 <- quantile(results$HR, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      results <- results[!(results$HR < (Q1 - 1.5 * IQR) | results$HR > (Q3 + 1.5 * IQR)), ]
    }
    
    return(results)
  })
  
  output$plot <- renderPlotly({
    req(results())
    if (input$plot_by == "p-value") {
      p <- ggplot(results(), aes(x = x_value, y = p_value, color = dataset, shape = shape)) +
        geom_point() +
        scale_shape_manual(values = c("Good Survival (HR < 1)" = 17, "Worse Survival (HR >= 1)" = 15)) +
        labs(color = "Dataset", shape = "Survival") + geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
        labs(x = ifelse(input$plot_type == "Percentile", "Percentile", "Marker Value"), y = "P Value", shape = "Survival Outcome", title = "Survival Analysis") +
        theme_minimal()
    } else {
      p <- ggplot(results(), aes(x = x_value, y = HR, color = dataset, shape = shape)) +
        geom_point() +
        scale_shape_manual(values = c("Good Survival (HR < 1)" = 17, "Worse Survival (HR >= 1)" = 15)) +
        labs(color = "Dataset", shape = "Survival") + geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
        labs(x = ifelse(input$plot_type == "Percentile", "Percentile", "Marker Value"), y = "Hazard Ratio", shape = "Survival Outcome", title = "Survival Analysis") +
        theme_minimal()
    }
    p <- ggplotly(p, tooltip = c("x", "y", "color"))
    
    p %>% layout(dragmode = "select") %>% onRender("
  function(el) {
    el.on('plotly_click', function(data) {
      var x = data.points[0].x;
      var y = data.points[0].y;
      var color = data.points[0].fullData.marker.color;
      Shiny.setInputValue('selected_point', {x: x, y: y, color: color});
    });
  }
")
  })
  
  observe({
    req(input$time_col, input$status_col)
    df <- read_excel(input$files$datapath[1])
    
    # Create 'group' variable
    df$group <- ifelse(df[[input$marker]] > median(df[[input$marker]], na.rm = TRUE), "High", "Low")
  })
  
  
  observeEvent(input$save, {
    removeModal()
  })
  
  # Reactive value to store the selected Kaplan-Meier plots
  selected_km_plot <- reactiveVal(NULL)
  
  observeEvent(input$selected_point, {
    if (!is.null(input$selected_point)) {
      selected_cutpoint <- input$selected_point$x
      selected_color <- input$selected_point$color
      
      km_plots <- list()
      
      for (i in seq_along(input$files$datapath)) {
        df <- read_excel(input$files$datapath[i])
        df$dataset <- tools::file_path_sans_ext(basename(input$files$name[i]))  # Add the dataset column
        
        if (input$plot_type == "Percentile") {
          cutpoint <- quantile(df[[input$marker]], selected_cutpoint, na.rm = TRUE)
        } else {
          values <- sort(unique(df[[input$marker]]))
          cutpoint <- max(values[values <= selected_cutpoint], na.rm = TRUE)
        }
        
        df$group <- ifelse(df[[input$marker]] > cutpoint, "Low", "High")
        
        if (length(unique(df$group)) < 2) next
        
        fit <- survfit(Surv(Time, Status) ~ group, data = df)
            p <- ggsurvplot(fit, data = df, risk.table = TRUE, conf.int = F, pval = TRUE,
                                        pval.coord = c(1, 0.1), pval.method = TRUE, pval.method.coord = c(1, 0.2),
                                        legend.title = "Groups", legend.labs = c("Low", "High"),
                                        xlab = "Survival Time", conf.int.style = "step",
                                        title = paste0("Survival Analysis for ", unique(df$dataset)),
                                        subtitle = input$marker,
                                        surv.median.line = "hv", surv.median.line.col = selected_color,
                                        ggtheme = theme_minimal(),
                                        font.main = 14, font.axis.title = 12, font.legend = 10, font.tickslab = 10,
                                        tables.theme = theme_cleantable(), tables.col = "strata", tables.y.text = FALSE,
                                        risk.table.height = 0.25, # Adjust the height of the risk table
                                        risk.table.title = "Risk Table", # Change the title of the risk table
                                        risk.table.col = "black" # Change the color of the risk table
                                        )

            # Combine the plot and the risk table
            combined_plot <- cowplot::plot_grid(p$plot, p$table, ncol = 1, rel_heights = c(0.8, 0.2))

            km_plots[[i]] <- combined_plot
                        }
                        
                        if (length(km_plots) > 0) {
                            selected_km_plot(km_plots)
                        }
                    }
                })
  
  # Render the selected Kaplan-Meier plots
output$km_plot <- renderPlot({
    if (!is.null(selected_km_plot())) {
        gridExtra::grid.arrange(grobs = selected_km_plot(), ncol = 2)
    }
})

 # Download Survival Plot as PNG
output$downloadPlotPNG <- downloadHandler(
    filename = function() {
        paste0("kaplan-meier-plots-", Sys.Date(), ".zip")
    },
    content = function(file) {
        temp_files <- lapply(seq_along(selected_km_plot()), function(i) {
            p <- selected_km_plot()[[i]]
            temp_file <- file.path("~/Downloads", paste0("plot", i, ".png"))
            ggsave(filename = temp_file, plot = p, width = 10, height = 7, units = "in", dpi = 300)
            return(temp_file)
        })
        zip::zip(file, unlist(temp_files))
    }
)

# Download Survival Plot as PDF
output$downloadPlotPDF <- downloadHandler(
    filename = function() {
        paste0("kaplan-meier-plots-", Sys.Date(), ".zip")
    },
    content = function(file) {
        temp_files <- lapply(seq_along(selected_km_plot()), function(i) {
            p <- selected_km_plot()[[i]]
            temp_file <- file.path("~/Downloads", paste0("plot", i, ".pdf"))
            ggsave(filename = temp_file, plot = p, width = 10, height = 7, units = "in", dpi = 300)
            return(temp_file)
        })
        zip::zip(file, unlist(temp_files))
    })

  ####################################################################################
  #Marker by variable code
  
  # Load the uploaded Excel file
  data <- reactive({
    req(input$file)
    inFile <- input$file
    read_excel(inFile$datapath)
  })
  observeEvent(input$edit_labels_2, {
    toggle("label_inputs_2")
  })
  # Dynamically generate variable selector
  output$variable_selector <- renderUI({
    req(data())
    selectInput("variable", "Select variable:",
                choices = colnames(data()))
  })
  
  # Dynamically generate marker selector
  output$marker_selector <- renderUI({
    req(data())
    selectInput("marker", "Select marker:",
                choices = colnames(data()))
  })
  
  # Dynamically generate download button
  output$download_button <- renderUI({
    if (is.null(input$file)) return(NULL)
    downloadButton("downloadPlot", "Download Plot")
  })
  
  # Dynamically generate color selector
  output$color_selector <- renderUI({
    if (is.null(input$file)) return(NULL)
    selectInput("color", "Select Plot Color:",
                choices = c("Red"="#ff6961",
                            "Orange"="#d95f02",
                            "Yellow"="#fdfd96",
                            "Green"="#1b9e77",
                            "Teal"="#00AFBB", 
                            "Light Blue"="#84b6f4",
                            "Dark Blue"="#226E9C",
                            "Pink"="#fdcae1",
                            "Gold"="#E7B800",
                            "Dark Grey"="#7f7f7f",
                            "Black"="#000000"))
  })
  
  
# Create a reactive expression to generate the plot
plot_output <- reactive({
  req(input$variable, input$marker)
  
  # Filter out rows with empty or NA values in the selected variable and marker
  filtered_data <- data() %>%
    filter(!is.na(!!sym(input$variable)) & !is.na(!!sym(input$marker)) & !!sym(input$variable) != "" & !!sym(input$marker) != "")
  
  # Calculate the levels of the selected marker in each group of the variable
  plot_data <- filtered_data %>%
    group_by(!!sym(input$variable)) %>%
    summarise_at(vars(!!sym(input$marker)), list(mean = mean, sem = ~sd(.)/sqrt(length(.)))) %>%
    pivot_longer(cols = contains("mean"),
                 names_to = "stat",
                 values_to = "value") %>%
    mutate(stat = ifelse(stat == "mean", "Mean", "SEM"))
    
  # Calculate the number of patients in each group
  group_counts <- filtered_data %>%
    group_by(!!sym(input$variable)) %>%
    summarise(num_patients = n())
  
  # Combine plot data and group counts
  plot_data <- plot_data %>%
    left_join(group_counts, by = c(as.character(sym(input$variable))))
  
  # Check the number of unique levels in the variable
  num_groups <- n_distinct(filtered_data[[input$variable]])
  
  # Perform ANOVA or t-test based on the number of groups
  if(input$test_type == "Anova") {
    # Perform ANOVA to compare means between groups
    anova_result <- aov(filtered_data[[input$marker]] ~ filtered_data[[input$variable]])
    
    # Extract p-value from ANOVA result
    p_value <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  } else if(input$test_type == "Student's t-test") {
    # Perform t-test between groups
    t_test_result <- t.test(filtered_data[[input$marker]] ~ filtered_data[[input$variable]])
    
    # Extract p-value from t-test result
    p_value <- t_test_result$p.value
  } else if(input$test_type == "Mann-Whitney U test") {
    # Perform Mann-Whitney U test between groups
    mann_whitney_result <- wilcox.test(filtered_data[[input$marker]] ~ filtered_data[[input$variable]])
    
    # Extract p-value from Mann-Whitney U test result
    p_value <- mann_whitney_result$p.value
  }
  
  # Calculate the maximum y value including standard deviation
  max_y <- max(plot_data$value + plot_data$sem)
  
  # Custom color palette
  my_colors <- input$color
  
  # Bar plot
  p <- ggplot(plot_data, aes(x = !!sym(input$variable), y = value, fill = stat)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.6) +
    geom_errorbar(aes(ymin = value - sem, ymax = value + sem), width = 0.2, position = position_dodge(width = 0.9)) +
    scale_fill_manual(values = my_colors) + # Custom color scheme 
    scale_y_continuous(expand = c(0, 0), limits = c(0, max_y*1.1)) + # Ensure y-axis starts from 0
    guides(fill = FALSE) +
    labs(x = input$variable, y = input$y_axis_title, title = input$plot_title, fill = "") +
    theme_minimal() +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
    # Add number of patients as text labels
    geom_text(aes(label = num_patients, y = 0), vjust = -0.5, position = position_dodge(width = 0.9), size = 4, color = "black", 
                fontface = "bold")
  
  # Add p-value annotation based on the number of groups
  if (num_groups >= 3) {
    p <- p + annotate("text", x = Inf, y = Inf, label = paste("p =", round(p_value, 4)), hjust = 1, vjust = 1, size = 4, fontface = "plain", family = "sans")
  } else if (num_groups == 2) {
    p <- p +
      geom_text(aes(x = 1.5, y = max_y*1.05, label = ifelse(p_value < 0.001, "p < 0.001", paste("p =", round(p_value, 4)))), hjust = 0.5, vjust = -0.5, size = 4, fontface = "plain", family = "sans") +
      geom_segment(aes(x = 1.1, xend = 1.9, y = max_y*1.05, yend = max_y*1.05), size = 1, color = "black")
  }

  return(p)
})
  
  # Create bar plot
  output$barplot <- renderPlot({
    plot_output()
  })
  
  # Download the Bar plot as a PNG
  output$download_png1 <- downloadHandler(
    filename = function() {
      paste("barplot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plot_output())
    }
  )
  # Download the Bar plot as a PDF
  output$download_pdf1 <- downloadHandler(
    filename = function() {paste("barplot_", Sys.Date(), ".pdf", sep = "")    
    },
    content = function(file) {
      ggsave(file, plot = plot_output())
    }
  )

# Display edit text button only when file is uploaded
output$edit_text <- renderUI({
  req(input$file)
  if (!is.null(input$file$name)) {
    actionButton("edit_text_button", "Edit Text")
  }
})
}


# Run the application 
shinyApp(ui = ui, server = server)