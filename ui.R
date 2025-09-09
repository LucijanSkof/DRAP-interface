library(shiny)
library(shinythemes)
library(readxl)
library(data.table)
library(DRAP)
library(ggplot2)


fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Interactive tumor analysis with DRAP"),
  
  sidebarLayout(
    sidebarPanel(
      
      # Data upload 
      width = 4,
      h4("Upload data tables for analysis"),
      fileInput("dataFiles", "Upload tables (.xlsx)", accept = ".xlsx", multiple = TRUE),
      
      uiOutput("uploaded_files"),
      
      actionButton("analyze", "Run analysis", class = "btn-success"),
      hr(),
      
      
      
      
      # Data visualization
      h4("Data visualization"),
      selectInput("plot_type", "Select plot type:", 
                  choices = list("Volume Growth Curve" = "plot_volume_gc",
                                 "Relative Change" = "plot_rc",
                                 "Tumor Growth Inhibition" = "plot_tgi",
                                 "Endpoint Analysis" = "plot_endpoint",
                                 "Response Level Analysis" = "plot_response_level")),
      
      numericInput("plot_width", "Plot width (px):", value = 800, min = 400),
      numericInput("plot_height", "Plot height (px):", value = 600, min = 300),
      
      conditionalPanel(
        condition = "input.plot_type == 'plot_tgi' || input.plot_type == 'plot_response_level'",
        selectInput("control_group", "Select control group:", choices = NULL)
      ),
      
      conditionalPanel(
        condition = "input.plot_type == 'plot_tgi'",
        selectInput("tgi_display", "Select display for TGI:", 
                    choices = list("TGI change over time" = "time_change", 
                                   "Default display" = "default"))
      ),
      
      conditionalPanel(
        condition = "input.plot_type == 'plot_volume_gc'",
        selectInput("growth_level", "Select level for analysis:", 
                    choices = list("Animal" = "Animal", "Arm" = "Arm"))
      ),
      
      # Statistical tests
      hr(),
      h4("Statistical analysis"),
      selectInput("stat_test", "Select statistical test:", 
                  choices = list("ANOVA" = "anova", 
                                 "Kruskal-Wallis" = "kruskal",
                                 "Mixed ANOVA" = "mixed_anova",
                                 "Linear Mixed Model" = "lmm"))
    ),
    
    mainPanel(
      width = 8,
      tabsetPanel(
        tabPanel("Data Visualization",
                 br(),
                 conditionalPanel(
                   condition = "output.data_loaded",
                   plotOutput("plot_output")
                 ),
                 conditionalPanel(
                   condition = "!output.data_loaded",
                   div(class = "alert alert-info", 
                       "Please upload data files and run analysis to see plots.")
                 )
        ),
        tabPanel("Statistical Results",
                 br(),
                 h4("Statistical analysis results"),
                 conditionalPanel(
                   condition = "output.data_loaded",
                   uiOutput("stat_output_box")
                 ),
                 br(),
                 h4("Explanation"),
                 conditionalPanel(
                   condition = "output.data_loaded",
                   uiOutput("stat_explanation_box")
                 )
        ),
        tabPanel("Data Preview",
                 br(),
                 h4("Transformed data preview"),
                 conditionalPanel(
                   condition = "output.data_loaded",
                   DT::dataTableOutput("data_preview")
                 )
        )
      )
    )
  ),
  
  # Footer note
  hr(),
  div(class = "text-muted text-center",
      p("DRAP: Drug Response Analysis and Visualization for PDX models"))
)
