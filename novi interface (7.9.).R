library(shiny)
library(shinythemes)
library(readxl)
library(data.table)
library(DRAP)
library(ggplot2)

# Transformacijska funkcija (ohranjena z dodanim error handlingom)
transformiraj_tabelo <- function(ime_tabele) {
  tryCatch({
    originalna_tabela <- read_xlsx(ime_tabele, col_names = TRUE)
    
    # Preveri če so podatki pravilni
    if(ncol(originalna_tabela) < 2) {
      stop("Tabela mora imeti vsaj 2 stolpca")
    }
    
    transponirano <- setDT(transpose(originalna_tabela))
    
    Times_vrednosti <- colnames(originalna_tabela)[-1]
    ID_vrednosti <- as.character(transponirano[1])
    
    volumni <- melt(transponirano[2:nrow(transponirano), ],
                    measure.vars = 1:ncol(transponirano),
                    variable.name = "ID", value.name = "Volume")
    volumni[, ID := ID_vrednosti[ID]]
    
    koncna_tabela <- data.frame(
      Times = as.numeric(rep(Times_vrednosti, length.out = nrow(volumni))),
      Volume = as.numeric(volumni$Volume),
      Arms = colnames(originalna_tabela)[1],
      ID = as.numeric(volumni$ID)
    )
    
    return(na.omit(koncna_tabela))
  }, error = function(e) {
    stop(paste("Napka pri transformaciji tabele:", e$message))
  })
}

# UI definicija - layout iz podane verzije
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Interactive tumor analysis with DRAP"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Upload data tables for analysis"),
      fileInput("dataFiles", "Upload tables (.xlsx)", accept = ".xlsx", multiple = TRUE),
      actionButton("analyze", "Run analysis", class = "btn-success"),
      hr(),
      
      # Vizualizacija podatkov
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
      
      # Statistični testi
      hr(),
      h4("Statistical analysis"),
      selectInput("stat_test", "Select statistical test:", 
                  choices = list("GR.ANOVA" = "anova", 
                                 "Kruskal-Wallis" = "kruskal",
                                 "Two-way ANOVA" = "twoway_anova"))
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
  
  # Dodaj pojasnilo na dno
  hr(),
  div(class = "text-muted text-center",
      p("DRAP: Drug Response Analysis and Visualization for PDX models"))
)

server <- function(input, output, session) {
  
  transformed_data <- reactiveVal(NULL)
  
  # Reactive value za preverjanje, ali so podatki naloženi
  output$data_loaded <- reactive({
    !is.null(transformed_data())
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  observeEvent(input$analyze, {
    req(input$dataFiles)
    
    tryCatch({
      # Prikaži progress indicator
      showModal(modalDialog(
        title = "Processing data...",
        "Please wait while data is being processed.",
        footer = NULL,
        easyClose = FALSE
      ))
      
      tables <- lapply(input$dataFiles$datapath, transformiraj_tabelo)
      combined_data <- do.call(rbind, tables)
      
      # Preveri če so podatki veljavni
      if(nrow(combined_data) == 0) {
        stop("No valid data found in uploaded files")
      }
      
      updateSelectInput(session, "control_group", choices = unique(combined_data$Arms))
      transformed_data(combined_data)
      
      removeModal()
      
      showModal(modalDialog(
        title = "Select control group",
        selectInput("modal_control_group", "Select control group:", 
                    choices = unique(combined_data$Arms)),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_control_group", "Confirm")
        )
      ))
      
    }, error = function(e) {
      removeModal()
      showModal(modalDialog(
        title = "Error",
        paste("Error processing data:", e$message),
        footer = modalButton("OK")
      ))
    })
  })
  
  observeEvent(input$confirm_control_group, {
    selected_control_group <- input$modal_control_group
    removeModal()
    updateSelectInput(session, "control_group", selected = selected_control_group)
  })
  
  
  
  # ORIGINALNA FUNKCIONALNOST GRAFOV - brez sprememb
  output$plot_output <- renderPlot({
    req(transformed_data())
    data <- transformed_data()
    
    tryCatch({
      selected_data <- data[data$Arms == input$control_group, ]
      num_columns <- length(unique(selected_data$Times))
      
      gg <- switch(input$plot_type,
                   plot_volume_gc = plotVolumeGC(data, level = input$growth_level, 
                                                 pattern = 'oneAN', position.dodge = 0.5),
                   plot_rc = {
                     rc_data <- data
                     plotRC(rc_data, type = 'Volume', pattern = 'oneAN', level = input$growth_level)
                   },
                   plot_tgi = {
                     tgi_data <- TGI(data = data[data$Times <= num_columns-1,], 
                                     neg.control = input$control_group,
                                     method = 'AUC', pattern = 'oneAN')
                     if (input$tgi_display == "time_change") {
                       plotTGI(tgi_data, pattern = 'oneAN', scope = 'all.point', 
                               position.dodge = 0.5)
                     } else {
                       plotTGI(tgi_data, pattern = 'oneAN', scope = 'end.point', 
                               position.dodge = 0.5)
                     }
                   },
                   plot_endpoint = plotEndpoint(subset(data, Times < 14), 
                                                pattern = 'oneAN', type = 'barplot'),
                   plot_response_level = {
                     response_data <- DRLevel(data = data, method = 'NPDXE.Response', 
                                              criteria = data.frame(
                                                BestResponse.lower = c(-1000, -0.95, -0.5, 0.35),
                                                BestResponse.upper = c(-0.95, -0.5, 0.35, 1000),
                                                BestAvgResponse.lower = c(-1000, -0.4, -0.2, 0.3),
                                                BestAvgResponse.upper = c(-0.4, -0.2, 0.3, 1000),
                                                Level = c('CR', 'PR', 'SD', 'PD')),
                                              neg.control = input$control_group)
                     plotDRLevel(data = response_data, by = 'Arms', pattern = 'oneAN')
                   })
      
      gg
    }, error = function(e) {
      # V primeru napake pri izrisu grafa
      ggplot() + 
        geom_text(aes(x = 0.5, y = 0.5, 
                      label = paste("Error creating plot:", e$message)), 
                  size = 4, hjust = 0.5) +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
    })
  }, width = reactive(input$plot_width), height = reactive(input$plot_height))
  
  # Lepši prikaz rezultatov statistike
  output$stat_output_box <- renderUI({
    div(
      style = "border:1px solid #ccc; background:#f8f9fa; padding:10px; border-radius:8px;",
      verbatimTextOutput("stat_output")
    )
  })
  
  output$stat_output <- renderPrint({
    req(transformed_data())
    data <- transformed_data()
    
    tryCatch({
      if(input$stat_test == "anova") {
        res <- DRAnalysis(data, pattern = 'oneAN', method = 'GR.ANOVA')
      } else if(input$stat_test == "kruskal") {
        endpoint_data <- subset(data, Times == max(data$Times))
        res <- DRAnalysis(endpoint_data, pattern = 'oneAN', method = 'Kruskal')
        if(is.null(res)) {
          cat("Result is statistically weak due to low sample size in some groups.\n")
          res <- kruskal.test(Volume ~ Arms, data = endpoint_data)
        }
      } else if(input$stat_test == "twoway_anova") {
        res <- DRAnalysis(data, pattern = 'oneAN', method = 'TwoWay.ANOVA')
        if(is.null(res)) cat("Two-way ANOVA: DRAP returned NULL (possibly small sample size)\n")
      }
      res
    }, error = function(e) {
      cat("Error in statistical analysis:", e$message, "\n")
      cat("This might be due to insufficient data or incorrect data format.\n")
      cat("Please check your data and ensure it follows DRAP format requirements.")
    })
  })
  
  
  
  
  
  # Lepši explanation box
  output$stat_explanation_box <- renderUI({
    req(input$stat_test)
    if (input$stat_test == "anova") {
      div(
        style = "border-left:4px solid #28a745; background:#eaf7f0; padding:12px; border-radius:6px;",
        h5("GR.ANOVA"),
        p("Use this test when tumor volumes are measured across multiple time points. It evaluates the whole growth curve, giving more statistical power than endpoint analysis."),
        tags$ul(
          tags$li("Df: degrees of freedom – number of groups and residual variability."),
          tags$li("Sum Sq: variability explained by groups vs. residual error."),
          tags$li("Mean Sq: average variability per degree of freedom."),
          tags$li("F value: ratio of group variance to residual variance."),
          tags$li("Pr(>F): p-value; low value (< 0.05) means significant differences between treatment arms.")
        )
      )
    } else if (input$stat_test == "kruskal") {
      div(
        style = "border-left:4px solid #28a745; background:#f0fff4; padding:12px; border-radius:6px;",
        h5("Kruskal-Wallis test"),
        p("This non-parametric test is used when data may not follow a normal distribution. It compares the medians of groups instead of means."),
        tags$ul(
          tags$li("Chi-sq: test statistic based on rank sums."),
          tags$li("Df: degrees of freedom – number of groups minus 1."),
          tags$li("Pr(>Chi): p-value; small value (< 0.05) indicates at least one group differs significantly.")
        )
      )
    } else if(input$stat_test == "twoway_anova") {
      div(
        style = "border-left:4px solid #007bff; background:#e7f1ff; padding:12px; border-radius:6px;",
        h5("Two-way ANOVA"),
        p("Evaluates treatment and time interaction effects. DRAP may return NULL if data is insufficient."),
        tags$ul(
          tags$li("Interaction: evaluates whether treatment effects vary across time."),
          tags$li("Main effects: evaluates overall treatment and time effects.")
        )
      )
    }
  })
  
  
  
  # Predogled podatkov
  output$data_preview <- DT::renderDataTable({
    req(transformed_data())
    DT::datatable(transformed_data(), 
                  options = list(pageLength = 15, scrollX = TRUE))
  })
}

shinyApp(ui = ui, server = server)