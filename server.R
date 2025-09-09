library(shiny)
library(shinythemes)
library(readxl)
library(data.table)
library(DRAP)
library(ggplot2)



#------------------SERVER-------------------
function(input, output, session) {
  
  transformed_data <- reactiveVal(NULL)
  
  
  # Transformation function 
  transform_table <- function(ime_tabele) {
    tryCatch({
      original_table <- read_xlsx(ime_tabele, col_names = TRUE)
      
      # Check if data is valid
      if(ncol(original_table) < 2) {
        stop("Tabela mora imeti vsaj 2 stolpca")
      }
      
      transponirano <- setDT(transpose(original_table))
      
      Times_values <- colnames(original_table)[-1]
      ID_vrednosti <- as.character(transponirano[1])
      
      volumes <- melt(transponirano[2:nrow(transponirano), ],
                      measure.vars = 1:ncol(transponirano),
                      variable.name = "ID", value.name = "Volume")
      volumes[, ID := ID_vrednosti[ID]]
      
      final_table <- data.frame(
        Times = as.numeric(rep(Times_values, length.out = nrow(volumes))),
        Volume = as.numeric(volumes$Volume),
        Arms = colnames(original_table)[1],
        ID = as.numeric(volumes$ID)
      )
      
      return(na.omit(final_table))
    }, error = function(e) {
      stop(paste("Error while transforming datatable:", e$message))
    })
  }
  
  
  # Reactive value to check if data is loaded
  output$data_loaded <- reactive({
    !is.null(transformed_data())
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  observeEvent(input$analyze, {
    req(input$dataFiles)
    
    tryCatch({
      # Show progress indicator
      showModal(modalDialog(
        title = "Processing data...",
        "Please wait while data is being processed.",
        footer = NULL,
        easyClose = FALSE
      ))
      
      tables <- lapply(input$dataFiles$datapath, transform_table)
      combined_data <- do.call(rbind, tables)
      
      # Check if data is valid
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
  
  # Show uploaded files 
  output$uploaded_files <- renderUI({
    req(input$dataFiles)
    file_names <- input$dataFiles$name
    tags$ul(
      lapply(file_names, function(f) {
        tags$li(f)
      })
    )
  })
  
  
  # ----Plot rendering----
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
  
  
  
  # ----Statistics rendering----
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
      if (input$stat_test == "anova") {
        res <- DRAnalysis(data, pattern = 'oneAN', method = 'GR.ANOVA')
        
        #emergency fallback
        if (is.null(res)) {
          cat("=== Running test: ANOVA ===\n")
          cat("⚠️ Not enough data – results may be unreliable.\n\n")
          res <- aov(Volume ~ Arms * Times + Error(ID/(Times)), data = data)
        }
      } else if (input$stat_test == "kruskal") {
        res <- DRAnalysis(data, pattern = 'oneAN', method = 'GR.KW')
        
        #emergency fallback
        if(is.null(res)) {
          cat("=== Running test: GR.KW ===\n")
          cat("⚠️ Not enough data – results may be unreliable.\n\n")
          res <- kruskal.test(Volume ~ Arms, data = endpoint_data)
        }
      } else if(input$stat_test == "mixed_anova") {
        res <- DRAnalysis(data, pattern = 'oneAN', method = 'mixed.ANOVA')
        
        #emergency fallback
        if(is.null(res)) {
          cat("=== Running test: mixed.ANOVA ===\n")
          cat("⚠️ Not enough data – results may be unreliable.\n\n")
          res <- aov(Volume ~ Arms * Times + Error(ID/(Times)), data = data)
        }
      } else if(input$stat_test == "lmm") {
        res <- DRAnalysis(data, pattern = 'oneAN', method = 'LMM')
        
        #emergency fallback
        if(is.null(res)) {
          cat("=== Running test: LMM ===\n")
          cat("⚠️ Not enough data – results may be unreliable.\n\n")
          res <- lmer(Volume ~ Arms + (1|ID), data = data)
        }
      }
      
      res
    }, error = function(e) {
      cat("Error in statistical analysis:", e$message, "\n")
      cat("This might be due to insufficient data or incorrect data format.\n")
      cat("Please check your data and ensure it follows DRAP format requirements.")
    })
  })
  
  
  
  
  
  
  # Explanation for statistics
  output$stat_explanation_box <- renderUI({
    req(input$stat_test)
    if (input$stat_test == "anova") {
      div(
        style = "border-left:4px solid #18bc9c; background:#c6f1ee; padding:12px; border-radius:6px;",
        h5("ANOVA"),
        p("Use this test when tumor volumes are measured across multiple time points. It evaluates the whole growth curve, giving more statistical power than endpoint analysis."),
        tags$ul(
          tags$li("Df: degrees of freedom – number of groups and residual variability."),
          tags$li("Sum Sq: variability explained by groups vs. residual error."),
          tags$li("Mean Sq: average variability per degree of freedom."),
          tags$li("F value: ratio of group variance to residual variance."),
          tags$li("Pr(>F): p-value; low value (< 0.05) means significant differences between treatment arms."),
          tags$li("Signif. codes:",
                  tags$ul(
                    tags$li("*** = p < 0.001 → highly significant"),
                    tags$li("**  = p < 0.01 → strongly significant"),
                    tags$li("*   = p < 0.05 → significant"),
                    tags$li(".   = p < 0.1 → weakly significant (trend)"),
                    tags$li("    = p ≥ 0.1 → not significant")
                  )
                )
          )
      )
    } else if (input$stat_test == "kruskal") {
      div(
        style = "border-left:4px solid #18bc9c; background:#c6f1ee; padding:12px; border-radius:6px;",
        h5("Kruskal-Wallis test"),
        p("This non-parametric test is used when data may not follow a normal distribution. It compares the medians of groups instead of means."),
        tags$ul(
          tags$li("Chi-sq: test statistic based on rank sums."),
          tags$li("Df: degrees of freedom – number of groups minus 1."),
          tags$li("Pr(>Chi): p-value; small value (< 0.05) indicates at least one group differs significantly.")
        )
      )
    } else if (input$stat_test == "mixed_anova") {
      div(
        style = "border-left:4px solid #18bc9c; background:#c6f1ee; padding:12px; border-radius:6px;",
        h5("Mixed ANOVA"),
        p("Tests treatment effects across time while accounting for repeated measures within animals."),
        tags$ul(
          tags$li("Error: ID – variability between subjects."),
          tags$li("Error: ID:Times – subject × time interaction."),
          tags$li("Within – residual error."),
          tags$li("Pr(>F): p-value; small value (< 0.05) indicates significant treatment differences."),
          tags$li("Signif. codes:",
                  tags$ul(
                    tags$li("*** = p < 0.001 → highly significant"),
                    tags$li("**  = p < 0.01 → strongly significant"),
                    tags$li("*   = p < 0.05 → significant"),
                    tags$li(".   = p < 0.1 → weakly significant (trend)"),
                    tags$li("    = p ≥ 0.1 → not significant")
                  )
          )
        )
      )
    } else if(input$stat_test == "lmm") {
      div(
        style = "border-left:4px solid #18bc9c; background:#c6f1ee; padding:12px; border-radius:6px;",
        h5("Linear Mixed Model (LMM)"),
        p("LMM accounts for both fixed effects (treatments) and random effects (subjects). Useful for repeated measures or hierarchical data."),
        tags$ul(
          tags$li("Fixed effects: treatment arms or other covariates."),
          tags$li("Random effects: subject-level variability (e.g., ID)."),
          tags$li("t-value and p-value: indicate significance of fixed effects."),
          tags$li("If DRAP returns NULL, fallback lmer model provides approximate results.")
        )
      )
    }
  })
  
  
  
  # Data preview
  output$data_preview <- DT::renderDataTable({
    req(transformed_data())
    DT::datatable(transformed_data(), 
                  options = list(pageLength = 15, scrollX = TRUE))
  })
}