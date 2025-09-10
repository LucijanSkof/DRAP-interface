#------------------SERVER-------------------
function(input, output, session) {
  
  transformed_data <- reactiveVal(NULL)
  # store latest printed statistical results as text for copying
  last_stat_text <- reactiveVal("")
  
  # Transformation function 
  transform_table <- function(ime_tabele) {
    tryCatch({
      original_table <- read_xlsx(ime_tabele, col_names = TRUE)
      
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
      showModal(modalDialog(
        title = "Processing data...",
        "Please wait while data is being processed.",
        footer = NULL,
        easyClose = FALSE
      ))
      
      tables <- lapply(input$dataFiles$datapath, transform_table)
      combined_data <- do.call(rbind, tables)
      
      if(nrow(combined_data) == 0) {
        stop("No valid data found in uploaded files")
      }
      
      # Update control group dropdown
      updateSelectInput(session, "control_group", choices = unique(combined_data$Arms))
      
      # Update intercept group dropdown for LMM
      updateSelectInput(session, "intercept_group",
                        choices = unique(combined_data$Arms),
                        selected = grep("Ctrl", unique(combined_data$Arms), value = TRUE)[1])
      
      transformed_data(combined_data)
      removeModal()
      
      showModal(modalDialog(
        title = "Select control group",
        selectInput(
          "modal_control_group", 
          "Select control group:", 
          choices = unique(combined_data$Arms),
          selected = grep("Ctrl", unique(combined_data$Arms), value = TRUE)[1]
        ),
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
      ggplot() + 
        geom_text(aes(x = 0.5, y = 0.5, 
                      label = paste("Error creating plot:", e$message, if (input$plot_type == "plot_tgi"){"\nBLBLA"})), 
                  size = 4, hjust = 0.5) +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
    })
  }, width = reactive(input$plot_width), height = reactive(input$plot_height))
  
  # ----Statistics rendering----
  output$stat_output_box <- renderUI({
    div(
      style = "position: relative; border:1px solid #ccc; background:#f8f9fa; 
             padding:10px; border-radius:8px;",
      
      actionButton("copy_button", "Copy", 
                   style = "position: absolute; top: 5px; right: 5px; 
                          padding:3px 8px; font-size: 12px; background-color: #2c3e50; "),
      
      verbatimTextOutput("stat_output")
    )
  })
  
  output$stat_output <- renderPrint({
    req(transformed_data())
    data <- transformed_data()
    
    results_text <- tryCatch({
      capture.output({
        if (input$stat_test == "anova") {
          res <- DRAnalysis(data, pattern = 'oneAN', method = 'GR.ANOVA')
          if (is.null(res)) {
            cat("=== Running test: GR.ANOVA ===\n")
            cat("⚠️ Not enough data – results may be unreliable.\n\n")
            res <- aov(Volume ~ Arms * Times + Error(ID/(Times)), data = data)
            print(summary(res))
          } else {
            print(res)
          }
        } else if (input$stat_test == "kruskal") {
          res <- DRAnalysis(data, pattern = 'oneAN', method = 'GR.KW')
          if (is.null(res)) {
            cat("=== Running test: GR.KW ===\n")
            cat("⚠️ DRAP returned NULL – falling back to base R Kruskal-Wallis on endpoint.\n\n")
            endpoint_data <- subset(data, Times == max(data$Times))
            res_base <- kruskal.test(Volume ~ Arms, data = endpoint_data)
            print(res_base)
          } else {
            print(res)
          }
        } else if (input$stat_test == "mixed_anova") {
          res <- DRAnalysis(data, pattern = 'oneAN', method = 'mixed.ANOVA')
          if (is.null(res)) {
            cat("=== Running test: mixed.ANOVA ===\n")
            cat("⚠️ DRAP returned NULL – using fallback aov with Error(ID/Times).\n\n")
            res <- aov(Volume ~ Arms * Times + Error(ID/(Times)), data = data)
            print(summary(res))
          } else {
            print(res)
          }
        } else if (input$stat_test == "lmm") {
          data$Arms <- factor(data$Arms)
          
          # Nastavi referenčno skupino iz izbire uporabnika
          if (!is.null(input$intercept_group) && input$intercept_group %in% levels(data$Arms)) {
            data$Arms <- relevel(data$Arms, ref = input$intercept_group)
          }
          
          res <- DRAnalysis(data, pattern = 'oneAN', method = 'LMM')
          if (is.null(res)) {
            cat("=== Running test: LMM ===\n")
            cat("⚠️ DRAP returned NULL – using fallback lmer model (if lme4 available).\n\n")
            if (requireNamespace("lme4", quietly = TRUE)) {
              fm <- lme4::lmer(Volume ~ Arms + (1|ID), data = data)
              print(summary(fm))
            } else {
              cat("lme4 package not installed; cannot run fallback LMM.\n")
            }
          } else {
            res
          }
        } else {
          cat("Unknown statistical test selected.\n")
        }
      })
    }, error = function(e) {
      paste("Error while running statistical test:", e$message)
    })
    
    last_stat_text(paste(results_text, collapse = "\n"))
    cat(last_stat_text())
  })
  
  # Copy to clipboard
  observeEvent(input$copy_button, {
    txt <- last_stat_text()
    if (is.null(txt) || nchar(txt) == 0) {
      showModal(modalDialog(
        title = "Nothing to copy",
        "There are no statistical results to copy yet.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    tryCatch({
      clipr::write_clip(txt)
      showModal(modalDialog(
        title = "Copied!",
        "Statistical results have been copied to the clipboard.",
        easyClose = TRUE,
        footer = NULL
      ))
    }, error = function(e) {
      showModal(modalDialog(
        title = "Copy failed",
        paste("Could not copy to clipboard:", e$message),
        easyClose = TRUE,
        footer = NULL
      ))
    })
  })
  
  # Explanation for statistics
  output$stat_explanation_box <- renderUI({
    req(input$stat_test)
    if (input$stat_test == "anova") {
      div(
        style = "border-left:4px solid #18bc9c; background:#c6f1ee; padding:12px; border-radius:6px;",
        p("Use ANOVA when you want to compare the mean tumor volumes (or growth rates) between 3 or more groups. Data should be normally distributed and have equal variances across groups."),
        tags$ul(
          tags$li("Df: degrees of freedom."),
          tags$li("Sum Sq: variation explained by groups vs. error."),
          tags$li("Mean Sq: average variation per degree of freedom."),
          tags$li("F value: ratio of group variance to residual variance (error)."),
          tags$li("Pr(>F) = p-value:",
                  tags$ul(
                    tags$li("*** = p < 0.001 → very strong evidence"),
                    tags$li("**  = p < 0.01 → strong evidence"),
                    tags$li("*   = p < 0.05 → clear statistical difference"),
                    tags$li(".   = p < 0.1 → weak evidence"),
                    tags$li("    = p ≥ 0.1 → no evidence of difference")
                  ))
        )
      )
    } else if (input$stat_test == "kruskal") {
      div(
        style = "border-left:4px solid #18bc9c; background:#c6f1ee; padding:12px; border-radius:6px;",
        p("Use Kruskal-Wallis when data are not normally distributed or when group variances are very different. It compares group distributions instead of means."),
        tags$ul(
          tags$li("Chi-sq: measures how different the rank sums are between groups. The larger it is, the stronger the evidence that at least one group differs."),
          tags$li("Df: N of groups - 1."),
          tags$li("p-value: < 0.05 = significant difference.")
        )
      )
    } else if (input$stat_test == "mixed_anova") {
      div(
        style = "border-left:4px solid #18bc9c; background:#c6f1ee; padding:12px; border-radius:6px;",
        p("Use Mixed ANOVA when you have both:

           <br> a between-subjects factor (e.g., treatment arms), and
            
           <br> a within-subjects factor (e.g., repeated measures over time).
            
           <br><br> Similar to ANOVA, but shows whether treatment effects differ over time. Data should be normally distributed."),
        
        tags$ul(
          tags$li("Error: ID – variability between subjects."),
          tags$li("Error: ID:Times – subject × time interaction."),
          tags$li("Df: degrees of freedom."),
          tags$li("Sum Sq: variation explained by groups vs. error."),
          tags$li("Mean Sq: average variation per degree of freedom."),
          tags$li("F value: ratio of group variance to residual variance (error)."),
          tags$li("Pr(>F) = p-value:",
                  tags$ul(
                    tags$li("*** = p < 0.001 → very strong evidence"),
                    tags$li("**  = p < 0.01 → strong evidence"),
                    tags$li("*   = p < 0.05 → clear statistical difference"),
                    tags$li(".   = p < 0.1 → weak evidence"),
                    tags$li("    = p ≥ 0.1 → no evidence of difference")
                  ))
        )
      )
    } else if (input$stat_test == "lmm") {
      div(
        style = "border-left:4px solid #18bc9c; background:#c6f1ee; padding:12px; border-radius:6px;",
        p("Use LMM when you have complex repeated measures or missing data, and want a flexible alternative to Mixed ANOVA."),
        tags$ul(
          tags$li("Intercept: Baseline value of the response variable for the reference group (when all predictors = 0). Other coefficients are interpreted relative to this."),
          tags$li("Value: Shows how much the response changes compared to the reference group."),
          tags$li("Std.Error: Standard error of the coefficient."),
          tags$li("DF: Degrees of freedom"),
          tags$li("t-value: Value / Std.Error. Larger absolute values indicate stronger evidence"),
          tags$li("p-value: < 0.05 = statistically significant differences")
        )
      )
    }
  })
  
  # Data preview
  output$data_preview <- DT::renderDataTable({
    req(transformed_data())
    DT::datatable(transformed_data(), options = list(pageLength = 15, scrollX = TRUE))
  })
  
  # Export transformed data as Excel
  output$export_data <- downloadHandler(
    filename = function() {
      paste0("transformed_data_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(transformed_data(), file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
}
