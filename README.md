# Interactive Tumor Analysis with DRAP

Overview
This Shiny application provides an interactive platform for analyzing tumor growth data using the DRAP (Drug Response Analysis and Visualization for PDX models) R package. The app supports multiple statistical tests, data visualization options, and allows users to handle repeated measures and hierarchical data through mixed ANOVA and linear mixed models (LMM).  


Features

- Upload Excel tables (.xlsx) containing tumor volume measurements for multiple treatment arms and time points.  
- Visualize tumor growth data through:
  - Volume Growth Curve
  - Relative Change
  - Tumor Growth Inhibition (TGI)
  - Endpoint Analysis
  - Response Level Analysis  
- Statistical Analysis:
  - GR.ANOVA (Growth Rate ANOVA)
  - Kruskal-Wallis test
  - Mixed ANOVA
  - Linear Mixed Model (LMM)
  - Automatic fallback to manual R implementation if DRAP returns NULL or data is insufficient  
- Interactive UI:
  - Select control group for comparison
  - Choose plot type and analysis level (Animal/Arm)
  - Dynamic display options for TGI plots  
- Copy results:
  - Easily copy statistical results to the clipboard with a single button.  
- Preview data:
  - View uploaded and transformed data in a searchable table.  

Online use:
https://lucijanskof.shinyapps.io/DRAP-interface/

Installation

1. Clone the repository or download the Shiny app files.  
2. Install required R packages:

install.packages(c("shiny", "shinythemes", "readxl", "data.table", "ggplot2", "clipr"))

 Install DRAP from Bioconductor or CRAN if needed
 BiocManager::install("DRAP")
