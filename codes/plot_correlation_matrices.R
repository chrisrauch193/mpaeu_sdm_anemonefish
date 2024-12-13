# Install necessary packages if you don't have them
# install.packages(c("corrplot", "gridExtra", "kableExtra"))

library(corrplot)
library(gridExtra)
library(knitr)
library(kableExtra)
library(grid)  # Add this line to load the 'grid' package

# --- Input ---
rds_file <- "data/log/vif_list_20241212.rds" # Path to your .rds file

# --- Load Data ---
vif_results <- readRDS(rds_file)

# --- Function to Plot Correlation Matrix ---
plot_cor_matrix <- function(cor_matrix, title) {
  corrplot(cor_matrix, 
           method = "color", 
           type = "upper", 
           tl.col = "black", 
           tl.srt = 45,
           title = title,
           mar = c(0, 0, 2, 0)) # Adjust margins to fit title
}

# --- Function to Create VIF Table ---
create_vif_table <- function(vif_df, caption) {
  kable(vif_df, 
        format = "html", 
        caption = caption,
        col.names = c("Variable", "VIF"), # Rename columns for clarity
        align = "lr") %>% # Align VIF column to the right
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = FALSE)
}

# --- Create Plots and Tables ---
pdf("collinearity_analysis_results.pdf", onefile = TRUE) # Output to a single PDF

for (group_depth in names(vif_results$vif_before)) {
  
  # 1. Plot Correlation Matrix (from vif_before)
  cor_matrix <- vif_results$vif_before[[group_depth]]@corMatrix
  plot_title <- paste("Correlation Matrix:", group_depth, "(Before VIF Selection)")
  plot_cor_matrix(cor_matrix, plot_title)
  
  # 2. Create VIF Table (from vif_after), if available for this group_depth
  vif_after_name <- names(vif_results$vif_after)[sapply(names(vif_results$vif_after), function(x) grepl(group_depth, x))]
  
  if (length(vif_after_name) > 0) {
    vif_table_data <- vif_results$vif_after[[vif_after_name]]
    
    if (!is.null(vif_table_data)) {
      vif_table_data <- as.data.frame(vif_table_data) # Ensure it's a data frame
      table_caption <- paste("VIF Scores:", vif_after_name, "(After VIF Selection)")
      vif_table <- create_vif_table(vif_table_data, table_caption)
      
      # Print the table to the PDF
      grid.newpage() # Start a new page for the table
      grid.table(vif_table_data)
    }
  } else {
    # Handle cases where vif_after data might be missing
    grid.newpage()
    grid.text(paste("No VIF results after selection for", group_depth))
  }
}

dev.off() # Close the PDF device

cat("Collinearity analysis results saved to collinearity_analysis_results.pdf\n")