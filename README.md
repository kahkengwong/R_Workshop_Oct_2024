# R_Workshop_Oct_2024 
Author: Assoc. Prof. Dr. Wong Kah Keng

Affiliation: Department of Immunology, School of Medical Sciences, Universiti Sains Malaysia

---

```r
##########################################################################
# Section 1: Install Packages, Load Libraries, and Set Working Directory
##########################################################################
# Install necessary packages 
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readxl")
install.packages("openxlsx")
install.packages("writexl")
install.packages("plotly")
install.packages("ComplexHeatmap")
install.packages("circlize")
install.packages("RColorBrewer")

# (Optional) If ComplexHeatmap or circlize failed to install, install BiocManager to install the packages from Bioconductor
# if (!requireNamespace("BiocManager", quietly = TRUE))
#    install.packages("BiocManager")
# BiocManager::install("ComplexHeatmap")
# BiocManager::install("circlize")

# Load required libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(openxlsx)
library(writexl)
library(plotly)
library(ComplexHeatmap)
library(circlize)
library(RColorBrewer)

# Set the working directory 
# Files --> Click on ... --> Choose your folder --> More --> "Set As Working Directory"
setwd("C:/Users/Wong/Desktop/R_Workshop_Oct2024")


######################################
# Section 2: Barplot with Jitterplot and Boxplot
######################################
### Subsection 1: Load the data ###
# 2.1 Read data from the Excel file
gene_data <- read_excel("Template_Data.xlsx")

# 2.2 Clean column names (remove spaces and unnecessary columns)
gene_data <- gene_data %>%
  rename(Expression_Level = `Expression levels`) %>%  # Correct column name
  select(-No)  # Remove the "No" column
# Without using "%>%" it will be "gene_data <- select(rename(read_excel("Template_Data.xlsx"), Expression_Level = `Expression levels`), -No)"

# 2.3 View the cleaned data
head(gene_data) # Can also view within RStudio's console


### Subsection 2: Calculate mean and SD ###
# 2.4 Calculate mean and standard deviation for each group
gene_summary <- gene_data %>%
  group_by(Gene, Regulation) %>%
  summarise(
    Mean_Expression = mean(Expression_Level),
    SD_Expression = sd(Expression_Level)
  )

# 2.5 View the summary data
print(gene_summary)


### Subsection 3: bar chart ###
# 2.6 Create bar chart with SD
ggplot(gene_summary, aes(x = Gene, y = Mean_Expression, fill = Regulation)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = Mean_Expression - SD_Expression, ymax = Mean_Expression + SD_Expression), 
                width = 0.25, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("TF" = "blue", "Upregulated" = "salmon", "Downregulated" = "skyblue")) +
  theme_minimal() +
  labs(title = "Gene Expression Levels with Standard Deviation", x = "Gene", y = "Mean Expression Level")


### Subsection 4: Shapiro-Wilk test ###
# 2.7 Perform Shapiro-Wilk test for each group
shapiro_results <- gene_data %>%
  group_by(Gene, Regulation) %>%
  summarise(
    p_value = shapiro.test(Expression_Level)$p.value
  ) %>%
  mutate(
    Normality = ifelse(p_value > 0.05, "Pass", "Fail"),
    Distribution = ifelse(p_value > 0.05, "Normal", "Not Normal")
  )

# 2.8 View results
print(shapiro_results)
# Due to some of the groups showing non-normal distribution, we should use non-parametric methods instead i.e., median and IQR
# i) Parametric (Pearson/t-test/ANOVA):
#    - Assumes a certain shape such as normal distribution (bell curve)
#    - Uses actual values and their distances
#    - Example: comparing students' exact test scores (70, 85, 90...)
#    - When to use: When data follows normal distribution and uses actual measurements

# ii) Non-parametric (Spearman/Mann-Whitney/Kruskal-Wallis):
#    - Doesn't assume any particular shape in data distribution
#    - Uses ranks rather than actual values
#    - Example: comparing students' class ranks (1st, 2nd, 3rd...) instead of actual scores
#    - When to use: When data is skewed or when dealing with rankings/orders


### Subsection 5: bar chart uses median and IQR instead ###
# 2.9 Calculate median and IQR for each group
gene_summary <- gene_data %>%
  group_by(Gene, Regulation) %>%
  summarise(
    Median_Expression = median(Expression_Level),
    Lower_IQR = quantile(Expression_Level, 0.25),
    Upper_IQR = quantile(Expression_Level, 0.75)
  )

# 2.10 View the summary data
print(gene_summary)

# 2.11 Create bar chart with IQR
ggplot(gene_summary, aes(x = Gene, y = Median_Expression, fill = Regulation)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = Lower_IQR, ymax = Upper_IQR), 
                width = 0.25, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("TF" = "blue", "Upregulated" = "salmon", "Downregulated" = "skyblue")) +
  theme_minimal() +
  labs(title = "Gene Expression Levels with IQR", x = "Gene", y = "Median Expression Level")


### Subsection 6: Boxplot ###
# 2.12 Boxplot for each gene with different regulation categories
ggplot(gene_data, aes(x = Gene, y = Expression_Level, fill = Regulation)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8), color = "black") +
  scale_fill_manual(values = c("TF" = "lightgray", "Upregulated" = "lightcoral", "Downregulated" = "lightblue")) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5)) + # Add thin black border
  labs(title = "Gene Expression Levels by Regulation Category", x = "Gene", y = "Expression Level")

# 2.13 Boxplot for each gene with scatter dots
ggplot(gene_data, aes(x = Gene, y = Expression_Level, fill = Regulation)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8), color = "black") +
  geom_jitter(aes(color = Regulation), width = 0.2, alpha = 0.5, size = 1.5) + # Add scatter dots with alpha
  scale_fill_manual(values = c("TF" = "#FFFFCC", "Upregulated" = "#fb9a99", "Downregulated" = "#a6cee3")) +
  scale_color_manual(values = c("TF" = "#FFB266", "Upregulated" = "#e31a1c", "Downregulated" = "#33a02c")) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5)) + # Thin black border
  labs(title = "Gene Expression Levels by Regulation Category", x = "Gene", y = "Expression Level")

# 2.14 Boxplot for each gene with scatter dots with color adjustments
ggplot(gene_data, aes(x = Gene, y = Expression_Level, fill = Regulation)) +
    geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8), color = "black", alpha = 0.8) +
    geom_jitter(aes(color = Regulation), width = 0.2, alpha = 0.5, size = 1.5) + # Add scatter dots with alpha
    # Modify colors
    scale_fill_manual(values = c("TF" = "#66c2a5", "Upregulated" = "#fc8d62", "Downregulated" = "#8da0cb")) +
    scale_color_manual(values = c("TF" = "#66c2a5", "Upregulated" = "#fc8d62", "Downregulated" = "#8da0cb")) +
    # Set background to light gray
    theme_minimal() +
    theme(
        panel.background = element_rect(fill = "lightgray", color = NA), # Light gray background
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), # Thin black border
        plot.title = element_text(hjust = 0.5)  # Center the title
    ) +
    labs(title = "Gene Expression Levels by Regulation Category", x = "Gene", y = "Expression Level")



###############################
# Section 3: Scatter Plot
###############################
### Subsection 1: Scatter plot without group ###
# 3.1 Load the data 
data <- read_excel("C:/Users/Wong/Desktop/R_Workshop_Oct2024/gene_scatter_data_grouped_v2.xlsx")

# 3.2 Generate scatter plot without group
ggplot(data, aes(x = Gene_1, y = Gene_2)) +
  geom_point() +
  labs(title = "Scatter Plot of Gene_1 vs Gene_2", x = "Gene_1", y = "Gene_2") +
  theme_minimal()

# 3.3 Generate scatter plot with customized colors
ggplot(data, aes(x = Gene_1, y = Gene_2)) +
  geom_point(color = "dodgerblue", fill = "skyblue", shape = 21, size = 3, stroke = 0.6, alpha = 0.65) +
  labs(title = "Scatter Plot of Gene_1 vs Gene_2", x = "Gene_1", y = "Gene_2") +
  theme_minimal()

# 3.4 Generate scatter plot with customized colors and customized theme
ggplot(data, aes(x = Gene_1, y = Gene_2)) +
  geom_point(color = "dodgerblue", fill = "skyblue", shape = 21, size = 3, stroke = 0.6, alpha = 0.65) +
  labs(title = "Scatter Plot of Gene_1 vs Gene_2", x = "Gene_1", y = "Gene_2") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    axis.title.x = element_text(face = "italic", color = "darkblue"),
    axis.title.y = element_text(face = "italic", color = "darkblue"),
    axis.text = element_text(color = "gray20")
  )
  
# 3.5 Generate scatter plot with customized colors and white background
ggplot(data, aes(x = Gene_1, y = Gene_2)) +
  geom_point(color = "dodgerblue", fill = "skyblue", shape = 21, size = 3, stroke = 0.6, alpha = 0.65) +
  labs(title = "Scatter Plot of Gene_1 vs Gene_2", x = "Gene_1", y = "Gene_2") +
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # Set background to white
    plot.background = element_rect(fill = "white", color = NA),   # Set entire plot background to white
    panel.grid.major = element_line(color = "#E8E8E8"),            
    panel.grid.minor = element_line(color = "#F0F0F0"),                     
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    axis.title.x = element_text(face = "italic", color = "darkblue"),
    axis.title.y = element_text(face = "italic", color = "darkblue"),
    axis.text = element_text(color = "gray20")
  )


### Subsection 2: Scatter plot with group ###
# 3.6 Generate scatter plot by group with italicized labels and plot border
ggplot(data, aes(x = Gene_1, y = Gene_2, color = Group)) +
  geom_point() +
  labs(title = "Scatter Plot of Gene_1 vs Gene_2 Across Groups", x = "Gene_1", y = "Gene_2") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(face = "italic"),  # Italicize x-axis label
    axis.title.y = element_text(face = "italic"),  # Italicize y-axis label
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)  # Thin black border around plot
  )

# 3.7 Generate scatter plot by group with filled, transparent dots and thin border
ggplot(data, aes(x = Gene_1, y = Gene_2, color = Group, fill = Group)) +
  geom_point(shape = 21, size = 4, stroke = 0.4, alpha = 0.6) +  # Customized filled dots
  labs(title = "Scatter Plot of Gene_1 vs Gene_2 Across Groups", x = "Gene_1", y = "Gene_2") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(face = "italic"),  # Italicize x-axis label
    axis.title.y = element_text(face = "italic"),  # Italicize y-axis label
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)  # Thin black border around plot
  )

# 3.8 Generate scatter plot by group with separate trendlines and 95% CI
ggplot(data, aes(x = Gene_1, y = Gene_2, color = Group, fill = Group)) +
    geom_point(shape = 21, size = 4, stroke = 0.4, alpha = 0.6) +  # Customized filled dots
    geom_smooth(method = "lm", aes(color = Group, fill = Group), alpha = 0.2, linetype = "solid") +  # Group-specific trendlines with 95% CI
    labs(title = "Scatter Plot of Gene_1 vs Gene_2 Across Groups", x = "Gene_1", y = "Gene_2") +
    theme_minimal() +
    theme(
        axis.title.x = element_text(face = "italic"),  # Italicize x-axis label
        axis.title.y = element_text(face = "italic"),  # Italicize y-axis label
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)  # Thin black border around plot
    )

# 3.9 Generate scatter plot by group with softer colors for dots' borders and trendlines
ggplot(data, aes(x = Gene_1, y = Gene_2, color = Group, fill = Group)) +
    geom_point(shape = 21, size = 4, stroke = 0.5, alpha = 0.6) +  # Softer borders for dots
    geom_smooth(method = "lm", aes(color = Group, fill = Group), alpha = 0.2, linetype = "solid", size = 1) +  # Softer trendlines
    scale_color_manual(values = c("Benign" = "#009E73", "Cancer" = "#B266FF", "Healthy" = "#56B4E9")) +  # Softer colors for trendlines and dot borders
    scale_fill_manual(values = c("Benign" = "#7AD151", "Cancer" = "#CC99FF", "Healthy" = "#00BFC4")) +  # Fill colors
    labs(title = "Scatter Plot of Gene_1 vs Gene_2 Across Groups", x = "Gene_1", y = "Gene_2") +
    theme_minimal() +
    theme(
        axis.title.x = element_text(face = "italic"),  # Italicize x-axis label
        axis.title.y = element_text(face = "italic"),  # Italicize y-axis label
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)  # Thin black border around plot
    )


### Subsection 3: Faceted plot using linear regression ###
# 3.10 Firstly, calculate correlation statistics. 
cor_stats <- data %>%
    group_by(Group) %>%
    summarize(
        r = round(cor(Gene_1, Gene_2, method = "pearson"), 2),
        p = round(cor.test(Gene_1, Gene_2)$p.value, 5),
        slope = round(coef(lm(Gene_2 ~ Gene_1))[2], 3),
        intercept = round(coef(lm(Gene_2 ~ Gene_1))[1], 3)
    ) %>%
    mutate(
        label = paste("r =", r, ", p =", p),
        eq = paste("y =", intercept, "+", slope, "x")
    )

# 3.11 Create the faceted plot
ggplot(data, aes(x = Gene_1, y = Gene_2, color = Group, fill = Group)) +
    facet_wrap(~Group, scales = "free") +
    
    # Add points and trend lines
    geom_point(shape = 21, size = 4, stroke = 0.5, alpha = 0.6) +
    geom_smooth(method = "lm", alpha = 0.2, linetype = "solid", size = 1) +
    
    # Add statistics
    geom_text(data = cor_stats, 
              aes(label = label),
              x = -Inf, y = Inf,
              hjust = -0.1, vjust = 2,
              color = "black", size = 3) +
    geom_text(data = cor_stats,
              aes(label = eq),
              x = -Inf, y = Inf,
              hjust = -0.1, vjust = 3.5,
              color = "black", size = 3) +
    
    # Colors
    scale_color_manual(values = c("Benign" = "#009E73", 
                                  "Cancer" = "#B266FF", 
                                  "Healthy" = "#56B4E9")) +
    scale_fill_manual(values = c("Benign" = "#7AD151", 
                                 "Cancer" = "#CC99FF", 
                                 "Healthy" = "#00BFC4")) +
    
    # Theme
    theme_minimal() +
    theme(
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        strip.background = element_rect(fill = "gray90", color = "black"),
        strip.text = element_text(face = "bold", size = 12),
        legend.position = "none",
        aspect.ratio = 1.5
    ) +
    scale_x_continuous(expand = expansion(mult = 0.2)) +
    scale_y_continuous(expand = expansion(mult = 0.2))  # Add 20% padding to axes

# We use Pearson's correlation (r) here because:
# i) method = "lm" fits a linear regression assuming normally distributed data
# ii) Both linear regression and Pearson's are parametric methods
# iii) Both assume linear relationships between variables


### Subsection 4: Faceted plot using LOESS (Locally Estimated Scatterplot Smoothing) ###
# Calculate Spearman's correlation statistics
cor_stats <- data %>%
    group_by(Group) %>%
    summarize(
        rs = round(cor.test(Gene_1, Gene_2, method = "spearman")$estimate, 2),
        p = round(cor.test(Gene_1, Gene_2, method = "spearman")$p.value, 5)
    ) %>%
    mutate(
        label = paste("r (rho) =", rs, ", p =", p)
    )

# 3.13 Create the faceted LOESS plot
ggplot(data, aes(x = Gene_1, y = Gene_2, color = Group, fill = Group)) +
    facet_wrap(~Group, scales = "free") +
    
    # Add points and trend lines
    geom_point(shape = 21, size = 4, stroke = 0.5, alpha = 0.6) +
    geom_smooth(method = "loess", alpha = 0.2, linetype = "solid", size = 1) +
    
    # Add statistics
    geom_text(data = cor_stats, 
              aes(label = label),
              x = -Inf, y = Inf,
              hjust = -0.1, vjust = 2,
              color = "black", size = 3) +
    
    # Colors
    scale_color_manual(values = c("Benign" = "#009E73", 
                                  "Cancer" = "#B266FF", 
                                  "Healthy" = "#56B4E9")) +
    scale_fill_manual(values = c("Benign" = "#7AD151", 
                                 "Cancer" = "#CC99FF", 
                                 "Healthy" = "#00BFC4")) +
    
    # Theme
    theme_minimal() +
    theme(
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        strip.background = element_rect(fill = "gray90", color = "black"),
        strip.text = element_text(face = "bold", size = 12),
        legend.position = "none",
        aspect.ratio = 1.5
    ) +
    scale_x_continuous(expand = expansion(mult = 0.2)) +
    scale_y_continuous(expand = expansion(mult = 0.2))

# We use Spearman's correlation (rho) here because:
# i) method = "loess" fits a flexible curve without assuming data distribution
# ii) Both LOESS and Spearman's are non-parametric methods
# iii) Both can handle non-linear relationships between variables


 
    
###############################
# Section 4: 3D Scatter Plot
###############################
### Subsection 1: 3D plot of all groups ###
# 4.1 Load data 
data_v2 <- read_excel("C:/Users/Wong/Desktop/R_Workshop_Oct2024/gene_scatter_data_grouped_v2.xlsx")

# 4.2 3D plot
# Load plotly package
library(plotly)

# 4.3 Create the 3D scatter plot
plot_ly(data = data_v2, 
        x = ~Gene_1, 
        y = ~Gene_2, 
        z = ~Gene_3,
        color = ~Group,
        colors = c("Benign" = "#7AD151",  
                   "Cancer" = "#CC99FF", 
                   "Healthy" = "#00BFC4"),
        type = "scatter3d",
        mode = "markers",
        marker = list(
            size = 5,
            opacity = 0.8,
            symbol = 'circle',
            line = list(
                width = 1,
                color = '#000000' 
            ),
            showscale = FALSE
        )) %>%
    layout(
        title = "3D Scatter Plot of Gene Expression",
        scene = list(
            xaxis = list(title = "Gene_1",
                         titlefont = list(size = 12)),
            yaxis = list(title = "Gene_2",
                         titlefont = list(size = 12)),
            zaxis = list(title = "Gene_3",
                         titlefont = list(size = 12))
        ),
        legend = list(
            x = 1.1,
            y = 0.9
        )
    )
    
    
### Subsection 2: 3D plot of two groups ###
# 4.4 Filter data for only Cancer and Healthy groups
data_filtered <- data_v2 %>%
    filter(Group %in% c("Cancer", "Healthy"))

# 4.5 Create the 3D scatter plot with bordered points
plot_ly(data = data_filtered, 
        x = ~Gene_1, 
        y = ~Gene_2, 
        z = ~Gene_3,
        color = ~Group,
        colors = c("Cancer" = "#CC99FF", 
                   "Healthy" = "#00BFC4"),  
        type = "scatter3d",
        mode = "markers",
        marker = list(
            size = 8,
            opacity = 0.8,
            symbol = 'circle',
            line = list(
                width = 1,
                color = 'black'  # Light black color
            ),
            showscale = FALSE
        )) %>%
    layout(
        title = "3D Scatter Plot of Gene Expression (Cancer vs Healthy)",
        scene = list(
            xaxis = list(title = "Gene_1",
                         titlefont = list(size = 12)),
            yaxis = list(title = "Gene_2",
                         titlefont = list(size = 12)),
            zaxis = list(title = "Gene_3",
                         titlefont = list(size = 12))
        ),
        legend = list(
            x = 1.1,
            y = 0.9
        )
    )



#########################
# Section 5: Heatmap
#########################
### Subsection 1: 3D plot of all groups ###
# 5.1 Prepare data matrix
expression_matrix <- data_v2 %>%
  select(Gene_1, Gene_2, Gene_3) %>%
  as.matrix()

# 5.2 Scale the data
scaled_matrix <- scale(expression_matrix)

# 5.3 Get actual range of z-scores
z_min <- floor(min(scaled_matrix))
z_max <- ceiling(max(scaled_matrix))

# 5.4 Add row names
rownames(expression_matrix) <- paste0("Sample_", 1:nrow(expression_matrix))

# 5.5 Create annotation for groups
ha_row <- HeatmapAnnotation(
  df = data.frame(Group = data_v2$Group),
  col = list(Group = c("Benign" = "#7AD151",
                      "Cancer" = "#CC99FF",
                      "Healthy" = "#00BFC4")),
  which = "row",
  show_legend = TRUE,
  annotation_name_side = "top"
)

# 5.6 Set up colors for heatmap with actual z-score range
col_fun <- colorRamp2(
  breaks = seq(z_min, z_max, length = 7),
  colors = rev(brewer.pal(7, "YlGnBu")) # Change the color palette using RColorBrewer's palettes
)

# 5.7 Open a new window for the plot
dev.new(width = 8, height = 10)

# 5.8 Create and draw the heatmap
set.seed(123)  # For reproducible clustering
heatmap <- Heatmap(
  # Data
  matrix = scaled_matrix,
  
  # Main settings
  name = "Z-score",
  column_title = "Genes",
  row_title = "Samples",
  
  # Clustering settings
  clustering_distance_rows = "euclidean",
  clustering_method_rows = "complete",
  clustering_distance_columns = "euclidean",
  clustering_method_columns = "complete",
  
  # Color settings
  col = col_fun,
  
  # Annotation
  left_annotation = ha_row,
  
  # Additional settings
  show_row_names = FALSE,
  show_column_names = TRUE,
  column_names_rot = 45,
  column_names_gp = gpar(fontsize = 8),
  rect_gp = gpar(col = "gray10", lwd = 0.5),
  
  # Legend settings
  heatmap_legend_param = list(
    title = "Z-score",
    color_bar = "continuous",
    legend_direction = "horizontal",
    legend_width = unit(4, "cm")
  )
)

# 5.9 Draw the heatmap
draw(heatmap, heatmap_legend_side = "bottom")


###############################################
# Section 6: Line plots with user-defined 95% CI bounds
###############################################
### Subsection 1: Multiple line plots ###
# 6.1 Load data and define x- and y-axis
lineplot <- read.xlsx("Ggplot2_line_ABC-DLBCL.xlsx", sheet="Sheet1")
x=lineplot$x
y=lineplot$y
lineplot2 <- read.xlsx("Ggplot2_line_GCB-DLBCL.xlsx", sheet="Sheet1")
x2=lineplot2$x
y2=lineplot2$y

# 6.2 Optimised for the first line graph (GCB vs immune/TCR heatmap)
ggplot(data = lineplot2, aes(x = x2, y = y2, fill = Type, color = Type)) +
  scale_colour_brewer(palette = "Set1") +
  geom_line(size = 1.5, alpha = 0.7) +
  geom_point(aes(x = x2, y = y2)) +
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.3, color = NA) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "gray72", size = 0.5),
    panel.background = element_blank()
  )

# 6.3 Optimised for the second line graph (ABC vs immune/TCR heatmap)
ggplot(data = lineplot, aes(x = x, y = y, fill = Type, color = Type)) +
  scale_colour_brewer(palette = "Set1") +
  geom_line(size = 1.5, alpha = 0.7) +
  geom_point(aes(x = x, y = y)) +
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.3, color = NA) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "gray72", size = 0.5),
    panel.background = element_blank()
  )
# Export as Pdf then print screen for the plot


### Subsection 2: Calculate lower and upper bound of 95% CI ###
# 6.4 Sample data
data <- c(10, 12, 11, 13, 9)

# 6.5 Function to calculate CI with flexible confidence level
calculate_CI <- function(data, conf.level = 0.95) {
    # Calculate mean and standard deviation
    mean_val <- mean(data)
    sd_val <- sd(data)
    
    # Sample size
    n <- length(data)
    
    # Standard Error of the Mean (SEM)
    sem <- sd_val / sqrt(n)
    
    # Alpha calculation
    alpha <- 1 - conf.level
    tail_prob <- 1 - alpha/2
    
    # Critical value selection based on sample size
    critical_value <- if(n <= 30) {
        # Use t-distribution for n ≤ 30
        qt(tail_prob, df = n - 1)
    } else {
        # Use z-distribution for n > 30
        qnorm(tail_prob)
    }
    
    # Margin of Error (ME)
    margin_error <- critical_value * sem
    
    # Confidence Interval
    ci <- c(mean_val - margin_error, mean_val + margin_error)
    
    # Output with information
    cat(sprintf("Sample size: %d\n", n))
    cat(sprintf("Confidence level: %.1f%%\n", conf.level * 100))
    cat(sprintf("Using %s distribution (critical value: %.3f)\n", 
                if(n <= 30) "t" else "z", 
                critical_value))
    cat(sprintf("CI: [%.3f, %.3f]\n", ci[1], ci[2]))
    
    return(ci)
}

# 6.6 95% CI upper and lower bounds
ci_95 <- calculate_CI(data, conf.level = 0.95)
