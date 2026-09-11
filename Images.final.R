# Load required libraries
setwd ("C:/Users/jw0104/OneDrive - University of Surrey/Documents/Elicitation/WOHC/Manuscript/Plos One/")
rm(list=ls())
library(ggplot2)
library(reshape2)
library(gridExtra)
library(cowplot)


dt.Jn24 <- read.csv("C:/Users/jw0104/OneDrive - University of Surrey/Documents/Elicitation/WOHC/Formatted_Data/Data_June24/Equity vs efficiency in health - WHO(VR) May 24_May 28, 2024_07.52_clean.csv")
dt <- read.csv("C:/Users/jw0104/OneDrive - University of Surrey/Documents/Elicitation/WOHC/Formatted_Data/Combined.data.csv")

dt.Jn24 <- dt.Jn24[,-c(1:2, 24:33)]
dt <- dt[,-c(1:3, 25:34)]

colnames(dt.Jn24 )<- c("Q2.1", "Q2.2", "Q2.3", "Q2.4", "Q2.5", "Q2.6", "Q2.7", "Q3.1", "Q3.2", "Q3.3", "Q3.4", "Q3.5", "Q3.6", "Q3.7", "Q4.1", "Q4.2",
                       "Q4.3", "Q4.4", "Q4.5", "Q4.6", "Q4.7")

dt.all <- rbind(dt.Jn24, dt)
data <- dt.all[-c(66, 68, 69, 71, 82, 94, 115:138, 150:152), ]

data.scenario.1 <- data[, 1:7]
data.scenario.2 <- data[, 8:14]
data.scenario.3 <- data[, 15:21]

nrow(data)

table(data.scenario.1$Q2.1)/119*100
table(data.scenario.1$Q2.2)/119*100
table(data.scenario.1$Q2.3)/119*100
table(data.scenario.1$Q2.4)/119*100
table(data.scenario.1$Q2.5)/119*100
table(data.scenario.1$Q2.6)/119*100
table(data.scenario.1$Q2.7)/119*100

table(data.scenario.2$Q3.1)/119*100
table(data.scenario.2$Q3.2)/119*100
table(data.scenario.2$Q3.3)/119*100
table(data.scenario.2$Q3.4)/119*100
table(data.scenario.2$Q3.5)/119*100
table(data.scenario.2$Q3.6)/119*100
table(data.scenario.2$Q3.7)/119*100

table(data.scenario.3$Q4.1)/119*100
table(data.scenario.3$Q4.2)/119*100
table(data.scenario.3$Q4.3)/119*100
table(data.scenario.3$Q4.4)/119*100
table(data.scenario.3$Q4.5)/119*100
table(data.scenario.3$Q4.6)/119*100
table(data.scenario.3$Q4.7)/119*100





# Function to process each scenario
process_scenario <- function(scenario_data, scenario_label) {
  
  colnames(scenario_data) <- paste("Q", 1:ncol(scenario_data), sep = "")
  scenario_data$id <- 1:nrow(scenario_data)
  long_data <- melt(scenario_data, id.vars = "id", 
                    variable.name = "Question", value.name = "Choice")
  
  # Plot heatmap
  ggplot(long_data, aes(x = Question, y = factor(id), fill = factor(Choice))) +
    geom_tile(color = "white") +
    scale_fill_manual(
      values = c("1" = "deepskyblue", "2" = "salmon"),
      labels = c("1" = "A", "2" = "B"),
      na.value = "grey19"
    ) +
    scale_y_discrete(breaks = seq(0, 120, by = 10)) +  # Adjust y-axis
    labs(title = paste("Participants choices - Scenario", scenario_label), 
         x = "Question", y = "Respondents", fill = "Programme choice") +
    theme_minimal()
}

# Run the function for all scenarios
plot1 <- process_scenario(data.scenario.1, "X")
plot2 <- process_scenario(data.scenario.2, "Y")
plot3 <- process_scenario(data.scenario.3, "Z")

# # Print plots
# pdf("Choices.plot.pdf", width = 5, height = 7, )
# plot_grid(plot1, plot2, plot3, ncol=1, labels = "")
# dev.off() 

tiff(
  filename = "Fig2.tiff",
  width = 5,
  height = 7,
  units = "in",
  res = 600,
  compression = "lzw"
)

plot_grid(plot1, plot2, plot3, ncol = 1, labels = "")

dev.off()


## Figure 1 barplot of ages 

rm(list=ls())  

dt <- read.csv("C:/Users/jw0104/OneDrive - University of Surrey/Documents/Elicitation/WOHC/Analysis and code/Arthur Stata analysis Sept 24/long data equity.csv")
dt.Feb22<- read.csv("C:/Users/jw0104/OneDrive - University of Surrey/Documents/Elicitation/WOHC/Formatted_Data/Combined.data.csv")
dt.Jn24 <- read.csv("C:/Users/jw0104/OneDrive - University of Surrey/Documents/Elicitation/WOHC/Formatted_Data/Data_June24/Equity vs efficiency in health - WHO(VR) May 24_May 28, 2024_07.52_clean.csv")

dt.Feb22 <- dt.Feb22[, -3]
colnames(dt.Jn24) <- c("Consent","Q36.preference","Q2.1","Q2.2","Q2.3","Q2.4","Q2.5","Q2.6","Q2.7",
                       "Q3.1","Q3.2","Q3.3","Q3.4","Q3.5","Q3.6","Q3.7","Q4.1","Q4.2","Q4.3","Q4.4","Q4.5","Q4.6","Q4.7",
                       "Gender","Age","Q37.Organisation","Q38.Sector","Q35.Yrs_experience","Q39.OH_network.","Q40.Equity_concept",
                       "Q77_health_status","Survey","ID")                
dt.combined <- rbind(dt.Feb22, dt.Jn24)

count <- table(dt.combined$Age) 
table(dt.combined$Survey)

cats <- c("20-25","26-30", "31-35", "36-40", "41-45", "46-50", "51-55", "56-60", "61-65", ">65", "unknown")

# pdf("Figure1.pdf", width = 9, height = 4) 
# barplot(count, names.arg=cats, 
#         xlab="Age Categories", ylab="Number of respondents",
#         col = "white", 
#         ylim = c(0,25), 
#         space=(0.2), bty="n")
# 
# dev.off()

tiff(
  "Fig1.tif",
  width = 9,
  height = 5,
  units = "in",
  res = 300
)
barplot(count, names.arg=cats, 
        xlab="Age Categories", ylab="Number of respondents",
        col = "white", 
        ylim = c(0,25), 
        space=(0.2))

dev.off()
