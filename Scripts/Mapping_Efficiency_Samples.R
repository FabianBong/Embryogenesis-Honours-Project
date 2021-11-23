## load library
library(ggplot2)

## Load data from csv
mapping_efficiency <- read.csv(file = '/Users/fabianbong/Documents/Honours_Project_R/Data/Overview/Mapping_Efficiency_R.csv', header = TRUE)

##create plot data frame
plot_df <- data.frame(plot_helper = factor(mapping_efficiency[1:75,3]), efficiency = mapping_efficiency[1:75,2])


## plot data 
ggplot(plot_df, aes(x = plot_helper, y = efficiency)) + 
  geom_point(data = plot_df, aes(x = plot_helper, y = efficiency, colour = factor(plot_helper)), size = 2) +
  labs(y = "Mapping Efficiency (%)" , x = "Developmental Stage") +
  theme_bw() + 
  theme(axis.title = element_text(size = 12), axis.text.x = element_blank(), axis.text = element_text(size=16), legend.title = element_blank(),
        axis.line = element_line(colour = "black",arrow = grid::arrow(length = unit(0.3, "cm")))) 
