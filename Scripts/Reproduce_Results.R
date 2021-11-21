## Import GGPlot for plotting
library(ggplot2)

## Reproducing the results from the paper
## Make sure that samples have been loaded with BedGraph_Import

average_methylation <- ifelse(samples[,3:11] >= 0.5, 1, 0)
col_average_methylation <- colMeans(average_methylation)
plot_x <- c(1,1,2,3,4,5,6,7,8)


## Plot graph
plot_df <- data.frame(plot_x, col_average_methylation)
plot_df[,2] <- plot_df[,2] * 100
ggplot(plot_df, aes(x = plot_x, y = col_average_methylation)) +
  geom_point() + 
  ##geom_text(aes(label=rownames(plot_df)),hjust=-0.3, vjust=-0.5)+
  geom_line(data = plot_df[2:3,], aes(x=plot_x, y = col_average_methylation))+
  geom_line(data = plot_df[c(1,3),], aes(x=plot_x, y = col_average_methylation))+
  geom_line(data = plot_df[3:9,], aes(x=plot_x, y = col_average_methylation)) + 
  labs(y = "DNA methylation level (%)", x = "Developmental Stage")
                                