## Import GGPlot for plotting
library(ggplot2)

## Reproducing the results from the paper
## Make sure that samples have been loaded with BedGraph_Import

average_methylation <- ifelse(samples[,3:11] >= 0.5, 1, 0)
col_average_methylation <- colMeans(average_methylation)
plot_x <- c(1,1,2,3,4,5,6,7,8)
plot_stages <- c('Sperm', "MII Ooctye", "Zygote", "2-Cell", "4-Cell", "8-Cell", "Morula", "ICM", "Post-Implantation")
color_replicated <- c("#fcf003","#00d146","#051fa3","#ff0509","#ff8ce8","#00fbff","#918979","#6200a8","#000000")

## Plot graph
plot_df <- data.frame(plot_x, col_average_methylation)
plot_df[,2] <- plot_df[,2] * 100

rownames(plot_df) <- plot_stages
colnames(plot_df) <- c("plot_x", "plot_percentages")


ggplot(plot_df, aes(x = plot_x, y = plot_percentages)) +
  geom_line(data = plot_df[2:3,], aes(x=plot_x, y = plot_percentages), color = "#006bc9",size=1.5,alpha = 0.8)+
  geom_line(data = plot_df[c(1,3),], aes(x=plot_x, y = plot_percentages), color = "#006bc9",size=1.5,alpha = 0.8)+
  geom_line(data = plot_df[3:9,], aes(x=plot_x, y = plot_percentages), color = "#006bc9",size=1.5,alpha = 0.8) + 
  geom_point(data = plot_df, aes(plot_x,plot_percentages),colour = color_replicated, size = 3,alpha = 1) +
  labs(y = "DNA methylation level (%)", x = "Developmental Stage") +
  geom_text_repel(aes(x = plot_x, y = plot_percentages, label=rownames(plot_df)),point.padding = 10,force= 15,nudge_x = 0.5, nudge_y = -0.6, size = 5)+
  theme(legend.title = element_blank(), legend.position = "none", axis.title = element_text(size = 12), axis.text.x = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.text = element_text(size=16),
        axis.ticks.x = element_blank(), axis.line = element_line(colour = "black",arrow = grid::arrow(length = unit(0.3, "cm"))))

                                