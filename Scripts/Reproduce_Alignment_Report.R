## Import ggplot
library(ggplot2)
library(ggrepel)

## Reproduce resutls from alignment report
plot_stages <- c('Sperm', "MII Ooctye", "Zygote", "2-Cell", "4-Cell", "8-Cell", "Morula", "ICM", "Post-Implantation")
plot_percentages <- c(35.628, 24.03,17.013, 10.75, 16, 19.73, 19.716, 17.725, 46.45)
plot_x <- c(1,1,2,3,4,5,6,7,8)
color_replicated <- c("#fcf003","#00d146","#051fa3","#ff0509","#ff8ce8","#00fbff","#918979","#6200a8","#000000")

## Plot averages
plot_df <- data.frame(plot_x, plot_percentages)
plot_df[,2] <- plot_df[,2]
rownames(plot_df) <- plot_stages


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
  