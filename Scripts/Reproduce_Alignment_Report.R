## Reproduce resutls from alignment report
plot_stages <- c('Sperm', "MIIOoctye", "Zygote", "2Cell", "4Cell", "8Cell", "Morula", "ICM", "PostImplantation")
plot_percentages <- c(35.628, 24.03,17.013, 10.75, 16, 19.73, 19.716, 17.725, 46.45)
plot_x <- c(1,1,2,3,4,5,6,7,8)


## Plot averages
plot_df <- data.frame(plot_x, plot_percentages)
plot_df[,2] <- plot_df[,2]

ggplot(plot_df, aes(x = plot_x, y = plot_percentages)) +
  geom_point() + 
  ##geom_text(aes(label=rownames(plot_df)),hjust=-0.3, vjust=-0.5)+
  geom_line(data = plot_df[2:3,], aes(x=plot_x, y = plot_percentages))+
  geom_line(data = plot_df[c(1,3),], aes(x=plot_x, y = plot_percentages))+
  geom_line(data = plot_df[3:9,], aes(x=plot_x, y = plot_percentages)) + 
  labs(y = "DNA methylation level (%)", x = "Developmental Stage")
