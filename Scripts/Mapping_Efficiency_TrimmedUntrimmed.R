trimmed_eff <- c(20.2,20.5,27.1, 18.3, 8.8)
untrimmed_eff <- c(16.4, 17.9,8.7,10.9,4)
samples_names <- c("SRR1295907", "SRR1295908", "SRR1295909", "SRR1295910", "SRR12959011")
plot_helper <- c(1:5)

plot_df <- data.frame(trimmed_eff,untrimmed_eff, plot_helper)
rownames(plot_df) <- samples_names

ggplot(plot_df, aes(x = plot_helper, y = trimmed_eff)) + 
  geom_point(data = plot_df, aes(x = plot_helper, y = trimmed_eff, colour="Trimmed"), size = 2) +
  geom_point(data = plot_df, aes(x = plot_helper, y = untrimmed_eff, colour="Untrimmed"), size = 2) +
  labs(y = "Mapping Efficiency (%)" , x = "Samples", color = "") +
  theme_bw() + 
  theme(axis.title = element_text(size = 12), axis.text.x = element_blank(), axis.text = element_text(size=16),
       axis.line = element_line(colour = "black",arrow = grid::arrow(length = unit(0.3, "cm")))) 
