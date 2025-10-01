library(ggplot2)
library(dplyr)

load("~/ec_project/data/final_dataset_euro_pooled_plus_guide.Rdata")

representation_plot <- ggplot(data = subset(dfpg, !is.na(diff_iv))) +
  geom_hline(yintercept = 0, col = "blue", linetype = 2) +
  #geom_point(aes(x=ysp,y=diff_iv)) +
  geom_line(aes(x = ysp, y = diff_iv)) +
  facet_wrap(~country) +
  labs(x = "Period", y = "Representation") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme_minimal()
print(representation_plot)

ggsave(
  filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/EU_Capacity_Online_Appendix/images/Representation_Plot.png",
  plot = representation_plot,
  width = 8,
  height = 4
)
