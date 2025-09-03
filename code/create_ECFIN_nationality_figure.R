library(ggplot2)

load("~/ec_project/data/staff_nat.Rdata")

staff_nat_plot <- ggplot(data = staff_nat) +
  geom_col(aes(x = ysp, y = ecfin)) +
  #geom_line(aes(x=ysp,y=total_ecfin)) +
  geom_vline(xintercept = 2014.75, col = "gray", linetype = 2) +
  facet_wrap(~country) +
  labs(x = "Period", y = "Staff at ECFIN") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme_minimal()
print(staff_nat_plot)

ggsave(
  filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/EU_Capacity/images/ECFIN_Nationality_Plot.pdf",
  plot = staff_nat_plot,
  width = 6,
  height = 4
)
