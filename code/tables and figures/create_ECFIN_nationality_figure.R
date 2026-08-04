library(ggplot2)

# staff_nat lives in staff_nat.Rdata. This previously loaded
# Commission_nationalities.Rdata, which contains `staff` and has no `ysp`
# column at all, so from a clean session the plot below errored -- and from a
# session where staff_nat happened to be in the global environment it silently
# plotted whatever was left there. The published figure was only reproducible
# by accident of run order.
load("~/EU_capacity/data/staff_nat.Rdata")

# Contract employees enter the staff population from the Autumn 2014 bulletin,
# so counts before and after this line are not directly comparable. The paper
# marks it for that reason.
CONTRACT_STAFF_ADDED <- 2014.75

staff_nat_plot <- ggplot(data = staff_nat) +
  geom_col(aes(x = ysp, y = ecfin)) +
  geom_vline(xintercept = CONTRACT_STAFF_ADDED, col = "gray", linetype = 2) +
  facet_wrap(~country) +
  labs(x = "Period", y = "Staff at ECFIN") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme_minimal()
print(staff_nat_plot)

ggsave(
  filename = "~/EU_capacity/overleaf/images/ECFIN_Nationality_Plot.pdf",
  plot = staff_nat_plot,
  width = 6,
  height = 4
)
