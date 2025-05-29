library(Amelia)

summary(d)

d$ecfin <- log(d$ecfin + 1)

a.out <- amelia(d, m = 5, ts = "ysp", cs = "country")

ggplot(a.out$imputations[[1]]) +
  geom_line(aes(ysp,exp(ecfin)-1)) +
  geom_point(aes(ysp,exp(ecfin)-1), color = "red", data = d) +
  facet_wrap(~country)

ggplot(a.out$imputations[[1]]) +
  geom_line(aes(ysp,wages)) +
  geom_point(aes(ysp,wages), color = "red", data = d) +
  facet_wrap(~country)

ggplot(a.out$imputations[[1]]) +
  geom_line(aes(ysp,prof_wages), data = a.out$imputations[[1]], color = "blue1") +
  geom_line(aes(ysp,prof_wages), data = a.out$imputations[[2]], color = "blue3") +
  geom_line(aes(ysp,prof_wages), data = a.out$imputations[[3]], color = "blue4") +
  geom_line(aes(ysp,prof_wages), data = a.out$imputations[[4]], color = "cyan4") +
  geom_line(aes(ysp,prof_wages), data = a.out$imputations[[5]], color = "cyan") +
  geom_point(aes(ysp,prof_wages), color = "red", data = d) +
  facet_wrap(~country)

d$avg_prof_wage <- (a.out$imputations[[1]]$prof_wages + a.out$imputations[[2]]$prof_wages + a.out$imputations[[3]]$prof_wages + a.out$imputations[[4]]$prof_wages + a.out$imputations[[5]]$prof_wages)/5

test <- as.data.frame(cbind(d$ysp,d$country,avg_prof_wage))

test <- test %>% rename(country = V2)

ggplot(d) +
  geom_line(aes(ysp,avg_prof_wage), color = "blue1") +
  geom_point(aes(ysp,prof_wages), color = "red") +
  facet_wrap(~country)
