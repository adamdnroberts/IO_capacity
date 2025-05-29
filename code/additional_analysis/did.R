library(ggplot2)

load("~/ec_project/data/final_dataset_euro.Rdata")

err0_fe <- feols(log(err0_sq) ~ 1|country+ysp+title, data = df)
err1_fe <- feols(log(err1_sq) ~ 1|country+ysp+title, data = df)

did0 <- df[obs(err0_fe),]
did1 <- df[obs(err1_fe),]

did0$err0_fe <- err0_fe$residuals
did1$err1_fe <- err1_fe$residuals

ggplot(subset(did0, title == "Total revenue: general government " & country == "Croatia")) +
  geom_line(aes(ysp,err0_fe)) +
  facet_wrap(~country)

ggplot(subset(did1, title == "Total revenue: general government " & country == "Croatia")) +
  geom_line(aes(ysp,err1_fe))

did_exp <- subset(did, title == "Total expenditure: general government " & (country == "Croatia" | country == "Montenegro"))

ggplot(subset(did0, title == "Total expenditure: general government " & country == "Croatia")) +
  geom_line(aes(ysp,err0_fe)) +
  facet_wrap(~country)

ggplot(subset(did1, title == "Total expenditure: general government " & country == "Croatia")) +
  geom_line(aes(ysp,err1_fe)) +
  facet_wrap(~country)

