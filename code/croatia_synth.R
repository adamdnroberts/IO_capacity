library(Synth)

test <- dataprep(df)

df_full <- read.csv("~/ecb_project/data/final_dataset_euro.csv")
nonEU <- c("Albania","Canada","Iceland","Japan","Macedonia FYR","Montenegro","North Macedonia","Norway","Serbia","Switzerland","Turkey","United Kingdom","United States","West Germany")

df <- subset(df_full, (title == "Total revenue: general government ") &
               (country == "Croatia" | country %in% nonEU))
summary(as.factor(df$country))

df$unit_num <- as.numeric(as.factor(df$country))

controls <- unique(df$country[df$country != "Croatia"])

#need to get gdp, population for other countries!!
test <- dataprep(foo=df, predictors=c("gdp","pop_int"), dependent="err1_sq", time.variable = "ysp", unit.variable = "unit_num", unit.names.variable = "country", treatment.identifier = "Croatia", controls.identifier = controls)          

summary(as.factor(df$country))

"Canada" %in%