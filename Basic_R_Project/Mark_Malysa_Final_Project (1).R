library("caret")

data = co2.mm.gl_csv
data.After2000s = data[data$Decimal.Date > 2000,]
plot(data.After2000s$Average~data.After2000s$Decimal.Date, main="CO2 Emissions Globally", xlab = "Date", ylab="Average CO2 emissions")
linear.pred <- lm(Average~Decimal.Date,data=data.After2000s)
abline(linear.pred, col="red")


nationData = fossil.fuel.co2.emissions.by.nation_csv
nationData.UnitedStates = nationData[nationData$Country == "UNITED STATES OF AMERICA" & nationData$Year >= 1900,]
plot(nationData.UnitedStates$Total~nationData.UnitedStates$Year, main="CO2 Emissions in United States", xlab = "Date", ylab="Total Carbon Emissions")

nationData.UnitedKingdoms = nationData[nationData$Country == "UNITED KINGDOM" & nationData$Year >= 1900,]
plot(nationData.UnitedKingdoms$Total~nationData.UnitedKingdoms$Year, main="CO2 Emissions in United Kingdom", xlab = "Date", ylab="Total Carbon Emissions")


#hypothesis 1
nationData.UnitedKingdoms.1915 = nationData.UnitedKingdoms[nationData.UnitedKingdoms$Year >= 1900 & nationData.UnitedKingdoms$Year <= 1915,]
mean(nationData.UnitedKingdoms.1915$Total)
sd1.1 = sd(nationData.UnitedKingdoms.1915$Total)

nationData.UnitedKingdoms.2015 = nationData.UnitedKingdoms[nationData.UnitedKingdoms$Year >= 2000 & nationData.UnitedKingdoms$Year <= 2015,]
mean(nationData.UnitedKingdoms.2015$Total)
sd1.2 = sd(nationData.UnitedKingdoms.2015$Total)

differenceOfMeans1 = mean(nationData.UnitedKingdoms.2015$Total) - mean(nationData.UnitedKingdoms.1915$Total)
differenceOfMeans1

se1 = sqrt(
  ((sd(nationData.UnitedKingdoms.1915$Total)^2)/nrow(nationData.UnitedKingdoms.1915)) +
    ((sd(nationData.UnitedKingdoms.2015$Total)^2)/nrow(nationData.UnitedKingdoms.2015))
)
se1
z.score1 = differenceOfMeans1/se1
z.score1
1 - pnorm(z.score1)
plot(x=seq(from = -5, to= 5, by=0.1),y=dnorm(seq(from = -5, to= 5,  by=0.1),mean=0),type='l',xlab = 'mean difference',  ylab='possibility')
abline(v=z.score1, col='red')


#hypothesis 2
nationData.UnitedStates = nationData[nationData$Country == "UNITED STATES OF AMERICA" & nationData$Year >= 2000,]
nationData.China = nationData[nationData$Country == "CHINA (MAINLAND)"& nationData$Year >= 2000,]

mean(nationData.UnitedStates$Total)
mean(nationData.China$Total)
diffOfMeans2 = mean(nationData.UnitedStates$Total) - mean(nationData.China$Total)
diffOfMeans2
sd2.1 = sd(nationData.UnitedStates$Total)
sd2.2 = sd(nationData.China$Total)
sd2.1
sd2.2

se2 = sqrt(
  ((sd2.1^2)/nrow(nationData.UnitedStates)) +
    ((sd2.2)^2)/nrow(nationData.China))
)
se2

z.score2 = diffOfMeans2/se2
z.score2

1 - pnorm(z.score2)
plot(x=seq(from = -5, to= 5, by=0.1),y=dnorm(seq(from = -5, to= 5,  by=0.1),mean=0),type='l',xlab = 'mean difference',  ylab='possibility')
abline(v=z.score2, col='red')

