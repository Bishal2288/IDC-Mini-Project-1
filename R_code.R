# First set working directory to source file
# Import datasets
expen <- read.csv("total-government-expenditure-on-education-gdp.csv")
regime <- read.csv("political-regime.csv")
dat <- merge(expen, regime, by = c("Entity", "Year")) #merge datasets

#Cleaning up
colnames(dat)
colnames(dat)[4] <- "Expenditure"
colnames(dat)[6] <- "Regime"
dat$Entity <- as.factor(dat$Entity)

#Summarising & data-cleaning
summary(dat)
unique(dat$Entity) #There are 167 countries
range(dat$Expenditure)
dat[which(dat$Expenditure>20),] #Zimbabwe has insanely high expenditure
dat[which(dat$Expenditure>10 & dat$Expenditure <20),] #some african countries have more than 10% expenditure

# Plot 1 : Boxplot of expenditure across regimes
par(mgp=c(3,2,0)) #sets location of tick names on x and y axis
with(dat, boxplot(Expenditure ~ Regime, 
                  ylab = "Expenditure on education (% of GDP)",
                  names = c("Closed \n autocracy","Electoral \n autocracy",
                            "Electoral \n democracy","Liberal \n democracy"),
                  las=1))
par(mgp=c(3,1,0)) #reset to default

# Plot 2 : Visualising the entire dataset over time
with(dat, plot(Expenditure ~ Year, col = Entity, pch = Regime, ylim = c(0,15),
               ylab = "Expenditure on education (% of GDP)"))
legend("topleft", pch = c(0,1,2,3), legend=c("Closed autocracy", "Electoral autocracy", "Electoral democracy", "Liberal democracy"), bty="n")


# Plot 3 : Subset of countries from plot 2
with(dat[dat$Entity == "India",], plot(Expenditure ~ Year, col = "pink", 
                                       ylim = c(0,10), xlim = range(dat$Year),
                                       pch = Regime, ylab="Expenditure on education as % of GDP"))
with(dat[dat$Entity == "Brazil",], points(Expenditure ~ Year, col = "red", pch = Regime))
with(dat[dat$Entity == "Russia",], points(Expenditure ~ Year, col = "green", pch = Regime))
with(dat[dat$Entity == "South Africa",], points(Expenditure ~ Year, col = "blue", pch = Regime))
with(dat[dat$Entity == "China",], points(Expenditure ~ Year, col = "black", pch = Regime))
legend("topleft", 
       legend=c("Red = Brazil", "Black = China", "Pink = India", "Green = Russia", "Blue = South Africa"), 
       text.col = c("red", "black", "pink", "green", "blue"), bty="n")
legend("topright", pch = c(0,1,2,3), legend=c("Closed autocracy", "Electoral autocracy", "Electoral democracy", "Liberal democracy"), bty="n")

# Plot 4: Checking relationship between expenditure and regime
plot(Expenditure ~ Regime, data = dat, col = Entity, 
     ylim = c(0,15)) #Not showing countries on plot that have expenditure greater than 15%
summary(lm(Expenditure ~ Regime, data = dat)) #Linear model
abline(coef(lm(Expenditure ~ Regime, data = dat)), lty = 2)
cor.test(x = dat$Regime, y = dat$Expenditure) #positive correlation

write.csv(x = dat, file = "merged.csv")

######## Adding continent-wise annual median values
conti <- read.csv("Grouped Regions_Mini Project 1 - GDP% Expenditure on Education and Regime.csv")
conti$Country <- as.factor(conti$Country)
conti$Continent.Region <- as.factor(conti$Continent.Region)
conti$Year <- as.factor(conti$Year)

# Cleaning up region names
sort(unique(conti$Continent.Region)) #There are two East Africas - "East Africa" and "East Africa " (one has space at end)
which(conti$Continent.Region == "East Africa") #rows with "East Africa"
which(conti$Continent.Region == "East Africa ") #rows with "East Africa " (space)
conti$Continent.Region[which(conti$Continent.Region == "East Africa ")] <- "East Africa" #remove space
conti$Continent.Region <- as.factor(conti$Continent.Region)
sort(unique(conti$Continent.Region)) #There are 20 levels now

# Cleaning up regime names
unique(conti$Regime)
conti$Regime[which(conti$Regime=="Electoral Aurocracy")] <- "Electoral Autocracy"
unique(conti$Regime)

# Going through 20 regions:
#1
test <- as.data.frame(with(conti[conti$Continent.Region=="Central Asia",], 
                           tapply(Expenditure, Year, median)))
test$Year <- as.factor(row.names(test))
colnames(test)[1] <- "region_med"
test3 <- merge(conti[conti$Continent.Region=="Central Asia",], test, by = "Year")
# write.csv(x=test3, file="Central Asia.csv")

#2
test <- as.data.frame(with(conti[conti$Continent.Region=="South America",], 
                           tapply(Expenditure, Year, median)))
test$Year <- as.factor(row.names(test))
colnames(test)[1] <- "region_med"
test2 <- merge(conti[conti$Continent.Region=="South America",], test, by = "Year")
# write.csv(x=test2, file="South America.csv")
test3 <- rbind(test3, test2)

#3
test <- as.data.frame(with(conti[conti$Continent.Region=="West Asia",], 
                           tapply(Expenditure, Year, median)))
test$Year <- as.factor(row.names(test))
colnames(test)[1] <- "region_med"
test2 <- merge(conti[conti$Continent.Region=="West Asia",], test, by = "Year")
# write.csv(x=test2, file="West Asia.csv")
test3 <- rbind(test3, test2)

#4
test <- as.data.frame(with(conti[conti$Continent.Region=="South Europe",], 
                           tapply(Expenditure, Year, median)))
test$Year <- as.factor(row.names(test))
colnames(test)[1] <- "region_med"
test2 <- merge(conti[conti$Continent.Region=="South Europe",], test, by = "Year")
# write.csv(x=test2, file="South Europe.csv")
test3 <- rbind(test3, test2)

#5
test <- as.data.frame(with(conti[conti$Continent.Region=="North America",], 
                           tapply(Expenditure, Year, median)))
test$Year <- as.factor(row.names(test))
colnames(test)[1] <- "region_med"
test2 <- merge(conti[conti$Continent.Region=="North America",], test, by = "Year")
# write.csv(x=test2, file="North America.csv")
test3 <- rbind(test3, test2)

#6
test <- as.data.frame(with(conti[conti$Continent.Region=="Oceania",], 
                           tapply(Expenditure, Year, median)))
test$Year <- as.factor(row.names(test))
colnames(test)[1] <- "region_med"
test2 <- merge(conti[conti$Continent.Region=="Oceania",], test, by = "Year")
# write.csv(x=test2, file="Oceania.csv")
test3 <- rbind(test3, test2)

#7
test <- as.data.frame(with(conti[conti$Continent.Region=="Central America",], 
                           tapply(Expenditure, Year, median)))
test$Year <- as.factor(row.names(test))
colnames(test)[1] <- "region_med"
test2 <- merge(conti[conti$Continent.Region=="Central America",], test, by = "Year")
# write.csv(x=test2, file="Central America.csv")
test3 <- rbind(test3, test2)

#8
test <- as.data.frame(with(conti[conti$Continent.Region=="The Middle East",], 
                           tapply(Expenditure, Year, median)))
test$Year <- as.factor(row.names(test))
colnames(test)[1] <- "region_med"
test2 <- merge(conti[conti$Continent.Region=="The Middle East",], test, by = "Year")
# write.csv(x=test2, file="The Middle East.csv")
test3 <- rbind(test3, test2)

#9
test <- as.data.frame(with(conti[conti$Continent.Region=="South Asia",], 
                           tapply(Expenditure, Year, median)))
test$Year <- as.factor(row.names(test))
colnames(test)[1] <- "region_med"
test2 <- merge(conti[conti$Continent.Region=="South Asia",], test, by = "Year")
# write.csv(x=test2, file="South Asia.csv")
test3 <- rbind(test3, test2)

#10
test <- as.data.frame(with(conti[conti$Continent.Region=="Southeast Asia",], 
                           tapply(Expenditure, Year, median)))
test$Year <- as.factor(row.names(test))
colnames(test)[1] <- "region_med"
test2 <- merge(conti[conti$Continent.Region=="Southeast Asia",], test, by = "Year")
# write.csv(x=test2, file="Southeast Asia.csv")
test3 <- rbind(test3, test2)

#11
test <- as.data.frame(with(conti[conti$Continent.Region=="East Asia",], 
                           tapply(Expenditure, Year, median)))
test$Year <- as.factor(row.names(test))
colnames(test)[1] <- "region_med"
test2 <- merge(conti[conti$Continent.Region=="East Asia",], test, by = "Year")
# write.csv(x=test2, file="East Asia.csv")
test3 <- rbind(test3, test2)

#12
test <- as.data.frame(with(conti[conti$Continent.Region=="North Africa",], 
                           tapply(Expenditure, Year, median)))
test$Year <- as.factor(row.names(test))
colnames(test)[1] <- "region_med"
test2 <- merge(conti[conti$Continent.Region=="North Africa",], test, by = "Year")
# write.csv(x=test2, file="North Africa.csv")
test3 <- rbind(test3, test2)

#13
test <- as.data.frame(with(conti[conti$Continent.Region=="Southern Africa",], 
                           tapply(Expenditure, Year, median)))
test$Year <- as.factor(row.names(test))
colnames(test)[1] <- "region_med"
test2 <- merge(conti[conti$Continent.Region=="Southern Africa",], test, by = "Year")
# write.csv(x=test2, file="Southern Africa.csv")
test3 <- rbind(test3, test2)

#14
test <- as.data.frame(with(conti[conti$Continent.Region=="West Africa",], 
                           tapply(Expenditure, Year, median)))
test$Year <- as.factor(row.names(test))
colnames(test)[1] <- "region_med"
test2 <- merge(conti[conti$Continent.Region=="West Africa",], test, by = "Year")
# write.csv(x=test2, file="West Africa.csv")
test3 <- rbind(test3, test2)

#15
test <- as.data.frame(with(conti[conti$Continent.Region=="Central Africa",], 
                           tapply(Expenditure, Year, median)))
test$Year <- as.factor(row.names(test))
colnames(test)[1] <- "region_med"
test2 <- merge(conti[conti$Continent.Region=="Central Africa",], test, by = "Year")
# write.csv(x=test2, file="Central Africa.csv")
test3 <- rbind(test3, test2)

#16
test <- as.data.frame(with(conti[conti$Continent.Region=="East Africa",], 
                           tapply(Expenditure, Year, median)))
test$Year <- as.factor(row.names(test))
colnames(test)[1] <- "region_med"
test2 <- merge(conti[conti$Continent.Region=="East Africa",], test, by = "Year")
#write.csv(x=test2, file="East Africa.csv")
test3 <- rbind(test3, test2)

#17
test <- as.data.frame(with(conti[conti$Continent.Region=="Central Europe",], 
                           tapply(Expenditure, Year, median)))
test$Year <- as.factor(row.names(test))
colnames(test)[1] <- "region_med"
test2 <- merge(conti[conti$Continent.Region=="Central Europe",], test, by = "Year")
#write.csv(x=test2, file="Central Europe.csv")
test3 <- rbind(test3, test2)

#18
test <- as.data.frame(with(conti[conti$Continent.Region=="East Europe",], 
                           tapply(Expenditure, Year, median)))
test$Year <- as.factor(row.names(test))
colnames(test)[1] <- "region_med"
test2 <- merge(conti[conti$Continent.Region=="East Europe",], test, by = "Year")
#write.csv(x=test2, file="East Europe.csv")
test3 <- rbind(test3, test2)

#19
test <- as.data.frame(with(conti[conti$Continent.Region=="West Europe",], 
                           tapply(Expenditure, Year, median)))
test$Year <- as.factor(row.names(test))
colnames(test)[1] <- "region_med"
test2 <- merge(conti[conti$Continent.Region=="West Europe",], test, by = "Year")
#write.csv(x=test2, file="West Europe.csv")
test3 <- rbind(test3, test2)

#20
test <- as.data.frame(with(conti[conti$Continent.Region=="North Europe",], 
                           tapply(Expenditure, Year, median)))
test$Year <- as.factor(row.names(test))
colnames(test)[1] <- "region_med"
test2 <- merge(conti[conti$Continent.Region=="North Europe",], test, by = "Year")
#write.csv(x=test2, file="North Europe.csv")
test3 <- rbind(test3, test2)


#####Calculating medians by political regimes for each year
#1 Closed Autocracy
blah <- as.data.frame(with(test3[test3$Regime=="Closed Autocracy",], 
                           tapply(Expenditure, Year, median)))
blah$Year <- as.factor(row.names(blah))
colnames(blah)[1] <- "regime_med"
blah3 <- merge(test3[test3$Regime=="Closed Autocracy",], blah, by = "Year")


#2 Electoral Autocracy
blah <- as.data.frame(with(test3[test3$Regime=="Electoral Autocracy",], 
                           tapply(Expenditure, Year, median)))
blah$Year <- as.factor(row.names(blah))
colnames(blah)[1] <- "regime_med"
blah2 <- merge(test3[test3$Regime=="Electoral Autocracy",], blah, by = "Year")
blah3 <- rbind(blah3, blah2)

#3 Liberal Democracy
blah <- as.data.frame(with(test3[test3$Regime=="Liberal Democracy",], 
                           tapply(Expenditure, Year, median)))
blah$Year <- as.factor(row.names(blah))
colnames(blah)[1] <- "regime_med"
blah2 <- merge(test3[test3$Regime=="Liberal Democracy",], blah, by = "Year")
blah3 <- rbind(blah3, blah2)

#4 Electoral Democracy
blah <- as.data.frame(with(test3[test3$Regime=="Electoral Democracy",], 
                           tapply(Expenditure, Year, median)))
blah$Year <- as.factor(row.names(blah))
colnames(blah)[1] <- "regime_med"
blah2 <- merge(test3[test3$Regime=="Electoral Democracy",], blah, by = "Year")
blah3 <- rbind(blah3, blah2)

write.csv(x=blah3, file="Region_regime_Medians.csv")
