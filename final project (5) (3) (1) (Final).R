library(tidyverse)
library(car)
library(AER)
library(interplot) 
library(broom)
library(stargazer)
library(rsq) 
library(readxl)
library(ggthemes)
#Data clean up
#convert Salary and Guaranteed into integer and delete the dolar sign
#Remove observations where salary is 0, change TS% to a number instead of a decimal <1 
players <- read_csv("2017-18_AdvancedStats_Salary.csv")
players$Salary <- gsub("[,$]","", players$Salary)
players$Salary <- as.numeric(players$Salary)
players$Guaranteed <- gsub("[,$]", "", players$Guaranteed)
players$Guaranteed <- as.numeric(players$Guaranteed)
players$`TS%` <- (players$`TS%` * 100)
players1 <- players[players$NBA_Country == "USA" & players$Salary != 0 & players$`TS%` <= 100,]
summary(players1)

#Replacing missing values in the TS% variable with the mean of TS% and with AST% and DRB% and age
players1$`TS%` <- ifelse(is.na(players1$`TS%`), mean(players1$`TS%`, na.rm = TRUE), players1$`TS%`)
players1$`AST%` <- ifelse(is.na(players1$`AST%`), mean(players1$`AST%`, na.rm = TRUE), players1$`AST%`)
players1$`DRB%` <- ifelse(is.na(players1$`DRB%`), mean(players1$`DRB%`, na.rm = TRUE), players1$`DRB%`)
players1$Age <- ifelse(is.na(players1$Age), mean(players1$Age, na.rm = TRUE), players1$Age)
#mean centering variables

#A summary of average salary grouped by countries with a ggplot showing the comparison.
avgSalary <- players %>%
  group_by(NBA_Country) %>%
  summarize(avg_salary = mean(Salary)) %>%
  arrange(desc(avg_salary))
avgSalary$NBA_Country <- reorder(avgSalary$NBA_Country, avgSalary$avg_salary, mean)

ggplot(data = avgSalary, aes(x = NBA_Country, y = avg_salary)) + 
  geom_bar(stat = "identity") + coord_flip() + theme_bw() + 
  labs(x = "NBA Countries", y = "Average Salary")

#A summary of summed salary grouped by countries with a ggplot showing the comparison. 
sumSalary <- players %>%
  group_by(NBA_Country) %>%
  summarize(sum_salary = sum(Salary))
salary <- left_join(avgSalary, sumSalary, by = "NBA_Country")
sumSalary$NBA_Country <- reorder(sumSalary$NBA_Country, sumSalary$sum_salary, mean)

ggplot(data = sumSalary, aes(x = NBA_Country, y = sum_salary)) + 
  geom_bar(stat = "identity") + coord_flip() + theme_bw() + 
  labs(x = "NBA Countries", y = "Average Salary")

#Summary statistics of the salary variable and a ggplot showing the distribution of salary
players3 <- players1
players3$Salary <- (players3$Salary/1000)
ggplot(players3, aes(x=Salary)) + geom_histogram(bins = 10) + theme_bw() + ggtitle("Distribution of Player Salary") +
  labs(x = "Salary in Thousands of Dollars", y = "Count")

summary(players1$Salary)


##ggplots comparing salary to guaranteed salary
ggplot(data = players1, aes(x = Age, y = Salary)) + 
  geom_point() + geom_smooth(se = FALSE) + theme_bw()
ggplot(data = players1, aes(x = Salary, y = Guaranteed)) + 
  geom_point(na.rm = T) + geom_smooth(se = FALSE, na.rm = T) + theme_bw()

##Linear regression models 1-12 using one variable to check initial 
##siginificance on the response variable and checking residuals
m1 <- lm(log(Salary) ~ scale(Age), data = players1)
summary(m1)
residualPlot(m1)


m2_1 <- lm(log(Salary) ~ `USG%` + MP, data = players1)
summary(m2_1)
residualPlots(m2_1)

m2_2 <- lm(log(Salary) ~ G , data = players1)
summary(m2_2)
residualPlot(m2_2)

m3 <- lm(log(Salary) ~ `TS%`, data = players1)
summary(m3)
residualPlot(m3)

m4 <- lm(log(Salary) ~ `AST%`, data = players1)
summary(m4)
residualPlot(m4)

m5 <- lm(log(Salary) ~ `STL%`, data = players1)
summary(m5)
residualPlot(m5)

m6 <- lm(log(Salary) ~ `ORB%`, data = players1)
summary(m6)
residualPlot(m6)

m7 <- lm(log(Salary) ~ `DRB%`, data = players1)
summary(m7)
residualPlot(m7)

m8 <- lm(log(Salary) ~ PER, data = players1)
summary(m8)
residualPlot(m8)

m9 <- lm(log(Salary) ~ VORP, data = players1)
summary(m9)
residualPlot(m9)

m10 <- lm(log(Salary) ~ WS, data = players1)
summary(m10)
residualPlot(m10)

m11 <- lm(log(Salary) ~ NBA_DraftNumber, data = players1)
summary(m11)
residualPlot(m11)
ggplot(data = players1, aes(x = NBA_DraftNumber, y = Salary)) + 
  geom_point() + geom_smooth(se = FALSE) + theme_bw() + labs(title = "Draft Number vs Salary", subtitle = "How salary distributes along draft number")

m12 <- lm(log(Salary) ~ Guaranteed, data = players1)
summary(m12)
residualPlot(m12)

#Preidcting additional effect of position on salary using true shooting percentage
#and offensive rebound percentage.
m13 <- lm(log(Salary) ~ `TS%` + Pos + Pos*`TS%`, data = players1)
summary(m13)
residualPlot(m13)

ggplot(data = players3, mapping = aes(x = `TS%`, y = Salary)) + 
  geom_point() + geom_smooth(se = F) + theme_bw() + facet_wrap(~Pos) + 
  labs(title = "How Shooting Efficiency Affects Salary", subtitle = "Salary distribution against true shooting % categorized by position")

##Linear regression model on Salary using significant personal player info variables
##with leverages and cook's distance check and regression visualizations.
m17 <- lm(log(Salary) ~ scale(Age) + Pos + MP + scale(NBA_DraftNumber), data = players1)
summary(m17)
residualPlot(m17)
qqPlot(m17)
stargazer(m17, type = "text")

infIndexPlot(m17, id = T, vars=c("Cook",  "hat"))

mean(players1$`AST%`)
mean(players1$`TS%`)
mean(players1$`DRB%`)
##Linear regression model on salary using in game player metrics
##with diagnostic plots and visuals for the model
m18 <- lm(log(Salary) ~ scale(`TS%`) + scale(`AST%`) + scale(`DRB%`), data = players1)
summary(m18)
residualPlot(m18)
qqPlot(m18)
vif(m18)
m18.tidy <- tidy(m18, conf.int = T)
m18.tidy$term <- c("Intercept","Age", "Position PF", "Position PG", "Position SF", "Position SG", "Minutes played", "Draft #")
m18.plot <- ggplot(m18.tidy, aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0)
print(m18.plot)
stargazer(m18, type = "text")

var <- c("TS%", "AST%", "DRB%", "Salary")
players18 <- players1[var]
ggpairs(players18)

#Check for unusual observations with hat values and cooks distance. 
infIndexPlot(m18, id = T, vars=c("Cook",  "hat"))

