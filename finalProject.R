library(tidyverse)
library(ggthemes)

data <- read.csv("September 29 - November 6, 2016 - Information engaged wary - CSV.csv")

data <- data %>%
  select(q6a, live1, eminuse, snsint2, q3e, oftvote, sex, age, marital,
         par, educ2, emplnw, race3m1, inc, party,
        ideo, cregion, density, weight, standwt) %>%
  rename(
    trustNews = q6a, 
    urbanRural = live1, 
    internetUser = eminuse,
    socialUser = snsint2,
    stubborn = q3e,
    oftVote = oftvote,
    parent = par,
    education = educ2,
    employmentStatus = emplnw,
    race = race3m1,
    income = inc,
    region = cregion,
    popDensity = density
  )

data <- subset(data, trustNews != 8 & trustNews != 9)

# making dependent variable binary
data <- data %>%
  mutate(trustNews = case_when(trustNews %in% c(1, 2) ~ 1, 
                                trustNews %in% c(3, 4) ~ 0))

# factors
data <- data %>%
  mutate_at(vars(trustNews,
                 urbanRural,
                 internetUser,
                 socialUser,
                 stubborn,
                 oftVote,
                 sex,
                 marital,
                 parent,
                 education,
                 employmentStatus,
                 race,
                 party,
                 ideo,
                 region),
            funs(factor))

# EDA
plotData <- data[complete.cases(data), ]

plotData1 <- subset(data, party == 1 | party == 2 | party == 3)
plt1 <- ggplot(plotData1, aes(x = trustNews, fill = party)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), 
           color = "black") +
  labs(x = "Trust in the National News",
       y = "Proportion",
       fill = "Party ID") +
  scale_fill_manual(values = c("#5DBCD2", "#3F285D", "#914b9c"),
                    labels = c("Republican", "Democrat", "Independent")) +
  theme_igray()
plt1

plotData2 <- subset(data, socialUser != 8 & socialUser != 9)
plt2 <- ggplot(plotData2, aes(x = trustNews, fill = socialUser)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), 
           color = "black") +
  labs(x = "Trust in the National News",
       y = "Proportion",
       fill = "Social Media User") +
  scale_fill_manual(values = c("#5DBCD2", "#914b9c"),
                    labels = c("Yes", "No")) +
  theme_igray()
plt2

plotData3 <- subset(data, stubborn != 8 & stubborn != 9)
plt3 <- ggplot(plotData3, aes(x = trustNews, fill = stubborn)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), 
           color = "black") +
  labs(x = "Trust in the National News",
       y = "Proportion",
       fill = "Stubborn") +
  scale_fill_manual(values = c("#5DBCD2", "#914b9c"),
                    labels = c("Yes", "No")) +
  theme_igray()
plt3

