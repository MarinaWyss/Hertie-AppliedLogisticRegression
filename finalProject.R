library(tidyverse)
library(ggthemes)
library(Amelia)
library(mice)
library(caret)

# loading data and renaming
data <- read.csv("September 29 - November 6, 2016 - Information engaged wary - CSV.csv")

data <- data %>%
  select(q6a, live1, eminuse, snsint2, sex, age, marital,
         par, educ2, emplnw, race3m1, inc, party,
         cregion, weight, standwt) %>%
  rename(
    trustNews = q6a, 
    urbanRural = live1, 
    internetUser = eminuse,
    socialUser = snsint2,
    married = marital,
    parent = par,
    education = educ2,
    employmentStatus = emplnw,
    race = race3m1,
    income = inc,
    region = cregion
  )

# dropping NAs on dependent variable
data <- data %>%
  filter(trustNews != 8 & trustNews != 9)

# impute missings
data <- data %>% 
  mutate(education = ifelse(education == 8, 7, education),
         income = ifelse(income == 8, 7, income),
         employmentStatus = ifelse(employmentStatus == 8, 7, employmentStatus))

data <- data %>% 
  mutate_each(funs(replace(., . == 8, NA))) %>% 
  mutate_each(funs(replace(., . == 9, NA))) %>% 
  mutate_each(funs(replace(., . == 98, NA))) %>% 
  mutate_each(funs(replace(., . == 99, NA)))

dataBinary <- data %>%
  mutate(trustNews = as_factor(case_when(trustNews %in% c(1, 2) ~ "1-trustNews", 
                                         trustNews %in% c(3, 4) ~ "0-noTrustNews")),
         urbanRural = as_factor(case_when(urbanRural %in% c(1, 2) ~ "1-urban", 
                                          urbanRural %in% c(3, 4) ~ "0-rural")),
         internetUser = as_factor(case_when(internetUser == 1 ~ "1-yes", 
                                            internetUser == 2 ~ "0-no")),
         socialUser = as_factor(case_when(socialUser == 1 ~ "1-yes", 
                                          socialUser == 2 ~ "0-no")),
         sex = as_factor(case_when(sex == 1 ~ "1-male",
                                   sex == 2 ~ "0-female")),
         married = as_factor(case_when(married == 1 ~ "1-married", 
                                       married %in% c(2, 3, 4, 5, 6) ~ "0-notMarried")),
         parent = as_factor(case_when(parent == 1 ~ "1-parent", 
                                      parent == 2 ~ "0-notParent"))) %>%
  select(trustNews, urbanRural, internetUser, socialUser, sex, married, parent)

dataOrdinal <- data %>%
  mutate(age = as_factor(case_when(age < 30 ~ "1-young",
                                   age >= 30 & age < 50 ~ "2-middle",
                                   age >= 50 & age < 70 ~ "3-old",
                                   age >= 70 & age < 98 ~ "4-retired")),
         education = as_factor(case_when(education %in% c(1, 2, 3) ~ "1-highSchool",
                                         education %in% c(4, 5, 6) ~ "2-college",
                                         education == 7 ~ "3-advanced")),
         income = as_factor(case_when(income %in% c(1, 2, 3) ~ "1-low",
                                      income %in% c(4, 5, 6) ~ "2-middle",
                                      income %in% c(7) ~ "3-high",
                                      income == 9 ~ "4-veryHigh"))) %>%
  select(age, education, income)

dataCategorical <- data %>%
  mutate(employmentStatus = as_factor(case_when(employmentStatus %in% c(1, 5) ~ "2-fullTime",
                                                employmentStatus == 2 ~ "1-partTime",
                                                employmentStatus %in% c(3, 4, 6, 7) ~ "0-notEmployed")),
         race = as_factor(case_when(race == 1 ~ "1-white",
                                    race == 2 ~ "2-black",
                                    race == 3 ~ "4-asian",
                                    race == 5 ~ "5-nativeAmerican",
                                    race == 6 ~ "6-pacificIslander",
                                    race == 7 ~ "3-hispanic")),
         party = as_factor(case_when(party == 1 ~ "1-republican",
                                     party == 2 ~ "3-democrat",
                                     party == 3 ~ "2-independent",
                                     TRUE ~ "4-other")),
         region = as_factor(case_when(region == 1 ~ "1-northEast",
                                      region == 2 ~ "2-midWest",
                                      region == 3 ~ "3-south",
                                      region == 4 ~ "4-west"))) %>% 
  select(employmentStatus, race, party, region)

set.seed(123)
require(mice)
imputedBinary <- mice(dataBinary, method = "logreg")
dataBinary <- complete(imputedBinary)

imputedOrdinal <- mice(dataOrdinal, method = "polr")
dataOrdinal <- complete(imputedOrdinal)

imputedCategorical <- mice(dataCategorical, method = "polyreg")
dataCategorical <- complete(imputedCategorical)

data <- cbind(dataBinary, dataOrdinal, dataCategorical)
rm(dataBinary, dataOrdinal, dataCategorical, 
   imputedBinary, imputedOrdinal, imputedCategorical)

# feature selection
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
results <- rfe(data[ ,2:14], data[ ,1], sizes = c(1:14), rfeControl = control)
print(results)
predictors(results)
plot(results, type = c("g", "o"))



# basic EDA
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

missmap(data)

