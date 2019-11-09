library(tidyverse)
library(ggthemes)
library(mice)
library(caret)
library(stargazer)
library(kableExtra)
library(lmtest)
library(MASS)

# loading data and renaming
data <- read.csv("Data/September 29 - November 6, 2016 - Information engaged wary - CSV.csv")

data <- data %>%
  dplyr::select(q6a, live1, eminuse, snsint2, sex, age, marital,
         par, educ2, emplnw, race3m1, inc, party,
         cregion, hh1, hh3) %>%
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
    region = cregion,
    liveInHouse = hh1, 
    adultsInHouse = hh3
  )

# dropping NAs on dependent variable
data <- data %>%
  filter(trustNews != 8 & trustNews != 9)

# data prep
data <- data %>% 
  mutate(education = ifelse(education == 8, 7, education),
         income = ifelse(income == 8, 7, income),
         employmentStatus = ifelse(employmentStatus == 8, 7, employmentStatus))

data <- data %>% 
  mutate_each(funs(replace(., . == 8, NA))) %>% 
  mutate_each(funs(replace(., . == 9, NA))) %>% 
  mutate_each(funs(replace(., . == 98, NA))) %>% 
  mutate_each(funs(replace(., . == 99, NA)))

# adjusting household income for number of people
data <- data %>% 
  mutate(adjustedIncome = income / (1 +
                                      (0.5 *(adultsInHouse - 1)) + 
                                      (0.3 * (liveInHouse - adultsInHouse)) )
  ) %>% 
  mutate(adjustedIncome = ifelse(liveInHouse > 1, adjustedIncome, income))

# make categorical and impute missings
dataBinary <- data %>%
  mutate(trustNews = factor(case_when(trustNews %in% c(1, 2) ~ "1-trustNews", 
                                      trustNews %in% c(3, 4) ~ "0-dontTrustNews"), 
                            levels = c("0-dontTrustNews", "1-trustNews")),
         urbanRural = factor(case_when(urbanRural %in% c(1, 2) ~ "1-urban", 
                                       urbanRural %in% c(3, 4) ~ "0-rural"),
                             levels = c("0-rural", "1-urban")),
         internetUser = factor(case_when(internetUser == 1 ~ "1-yes", 
                                         internetUser == 2 ~ "0-no"),
                               levels = c("0-no", "1-yes")),
         socialUser = factor(case_when(socialUser == 1 ~ "1-yes", 
                                       socialUser == 2 ~ "0-no"),
                             levels = c("0-no", "1-yes")),
         sex = factor(case_when(sex == 1 ~ "1-male",
                                sex == 2 ~ "0-female"),
                         levels = c("0-female", "1-male")),
         married = factor(case_when(married == 1 ~ "1-married", 
                                    married %in% c(2, 3, 4, 5, 6) ~ "0-notMarried"),
                             levels = c("0-notMarried", "1-married")),
         parent = factor(case_when(parent == 1 ~ "1-parent", 
                                   parent == 2 ~ "0-notParent"),
                         levels = c("0-notParent", "1-parent"))) %>%
  dplyr::select(trustNews, urbanRural, internetUser, socialUser, sex, married, parent)

dataOrdinal <- data %>%
  mutate(age = factor(case_when(age < 30 ~ "1-young",
                                age >= 30 & age < 50 ~ "2-middle",
                                age >= 50 & age < 70 ~ "3-old",
                                age >= 70 & age < 98 ~ "4-retired"),
                       levels = c("1-young", "2-middle", "3-old", "4-retired")),
         education = factor(case_when(education %in% c(1, 2, 3) ~ "1-highSchool",
                                      education %in% c(4, 5, 6) ~ "2-college",
                                      education == 7 ~ "3-advanced"),
                            levels = c("1-highSchool", "2-college", "3-advanced")),
         adjustedIncome = factor(case_when(adjustedIncome <= 3 ~ "1-low",
                                   adjustedIncome > 3 & adjustedIncome <= 6 ~ "2-middle",
                                   adjustedIncome > 6 & adjustedIncome <= 9 ~ "3-high"),
                         levels = c("1-low", "2-middle", "3-high"))) %>%
  dplyr::select(age, education, adjustedIncome)

dataCategorical <- data %>%
  mutate(employmentStatus = factor(case_when(employmentStatus %in% c(1, 5) ~ "3-fullTime",
                                             employmentStatus == 2 ~ "2-partTime",
                                             employmentStatus %in% c(3, 4, 6, 7) ~ "1-notEmployed"), 
                                   levels = c("1-notEmployed", "2-partTime", "3-fullTime")),
         race = factor(case_when(race == 1 ~ "1-white",
                                 race == 2 ~ "3-black",
                                 race == 3 ~ "4-asian",
                                 race %in% c(5, 6) ~ "5-other",
                                 race == 7 ~ "2-hispanic"), 
                       levels = c("1-white", "2-hispanic", "3-black", "4-asian", "5-other")),
         party = factor(case_when(party == 1 ~ "3-republican",
                                  party == 2 ~ "1-democrat",
                                  party == 3 ~ "2-independent",
                                  TRUE ~ "4-other"),
                        levels = c("1-democrat", "2-independent", "3-republican", "4-other")),
         region = factor(case_when(region == 1 ~ "1-northEast",
                                   region == 2 ~ "2-midWest",
                                   region == 3 ~ "3-south",
                                   region == 4 ~ "4-west"))) %>% 
  dplyr::select(employmentStatus, race, party, region)

## imputation using MICE by variable type
set.seed(123)
require(mice)
imputedBinary <- mice(dataBinary, method = "logreg")
dataBinary <- complete(imputedBinary)

imputedOrdinal <- mice(dataOrdinal, method = "polr")
dataOrdinal <- complete(imputedOrdinal)

imputedCategorical <- mice(dataCategorical, method = "polyreg")
dataCategorical <- complete(imputedCategorical)

## rejoining data and removing intermediate steps
data <- cbind(dataBinary, dataOrdinal, dataCategorical)
rm(dataBinary, dataOrdinal, dataCategorical, 
   imputedBinary, imputedOrdinal, imputedCategorical)

# cross tables
partyCross <- table(data$trustNews, data$party) %>% 
  prop.table(2) %>% 
  round(2)
partyCross <- data.frame(partyCross)
partyCross <- reshape(partyCross, idvar = "Var1", timevar = "Var2", direction = "wide")

socialCross <- table(data$trustNews, data$socialUser) %>% 
  prop.table(2) %>% 
  round(2)
socialCross <- data.frame(socialCross)
socialCross <- reshape(socialCross, idvar = "Var1", timevar = "Var2", direction = "wide")

educationCross <- table(data$trustNews, data$education) %>% 
  prop.table(2) %>% 
  round(2)
educationCross <- data.frame(educationCross)
educationCross <- reshape(educationCross, idvar = "Var1", timevar = "Var2", direction = "wide")

ageCross <- table(data$trustNews, data$age) %>% 
  prop.table(2) %>% 
  round(2)
ageCross <- data.frame(ageCross)
ageCross <- reshape(ageCross, idvar = "Var1", timevar = "Var2", direction = "wide")

allCross <- left_join(partyCross, socialCross, by = "Var1")
allCross <- left_join(allCross, educationCross, by = "Var1")
allCross <- left_join(allCross, ageCross, by = "Var1")


names(allCross) <- c("", "Democrat", "Independent", "Republican", "Other Party",
                     "No", "Yes",
                     "High School", "College", "Advanced", 
                     "Young", "Middle-Aged", "Old", "Retired")

allCrossTable <- allCross %>% 
  kable() %>%  
  kable_styling() %>% 
  column_spec(1, bold = T, border_right = T) %>%
  column_spec(5, border_right = T) %>% 
  column_spec(7, border_right = T) %>% 
  column_spec(10, border_right = T) %>% 
  add_header_above(c("", "Party Affiliation" = 4, "Social Media User" = 2, 
                     "Highest Level of Education" = 3, "Age" = 4)) %>% 
  add_header_above(c("", "Table 1: Cross-Tabs of Trust in News - Political Preference, Social Media Use, and Education (Column Percent)" = 13))


# feature selection
## chi squared
x2 <- data.frame(lapply(data[,-1], 
                        function(x) chisq.test(table(x, data$trustNews), 
                                               simulate.p.value = TRUE)$p.value))
x2Table <- x2 %>% 
  type_convert %>% 
  mutate_if(is.numeric, round, digits = 4) %>% 
  t %>% 
  as.data.frame() %>% 
  mutate(Variable = names(x2)) %>% 
  mutate(Significance = case_when(V1 <= 0.1 & V1 > 0.05 ~ "*", 
                                  V1 <= 0.05 & V1 > 0.01 ~ "**", 
                                  V1 <= 0.01 ~ "***",
                                  TRUE ~ " ")) %>% 
  rename(ChiSquaredOutput = V1) %>% 
  dplyr::select(Variable, ChiSquaredOutput, Significance) %>% 
  kable() %>% 
  kable_styling() %>% 
  footnote(general = "* = 0.1, ** = 0.05, *** = 0.01") %>% 
  add_header_above(c("Table 2: Chi-Squared Results for Trust in News Explanatory Variables" = 3))

## rfe
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
results <- rfe(data[ ,2:14], data[ ,1], sizes = c(1:14), rfeControl = control)
print(results)
predictors(results)
plot(results, type = c("g", "o"))

## stepwise
stepModelFull <- glm(trustNews ~., 
              family = binomial,
              data = data)

stepModelFull <- stepModelFull %>% stepAIC(trace = FALSE)

# models
model1 <- glm(trustNews ~
                age + sex + socialUser + adjustedIncome + party, 
              family = binomial,
              data = data)

model2 <- glm(trustNews ~
                age + sex + race + socialUser + internetUser + party +
                urbanRural + adjustedIncome + education + employmentStatus, 
              family = binomial,
              data = data)

model3 <- glm(trustNews ~ age:socialUser + sex + race + internetUser + 
                party + urbanRural + adjustedIncome + education + employmentStatus, 
              family = binomial,
              data = data)

# stargazer
model1or <- exp(coef(model1))
model1conf <- exp(confint(model1))
model1p <- list(summary(model1)$coefficients[ ,4])

model2or <- exp(coef(model2))
model2conf <- exp(confint(model2))
model2p <- list(summary(model2)$coefficients[ ,4])

model3or <- exp(coef(model3))
model3conf <- exp(confint(model3))
model3p <- list(summary(model3)$coefficients[ ,4]) 

stargazer(model1, model2, model3, 
          coef = list(model1or, model2or, model3or),
          ci = T,
          ci.custom = list(model1conf, model2conf, model3conf),
          p = c(model1p, model2p, model3p),
          header = FALSE,
          title = "Table 3: Odds-ratio Models of Trust in News", 
          type = "text")

# interaction effects plots
model3coef <- as.data.frame(summary(model3)$coefficients) 
plotData <- rownames_to_column(model3coef)[18:24, 1:2]

plotOR <- as.data.frame(model3or)
plotOR <- rownames_to_column(plotOR)[18:24, 1:2]

plotCI <- as.data.frame(model3conf)
plotCI <- rownames_to_column(plotCI)[18:24, 1:3]

plotData <- left_join(plotData, plotOR, by = "rowname")
plotData <- left_join(plotData, plotCI, by = "rowname")
plotData[nrow(plotData) + 1, ] <- 1

plotData$rowname <- c("young", "middleAge", "old", "retired",
                      "young:socialUser", "middleAge:socialUser", "oldAge:socialUser", 
                      "retired:socialUser")

palette <- c("#999999", "#999999", "#f6e8c3", "#f6e8c3", "#c7eae5",
             "#c7eae5", "#018571", "#018571")

plotInteraction <- ggplot(plotData, aes(x = reorder(rowname, model3or), y = model3or)) + 
  geom_bar(stat = "identity", aes(fill = rowname), color = "black") +
  geom_text(aes(label = round(model3or, 2)), vjust = -0.5) +
  theme(legend.position = "none",
        text = element_text(family = "Times")) +
  scale_fill_manual(values = palette) + 
  labs(title = "Fig. 1: Odds ratios - Interaction of Age and Social Media Use on Trust in the News",
       x = "Age and Social Media Use",
       y = "Odds Ratios")

plotInteraction
