# Insurance dataset

insurance_data <- read.csv("insurance.csv")
str(insurance_data)

# several variables need to be converted
# sex - male = 0, female = 1
# smoker - yes = 1, no = 0
# Region - 4 Regions
# N = 4, so we need n-1 indicator variables
summary(factor(insurance_data$region))

# comvert the sex variable 
insurance_data$sex <- factor(insurance_data$sex, 
                             levels = c("male", "female"), 
                             ordered = FALSE)

insurance_data$smoker <- factor(insurance_data$smoker, 
                                levels = c("yes", "no"), 
                                ordered = FALSE)

insurance_data$region <- factor(insurance_data$region, 
                                levels = c("northeast", 
                                           "northwest", 
                                           "southeast", 
                                           "southwest"), 
                                ordered = FALSE)

str(insurance_data)

library(psych)

# Examine correlation between the variable
pairs.panels(insurance_data,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

# If build the model at this point, R
# will automatically split the factor variables
# Alternatively want to control the model build process

# in linear regresison, mode lis:
# y = b0 + B1x1 + B2x2 + B3x3 + e
# where y = insurance charges
# x1 = age of the person
# x2  sex of the person
# x3 = bmi of the person
# x4 = children
# x5 = smoker
# x6 = region
# It is clear that x1, and x3 are continuous and x2, x4, x5, x6 are categorical
# therefore we need to create dummy variables for the categorical
# variables
# For the sex variable x2
# x2 = 1 if person is male
# x2 = 0 if person is female

set.seed(1)
model <- lm(formula = charges ~ age + sex + bmi + children + smoker + region, 
            data = insurance_data)

summary(model)

# obviously age, bmi, children, smokerno have an 
# influence over the dependent variable "charges"
# p-value less than the sig value
# sexfemale is not influential on the model
# keep region for now as need to use it
# for the research question
insurance_data <- insurance_data[c(1, 3:7)]

# Round the BMI and charges values
insurance_data$bmi <- round(insurance_data$bmi, 1)
insurance_data$charges <- round(insurance_data$charges, 2)

# Create the model again with the amended changes
model <- lm(formula = charges ~ age + bmi + children + smoker + region, 
            data = insurance_data)

summary(model)

# now check the model assumption
# Linearity
# examine whether a linear
# correlation exists between continuous variable
scatter.smooth(insurance_data$age, 
               insurance_data$charges, 
               main = "Insurance charges ~ age", 
               ylab = "Insurance charges (,000)", 
               xlab = "Age (years)")

# BMI
scatter.smooth(insurance_data$bmi, 
               insurance_data$charges, 
               main = "Insurance charges ~ bmi", 
               ylab = "Insurance charges (,000)", 
               xlab = "BMI")

scatter.smooth(insurance_data$children, 
               insurance_data$charges, 
               main = "Insurance charges ~ children", 
               ylab = "Insurance charges (,000)", 
               xlab = "Children")

scatter.smooth(insurance_data$smoker, 
               insurance_data$charges, 
               main = "Insurance charges ~ smoker", 
               ylab = "Insurance charges (,000)", 
               xlab = "Smoker")

scatter.smooth(insurance_data$region, 
               insurance_data$charges, 
               main = "Insurance charges ~ region", 
               ylab = "Insurance charges (,000)", 
               xlab = "Region")

# No info available for plot of continuous versus categorical
# Instead examine relationship with dependent
# and categorical data using a bar plot
scatter.smooth(insurance_data$smoker, 
               insurance_data$charges, 
               main = "Insurance charges ~ smoker", 
               ylab = "Insurance charges (,000)", 
               xlab = "Smoker")

plot(insurance_data$smoker, 
     insurance_data$charges, 
     main = "Charges by smoker status", 
     xlab = "Smoker", 
     ylab = "Insurance charges")

plot(insurance_data$region, 
     insurance_data$charges, 
     main = "Charges by region", 
     xlab = "Region", 
     ylab = "Insurance charges")

# Only use this method if
# sure about the underlying nature
# of the variables that are correlating
insurance_data$cor_smoker <- ifelse(insurance_data$smoker == "yes", 1, 0)

cor(insurance_data$charges, insurance_data$cor_smoker)

# Normality
with(insurance_data, {qqnorm(age, 
         main = "Normality analytics of age data") 
  qqline(age)
  })

# Repeat for other continuous data
with(insurance_data, {qqnorm(charges[smoker == "yes"], 
                             main = "Normality analytics of smoker = 'yes' data") 
  qqline(charges[smoker == "yes"])
})

with(insurance_data, {qqnorm(charges[smoker == "no"], 
                             main = "Normality analytics of smoker = 'no' data") 
  qqline(charges[smoker == "no"])
})

with(insurance_data, {qqnorm(charges[region == "southwest"], 
                             main = "Normality analytics of regional southwest data") 
  qqline(charges[region == "southwest"])
})

with(insurance_data, {qqnorm(charges[region == "southeast"],
                             main = "Normality analytics of regional southeast data") 
  qqline(charges[region == "southeast"])
})

with(insurance_data, {qqnorm(charges[region == "northwest"],
                             main = "Normality analytics of regional northwest data") 
  qqline(charges[region == "northwest"])
})

with(insurance_data, {qqnorm(charges[region == "northeast"],
                             main = "Normality analytics of regional northeast data") 
  qqline(charges[region == "northeast"])
})

qqnorm(insurance_data$age)
qqline(insurance_data$age, col = "red")

# 
shapiro.test(insurance_data$age)$p.value
shapiro.test(insurance_data$bmi)$p.value
shapiro.test(insurance_data$children)$p.value
shapiro.test(insurance_data$charges)$p.value

region_normality_test <- with(insurance_data, tapply(charges, region, shapiro.test))
region_normality_test$northeast$p.value
region_normality_test$northwest$p.value
region_normality_test$southeast$p.value
region_normality_test$southwest$p.value

smoker_normality_test <- with(insurance_data, tapply(charges, smoker, shapiro.test))
smoker_normality_test$yes$p.value
smoker_normality_test$no$p.value

# Measure colinearity which is the relationship
# between multiple variables
# The "tolerance" is an indication of the percent
# variance in the predictor that cannot
# be a counted for by the other predictors
# hence very small values indicate a predictor is redundant

# VIF score should be close to 1 but under 5
# 10+ indicates that the variable is not needed
# and can be removed from the model
library(car)
vif(model)

with(insurance_data, qqplot(charges, male, 
                            main = "Comparing two samples of activity data", 
                            xlab = "Insurance charges (,000)", 
                            ylab = "Male"))
abline(0, 1)

# Instead control the process
# Create a own variables
insurance_data$male <- ifelse(insurance_data$sex == "male", 1, 0)
insurance_data$female <- ifelse(insurance_data$sex == "female", 1, 0)

insurance_data$smokers <- ifelse(insurance_data$smoker == "yes", 1, 0)
insurance_data$nonsmokers <- ifelse(insurance_data$smoker == "no", 1, 0)

insurance_data$ne <- ifelse(insurance_data$region == "northeast", 1, 0)
insurance_data$nw <- ifelse(insurance_data$region == "northwest", 1, 0)
insurance_data$se <- ifelse(insurance_data$region == "southeast", 1, 0)
insurance_data$sw <- ifelse(insurance_data$region == "southwest", 1, 0)

# drop unneeded variables first
# using age, male, female, BMI, smoker, nonsmoker
# ne, nw, se, sw
insurance_data <- insurance_data[c(1, 3, 4, 7:15)]

# check model assumptions
# Linearity
scatter.smooth(insurance_data$charges, 
               insurance_data$age, 
               main = "Insurance charges ~ age", 
               xlab = "Insurance charges (,000)", 
               ylab = "Age (years)")

# Males
scatter.smooth(insurance_data$charges, 
               insurance_data$male, 
               main = "Insurance charges ~ male", 
               xlab = "Insurance charges (,000)", 
               ylab = "Male")

# Females
scatter.smooth(insurance_data$charges, 
               insurance_data$female, 
               main = "Insurance charges ~ female", 
               xlab = "Insurance charges (,000)", 
               ylab = "Female")

# BMI
scatter.smooth(insurance_data$charges, 
               insurance_data$bmi, 
               main = "Insurance charges ~ bmi", 
               xlab = "Insurance charges (,000)", 
               ylab = "BMI")

# Smoker
scatter.smooth(insurance_data$charges, 
               insurance_data$smokers, 
               main = "Insurance charges ~ smoker", 
               xlab = "Insurance charges (,000)", 
               ylab = "Smokers")

# Non-smoker
scatter.smooth(insurance_data$charges, 
               insurance_data$nonsmokers, 
               main = "Insurance charges ~ male", 
               xlab = "Insurance charges (,000)", 
               ylab = "Non-smoker")

# no info avaliable for plot of continuous
# variable aganist cat variable

# instead check correlation between both variables
cor(insurance_data$charges, insurance_data$age)
cor(insurance_data$charges, insurance_data$male)
cor(insurance_data$charges, insurance_data$female)
cor(insurance_data$charges, insurance_data$bmi)
cor(insurance_data$charges, insurance_data$smokers)
cor(insurance_data$charges, insurance_data$nonsmokers)

paste("Correlation for charges and age: ", cor(insurance_data$charges, insurance_data$age))

# Need to check also for outliers, normality, colinearity

# Normality
with(insurance_data, qqplot(charges, male, 
                            main = "Comparing two samples of activity data", 
                            xlab = "Insurance charges (,000)", 
                            ylab = "Male"))
abline(0, 1)

# Measure colinearity
# this is a measure of the relationship between multiple variables

# check colinearity of the model
model <- lm(charges ~ age + bmi + male + female + ne + nw + se +sw, 
            insurance_data)
summary(model)

library(car)
qqPlot(model, 
       labels = row.names(insurance_data), 
       id.method = "identify", 
       simulate = TRUE, 
       main = "Q-Q plot")


