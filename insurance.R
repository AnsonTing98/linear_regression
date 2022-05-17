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
str(insurance_data)

insurance_data$smoker <- factor(insurance_data$smoker, 
                                levels = c("yes", "no"), 
                                ordered = FALSE)

insurance_data$region <- factor(insurance_data$region, 
                                levels = c("northeast", 
                                           "northwest", 
                                           "southeast", 
                                           "southwest"), 
                                ordered = FALSE)

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

set.seed(1)
model <- lm(formula = charges ~ age + sex + bmi + children + smoker + region, 
            data = insurance_data)

summary(model)

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

# Round the BMI and charges values
insurance_data$bmi <- round(insurance_data$bmi, 1)
insurance_data$charges <- round(insurance_data$charges, 2)

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
