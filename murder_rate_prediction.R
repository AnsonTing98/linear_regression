str(state.x77)

# check the class of the data
# show matrix and array
class(state.x77)

# convert the data to data frame
# called state.x77_data
states <- data.frame(state.x77)

# check the structure of data
str(states)

# Move state name into new variable
states$name <- state.name

# rename the Life exp and HS grad variables 
# to Life_exp and HS_grade
colnames(states)[colnames(states) == "Life.Exp"] <- "Life_Exp"
colnames(states)[colnames(states) == "HS.Grad"] <- "HS_Grad"
colnames(states)

numeric_variable <- sapply(states_data, is.numeric)
numeric_data <- states_data[numeric_variable]

pairs(numeric_data)

library(psych)
pairs.panels(states_data,
             smooth = TRUE,
             scale = FALSE,
             density = TRUE,
             ellipses = TRUE,
             method = "spearman",
             pch = 21,
             lm = FALSE, 
             cor = TRUE,
             jiggle = FALSE,
             factor = 2,
             hist.col = 4,
             stars = TRUE,
             ci = TRUE) 

# Check for outliers
# An outlier = 1.5 * interquartile range
# IQR = distance between 25th and 75th percentile
# need to check speed and distance for outliers

attach(states)
boxplot(Population, 
        main = "Population", 
        sub = paste("Outlier rows: ", 
                    paste(boxplot.stats(Population)$out, 
                          sep = " ", collapse = ", ")))

scatter.smooth(x = Population,
               y = Murder,
               xlab = "Population (,000)",
               ylab = "Murder %", main = "Correlation of murder ~ population")

cor(Murder, Population)

scatter.smooth(x = Illiteracy,
               y = Murder,
               main = "Correlation of Murder ~ Illiteracy",
               xlab = "Illiteracy %",
               ylab = "Murder %")

cor(Murder, Illiteracy)

scatter.smooth(x = Frost,
               y = Murder,
               main = "Correlation of Murder ~ Frost",
               xlab = "Frost",
               ylab = "Murder %")

cor(Murder, Frost)

paste("Correlation for Murder and Frost: ", cor(Murder, Frost))
paste("Correlation for Murder and Illiteracy: ", cor(Murder, Illiteracy))
paste("Correlation for Murder and Population: ", cor(Murder, Population))
paste("Correlation for Murder and HS Grad: ", cor(Murder, HS_Grad))
paste("Correlation for Murder and Income: ", cor(Murder, Income)) 
paste("Correlation for Murder and Life Exp: ", cor(Murder, Life_Exp))
paste("Correlation for Murder and Area: ", cor(Murder, Area))

states <- subset(states, select = -c(Area))

head(states)

opar <- par(no.readonly = TRUE)
par(mfrow = c(4, 2)) # divide graph area in 3 rows by 2 columns
attach(states)
boxplot(Murder,
        main = "Murder",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Murder)$out)) # box plot for 'murder'
boxplot(Population,
        main = "Population",
        sub = paste("Outlier rows: ",
                    paste(boxplot.stats(Population)$out, collapse = ", "))) # box plot for 'Population'
boxplot(states$HS_Grad,
        main = "Graduation",
        sub = paste("Outlier rows: ",
                    boxplot.stats(states$HS_Grad)$out)) # box plot for 'HS Grad'
boxplot(Illiteracy,
        main = "Illiteracy",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Illiteracy)$out)) # box plot for 'HS Grad'
boxplot(Income,
        main = "Income",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Income)$out)) # box plot for 'HS Grad'
boxplot(Frost,
        main = "Frost",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Frost)$out)) # box plot for 'HS Grad'
boxplot(states$Life_Exp,
        main = "Life Exp",
        sub = paste("Outlier rows: ",
                    boxplot.stats(states$Life_Exp)$out)) # box plot for 'HS Grad'
detach(states)
par(opar)

# Remove population outliers
states <- subset(states,
                 states$Population != 21198
                 & states$Population != 11197
                 & states$Population != 18076
                 & states$Population != 11860
                 & states$Population != 12237)
# Remove Income outliers
states <- subset(states, states$Income != 6315)

# Skewness function to examine normality
#install.packages("e1071")
library(e1071)
opar <- par(no.readonly = TRUE)
par(mfrow = c(3,2)) # divide graph area into 1 row x 2 cols
# density plot for speed
# minimally skewed to the left
# skewness of < -1 or > 1 = highly skewed
# -1 to -0.5 and 0.5 to 1 = moderately skewed
# Skewness of -0.5 to 0.5 = approx symetrical

plot(density(states$Population),
     main = "Density plot : Population",
     ylab = "Frequency", xlab = "Population",
     sub = paste("Skewness : ", round(e1071::skewness(states$Population), 2)))

# fill the area under the plot

polygon(density(states$Population), col = "red")

plot(density(states$Murder),
     main = "Density plot : Murder",
     ylab = "Frequency", xlab = "Murder",
     sub = paste("Skewness : ", round(e1071::skewness(states$Murder), 2)))
polygon(density(states$Murder), col = "red")

plot(density(states$HS_Grad),
     main = "Density plot : HS grade",
     ylab = "Frequency", xlab = "HS grade",
     sub = paste("Skewness : ", round(e1071::skewness(states$HS_Grad), 2)))
# fill the area under the plot
polygon(density(states$HS_Grad), col = "red")

plot(density(states$Illiteracy),
     main = "Density plot : Illiteracy",
     ylab = "Frequency", xlab = "Illiteracy",
     sub = paste("Skewness : ", round(e1071::skewness(states$Illiteracy), 2)))
polygon(density(states$Illiteracy), col = "red")

plot(density(states$Income),
     main = "Density plot : Income",
     ylab = "Frequency", xlab = "Income",
     sub = paste("Skewness : ", round(e1071::skewness(states$Income), 2)))
# fill the area under the plot
polygon(density(states$Income), col = "red")

plot(density(states$Frost),
     main = "Density plot : Frost",
     ylab = "Frequency", xlab = "Feost",
     sub = paste("Skewness : ", round(e1071::skewness(states$Frost), 2)))
# fill the area under the plot
polygon(density(states$Frost), col = "red")
par(opar)

# Display histogram and QQ plot side by side
par(mfrow = c(1, 2))
hist(states$Murder, 
     main = "Normality proportion for murder", 
     xlab = "Murder rate")

qqnorm(states$Murder)
qqline(states$Murder)

par(opar)

# Run the normality test
# Make the decisions on whether to keep vars or not
murder_model <- lm(Murder ~ Population + Income + Illiteracy + Life_Exp + 
                     HS_Grad + Frost, states)
summary(murder_model)

# Probability plot - if all points
# we've met 
library(car)
qqPlot(murder_model, 
       labels = row.names(states), 
       id.method = "identify", 
       simulate = TRUE, 
       main = "Q-Q plot")

# split dataset into training and testing data
# 70% train, 30% test split
set.seed(1)
no_rows_data <- nrow(states)
sample_data <- sample(1:no_rows_data, 
                      size = round(0.7 * no_rows_data), 
                      replace = FALSE)
training_data <- states[sample_data, ]
testing_data <- states[-sample_data, ]

murder_model <- lm(Murder ~ Population + Income + Illiteracy + Life_Exp + 
                     HS_Grad + Frost, training_data)
summary(murder_model)

qqPlot(murder_model, 
       labels = row.names(states), 
       id.method = "identify", 
       simulate = TRUE, 
       main = "Q-Q plot")

# Examine the outliers to investigate the issues
training_data["Nevada", ]
training_data["Maine", ]
fitted(murder_model)["Nevada"]
fitted(murder_model)["Maine"]

student_murder_model <- rstudent(murder_model)
hist(student_murder_model, 
     breaks = 10, 
     freq = FALSE, 
     xlab = "Studentised residual", 
     main = "Distribution of errors")

curve(dnorm(x, mean = mean(student_murder_model), 
            sd = sd(student_murder_model)), 
      add = TRUE, 
      col = "blue", 
      lwd = 2)
lines(density(student_murder_model)$x, 
      density(student_murder_model)$y, 
      col = "red", 
      lty = 2, 
      lwd = 2)
legend("topright", legend = c("Normal curve", "kernel density curve"), 
       lty = 1, 
       col = c("blue", "red"), 
       cex = .7)

outlierTest(murder_model)

# Remove Nevada from the dataset
states <- states[rownames(states) != "Nevada", ]
training_data <- subset(training_data)

