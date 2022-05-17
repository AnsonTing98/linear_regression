str(state.x77)

# check the class of the data
# show matrix and array
class(state.x77)

# convert the data to data frame
# called state.x77_data
states_data <- data.frame(state.x77)

# check the structure of data
str(states_data)

# Move state name into new variable
states_data$name <- state.name

# rename the Life exp and HS grad variables 
# to Life_exp and HS_grade
colnames(states_data)[colnames(states_data) == "Life.Exp"] <- "Life_exp"
colnames(states_data)[colnames(states_data) == "HS.Grad"] <- "HS_grade"
colnames(states_data)

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

attach(states_data)
boxplot(Population, 
        main = "Population", 
        sub = paste("Outlier rows: ", boxplot.stats(Population)$out))
