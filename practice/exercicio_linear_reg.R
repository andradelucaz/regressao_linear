
# Load libraries
library(tidyverse)
library(PerformanceAnalytics)


# Import data
Bodyfat <- read_csv("dados/Bodyfat.csv")

# Create a new dataset

bodyfeatures <- Bodyfat %>% 
  dplyr::select(-Density)

#### Create a intital model ####

model_first <- lm(bodyfat ~ .,
                  data = bodyfeatures)

summary(model_first)

##### Feature Selection ####

# Accuracy stepwise selection
model_step <- step(model_first, direction = "both")
summary(model_step)

# Statistical stepwise selection

model_pvalue <- step(model_first, k = 3.841459)
summary(model_pvalue)

#### Multivariate correlation analysis ####

# Create new data set woth features selected by stepwose p-value

bodyfeatures_2 <- bodyfeatures %>% 
  dplyr::select(bodyfat, Weight, Abdomen, Forearm, Wrist,
                Age, Neck, Hip, Thigh, Forearm)

# Defining correlation matrix
cor_matrix <- cor(bodyfeatures_2)

# Plot correlation matrix

chart.Correlation((bodyfeatures_2), histogram = TRUE)

corrplot::corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45, 
         tl.cex = 0.8, addCoef.col = "black", number.cex = 0.7, mar = c(0,0,2,0)) 

# VIF calculate

ols_vif_tol(model_step)
ols_vif_tol(model_pvalue)
ols_vif_tol(refined_model)

# Define new features based on multicolinearity analysis

features <- c ('bodyfat','Age', 'Neck', 'Thigh', 'Forearm', 'Wrist',
               'Abdomen')

bodyfeatures_3 <- bodyfeatures %>% 
  dplyr::select(all_of(features))

#### Model analysis ####

summary(model_pvalue)
AIC(model_pvalue)
summary(model_step)
AIC(model_step)

refined_model <- lm(bodyfat ~ ., bodyfeatures_3)
summary(refined_model)
AIC(refined_model)
