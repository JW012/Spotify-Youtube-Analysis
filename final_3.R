library(orcutt)
library(knitr)
library(car)
library(MASS)
# Load the data
spotify_youtube <- read.csv(file = "Spotify_Youtube.csv", header = TRUE)

# Remove rows with 0 views
spotify_youtube <- spotify_youtube[spotify_youtube$Views > 0, ]
spotify_youtube <- na.omit(spotify_youtube)
                           
# Set seed for reproducibility
set.seed(123) # You can choose any number

# Determine the number of rows in the dataset
n <- nrow(spotify_youtube)

# Generate a random sample of row indices for the training set
train_indices <- sample(1:n, size = round(0.6 * n))

# Split the data into training and testing sets
train_data <- spotify_youtube[train_indices, ]
test_data <- spotify_youtube[-train_indices, ]
spotify_youtube <- train_data


par(mar = c(1, 1, 1, 1))
#pairs(Views ~ Danceability + Energy + Loudness + Speechiness + 
        #Acousticness + Instrumentalness + Liveness + Valence + Tempo + 
        #Duration_ms,spotify_youtube)
options(width = 150)
cor(spotify_youtube[, c("Danceability", "Energy", "Loudness", "Speechiness", 
                     "Acousticness", "Instrumentalness", "Liveness", "Valence", 
                     "Tempo", "Duration_ms")])
predictors <- spotify_youtube[, c("Danceability", "Energy", "Loudness", "Speechiness", 
                                  "Acousticness", "Instrumentalness", "Liveness", "Valence", 
                                  "Tempo", "Duration_ms")]
vif(lm(spotify_youtube$Views ~ ., data = predictors))


# Fit the full linear regression model
model_full <- lm(Views ~ Danceability + Energy + Loudness + Speechiness + 
                   Acousticness + Instrumentalness + Liveness + Valence + Tempo + 
                   Duration_ms, data = spotify_youtube)
summary(model_full)
par(mfrow=c(2,2))
plot(model_full)

# Calculate Cook's distance
cooksd <- cooks.distance(model)

# Plot Cook's distance
par(mfrow=c(1,2))
plot(cooksd, pch = 19, type = "h", main = "Cook's Distance Plot")
abline(h = 4/length(model$residuals), col = "red")  # Add threshold line (typically 4/n)
# Calculate leverage values
leverage <- hatvalues(model)

# Plot leverage values
plot(leverage, pch = 19, type = "h", main = "Leverage Plot")
abline(h = 2 * length(coefficients(model))/nrow(spotify_youtube), col = "red") 

# Identify observations with high Cook's distance
outliers_cooks <- which(cooksd > 4/length(model$residuals))

# Identify observations with high leverage
outliers_leverage <- which(leverage > 2 * length(coefficients(model))/nrow(spotify_youtube))

# Combine outlier indices
outliers <- unique(c(outliers_cooks, outliers_leverage))

# Print outlier indices
print("Indices of Outliers:")
print(outliers)
# Get the fitted values and residuals from the unweighted model
fitted_values_unweighted <- fitted(model_full_log)
residuals_unweighted <- residuals(model_full_log)

# Remove outliers from the dataset
spotify_youtube <- spotify_youtube[-outliers, ]

boxcox_result <- boxcox(model_full)

# Plot the Box-Cox results to find the optimal lambda
plot(boxcox_result)

# Create Log_Views by taking the logarithm of Views
spotify_youtube$Log_Views <- log(spotify_youtube$Views)

# Create the full model with log-transformed Log_Views
model_full_log <- model_full_log <- lm(Log_Views ~ Danceability + Energy + Loudness + Speechiness +
                                         Acousticness + Instrumentalness + Liveness + Valence + Tempo +
                                         Duration_ms, data = spotify_youtube)

summary(model_full_log)
par(mfrow=c(2,2))
plot(model_full_log)
model_poly_log <- lm(Log_Views ~ poly(Danceability, 2) + poly(Energy, 2) + 
                       poly(Loudness, 2) + poly(Speechiness, 2) +
                       poly(Acousticness, 2) + poly(Instrumentalness, 2) + 
                       poly(Liveness, 2) + poly(Valence, 2) + poly(Tempo, 2) +
                       poly(Duration_ms, 2), data = spotify_youtube)

# Perform ANOVA to compare the models
anova(model_full_log, model_poly_log)
summary(model_poly_log)
plot(model_poly_log)

model_poly_reduced <- lm(Log_Views ~ Danceability + Energy + 
                       poly(Loudness, 2) + poly(Speechiness, 2) +
                       poly(Acousticness, 2) + poly(Instrumentalness, 2) + 
                       poly(Liveness, 2) + poly(Valence, 2)[,2] + Tempo +
                       poly(Duration_ms, 2), data = spotify_youtube)

summary(model_poly_reduced)
plot(model_poly_reduced)
anova(model_poly_reduced, model_poly_log)

AIC(model_full)
AIC(model_full_log)
AIC(model_poly_log)
AIC(model_poly_reduced)
BIC(model_full)
BIC(model_full_log)
BIC(model_poly_log)
BIC(model_poly_reduced)




