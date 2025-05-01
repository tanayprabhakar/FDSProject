# Load necessary libraries
library(ggplot2) # Visualization
library(ggrepel)
library(ggthemes) # Visualization
library(scales) # Visualization
library(dplyr) # Data manipulation
library(VIM)
library(data.table)
library(formattable)
library(plotly)
library(corrplot)
library(GGally)
library(caret)
library(car)


# Load dataset
IMDB <- read.csv("movie_metadata.csv")
str(IMDB) # Check structure of dataset

# Remove duplicate rows
sum(duplicated(IMDB)) # Count duplicates
IMDB <- IMDB[!duplicated(IMDB), ] # Keep unique rows

# Clean movie title column
library(stringr)
IMDB$movie_title <- gsub("Â", "", as.character(factor(IMDB$movie_title)))
str_trim(IMDB$movie_title, side = "right")

# Check genre column
head(IMDB$genres) # Display first few rows of genres

# Create new dataframe for genres and imdb_score
genres.df <- as.data.frame(IMDB[,c("genres", "imdb_score")])

# Convert genres into separate binary columns
genres_list <- c("Action", "Adventure", "Animation", "Biography", "Comedy", "Crime", "Documentary", "Drama", "Family", "Fantasy", "Film-Noir", "History", "Horror", "Musical", "Mystery", "News", "Romance", "Sci-Fi", "Short", "Sport", "Thriller", "War", "Western")
for (genre in genres_list) {
  genres.df[[genre]] <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% genre) 1 else 0)
}

# Calculate mean IMDb scores for each genre
means <- sapply(3:25, function(i) mean(genres.df$imdb_score[genres.df[[i]] == 1]))

# Plot average IMDb scores for different genres
barplot(means, main = "Average IMDb scores for different genres")

# Remove 'genres' column as it has little impact on IMDb score
IMDB <- subset(IMDB, select = -c(genres))
# Identify missing values
colSums(sapply(IMDB, is.na))

# Visualize missing values
missing.values <- aggr(IMDB, sortVars = TRUE, prop = TRUE, sortCombs = TRUE, cex.lab = 1.5, cex.axis = 0.6, cex.numbers = 5, combined = FALSE, gap = -0.2)

# Remove rows with missing 'gross' and 'budget' values
IMDB <- IMDB[!is.na(IMDB$gross), ]
IMDB <- IMDB[!is.na(IMDB$budget), ]
dim(IMDB) # Check new dataset size

# Count complete cases
sum(complete.cases(IMDB))

# Check columns with missing values again
colSums(sapply(IMDB, is.na))

# Analyze 'aspect_ratio' variable
table(IMDB$aspect_ratio)

# Replace NA aspect_ratio with 0
IMDB$aspect_ratio[is.na(IMDB$aspect_ratio)] <- 0

# Compute mean IMDb scores by aspect ratio
mean(IMDB$imdb_score[IMDB$aspect_ratio == 1.85])
mean(IMDB$imdb_score[IMDB$aspect_ratio == 2.35])
mean(IMDB$imdb_score[IMDB$aspect_ratio != 1.85 & IMDB$aspect_ratio != 2.35])

# Remove 'aspect_ratio' as it has little impact on IMDb score
IMDB <- subset(IMDB, select = -c(aspect_ratio))

# Replace NA values in 'facenumber_in_poster' with column mean
IMDB$facenumber_in_poster[is.na(IMDB$facenumber_in_poster)] <- round(mean(IMDB$facenumber_in_poster, na.rm = TRUE))

# Replace 0s with NA in selected predictors
IMDB[,c(5,6,8,13,24,26)][IMDB[,c(5,6,8,13,24,26)] == 0] <- NA

# Impute missing values with column mean
IMDB$num_critic_for_reviews[is.na(IMDB$num_critic_for_reviews)] <- round(mean(IMDB$num_critic_for_reviews, na.rm = TRUE))
IMDB$duration[is.na(IMDB$duration)] <- round(mean(IMDB$duration, na.rm = TRUE))
IMDB$director_facebook_likes[is.na(IMDB$director_facebook_likes)] <- round(mean(IMDB$director_facebook_likes, na.rm = TRUE))
IMDB$actor_3_facebook_likes[is.na(IMDB$actor_3_facebook_likes)] <- round(mean(IMDB$actor_3_facebook_likes, na.rm = TRUE))
IMDB$actor_1_facebook_likes[is.na(IMDB$actor_1_facebook_likes)] <- round(mean(IMDB$actor_1_facebook_likes, na.rm = TRUE))
IMDB$cast_total_facebook_likes[is.na(IMDB$cast_total_facebook_likes)] <- round(mean(IMDB$cast_total_facebook_likes, na.rm = TRUE))
IMDB$actor_2_facebook_likes[is.na(IMDB$actor_2_facebook_likes)] <- round(mean(IMDB$actor_2_facebook_likes, na.rm = TRUE))
IMDB$movie_facebook_likes[is.na(IMDB$movie_facebook_likes)] <- round(mean(IMDB$movie_facebook_likes, na.rm = TRUE))

# Standardize content ratings
IMDB$content_rating[IMDB$content_rating == 'M']   <- 'PG' 
IMDB$content_rating[IMDB$content_rating == 'GP']  <- 'PG' 
IMDB$content_rating[IMDB$content_rating == 'X']   <- 'NC-17'

# Replace less common ratings with 'R'
IMDB$content_rating[IMDB$content_rating == 'Approved']  <- 'R' 
IMDB$content_rating[IMDB$content_rating == 'Not Rated'] <- 'R' 
IMDB$content_rating[IMDB$content_rating == 'Passed']    <- 'R' 
IMDB$content_rating[IMDB$content_rating == 'Unrated']   <- 'R' 
IMDB$content_rating <- factor(IMDB$content_rating)
table(IMDB$content_rating)

# Add profit and ROI columns
IMDB <- IMDB %>% 
  mutate(profit = gross - budget,
         return_on_investment_perc = (profit/budget)*100)

# Remove nearly constant predictors
IMDB <- subset(IMDB, select = -c(color, language))

# Simplify country variable
levels(IMDB$country) <- c(levels(IMDB$country), "Others")
IMDB$country[(IMDB$country != 'USA')&(IMDB$country != 'UK')] <- 'Others' 
IMDB$country <- factor(IMDB$country)
table(IMDB$country)

# Visualize movie releases over the years
ggplot(IMDB, aes(title_year)) +
  geom_bar() +
  labs(x = "Year movie was released", y = "Movie Count", title = "Histogram of Movie released") +
  theme(plot.title = element_text(hjust = 0.5))

# Remove records of movies released before 1980
IMDB <- IMDB[IMDB$title_year >= 1980,]

# Top 20 movies based on Profit
IMDB %>%
  filter(title_year %in% 2000:2016) %>%  # Filter movies from 2000 to 2016
  arrange(desc(profit)) %>%  # Sort by highest profit
  top_n(20, profit) %>%  # Select top 20 movies by profit
  ggplot(aes(x = budget / 1e6, y = profit / 1e6)) +  # Convert to million dollars
  geom_point() + 
  geom_smooth() + 
  geom_text_repel(aes(label = movie_title)) + 
  labs(x = "Budget ($ million)", y = "Profit ($ million)", title = "Top 20 Profitable Movies") +
  theme(plot.title = element_text(hjust = 0.5))

# Top 20 movies based on Return on Investment (ROI)
IMDB %>%
  filter(budget > 100000) %>%  # Exclude very low-budget films
  mutate(profit = gross - budget, return_on_investment_perc = (profit / budget) * 100) %>%  
  arrange(desc(profit)) %>%  
  top_n(20, profit) %>%  
  ggplot(aes(x = budget / 1e6, y = return_on_investment_perc)) +  
  geom_point(size = 2) +  
  geom_smooth(size = 1) +  
  geom_text_repel(aes(label = movie_title), size = 3) +  
  labs(x = "Budget ($ million)", y = "ROI (%)", title = "Top 20 Movies by Return on Investment") +
  theme(plot.title = element_text(hjust = 0.5))

# Top 20 directors with the highest average IMDb score
IMDB %>%
  group_by(director_name) %>%  # Group by director
  summarise(avg_imdb = mean(imdb_score, na.rm = TRUE)) %>%  # Compute average IMDb score
  arrange(desc(avg_imdb)) %>%  # Sort by highest IMDb score
  top_n(20, avg_imdb) %>%  # Select top 20 directors
  formattable(list(avg_imdb = color_bar("orange")), align = 'l')

# Commercial Success vs. Critical Acclaim
IMDB %>%
  top_n(20, profit) %>%  # Select top 20 most profitable movies
  ggplot(aes(x = imdb_score, y = gross / 1e6, size = profit / 1e6, color = content_rating)) +  
  geom_point() +  
  geom_hline(yintercept = 600) +  
  geom_vline(xintercept = 7.75) +  
  geom_text_repel(aes(label = movie_title), size = 4) +  
  labs(x = "IMDb Score", y = "Gross Revenue ($ million)", title = "Commercial Success vs. Critical Acclaim") +
  annotate("text", x = 8.5, y = 700, label = "High Ratings & High Gross") +
  theme(plot.title = element_text(hjust = 0.5))


IMDB %>%
  plot_ly(x = ~movie_facebook_likes, y = ~imdb_score, color = ~content_rating, mode = "markers", text = ~content_rating, alpha = 0.7, type = "scatter")

# Remove unnecessary columns
IMDB <- IMDB %>% select(-c(director_name, actor_1_name, actor_2_name, actor_3_name, movie_title, plot_keywords, movie_imdb_link))

# Remove derived variables to prevent multicollinearity
IMDB <- IMDB %>% select(-c(profit, return_on_investment_perc))

# Handle highly correlated variables
IMDB <- IMDB %>%
  mutate(other_actors_facebook_likes = actor_2_facebook_likes + actor_3_facebook_likes,
         critic_review_ratio = num_critic_for_reviews / num_user_for_reviews) %>%
  select(-c(cast_total_facebook_likes, actor_2_facebook_likes, actor_3_facebook_likes, num_critic_for_reviews, num_user_for_reviews))

# Bin IMDb scores into categories
IMDB$binned_score <- cut(IMDB$imdb_score, breaks = c(0, 4, 6, 8, 10), labels = c("Bad", "OK", "Good", "Excellent"))

# Organize dataset and rename columns for clarity
IMDB <- IMDB %>%
  select(budget, gross, num_voted_users, critic_review_ratio, movie_facebook_likes, director_facebook_likes, actor_1_facebook_likes, other_actors_facebook_likes, duration, facenumber_in_poster, title_year, country, content_rating, imdb_score, binned_score) %>%
  rename(user_votes = num_voted_users, movie_fb = movie_facebook_likes, director_fb = director_facebook_likes, actor1_fb = actor_1_facebook_likes, other_actors_fb = other_actors_facebook_likes, face_number = facenumber_in_poster, year = title_year, content = content_rating)

# Split data into training, validation, and test sets (60:20:20)
set.seed(45)
train_index <- sample(seq_len(nrow(IMDB)), size = 0.6 * nrow(IMDB))
valid_index <- sample(setdiff(seq_len(nrow(IMDB)), train_index), size = 0.2 * nrow(IMDB))
test_index <- setdiff(seq_len(nrow(IMDB)), c(train_index, valid_index))

train <- IMDB[train_index, ]
valid <- IMDB[valid_index, ]
test <- IMDB[test_index, ]


# -------------------------------
# 6.1 Classification Tree
# -------------------------------

# 6.1.1 Full-grown Tree
set.seed(50)
class.tree <- rpart(binned_score ~ . -imdb_score, data = train, method = "class")

# Plot the tree
prp(class.tree, type = 1, extra = 1, under = TRUE, split.font = 2, varlen = 0)

# 6.1.2 Best-pruned Tree
set.seed(51)
cv.ct <- rpart(binned_score ~ . -imdb_score, data = train, method = "class", 
               cp = 0.00001, minsplit = 5, xval = 5)

# Print cross-validation results
printcp(cv.ct)

# Prune the tree based on lowest cross-validation error
pruned.ct <- prune(cv.ct, cp = cv.ct$cptable[which.min(cv.ct$cptable[, "xerror"]), "CP"])

# Plot the pruned tree
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10)

# 6.1.3 Apply Model

# Apply the pruned tree on the training set
tree.pred.train <- predict(pruned.ct, train, type = "class")
confusionMatrix(tree.pred.train, train$binned_score)

# Apply the pruned tree on the validation set
tree.pred.valid <- predict(pruned.ct, valid, type = "class")
confusionMatrix(tree.pred.valid, valid$binned_score)

# Apply the pruned tree on the test set
tree.pred.test <- predict(pruned.ct, test, type = "class")
confusionMatrix(tree.pred.test, test$binned_score)

# -------------------------------
# 6.2 K-Nearest Neighbors
# -------------------------------

# 6.2.1 Data Pre-processing
library(FNN)
# Convert categorical variables into factors
IMDB2 <- IMDB
IMDB2$country <- as.factor(IMDB2$country)
IMDB2$content <- as.factor(IMDB2$content)

# Create dummy variables for categorical features
IMDB2[, c("country_UK", "country_USA", "country_Others")] <- model.matrix(~ country - 1, data = IMDB2)
IMDB2[, c("content_G", "content_NC17", "content_PG", "content_PG13", "content_R")] <- model.matrix(~ content - 1, data = IMDB2)

# Select relevant features
IMDB2 <- IMDB2[, c(1,2,3,4,5,6,7,8,9,10,11,16,17,18,19,20,21,22,23,15)]

# Partition the dataset
set.seed(52)
train2 <- IMDB2[train_index, ]
valid2 <- IMDB2[valid_index, ]
test2 <- IMDB2[test_index, ]

# Normalize the data
train2.norm <- train2
valid2.norm <- valid2
test2.norm <- test2
IMDB2.norm <- IMDB2

norm.values <- preProcess(train2[, -20], method = c("center", "scale"))
train2.norm[, -20] <- predict(norm.values, train2[, -20])
valid2.norm[, -20] <- predict(norm.values, valid2[, -20])
test2.norm[, -20] <- predict(norm.values, test2[, -20])
IMDB2.norm[, -20] <- predict(norm.values, IMDB2[, -20])  

# 6.2.2 Find the best k

# Initialize a dataframe to store k values and accuracy
accuracy.df <- data.frame(k = seq(1, 20, 1), accuracy = rep(0, 20))

# Compute accuracy for different k values using validation data
for(i in 1:20) {
  knn.pred <- knn(train2.norm[, -20], valid2.norm[, -20], cl = train2.norm[, 20], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, valid2.norm[, 20])$overall[1]
}

# Print accuracy results
accuracy.df

# 6.2.3 Apply model on test set

# Apply KNN model with best k (e.g., k=9)
knn.pred.test <- knn(train2.norm[, -20], test2.norm[, -20], cl = train2.norm[, 20], k = 9)

# Generate confusion matrix and calculate accuracy
accuracy <- confusionMatrix(knn.pred.test, test2.norm[, 20])$overall[1]
accuracy


# -------------------------------
# 6.3 Random Forest
# -------------------------------

# 6.3.1 Build Model
library(randomForest)
set.seed(53)
rf <- randomForest(binned_score ~ . -imdb_score, data = train, mtry = 5)

# Plot model error rate
plot(rf)
legend('topright', colnames(rf$err.rate), col = 1:5, fill = 1:5)

# 6.3.1 Variable Importance
importance <- importance(rf)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'], 2))

# Rank variables by importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#', dense_rank(desc(Importance))))

# Plot variable importance
ggplot(rankImportance, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) +
  geom_bar(stat = 'identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust = 0, vjust = 0.55, size = 4, colour = 'grey') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

# -------------------------------
# 6.3.2 Apply Model
# -------------------------------

# Apply model on validation set
set.seed(632)
rf.pred.valid <- predict(rf, valid)
confusionMatrix(rf.pred.valid, valid$binned_score)

# Apply model on test set
set.seed(633)
rf.pred.test <- predict(rf, test)
confusionMatrix(rf.pred.test, test$binned_score)

# -------------------------------
# 7 Conclusion
# -------------------------------

# Summary table of model accuracy
accuracy_table <- data.frame(
  Dataset = c("Training", "Validation", "Test"),
  Decision_Tree = c(0.7803, 0.7129, 0.7241),
  KNN = c(NA, 0.7143, 0.7456),
  Random_Forest = c(NA, 0.7642, 0.7658)
)

print(accuracy_table)

# Save Decision Tree Model
saveRDS(pruned.ct, "pruned_ct_model.rds")

# Save KNN Normalization and Training Data
saveRDS(norm.values, "norm_values.rds")
saveRDS(train2.norm, "train2_norm.rds")

# Save Random Forest Model
saveRDS(rf, "rf_model.rds")

