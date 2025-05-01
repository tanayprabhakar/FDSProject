# IMDb Score Prediction Project


## Overview
This project classifies films into four IMDb score categories (*Bad*, *OK*, *Good*, *Excellent*) using movie metadata, financial data, and social media metrics. We employ **Decision Trees**, **K-Nearest Neighbors (KNN)**, and **Random Forests** to predict critical reception and understand key drivers of success.

---

## Dataset
**Source**: [Kaggle IMDb 5000 Movie Dataset](https://www.kaggle.com/deepmatrix/imdb-5000-movie-dataset)

**Location**: `movie_metadata.csv`

**Key Features**:
- **Target**: `imdb_score` (binned into 4 categories)
- **Financial**: `budget`, `gross`
- **Metadata**: `duration`, `year`, `country`, `content_rating`
- **Social Metrics**: `movie_facebook_likes`, `director_facebook_likes`, `actor_1_facebook_likes`, `actor_2_facebook_likes`, `actor_3_facebook_likes`
- **Derived**: `critic_review_ratio`, `other_actors_facebook_likes`

---

## Installation
1. **Clone the repository**
   ```bash
   git clone https://github.com/tanayprabhakar/FDSProj.git
   cd FDSProj
   ```

2. **Install R dependencies**
   ```r
   install.packages(c(
     "dplyr",
     "ggplot2",
     "caret",
     "randomForest",
     "rpart",
     "ggrepel",
     "VIM",
     "plotly"
   ))
   ```

3. **Project structure**
   ```text
   FDSProj/
   │── movie_metadata.csv
   │── data_preprocessing.R
   │── rf_model.rds
   │── knn_model.rds
   ├── README.md
   ```

---

## Usage
1. **Run the full analysis**
   ```r
   # in R console
   source("data_preprocessing.R")
   ```

---

## Workflow

### 1. Data Preprocessing
- Removed duplicates and handled missing values
- Binned `imdb_score` into four categories: *Bad* (<=5), *OK* (5–6.5), *Good* (6.5–8), *Excellent* (>8)
- Engineered `critic_review_ratio` (num_critic_for_reviews / num_user_for_reviews)
- Combined `actor_2_facebook_likes` and `actor_3_facebook_likes` into `other_actors_facebook_likes`
- Simplified `country` to `USA`/`UK`/`Others`
- Standardized `content_rating` levels

### 2. Exploratory Data Analysis (EDA)
- High-budget (> $100M) films seldom achieve "Excellent" ratings
- UK films average 0.3 points higher IMDb scores than US films
- Weak correlation between Facebook likes and IMDb scores (r = 0.18)

### 3. Modeling
| Model           | Training Accuracy | Validation Accuracy | Test Accuracy |
|-----------------|-------------------|---------------------|---------------|
| Decision Tree   | 78.0%             | 71.3%               | 72.4%         |
| KNN (k = 9)     | —                 | 71.4%               | 74.6%         |
| Random Forest   | —                 | 76.4%               | 76.6%         |

**Top Features** (Random Forest):
1. `num_voted_users` (Importance: 42.6)
2. `duration` (38.1)
3. `critic_review_ratio` (35.8)

---

## Results Interpretation
1. **Critic Influence**: Films with `critic_review_ratio > 0.5` are 3× more likely to be *Excellent*.
2. **Ideal Runtime**: 100–120 minutes yields the highest average score (7.2).
3. **Country Effect**: UK productions score on average 0.4 points higher than US films.

---

## Contributing
Contributions are welcome! Please fork the repository and create a pull request for bug fixes or feature requests.

---

## Contact
**Maintainer**: Tanay & Tarang
**Last Updated**: May 2025

