# Highway-Amenities-Prediction
Highway Amenities Prediction

This project applies predictive analytics to optimize highway amenity placement using coupon response data from 10,000 drivers (25 variables). The objective is to identify which amenities (Coffee House, Bar, Carry Away, Restaurant <$20, Restaurant $20–$50) drivers are most likely to engage with, improving operational efficiency and profitability.

Project Workflow

- EDA: Analyzed acceptance trends by occupation, income, education, age group, time of day, and visit frequency.

- Feature Engineering:

  - Converted categorical variables to factors

  - Created Age_Group (50 Above / 50 Below)

  - Derived Time_of_Day categories

  - Handled missing values using “Unknown” labels

- Modeling: 80:20 train-test split using caret

  - Logistic Regression

  - Random Forest

  - Gradient Boosting (with cross-validation & repeated CV)

Model Performance
   Model | Accuracy	| F1 Score
Logistic Regression |	55.51% | 64.43%
Random Forest |	76.06%	| 80.21%
Gradient Boosting |	66.65%	| 72.54%
GBM Tuned (CV=20) |	73.20%	| 75.66%
GBM Tuned (Repeated CV)|	71.80%	|74.15%

(Random Forest selected as final model) 


Key Insights

- Occupation, Income, Education, and Time of Day are top predictors.

- Morning time slots show highest coupon acceptance.

- Middle-income segments demonstrate strong engagement.

- Drivers above 50 show significantly higher acceptance rates.

Output

 - ROC comparison of models

 - Feature importance analysis

 - Final scoring predictions exported as Team1_Scoring.csv
