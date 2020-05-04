#
# Part 1 - load the data
#
library(e1071)
library(readr)
library(dplyr)

data_file <- file.path("assets", "statewide_covid_19_data_04_27_2020.csv")
df <- read_csv(data_file)
# coerce severity to a factor (so RF algorithm uses classification)
df <-
    df %>%
    mutate(Y04 = as.factor(df$Y04))


metadata_file <- file.path("assets", "statewide_covid_19_metadata_04_27_2020.csv")
mdf <- read_csv(metadata_file)
mdf

#
# Part 2 - select features and label
#

# describe all possible features and labels
print(mdf, n = nrow(mdf))

# select some features and the severity index label
my_x <- c("X01","X10","X12","X13","X23")
my_y <- "Y04"
my_xy <- c(my_x, my_y)

# get descriptions of the selected features and label
mdf %>% filter(Code %in% my_xy)

# subset the dataframe
rf_df <- df %>% select(all_of(my_xy))

#
# Part 3 - create and train the model
#
# split into train (75%) and test (25%) datasets
#
# note that `row_nubmer()` generates a vector of row numbers, and
#     > c(1, 2, 3, 4, 5, 6) %% 4 is
#     [1] 1 2 3 0 1 2

train <- rf_df %>% filter((row_number() %% 4) %in% 1:3)
test <- rf_df %>% filter(row_number() %% 4 == 0)

# create and train the RF model
model <- svm(Y04 ~ X01 + X10 + X12 + X13 + X23,
             data = train,
             probability = TRUE)

# summarize the model
summary(model)

#
# Part 4 - evaluate the model using test data
#

# extract test predictions
preds <- predict(model, test)

# confusion matrix
actual <- factor(test$Y04)
predicted <- factor(preds)
common_levels <- sort(unique(c(levels(actual), levels(predicted))))
actual <- factor(actual, levels = common_levels)
predicted <- factor(predicted, levels = common_levels)
confusion <- table(actual,predicted)
print(confusion)

#classification accuracy
accuracy <- sum(diag(confusion)/nrow(test))
cat("Classification Accuracy = ", accuracy, "\n")

