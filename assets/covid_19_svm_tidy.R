
setwd("C:\\Matott\\QuaRantine")

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

#
# Part 5 - ROC/AUC
#

# roc_one_vs_all()
#   A helper function to compute one vs. all ROC/AUC for a given level (i)
roc_one_vs_all <- function(i) {
    if(is.na(sum(probs))){
        return(NA)
    }
    
    actual <- as.numeric(y_test[[1]] == i)
    score <- probs[,i]
    
    pred <- ROCR::prediction(score, actual)
    perf <- ROCR::performance(pred, "tpr", "fpr")
    
    ROCR::plot(perf, 
               main="ROC Curve", 
               col=cols[as.numeric(i)], 
               add = i != lvls[[1]])
    
    # calculate the AUC and print
    auc_i <- ROCR::performance(pred, measure="auc")
    as.numeric(auc_i@y.values)
}

# prepare data for computing ROC/AUC
x_test <- select(test, all_of(my_x))
y_test <- select(test, all_of(my_y))
# how we obtain probs depends on the algorithm
if (inherits(model, "randomForest")) 
{
    probs <- predict(model, x_test, type='prob')
} else if (inherits(model, "svm"))
{
    probs <- predict(model, x_test, probability = TRUE)
    probs <- attributes(probs)
    probs <- as.data.frame(probs$probabilities)
    probs <- probs[,order(colnames(probs))]
} else # the KNN alg requires approximation of probabilities
{
    # highest predicted probabilities
    pnrst <- attributes(model)$prob
    # corresponding one-based categories
    y_one <- as.numeric(min(levels(df$Y04))) - 1
    vpreds <- as.integer(as.character(preds)) - y_one
    # map probabilities into a matrix of zeroes
    probs <- matrix(0.00, 
                    nrow = length(pnrst),
                    ncol = length(levels(df$Y04)))
    probs[cbind(seq_along(vpreds), vpreds)] <- pnrst
    # coerce to data frame
    probs <- data.frame(probs)
    colnames(probs) <- levels(df$Y04)
}
lvls <- unique(as.character(y_test[[1]]))
cols <- c("red","orange","yellow","green",
          "blue","purple","violet","black")

# vapply helper function across levels
auc <- vapply(lvls, roc_one_vs_all, numeric(1))

# tack on legend and non-informative line
legend("bottomright",
       legend=paste("PSI =",lvls), 
       col=cols[as.numeric(lvls)], 
       lty=1, 
       cex=1.0)
lines(x=c(0,1),
      y=c(0,1),
      lty=2)

