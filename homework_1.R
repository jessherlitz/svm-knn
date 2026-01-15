##### Question 2.2.1

data <- read.table(
  "credit_card_data.txt", 
  header = FALSE, 
  sep = "", 
  stringsAsFactors = FALSE
)

#View(data)

missing_values <- anyNA(data)
missing_values

y <- as.factor(data[, 11])

class_counts <- table(y)
class_counts

class_props <- prop.table(class_counts)
class_props

imbalance_ratio <- max(class_counts) / min(class_counts)
imbalance_ratio

#install.packages("kernlab")
library(kernlab)

model <- ksvm(
  as.matrix(data[, 1:10]),
  as.factor(data[, 11]),
  type="C-svc",
  kernel="vanilladot",
  C=0.01,
  scaled=TRUE
)

pred <- predict(model, as.matrix(data[, 1:10]))
pred

mean(pred == as.factor(data[, 11]))

a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
a

a0 <- -model@b
a0

cm <- table(
  Predicted = pred, 
  Actual = as.factor(data[, 11])
)
cm

####### Question 2.2.2

model <- ksvm(
  as.matrix(data[, 1:10]),
  as.factor(data[, 11]),
  type="C-svc",
  kernel="rbfdot",
  C=10,
  scaled=TRUE
)

pred <- predict(model, as.matrix(data[, 1:10]))
pred

mean(pred == as.factor(data[, 11]))

cm <- table(
  Predicted = pred, 
  Actual = as.factor(data[, 11])
)
cm

####### Question Question 2.2.3

#install.packages("kknn")
#View(data)
library(kknn)

k_values <- c(1, 3, 5, 7, 9, 11, 15, 31, 51, 77)
k_values

n <- nrow(data)
n

accuracies_for_k <- numeric(10) 
accuracies_for_k

for (ki in 1:10) {
  k <- k_values[ki]
  predictions <- numeric(n)
  
  for (i in 1:n) {
    train <- data[-i, ]                 
    test  <- data[i, -11, drop = FALSE]   
    
    knn_model <- kknn(
      V11 ~ ., 
      train = train, 
      test = test,
      k = k,
      scale = TRUE
    )
    
    pred <- fitted(knn_model)[1]           
    predictions[i] <- ifelse(pred >= 0.5, 1, 0)
    
  }
  
  cm <- table(
    Predicted = predictions,
    Actual = as.factor(data[, 11])
  )
  
  cat("Confusion Matrix --- K =", k, "\n")
  print(cm)
  
  accuracies_for_k[ki] <- mean(predictions == as.factor(data[, 11]))
}

accuracies_for_k







