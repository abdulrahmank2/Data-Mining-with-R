Abdul <- read.csv("~/Desktop/melanoma.csv", stringsAsFactors = FALSE)

str(Abdul)

Abdul <- Abdul[-1]


table(Abdul$sex)

Abdul$sex <- factor(Abdul$sex, levels = c("0", "1"),
                         labels = c("Male", "Femele"))

str(Abdul)

round(prop.table(table(Abdul$sex)) * 100, digits = 1)

summary(Abdul[c("age", "thickness")])

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))}

Abdul_n <- as.data.frame(lapply(Abdul[4:6], normalize))

summary(Abdul_n$thickness)

Abdul_train <- Abdul_n[1:149, ]
Abdul_test <- Abdul_n[150:205, ]

Abdul_train_labels <- Abdul[1:149, 3]
Abdul_test_labels <- Abdul[150:205, 3]

install.packages 
library(class)

Abdul_test_pred <- knn(train = Abdul_train, test = Abdul_test,
                      cl = Abdul_train_labels, k=21)

install.packages('gmodels')
library(gmodels)

CrossTable(x = Abdul_test_labels, y = Abdul_test_pred,
           prop.chisq=FALSE)

Abdul_z <- as.data.frame(scale(Abdul[-3]))

summary(Abdul_z$status)

Abdul_train <- Abdul_z[1:149, ]
Abdul_test <- Abdul_z[150:205, ]

Abdul_test_pred <- knn(train = Abdul_train, test = Abdul_test,
                      cl = Abdul_train_labels, k=1)

CrossTable(x = Abdul_test_labels, y = Abdul_test_pred,
           prop.chisq=FALSE)

Abdul_train <- Abdul_z[1:149, ]
Abdul_test <- Abdul_z[150:205, ]

Abdul_test_pred <- knn(train = Abdul_train, test = Abdul_test, cl = Abdul_train_labels, k=1)
CrossTable(x = Abdul_test_labels, y = Abdul_test_pred, prop.chisq=FALSE)

Abdul_test_pred <- knn(train = Abdul_train, test = Abdul_test, cl = Abdul_train_labels, k=2)
CrossTable(x = Abdul_test_labels, y = Abdul_test_pred, prop.chisq=FALSE)

Abdul_test_pred <- knn(train = Abdul_train, test = Abdul_test, cl = Abdul_train_labels, k=3)
CrossTable(x = Abdul_test_labels, y = Abdul_test_pred, prop.chisq=FALSE)
