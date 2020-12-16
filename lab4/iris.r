#UTF-8 encoding
#устанавливаем пакеты
install.packages("RClickhouse")
install.packages("ellipse")
con <- DBI::dbConnect(RClickhouse::clickhouse(), host="kgsu.demo.octonica.com", password="")
res <- DBI::dbGetQuery(con, "Select 2+2 as result")

install.packages("e1071")
library(caret)
#запрос с бд
dataset <- DBI::dbGetQuery(con, "Select * from bomba409.iris")
dataset$Class <- as.factor(dataset$Class)


class(dataset)
summary(dataset)
levels(dataset$Class)
sapply(dataset, class)
#раскидываем значения и факторы
x <- dataset[,1:4]
y <- dataset[,5]

#диаграммы
par(mfrow=c(1,4))
for(i in 1:4)
  boxplot(x[,i], main=names(dataset)[i])

featurePlot(x=x, y=y, plot="ellipse")
featurePlot(x=x, y=y, plot="box")

#машин лернинг
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
set.seed(13)
fit.lda <- train(Class~., data=dataset, method="lda", metric=metric, trControl=control)

set.seed(13)
fit.cart <- train(Class~., data=dataset, method="rpart", metric=metric, trControl=control)

set.seed(13)
fit.knn <- train(Class~., data=dataset, method="knn", metric=metric, trControl=control)

set.seed(13)
fit.svm <- train(Class~., data=dataset, method="svmRadial", metric=metric, trControl=control)

set.seed(13)
fit.rf <- train(Class~., data=dataset, method="rf", metric=metric, trControl=control)

#результат
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
#тесты
predictions <- predict(fit.lda, x)
confusionMatrix(predictions, y)
#все ок
