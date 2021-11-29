library(tidyverse)
library(dplyr)
library(ggplot2)
library(caret)
library(broom)
library(GGally)
library(scales)
library(nnet)
library(naivebayes)
library(knitr)
####
#Area (A): The area of a bean zone and the number of pixels within its boundaries.
#Perimeter (P): Bean circumference is defined as the length of its border.
#Major axis length (L): The distance between the ends of the longest line that can be drawn from a bean.
#Minor axis length (l): The longest line that can be drawn from the bean while standing perpendicular to the main axis.
#Aspect ratio (K): Defines the relationship between L and l.
#Eccentricity (Ec): Eccentricity of the ellipse having the same moments as the region.
#Convex area (C): Number of pixels in the smallest convex polygon that can contain the area of a bean seed.
#Equivalent diameter (Ed): The diameter of a circle having the same area as a bean seed area.
#Extent (Ex): The ratio of the pixels in the bounding box to the bean area.
#Solidity (S): Also known as convexity. The ratio of the pixels in the convex shell to those found in beans.
#Roundness (R): Calculated with the following formula: (4piA)/(P^2)
#Compactness (CO): Measures the roundness of an object: Ed/L
#ShapeFactor1 (SF1)
#ShapeFactor2 (SF2)
#ShapeFactor3 (SF3)
#ShapeFactor4 (SF4)
#Class: (Seker, Barbunya, Bombay, Cali, Dermosan, Horoz and Sira)

Dry_Bean <- read.csv("C:/Users/ctruxillo/OneDrive - Technomics/Documents/Capstone/BEANS/data/Dry_Bean.csv")

table(Dry_Bean$Class)
Dry_Bean %>%
  group_by(Class) %>%
  summarise(n=n(),
            perc = n/13611,
            labels = percent(perc)) %>%
  ggplot(aes(x="", y=perc, fill = Class)) +
  geom_col() +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme_void() +
  ggtitle("Bean Breakdown")
names(Dry_Bean)
Dry_Bean %>% ggplot(aes(x=Area, group = Class, fill= Class)) +
  geom_density(adjust=1.5) +
  facet_wrap(~Class) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    axis.ticks.x=element_blank()
  )
  
Dry_Bean %>% ggplot(aes(x=roundness, group = Class, fill= Class)) +
  geom_density(adjust=1.5) +
  facet_wrap(~Class) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    axis.ticks.x=element_blank()
  )
Dry_Bean %>% ggplot(aes(x=ShapeFactor1, group = Class, fill= Class)) +
  geom_density(adjust=1.5) +
  facet_wrap(~Class) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    axis.ticks.x=element_blank()
  )

GGally::ggcorr(Dry_Bean)

#### making a train/ test set
set.seed(27, sample.kind="Rounding")
test_index <- createDataPartition(Dry_Bean$Class, times = 1, p = 0.15, list = FALSE)
train_set <- Dry_Bean %>% slice(-test_index)
test_set <- Dry_Bean %>% slice(test_index)
y_test<- factor(test_set$Class)

names(train_set)

### multinomial Regression
set.seed(28, sample.kind = "Rounding") 
fit_mlr<- multinom(Class ~., data= train_set)
summary(fit_mlr)
z<- summary(fit_mlr)$coefficients/summary(fit_mlr)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1))*2
p
yhat_mlr<-predict(fit_mlr,test_set)
head(yhat_mlr)

confusionMatrix(yhat_mlr, y_test)$overall[["Accuracy"]]

ensemble<- data.frame(true = y_test,
                      MLR = yhat_mlr)
accuracy <- data.frame(mlr = confusionMatrix(yhat_mlr, y_test)$overall[["Accuracy"]]) 

###knn
set.seed(29, sample.kind = "Rounding") 
fit_knn <- train(Class ~ ., method = "knn", 
                   data = train_set,
                   tuneGrid = data.frame(k = seq(3, 10, 1)))
data.frame(k=fit_knn$results$k,
           accuracy = fit_knn$results$Accuracy) %>%
  ggplot(aes(x=k, y=accuracy)) +
  geom_point()
fit_knn$bestTune
yhat_knn<- predict(fit_knn, test_set)
confusionMatrix(yhat_knn, y_test)$overall[["Accuracy"]]
ensemble<- ensemble %>%
  mutate(knn = yhat_knn)
head(ensemble)
accuracy<-accuracy %>% mutate(knn = confusionMatrix(yhat_knn, y_test)$overall[["Accuracy"]])


###LDA
set.seed(30, sample.kind = "Rounding") 
fit_lda<- train(Class ~., data=train_set, method ="lda")
yhat_lda<- predict(fit_lda, test_set)
confusionMatrix(yhat_lda, y_test)$overall[["Accuracy"]]
ensemble <-ensemble %>%
  mutate(lda = yhat_lda)
head(ensemble)
accuracy<-accuracy %>% mutate(lda =confusionMatrix(yhat_lda, y_test)$overall[["Accuracy"]] )

###QDA
set.seed(31, sample.kind = "Rounding") 
fit_qda<- train(Class ~., data=train_set, method ="qda")
yhat_qda<- predict(fit_qda, test_set)
confusionMatrix(yhat_qda, y_test)$overall[["Accuracy"]]
ensemble <-ensemble %>%
  mutate(qda = yhat_qda)
head(ensemble)
accuracy<-accuracy %>% mutate(qda = confusionMatrix(yhat_qda, y_test)$overall[["Accuracy"]])
###RF
set.seed(32, sample.kind = "Rounding")
#fit <- train(Class ~., data=train_set, method = "rf", tuneGrid = data.frame(mtry = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)), importance = TRUE)
fit_rf <- train(Class ~., data=train_set, method = "rf", tuneGrid = data.frame(mtry = c(5,6,7)), importance = TRUE)

data.frame(mtry = fit_rf$results$mtry,
           accuracy = fit_rf$results$Accuracy) %>%
  ggplot(aes(x=mtry, y = accuracy)) +
  geom_point()
fit_rf$bestTune
#the answer was 6 mtry
yhat_rf<-predict(fit_rf, test_set)
confusionMatrix(yhat_rf, y_test)$overall[["Accuracy"]]
varImp(fit_rf)


ensemble<- ensemble %>% 
  mutate(rf = yhat_rf)
head(ensemble)
accuracy<-accuracy %>% mutate(rf = confusionMatrix(yhat_rf, y_test)$overall[["Accuracy"]])
###
accuracy
acc_en<-ensemble %>% 
  mutate(MLR = ifelse(MLR == true,1,0),
         knn = ifelse(knn==true,1,0),
         lda = ifelse(lda==true,1,0),
         qda = ifelse(qda==true,1,0),
         rf = ifelse(rf==true,1,0),
         accuracy = ifelse((MLR + knn + lda + qda + rf)>=3,1,0)) %>% 
  summarise(ensemble = mean(accuracy))
accuracy <- accuracy %>%
  mutate(ensemble = acc_en)
accuracy
test_set %>% 
  group_by(Class) %>%
  mutate
ensemble %>% 
  mutate(MLR = ifelse(MLR == true,1,0),
         knn = ifelse(knn==true,1,0),
         lda = ifelse(lda==true,1,0),
         qda = ifelse(qda==true,1,0),
         rf = ifelse(rf==true,1,0),
         accuracy = ifelse((MLR + knn + lda + qda + rf)>=3,1,0)) %>%
  group_by(true, accuracy) %>%
  summarise(n=n()) %>%
  ggplot(aes(fill = forcats::fct_rev(as.factor(accuracy)), y = n, x = true))+
  geom_bar(position = position_fill(reverse = TRUE), stat="identity")


###
ensemble %>% 
  mutate(MLR1 = ifelse(MLR == true,1,0),
         knn1 = ifelse(knn==true,1,0),
         lda1 = ifelse(lda==true,1,0),
         qda1 = ifelse(qda==true,1,0),
         rf1 = ifelse(rf==true,1,0),
         accuracy = ifelse((MLR1 + knn1 + lda1 + qda1 + rf1)>=3,1,0)) %>%
  filter(accuracy == 0, true == "BARBUNYA") %>%
  mutate(id = nrow(true)) %>%
  select(-MLR1, -knn1, -lda1, -qda1, -rf1, -accuracy, -true)%>%
  pivot_longer(cols = c("MLR","knn", "lda","qda","rf"),names_to = "method", values_to = "prediction") %>%
  group_by(method, prediction) %>%
  summarise(n=n()) %>%
  ggplot(aes(fill = prediction, x=method, y = n))+
  geom_bar(position = position_fill(reverse = TRUE), stat="identity")

ensemble %>% 
  mutate(MLR1 = ifelse(MLR == true,1,0),
         knn1 = ifelse(knn==true,1,0),
         lda1 = ifelse(lda==true,1,0),
         qda1 = ifelse(qda==true,1,0),
         rf1 = ifelse(rf==true,1,0),
         accuracy = ifelse((MLR1 + knn1 + lda1 + qda1 + rf1)>=3,1,0)) %>%
  filter(accuracy == 0, true == "DERMASON") %>%
  mutate(id = nrow(true)) %>%
  select(-MLR1, -knn1, -lda1, -qda1, -rf1, -accuracy, -true)%>%
  pivot_longer(cols = c("MLR","knn", "lda","qda","rf"),names_to = "method", values_to = "prediction") %>%
  group_by(method, prediction) %>%
  summarise(n=n()) %>%
  ggplot(aes(fill = prediction, x=method, y = n))+
  geom_bar(position = position_fill(reverse = TRUE), stat="identity")

train_set %>% 
  ggplot(aes(x=Class, y= ShapeFactor4, fill = Class)) +
  geom_boxplot() 
names(train_set)

ensemble %>% knitr::kable()

