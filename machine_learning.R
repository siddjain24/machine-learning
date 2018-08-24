
library(dplyr)
library(ggplot2)
adv = read.csv('/datasets/Advertising.csv')

View(adv)


ggplot(adv, aes(x=TV, y=sales))+geom_point() + 
  geom_smooth(method='lm')

cor(adv$radio, adv$sales)
View(cor(adv))

# Model = TV ~ Sales


dim(adv)
0.7*200

train = adv[1:140,]
test = adv[141:nrow(adv),]

# Fit a linear model
model = lm(sales~newspaper, data=train)
model

# Predict the values using test data
test$sales_predict = 0.04 * test$newspaper + 12.79

plot(test$sales, type='l')
lines(test$sales_predict, col='red')

## Classification

hr = read.csv('/datasets/HR Analytics.csv')
View(hr)

dim(hr)
install.packages("rpart")

library(rpart)

0.7*nrow(hr)

hr$Attrition = as.factor(hr$Attrition)
train = hr[1:1029,]
test = hr[1030:nrow(hr),]

model = rpart(Attrition~.,
              data=train,   
              control=rpart.control(maxdepth=3))   
fancyRpartPlot(model)
model
plot(model)
text(model)
install.packages('rattle')
library(rattle)

colnames(hr)


(train %>% filter(OverTime=='No') %>% nrow()) / nrow(train)


(train %>% filter(Attrition == 1) %>% nrow()) / nrow(train)


train %>% filter(OverTime == 'No') %>% nrow()


train %>% filter(OverTime == 'No', Attrition==0) %>% nrow()

650/726*100


train %>% filter(OverTime == 'Yes') %>% nrow()

train %>% filter(OverTime == 'Yes', Attrition==0) %>% nrow()

nrow(train)


train %>% filter(OverTime=='Yes',
                 MonthlyIncome>=3485,
                 Attrition==0) %>% nrow()



## Gini impurity

n = nrow(train)
nl = train %>% filter(OverTime=='No') %>% nrow()
nr = train %>% filter(OverTime=='Yes') %>% nrow()

## Gini impurity for left child
p0_left = (train %>%
             filter(OverTime=='No',Attrition==0) %>% 
             nrow()) / nl
p1_left = (train %>%
             filter(OverTime=='No',Attrition==1) %>% 
             nrow()) / nl         

p0_left + p1_left # This should be = 1

gi_left = 1 - p0_left^2 - p1_left^2
gi_left

## Gini impurity for right child
p0_right = (train %>%
             filter(OverTime=='Yes',Attrition==0) %>% 
             nrow()) / nr
p1_right = (train %>%
             filter(OverTime=='Yes',Attrition==1) %>% 
             nrow()) / nr        

p0_right + p1_right # This should be = 1

gi_right = 1 - p0_right^2 - p1_right^2
gi_right
## Overall gini impurity
# gi = (nleft/n*gileft) + (nright/n*giright)
gi = (nl/n*gi_left) + (nr/n*gi_right)
gi





## Gini impurity for numerical column


View(mis)


gis = c()
cuts = c()
train_subset = train %>% filter(OverTime=='Yes')
mis = sort(unique(train_subset$MonthlyIncome))
n = nrow(train_subset)
for (i in seq(1, (length(mis)-1))){
  cut = (mis[i] + mis[i+1]) / 2
  nl = train_subset %>% filter(MonthlyIncome < cut) %>% nrow()
  nr = train_subset %>% filter(MonthlyIncome > cut) %>% nrow()
  # GI Left
  p0_left = (train_subset %>%
               filter(MonthlyIncome < cut,Attrition==0) %>% 
               nrow()) / nl
  p1_left = (train_subset %>%
               filter(MonthlyIncome < cut ,Attrition==1) %>% 
               nrow()) / nl         
  p0_left + p1_left # This should be = 1
  gi_left = 1 - p0_left^2 - p1_left^2
  # GI Right
  p0_right = (train_subset %>%
                filter(MonthlyIncome > cut,Attrition==0) %>% 
                nrow()) / nr
  p1_right = (train_subset %>%
                filter(MonthlyIncome > cut,Attrition==1) %>% 
                nrow()) / nr        
  
  p0_right + p1_right # This should be = 1
  
  gi_right = 1 - p0_right^2 - p1_right^2
  
  gi = (nl/n*gi_left) + (nr/n*gi_right)
  
  gis = append(gis, gi)
  cuts = append(cuts, cut)
}
result = data.frame(cut=cuts, gi=gis)
View(result)


### Prediction
pred = predict(model, test, type = 'class')
View(pred)

test$pred = pred

accuracy =  sum(test$Attrition == test$pred) / nrow(test) * 100
accuracy


table(test$Attrition) / nrow(test) * 100
library(caret)
install.packages('caret')
confusionMatrix(test$pred, test$Attrition, positive="1")

test %>% filter(pred==0, Attrition==0) %>% nrow() # TN
test %>% filter(pred==1, Attrition==1) %>% nrow() # TP
test %>% filter(pred==1, Attrition==0) %>% nrow() # FP
test %>% filter(pred==0, Attrition==1) %>% nrow() # FN

16 / (49+16) 
nrow(test)

### Bank Marketing classification

bank = read.csv('/datasets/bank/bank-full.csv', sep=";")
dim(bank)

# Target -> y
table(bank$y) / nrow(bank)




hr$Attrition = as.factor(hr$Attrition)
train = hr[1:1029,]
test = hr[1030:nrow(hr),]
model = rpart(Attrition~.,
              data=train)   
library(rattle)
library(caret)
fancyRpartPlot(model)

train_predict = predict(model, train, 
                        type='class')
confusionMatrix(train_predict, 
                train$Attrition,
                positive='1')

library(randomForest)
rf = randomForest(Attrition~.,
                  data=train,
                  mtry=6,
                  ntree=401)
test$predict = predict(rf, test, type='class')
train$predict = predict(rf, train, type='class')
confusionMatrix(train$predict,
                train$Attrition,
                positive="1")


probs = predict(rf, test, type='prob')
predict_new = ifelse(probs[,2]>0.3, 1, 0)
predict_new = as.factor(predict_new)
confusionMatrix(predict_new,
                test$Attrition,
                positive="1")


data(iris)
View(iris)



train = iris[1:100,]
test = iris[101:nrow(iris),]
library(rpart)
library(caret)
model = rpart(Species~., data=train)
test$pred = predict(model, test, type="class")
confusionMatrix(test$pred, test$Species)
unique(iris$Species)

View(iris)
View(test)
library(rattle)
library(randomForest)
fancyRpartPlot(model)


?sample
rows_train = sample(seq(1, nrow(iris)), 100)

train = iris[rows_train,]
test = iris[-rows_train,]

model = randomForest(Species~., data=train)
test$pred = predict(model, test, type="class")

View(predict(model, test, type="prob"))
confusionMatrix(test$pred, test$Species)

library(dplyr)
test %>% filter(pred!='setosa',
                Species=='setosa') %>% nrow()


### KNN
set.seed(100)
rows_train = sample(seq(1, nrow(iris)), 100)
train = iris[rows_train,]
test = iris[-rows_train,]

dist(rbind(test[1,1:4], train[1, 1:4]))[1]

rows = c()
dists = c()     
for (i in seq(1, nrow(train))){
  curr_dist = dist(rbind(test[1,1:4],
                         train[i,1:4]))
  dists = append(dists, curr_dist)
  rows = append(rows, i)
}
result = data.frame(rows=rows, ed=dists)
result = result %>% arrange(dists)
View(result)
kneigh = head(result$rows,7)
View(train[kneigh,])



library(class)
library(caret)
pred_iris = knn(train %>% select(-Species),
            test %>% select(-Species),
            cl= train$Species,
            k=7)

confusionMatrix(pred_iris,
                test$Species)

## Covert categorical columns to numerical columns
hr$Attrition = as.integer(hr$Attrition)
dummy_obj = dummyVars(~., data=hr %>% select(-Over18))
hr_new = data.frame(predict(dummy_obj, newdata = hr))
View(hr_new)
set.seed(100)
rows_train = sample(seq(1, nrow(hr_new)), 1029) # 0.7*nrow(hr)=1029
train = hr_new[rows_train,]
test = hr_new[-rows_train,]

pred_hr = knn(train %>% select(-Attrition),
                test %>% select(-Attrition),
                cl= train$Attrition,
                k=7)
confusionMatrix(as.factor(pred_hr),
                as.factor(test$Attrition),
                positive="1")


### Clustering
iris_new = iris %>% select(-Species)

kmeans_model = kmeans(iris_new,  centers = 3)
iris$cluster = kmeans_model$cluster
View(iris)



