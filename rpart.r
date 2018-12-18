
l_inter <- glm(q_views ~ weekends+
             cum_fs_d+
             cum_5_views_d+
             license_categ+
             occup_categ+
             tenure_categ+
             license_categ:occup_categ+
             license_categ:tenure_categ+
             tenure_categ:occup_categ
             , data=join1,family=binomial(link='logit'))  
round(summary(l_inter)$coefficients,3)






#Set seed to make prediction reproducible
set.seed(145)

#Split data: training/test set
trainIndex = createDataPartition(join1$q_views, p=0.7, list=FALSE,times=1)
train = join1[trainIndex,]
test = join1[-trainIndex,]

#Perform mixed over/under sampling on the training set
data_balanced_both <- ovun.sample(q_views ~ ., data = join1, method = "both",seed=1)$data
#data_balanced_under <- ovun.sample(q_views ~ ., data = join1, method = "under",seed=1)$data
#data_balanced_over <- ovun.sample(q_views ~ ., data = join1, method = "over",seed=1)$data
#data_synth <- ROSE(q_views ~ ., data = join1, seed = 1)$data

table(data_balanced_both$q_views)




#Model
fit_dtree <- rpart(q_views ~ weekends+
                     cum_fs_d+
                     cum_5_views_d+
                     license_categ+
                     occup_categ+
                     tenure_categ,
                   data=data_balanced_both, method="class",control=rpart.control(minsplit=1,minbucket=1, cp=0.001)) 
#Visualizing tree
rpart.plot(fit_dtree,type=4)

 
In [143]:
pred <- predict(fit_dtree, newdata = test)
roc.curve(test$q_views, pred[,2])


fit_dtree2 <- rpart(q_views ~ weekends+
                     license_categ+
                     occup_categ+
                     tenure_categ,
                   data=data_balanced_both, method="class",control=rpart.control(minsplit=1,minbucket=1, cp=0.001)) 

rpart.plot(fit_dtree2,type=4)

pred2 <- predict(fit_dtree2, newdata = test)
roc.curve(test$q_views, pred2[,2])






