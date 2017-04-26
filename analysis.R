# Yelp data exploration
# attempt to build a model that will predict number of reviews users write
# regression to classify users

# load required libraries
library(e1071) # Skewness and Kurtosis
library(broom) # For residual analysis (augment())
library(ggplot2) # Plotting
library(sqldf) #to reformat the data
library(dplyr) #data prep
library(psych) #for descriptive stats
library(factoextra) #for scaling data
library(pls) # for PCR
library(neuralnet) # for neural net prediction

# read in data
Yusers <- read.csv("./yUsersFull.csv")
users <- data.frame(Yusers)

######################## Format year ############################
# change yelping_since to a 4 digit numeric value for year
head(users)
typeof(users$yelping_since)
users$yelping_since <- as.character(users$yelping_since)
users$yelping_since <- substr(users$yelping_since,start=1,stop=4)
users$yelping_since <- as.numeric(users$yelping_since)
head(users,1)

################ descriptive stats #####################
des.users <- users[,-1]
describe(des.users)

count_group  <- sqldf("SELECT *, CASE WHEN review_count > 50 THEN 6
                      WHEN review_count >= 40 THEN 5
                      WHEN review_count >= 30 THEN 4
                      WHEN review_count >= 20 THEN 3
                      WHEN review_count >= 10 THEN 2
                      WHEN review_count >= 1 THEN 1
                      ELSE 0 END as groups
                      FROM users")
head(count_group)

groups_des <- sqldf("SELECT groups,COUNT(*) as Count FROM count_group GROUP BY groups")
groups_des

review_count_group <- sqldf("select groups, sum(review_count) as reviews from count_group group by groups")
review_count_group

sqldf("select sum(review_count) as reviews from count_group")

users6 <- sqldf("select * from count_group where groups = 6")
head(users6)

################# plot to become familiar with data ########################
ggplot(data=count_group,aes(x=count_group$groups))+
  geom_bar(colour="black", fill="#DD8888", width=.8,stat='count')+
  guides(fill=FALSE) +
  xlab("Group") + ylab("Total Users") +
  ggtitle("Number of Users by Review Group")
# bars for number of users in review groups
ggplot(data=review_count_group,aes(x=review_count_group$groups,y=review_count_group$reviews))+
  geom_bar(colour="black", fill="deepskyblue", width=.8,stat='identity')+
  guides(fill=FALSE) +
  xlab("Group") + ylab("Total Reviews") +
  ggtitle("Number of Reviews by Review Group")

############################################################
# useful vs review_count
p.useful <- ggplot(users6,aes(x=useful,y=review_count))+
  geom_point(shape=1)+
  geom_smooth(method=lm,color="red",se=FALSE)
print(p.useful)

# funny vs review_count
p.funny <- ggplot(users6,aes(x=funny,y=review_count))+
  geom_point(shape=1)+
  geom_smooth(method=lm,color="red",se=FALSE)
print(p.funny)

# cool vs review_count
p.cool <- ggplot(users6,aes(x=cool,y=review_count))+
  geom_point(shape=1)+
  geom_smooth(method=lm,color="red",se=FALSE)
print(p.cool)

# fans vs review_count
p.fans <- ggplot(users6,aes(x=fans,y=review_count))+
  geom_point(shape=1)+
  geom_smooth(method=lm,color="red",se=FALSE)
print(p.fans)

# avg stars vs review_count
p.average_stars <- ggplot(users6,aes(x=average_stars,y=review_count))+
  geom_point(shape=1)
print(p.average_stars)

# compliment_hot vs review_count
p.hot <- ggplot(users6,aes(x=compliment_hot,y=review_count))+
  geom_point(shape=1)+
  geom_smooth(method=lm,color="red",se=FALSE)
print(p.hot)

# compliment_more vs review_count
p.more <- ggplot(users6,aes(x=compliment_more,y=review_count))+
  geom_point(shape=1)+
  geom_smooth(method=lm,color="red",se=FALSE)
print(p.more)

# compliment_profile vs review_count
p.profile <- ggplot(users6,aes(x=compliment_profile,y=review_count))+
  geom_point(shape=1)+
  geom_smooth(method=lm,color="red",se=FALSE)
print(p.profile)

# compliment_cute vs review_count
p.cute <- ggplot(users6,aes(x=compliment_cute,y=review_count))+
  geom_point(shape=1)+
  geom_smooth(method=lm,color="red",se=FALSE)
print(p.cute)

# compliment_list vs review_count
p.list <- ggplot(users6,aes(x=compliment_list,y=review_count))+
  geom_point(shape=1)+
  geom_smooth(method=lm,color="red",se=FALSE)
print(p.list)

# compliment_note vs review_count
p.note <- ggplot(users6,aes(x=compliment_note,y=review_count))+
  geom_point(shape=1)+
  geom_smooth(method=lm,color="red",se=FALSE)
print(p.note)

# compliment_plain vs review_count
p.plain <- ggplot(users6,aes(x=compliment_plain,y=review_count))+
  geom_point(shape=1)+
  geom_smooth(method=lm,color="red",se=FALSE)
print(p.plain)

# compliment_writer vs review_count
p.writer <- ggplot(users6,aes(x=compliment_writer,y=review_count))+
  geom_point(shape=1)+
  geom_smooth(method=lm,color="red",se=FALSE)
print(p.writer)

# compliment_photos vs review_count
p.photos <- ggplot(users6,aes(x=compliment_photos,y=review_count))+
  geom_point(shape=1)+
  geom_smooth(method=lm,color="red",se=FALSE)
print(p.photos)

# histogram for yelping_since vs review_count years are categorical
p.since <- ggplot(users6,aes(x=yelping_since,y=review_count))+
  geom_point(shape=1)
print(p.since)

########################## Correlation Matrix to determine significant variables ###############################
users6 <- users6[,c(-1,-3,-18)]
describe(users6)
users.corr <- cor(users6)
print(users.corr)

##################### OLS simple regression model #######################
# OLS regression on full users dataset
revc.1 <- lm(review_count~.,data=users6)
revc.1.summary <- summary(revc.1)
print(revc.1.summary)
revc.1

############################# Use PCR to predict review count ########################
# build principal component regression model
# scale = TRUE to normalize the data
set.seed(1776)
pcrModel <- pcr(review_count~.,data=users6,scale=TRUE,validation="CV")
summary(pcrModel)

# plot root mean squared error
pcr.val <- validationplot(pcrModel)
pcr.val
# plot r2 
pcr.r2 <- validationplot(pcrModel,val.type="R2")
pcr.r2
# predicted plot
pcr.pred.p <- predplot(pcrModel)

# regression coefficients plot
pcr.coef <- coefplot(pcrModel)

# train - test split
train <- users6[1:80000,]
y_test <- as.data.frame(users6[80000:100093,1])
test.pcr <- users6[80000:100093,2:15]

# build model with test data
pcr.model <- pcr(review_count~.,data=train,scale=TRUE,validation="CV")
summary(pcr.model)

# test model using 8 components for 95% of variance explained
pcr.pred <- predict(pcr.model,test.pcr,ncomp=7)
summary(pcr.pred)
rmse.pred <- sqrt(mean((pcr.pred-y_test)^2))
rmse.pred

# validation plots for test vs predicted
# plot root mean squared error
pred.mse <- validationplot(pcr.model)
# plot r2 
pred.r2 <- validationplot(pcr.model,val.type="R2")

########################## Prediction with NN ##########################

# build training and test data for neural net
index <- sample(1:nrow(users6),round(.75*nrow(users6)))
train <- users6[index,]
test <- users6[-index,]
lm.fit <- lm(review_count~.,data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
mse.lm <- sum((pr.lm-test$review_count)^2)/nrow(test)
mse.lm
sqrt(mse.lm)

# fit the neural net
maxs <- apply(users6,2,max)
mins <- apply(users6,2,min)
pcr.scaled <- as.data.frame(scale(users6,center=mins,scale=maxs-mins))

train_ <- pcr.scaled[index,]
test_ <- pcr.scaled[-index,]

n <- names(train_)
f <- as.formula(paste("review_count ~",paste(n[!n %in% "review_count"], collapse = " + ")))

########################## build the ridiculously long to train neural net ##########################
# build the NN with 1 hidden layer and 8 nodes in that layer
# decided by mean of inputs rounding up
nn <- neuralnet(f,data=train_,hidden=8,linear.output = TRUE,stepmax=1e6)
plot(nn)

# prediction
pr.nn <- compute(nn,test_[,1:15])
pr.nn_ <- pr.nn$net.result*(max(users$review_count)-min(userse$review_count))+
          min(users$review_count)
test.r <- (test_$review_count)*(max(users$review_count)-min(users$review_count))+
          min(users$review_count)
#mse of neural net
mse.nn <- sum((test.r-pr.nn)^2)/nrow(test_)

print(paste(mse.lm,mse.nn))

# plots to compare
par(mfrow=c(1,2))

# neural net plot
plot(test$review_count,pr.nn_,col='red',main='Real vs Predicted NN',pch=18,cex=0.7)
abline(o,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red',bty='n')

# linear model
plot(test$review_count,pr.lm,col='blue',main='Real vs Predicted LM',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue',bty='n',cex=.95)

# plot real vs predicted NN together
plot(test$review_count,pr.nn_,col='red',main='Real vs Predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))







