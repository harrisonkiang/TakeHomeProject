install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
install.packages('party')
library(party)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(rpart)
library(randomForest)
######### Pre-Processing Data ##########

#### Read in the Train and Test data ###
dsTrain <- read.delim("~/Documents/takehome/training.tsv", header = FALSE)
dsTest <- read.delim("~/Documents/takehome/test.tsv", header = FALSE)

### Create the Features ###
# Each event is a category with 1 or 0
dsTest$EmailClickthrough <- ifelse(dsTest$V3=="EmailClickthrough",1,0)
dsTest$EmailOpen <- ifelse(dsTest$V3=="EmailOpen",1,0)
dsTest$FormSubmit <- ifelse(dsTest$V3=="FormSubmit",1,0)
dsTest$PageView <- ifelse(dsTest$V3=="PageView",1,0)
dsTest$WebVisit <- ifelse(dsTest$V3=="WebVisit",1,0)
head(dsTest)
# The relevant predictors (V1 is user_ID)
dsTest2 <-dsTest[ , c("V1","EmailClickthrough","EmailOpen","FormSubmit","PageView","WebVisit")]
head(dsTest2)
# Aggregate or group up the events by user_ID
dsTest3 <- aggregate(. ~ V1, dsTest2, FUN = sum)
head(dsTest3)

# Same process for Train
head(dsTrain)
dsTrain$CustomerSupport <- ifelse(dsTrain$V3=="CustomerSupport",1,0)
dsTrain$EmailClickthrough <- ifelse(dsTrain$V3=="EmailClickthrough",1,0)
dsTrain$EmailOpen <- ifelse(dsTrain$V3=="EmailOpen",1,0)
dsTrain$FormSubmit <- ifelse(dsTrain$V3=="FormSubmit",1,0)
dsTrain$PageView <- ifelse(dsTrain$V3=="PageView",1,0)
dsTrain$Purchase <- ifelse(dsTrain$V3=="Purchase",1,0)
dsTrain$WebVisit <- ifelse(dsTrain$V3=="WebVisit",1,0)
dsTrain$Date <- as.Date(dsTrain$V2)

dsTrain2 <- dsTrain[,c("V1","CustomerSupport","EmailClickthrough","EmailOpen","FormSubmit","PageView","Purchase","WebVisit")]
dsTrain3 <- aggregate(. ~ V1, dsTrain2, FUN = sum)
############ Analysis #######################

###### rpart tree ########
# First I chose to do a tree to try to predict Purchase
rc1 <- rpart.control(xval = 10, maxcompete = 5, cp = .01) 
test <- rpart(Purchase ~ EmailClickthrough + EmailOpen + FormSubmit + PageView + WebVisit, data = dsTrain3, 
              method = "anova", 
              control = rc1)
printcp(test) #the results of the tree
plotcp(test) #the cross validation plot
summary(test) #the summary of the tree
rpart.plot(test, digits = 4) #plot of the tree
Prediction <- predict(test, dsTest3, type="vector") #predict the test data using tree
submit <- data.frame(UserID = dsTest3$V1, Purchase = Prediction)
ranked <- submit[order(submit$Purchase, decreasing = TRUE), ]
topthousand <- ranked[1:1000, ]
write.csv(topthousand, file = "~/Documents/takehome/TopThousandPredictions_rpart.csv")
#### Random Forest ####
set.seed(2017)
rf <- randomForest(as.factor(Purchase) ~ EmailClickthrough + EmailOpen + FormSubmit + PageView + WebVisit, data = dsTrain3, importance=TRUE, ntree=200)
varImpPlot(rf) #variable importance plot, EmailOpen is most important, next is FormSubmit, and then EmailClickthrough
#imp <- importance(rf)
#impvar <- rownames(imp)[order(imp[, 1], decreasing = T)]
#  for (i in seq_along(impvar)) {
#    partialPlot(rf, dsTrain3, impvar[i], xlab=impvar[i],
#                main=paste("Partial Dependence on", impvar[i]),
#                ylim=c(0, 70))
#  }
# par(op)

#use random forest to predict test data
p1 <- predict(rf, dsTest3)
result <- data.frame(UserID = dsTest3$V1, Purchase = p1)
rankedrfpred <- result[order(result$Purchase, decreasing = TRUE), ]
topthousand_rf <- rankedrfpred[1:1000, ]
write.csv(topthousand_rf, file = "~/Documents/takehome/TopThousandPredictions_randomforest.csv")
