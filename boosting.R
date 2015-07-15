set.seed(17)
library(gbm)
finaldata<-read.csv("~/finaldata.csv")
#reading in data
winexpectation<-read.table("~/winexpectation.csv", header=T, quote="\"")#reading in data
#finalplayerdata <- read.csv("~/finalplayerdata.csv") #not necessary for analysis
#calculating the win expectation as predicted by RAPM
data<-finaldata[,-seq(1,length(finaldata),by=51)]  #need to take out names for final analysis
data[is.na(data)]<-0 #replaces missing measurements with zero since we assume NA is due to low minutes
data<-cbind(winexpectation,data)
names(data)[1] <- "wins"
#######################
off<-finaldata[,seq(29,length(finaldata),by=51)]#offensive RAPM
def<-finaldata[,seq(30,length(finaldata),by=51)]#defensive RAMP 
#######################
combined<-finaldata[,seq(31,length(finaldata),by=51)]
# using wins estimated by http://statitudes.com/blog/2013/09/09/pythagoras-of-the-hardwood/
#rapm estimates
est<-rep(NA,nrow(off))
for(i in 1:nrow(off)){
  est[i]<-1/(1+exp(-0.13959*sum(combined[i,])))
}

#MSE error
mean((est - winexpectation[-train_rows, ])^2,na.rm = TRUE)
#0.1854614 full
#0.237124 test 


attach(data)
response_column <- which(colnames(data) == "wins")
gbm_formula<-as.formula(paste0("wins ~ ", paste(colnames(data[, -c(response_column)]), 
                                                collapse = " + ")))

train_rows <- sample(nrow(data), round(nrow(data) * 0.5))
traindf <- data[train_rows, ]
testdf <- data[-train_rows, ]


#fiting the boosting algorithm
gbm1<-gbm(gbm_formula,           # formula
          data=data,                   # dataset
          distribution="gaussian",     # see the help for other choices
          n.trees=1500,                # number of trees
          shrinkage=0.001,             # shrinkage or learning rate,
          # 0.001 to 0.1 usually work
          interaction.depth=5,         # 1: additive model, 2: two-way interactions, etc.
          bag.fraction = 0.5,          # subsampling fraction, 0.5 is probably best
          train.fraction = 0.9,        # fraction of data for training,
          # first train.fraction*N used for training
          n.minobsinnode = 10,         # minimum total weight needed in each node
          cv.folds = 2,                # do n-fold cross-validation
          keep.data=TRUE,              # keep a copy of the dataset with the object
          verbose=TRUE,                # don't print out progress
          n.cores=1)   

gbm_perf <- gbm.perf(gbm1, method = "cv")
pred<-predict(gbm1, newdata=testdf,type="response", n.trees=1000)

mean((pred - winexpectation[-train_rows, ])^2,na.rm = TRUE)
#0.1881644 test


################## testing on 2014 season
lineup.2014<-read.csv("~/short2014lineup")
data.2014<-read.csv("~/2014data.csv")
#data.2014<-data.2014[,-c(seq(1,length(finaldata),by=51))]  #need to take out names for final analysis
data.2014[is.na(data.2014)]<-0
#data.2014[data.2014=="SG-SF"]<-"SF"
#data.2014[,1]<-as.character(data.2014[,1])
#data.2014[,1]<-as.factor(data.2014[,1])


avg.2014<-(lineup.2014[,20])/((lineup.2014[,6]/(48*82))*lineup.2014[,5])
wins.2014<-1/(1+exp(-0.13959*avg.2014))
#########################################################################
combined<-data.2014[,seq(31,length(data.2014),by=51)]
# using wins estimated by http://statitudes.com/blog/2013/09/09/pythagoras-of-the-hardwood/
#rapm estimates
est<-rep(NA,nrow(combined))
for(i in 1:nrow(combined)){
  est[i]<-1/(1+exp(-0.13959*sum(combined[i,])))
}
mean((est-wins.2014)^2,na.rm = TRUE)
#0.2053992 future MSE



pred<-predict(gbm1, newdata=data.2014,type="response", n.trees=1000)
mean((pred-wins.2014)^2,na.rm = TRUE)
#0.2020012
