set.seed(17)
library(gbm)
library(dplyr)

#####################################################################################

final_data <- read.csv("~/lineup_forecasting/data/final_data.csv", stringsAsFactors = F) #reading in data

#winexpectation<-read.table("~/winexpectation.csv", header=T, quote="\"" )#reading in data
winexpectation <- read.csv("~/lineup_forecasting/data/win_expectation2015.csv", stringsAsFactors = F)# reading in win_expectation

#calculating the win expectation as predicted by RAPM
df_rapm <- final_data[,!grepl("Player", colnames(final_data))]  #need to take out names for final analysis
df_rapm[is.na(df_rapm)] <- 0 #replaces missing measurements with zero since we assume NA is due to low minutes
df_rapm <- df_rapm %>% mutate(wins = winexpectation$x, Pos = as.factor(Pos), Pos.1 = as.factor(Pos.1), Pos.2 = as.factor(Pos.2),
                              Pos.3 = as.factor(Pos.3), Pos.4 = as.factor(Pos.4), Tm = as.factor(Tm), Tm.1 = as.factor(Tm.1),
                              Tm.2 = as.factor(Tm.2), Tm.3 = as.factor(Tm.3), Tm.4 = as.factor(Tm.4))

#######################
off <- final_data[,grepl("ORPM", colnames(final_data))] #offensive RAPM
def <- final_data[,grepl("DRPM", colnames(final_data))] #defensive RAPM 
#######################
combined <- final_data %>% mutate(total = ORPM + DRPM) %>% select(total) #combined RAPM

# using wins estimated by http://statitudes.com/blog/2013/09/09/pythagoras-of-the-hardwood/
#rapm estimates
est<-rep(NA,nrow(off))
for(i in 1:nrow(off)){
  est[i]<-1/(1+exp(-0.13959*sum(combined[i,])))
}


attach(df_rapm)
response_column <- which(colnames(df_rapm) == "wins")
gbm_formula<-as.formula(paste0("wins ~ ", paste(colnames(df_rapm[, -c(response_column)]), 
                                                collapse = " + ")))

train_rows <- sample(nrow(df_rapm), round(nrow(df_rapm) * 0.5))
traindf <- df_rapm[train_rows, ]
testdf <- df_rapm[-train_rows, ]


#fiting the boosting algorithm
gbm1<-gbm(gbm_formula,           # formula
          data=traindf,                # dataset
          distribution="gaussian",     # see the help for other choices
          n.trees=1500,                # number of trees
          shrinkage=0.01,             # shrinkage or learning rate,
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

mean((pred - winexpectation$x[-train_rows])^2,na.rm = TRUE)

#0.1881644 test
#MSE error
mean((est - winexpectation$x[-train_rows])^2,na.rm = TRUE)





