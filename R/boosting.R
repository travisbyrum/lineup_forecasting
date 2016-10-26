library(gbm)
library(dplyr)
library(readr)

set.seed(17)

final_data <- read_csv("~/lineuper/data/final_data.csv") #reading in data
winexpectation <- final_data$win_expectation # reading in win_expectation

#calculating the win expectation as predicted by RAPM
df_rapm <- final_data %>%
  mutate(
    wins  = winexpectation,
    Pos   = as.factor(Pos),
    Pos.1 = as.factor(Pos.1),
    Pos.2 = as.factor(Pos.2),
    Pos.3 = as.factor(Pos.3),
    Pos.4 = as.factor(Pos.4),
    Tm    = as.factor(Tm),
    Tm.1  = as.factor(Tm.1),
    Tm.2  = as.factor(Tm.2),
    Tm.3  = as.factor(Tm.3),
    Tm.4  = as.factor(Tm.4)
  )

off <- final_data %>%
  select(contains("ORPM"))
def <- final_data %>%
  select(containts("DRPM"))

combined <- final_data %>%
  mutate(total = ORPM + DRPM) %>%
  select(total) #combined RAPM

# using wins estimated by http://statitudes.com/blog/2013/09/09/pythagoras-of-the-hardwood/
#rapm estimates
est <- vapply(
  NROW(off),
  function(n) 1/(1+exp(-0.13959*sum(combined[i,]))),
  numeric(1)
)

gbm_formula <- as.formula(
  paste0("wins ~ ", paste(
    setdiff(colnames(df_rapm), "wins"),
    collapse = " + ")
  )
)

train_rows <- sample(
  NROW(df_rapm),
  round(NROW(df_rapm) * 0.5)
)

traindf <- df_rapm[train_rows, ]
testdf <- df_rapm[-train_rows, ]


#fiting the boosting algorithm
gbm1 <- gbm(
  gbm_formula,          
  data              = traindf,     # dataset
  distribution      = "gaussian",  # see the help for other choices
  n.trees           = 1500,        # number of trees
  shrinkage         = 0.01,        # shrinkage or learning rate,
  interaction.depth = 5,           # 1: additive model, 2: two-way interactions, etc.
  bag.fraction      = 0.5,         # subsampling fraction, 0.5 is probably best
  train.fraction    = 0.9,         # fraction of data for training,
  n.minobsinnode    = 10,          # minimum total weight needed in each node
  cv.folds          = 2,           # do n-fold cross-validation
  keep.data         = TRUE,        # keep a copy of the dataset with the object
  verbose           = TRUE,        # don't print out progress
  n.cores           = 1
)

gbm_perf <- gbm.perf(gbm1, method = "cv")
pred <- predict(
  gbm1, 
  newdata = testdf,
  type    = "response",
  n.trees =1000
)

mean((pred - winexpectation$x[-train_rows]) ** 2, na.rm = TRUE)

#MSE error
mean((est - winexpectation$x[-train_rows]) ** 2, na.rm = TRUE)





