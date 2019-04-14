set.seed(100)



income <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data")

names(income) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours","native.country","high.earner")



# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names)

#   age: continuous.

#   workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.

#   fnlwgt: continuous.

#   education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.

#   education-num: continuous.

#   marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.

#   occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.

#   relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.

#   race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.

#   sex: Female, Male.

#   capital-gain: continuous.

#   capital-loss: continuous.

#   hours-per-week: continuous.

#   native-country: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands.



######################

# Clean up the data

######################

# Drop unnecessary columns

income$native.country <- NULL

income$fnlwgt         <- NULL

# Make sure continuous variables are coded as such

income$age            <- as.numeric(income$age)

income$hours          <- as.numeric(income$hours)

income$education.num  <- as.numeric(income$education.num)

income$capital.gain   <- as.numeric(income$capital.gain)

income$capital.loss   <- as.numeric(income$capital.loss)

# Combine levels of categorical variables that currently have too many levels

levels(income$education) <- list(Advanced = c("Masters,","Doctorate,","Prof-school,"), Bachelors = c("Bachelors,"), "Some-college" = c("Some-college,","Assoc-acdm,","Assoc-voc,"), "HS-grad" = c("HS-grad,","12th,"), "HS-drop" = c("11th,","9th,","7th-8th,","1st-4th,","10th,","5th-6th,","Preschool,"))

levels(income$marital.status) <- list(Married = c("Married-civ-spouse,","Married-spouse-absent,","Married-AF-spouse,"), Divorced = c("Divorced,","Separated,"), Widowed = c("Widowed,"), "Never-married" = c("Never-married,"))

levels(income$race) <- list(White = c("White,"), Black = c("Black,"), Asian = c("Asian-Pac-Islander,"), Other = c("Other,","Amer-Indian-Eskimo,"))

levels(income$workclass) <- list(Private = c("Private,"), "Self-emp" = c("Self-emp-not-inc,","Self-emp-inc,"), Gov = c("Federal-gov,","Local-gov,","State-gov,"), Other = c("Without-pay,","Never-worked,","?,"))

levels(income$occupation) <- list("Blue-collar" = c("?,","Craft-repair,","Farming-fishing,","Handlers-cleaners,","Machine-op-inspct,","Transport-moving,"), "White-collar" = c("Adm-clerical,","Exec-managerial,","Prof-specialty,","Sales,","Tech-support,"), Services = c("Armed-Forces,","Other-service,","Priv-house-serv,","Protective-serv,"))


# Break up the data:

n <- nrow(income)

train <- sample(n, size = .8*n)

test  <- setdiff(1:n, train)

income.train <- income[train,]

income.test  <- income[test, ]

#Defining the Task 
library(mlr)
library(rpart)
library(e1071)
library(kknn)
library(nnet)

theTask <- makeClassifTask(id = "taskname", data = income.train, target = 'high.earner')
print(theTask)

#resampling strat

resampleStrat<-makeResampleDesc(method="CV", iters =3)

# the tuning strategy

tuneMethod <- makeTuneControlRandom(maxit = 10L)

#prediction algorithms

predAlg.tree<- makeLearner("classif.rpart",predict.type = "response")
predAlg.Logit<- makeLearner("classif.glmnet",predict.type = "response")
predAlg.Neural <- makeLearner("classif.nnet",predict.type = "response")
predAlg.Bayes<- makeLearner("classif.naiveBayes",predict.type = "response")
predAlg.kNN <- makeLearner("classif.kknn",predict.type = "response")
predAlg.SVM <- makeLearner("classif.svm",predict.type = "response")

#model parameters 
modelParams.tree <- makeParamSet(makeIntegerParam("minsplit",lower = 10, upper = 50),
                                 makeIntegerParam("minbucket", lower = 5, upper = 50),
                                 makeNumericParam("cp", lower = 0.001, upper = 0.2))
modelParams.logit <- makeParamSet(makeNumericParam("lambda",lower=0,upper=3),
                                  makeNumericParam("alpha",lower=0,upper=1))
modelParams.Neural <- makeParamSet(makeIntegerParam("size", lower=1, upper=10),
                                   makeNumericParam("decay", lower = .1, upper = .5),
                                   makeIntegerParam("maxit",lower=1000,upper=1000))
modelParams.kNN <- makeParamSet(makeIntegerParam("k", lower=1, upper=30))
modelParams.SVM <- makeParamSet(makeDiscreteParam("kernel", values = c("radial")),
                                makeDiscreteParam("cost", values = c(2^-2,2^-1,2^0,2^1,2^2,2^10)),
                                makeDiscreteParam("gamma", values = c(2^-2,2^-1,2^0,2^1,2^2,2^10)))

#tuning the model 
tunedModel.tree <- tuneParams(learner = predAlg.tree,
                         task = theTask,
                         resampling = resampleStrat,
                         measures = list(f1,gmean),       # RMSE performance measure, this can be changed to one or many
                         par.set = modelParams.tree,
                         control = tuneMethod,
                         show.info = TRUE)
tunedModel.logit <- tuneParams(learner = predAlg.Logit,
                         task = theTask,
                         resampling = resampleStrat,
                         measures = list(f1,gmean),       # RMSE performance measure, this can be changed to one or many
                         par.set = modelParams.logit,
                         control = tuneMethod,
                         show.info = TRUE)
tunedModel.Neural <- tuneParams(learner = predAlg.Neural,
                         task = theTask,
                         resampling = resampleStrat,
                         measures = list(f1,gmean),       # RMSE performance measure, this can be changed to one or many
                         par.set = modelParams.Neural,
                         control = tuneMethod,
                         show.info = TRUE)
tunedModel.kNN <- tuneParams(learner = predAlg.kNN,
                         task = theTask,
                         resampling = resampleStrat,
                         measures = list(f1,gmean),       # RMSE performance measure, this can be changed to one or many
                         par.set = modelParams.kNN,
                         control = tuneMethod,
                         show.info = TRUE)
tunedModel.SVM <- tuneParams(learner = predAlg.SVM,
                         task = theTask,
                         resampling = resampleStrat,
                         measures = list(f1,gmean),       # RMSE performance measure, this can be changed to one or many
                         par.set = modelParams.SVM,
                         control = tuneMethod,
                         show.info = TRUE)
#hyper parameters
predAlg.tree.hyp <- setHyperPars(learner=predAlg.tree, par.vals = tunedModel.tree$x)
predAlg.Logit.hyp <- setHyperPars(learner=predAlg.Logit, par.vals = tunedModel.logit$x)
predAlg.Neural.hyp <- setHyperPars(learner=predAlg.Neural, par.vals = tunedModel.Neural$x)
predAlg.kNN.hyp <- setHyperPars(learner=predAlg.kNN, par.vals = tunedModel.kNN$x)
predAlg.SVM.hyp <- setHyperPars(learner=predAlg.SVM, par.vals = tunedModel.SVM$x)

#training 
trained.tree <- train(learner = predAlg.tree.hyp,task = theTask)
trained.Logit <- train(learner = predAlg.Logit.hyp,task = theTask)
trained.Bayes <- train(learner = predAlg.Bayes, task = theTask)
trained.Neural <- train(learner = predAlg.Neural.hyp,task = theTask)
trained.kNN <- train(learner = predAlg.kNN.hyp,task = theTask)
trained.SVM<- train(learner = predAlg.SVM.hyp,task= theTask)


#predictions
prediction.tree <- predict(trained.tree, newdata = income.test)
prediction.Logit<- predict(trained.Logit, newdata = income.test)
prediction.Bayes<- predict(trained.Bayes, newdata = income.test)
prediction.Neural<- predict(trained.Neural, newdata = income.test)
prediction.kNN<- predict(trained.kNN, newdata = income.test)
prediction.SVM<- predict(trained.SVM, newdata = income.test)

#performance 

# mean square error
performance.tree.sqr <- mean((as.numeric(prediction.tree$data$truth) - as.numeric(prediction.tree$data$response))^2)
performance.Logit.sqr <- mean((as.numeric(prediction.Logit$data$truth) - as.numeric(prediction.Logit$data$response))^2)
performance.Bayes.sqr <- mean((as.numeric(prediction.Bayes$data$truth) - as.numeric(prediction.Bayes$data$response))^2)
performance.Neural.sqr <- mean((as.numeric(prediction.Neural$data$truth) - as.numeric(prediction.Neural$data$response))^2)
performance.kNN.sqr <- mean((as.numeric(prediction.kNN$data$truth) - as.numeric(prediction.kNN$data$response))^2)
performance.SVM.sqr <- mean((as.numeric(prediction.SVM$data$truth) - as.numeric(prediction.SVM$data$response))^2)

performance.tree.sqr
performance.Logit.sqr
performance.Bayes.sqr
performance.Neural.sqr
performance.kNN.sqr
performance.SVM.sqr
#mean absolute error
performance.tree.abs <- mean(abs(as.numeric(prediction.tree$data$truth) - as.numeric(prediction.tree$data$response)))
performance.Logit.abs <- mean(abs(as.numeric(prediction.Logit$data$truth) - as.numeric(prediction.Logit$data$response)))
performance.Bayes.abs <- mean(abs(as.numeric(prediction.Bayes$data$truth) - as.numeric(prediction.Bayes$data$response)))
performance.Neural.abs <- mean(abs(as.numeric(prediction.Neural$data$truth) - as.numeric(prediction.Neural$data$response)))
performance.kNN.abs <- mean(abs(as.numeric(prediction.kNN$data$truth) - as.numeric(prediction.kNN$data$response)))
performance.SVM.abs <- mean(abs(as.numeric(prediction.SVM$data$truth) - as.numeric(prediction.SVM$data$response)))

performance.tree.abs
performance.Logit.abs
performance.Bayes.abs
performance.Neural.abs
performance.kNN.abs
performance.SVM.abs
