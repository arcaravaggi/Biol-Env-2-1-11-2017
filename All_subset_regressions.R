qDAT <- read.csv("qDAT2.txt", header = TRUE, sep = "\t")
qDAT <- qDAT[complete.cases(qDAT),]
qDAT$ZoDa <- qDAT$Zone*qDAT$data


#All subsets with leaps

library(leaps)
regsubsets.out <-
  regsubsets(cull ~ .,
             data = qDAT,
             nbest = 5,       # 1 best model for each number of predictors
             nvmax = NULL,    # NULL for no limit on number of variables
             force.in = NULL, 
             force.out = NULL,
             method = "exhaustive")
regsubsets.out

#The best model in the 10-variable case includes all variables, as that is the only way to have 10 variables
summary.out <- summary(regsubsets.out)
as.data.frame(summary.out$outmat)

names(summary(regsubsets.out))
summary(regsubsets.out,all.best = TRUE,matrix=TRUE,matrix.logical=FALSE)
summary(regsubsets.out)$outmat
summary(regsubsets.out)$adjr2
summary(regsubsets.out)$cp

# Adjusted R2
# By adjusted \( R^2 \), the best model includes lwt, race.cat, preterm, ht, and ui (variables that have black boxes at the higest Y-axis value).
plot(regsubsets.out, scale = "adjr2", main = "Adjusted R^2")



# Plot Output from regsubsets Function in leaps package
# This is just another way of presenting the same information for adjusted \( R^2 \). The model with 7 variables (counting dummy variables seprately) has the highest adjusted \( R^2 \).
# Mallow Cp is used to decide on the number of predictors to include. The stopping rule is to start with the smallest model and gradually increase number of variables, and stop when Mallow Cp is approximately (number of regressors + 1, broken line) for the first time. In this case, the model with 6 regressors is the first one to achieve such a condition.

library(car)
layout(matrix(1:2, ncol = 2))
## Adjusted R2
res.legend <-
  subsets(regsubsets.out, statistic="adjr2", legend = FALSE, min.size = 5, main = "Adjusted R^2")
## Mallow Cp
res.legend <-
  subsets(regsubsets.out, statistic="cp", legend = FALSE, min.size = 5, main = "Mallow Cp")
abline(a = 1, b = 1, lty = 2)

res.legend

# See which model has the highest adjusted R2
# The model with 7 variables (counting dummy variables separately) has the highest adjusted \( R^2 \). Variables marked with TRUE are the ones chosen.

which.max(summary.out$adjr2)

summary.out$which[6,]

# Do regression with the best model

best.model <- lm(cull ~ Zone + data + supp.mgmt + petition + hunt + supp.gvt + ZoDa, data = qDAT)
summary(best.model)


# Calculate Relative Importance for Each Predictor
library(relaimpo)
calc.relimp(best.model,type=c("lmg","last","first","pratt"),
            rela=TRUE)

# Bootstrap Measures of Relative Importance (1000 samples) 
boot <- boot.relimp(best.model, b = 1000, type = c("lmg", 
                                            "last", "first", "pratt"), rank = TRUE, 
                    diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result


#bestglm
# Best subset glm using AIC, BIC, EBIC, BICq or Cross-Validation. For the normal case, the 'leaps' is used. Otherwise, a slower exhaustive search. The 'xtable' package is needed for vignette 'SimExperimentBICq.Rnw' accompanying this package.

qDAT <- read.csv("qDAT2.txt", header = TRUE, sep = "\t")
qDAT <- qDAT[complete.cases(qDAT),]
qDAT$ZoDa <- qDAT$Zone*qDAT$data

library(bestglm)

# The outcome variable must be named y, no extraneous variables should be present in the dataset.

qDAT.for.bestglm <- within(qDAT, {
  y    <- cull         # cull into y
  cull  <- NULL        # Delete bwt
})

# Perform all-subset binomial regression based on Akaike Information Criteria (AIC)

res.bestglm <-
  bestglm(Xy = qDAT.for.bestglm,
          family = binomial,
          IC = "AIC",                 # Information criteria for
          method = "exhaustive")

# Show result for the best model

res.bestglm$BestModels

summary(res.bestglm$BestModel)





# glmulti
# Automated model selection and model-averaging. Provides a wrapper for glm and other functions, automatically generating all possible models (under constraints set by the user) with the specified response and explanatory variables, and finding the best models in terms of some Information Criterion (AIC, AICc or BIC). Can handle very large numbers of candidate models. Features a Genetic Algorithm to find the best models when an exhaustive screening of the candidates is not feasible.

library(glmulti)

qDAT <- read.csv("qDAT2.txt", header = TRUE, sep = "\t")
qDAT <- qDAT[complete.cases(qDAT),]
qDAT$ZoDa <- qDAT$Zone*qDAT$data

# All-subset logistic regression using lm() based on AIC

glmulti.logistic.out <-
  glmulti(cull ~ ., data = qDAT,
          level = 1,               # No interaction considered
          method = "h",            # Exhaustive approach
          crit = "aic",            # AIC as criteria
          confsetsize = 5,         # Keep 5 best models
          plotty = F, report = F,  # No plot or interim reports
          fitfunction = "glm",     # glm function
          family = binomial)       # binomial family for logistic regression

## Show 5 best models (Use @ instead of $ for an S4 object)
glmulti.logistic.out@formulas

## Show result for the best model
summary(glmulti.logistic.out@objects[[1]])



#AIC
#set max print to limit output of final model#

library(MuMIn)

options(max.print=500) 

mDAT1.glm  <- glm(cull ~  ., data = qDAT, family=binomial(link=probit))

vif(mDAT1.glm)
sqrt(vif(mDAT1.glm)) > 2 

summary (mDAT1.glm)

options(na.action = "na.fail") 


#AIC#
#model.aic <- dredge(model.glm, rank = "AIC")
#model.aic <- as.data.frame(model.aic)
#model.aic
#model.aic$delta <- shows all delta values
#model.aic$AIC <- shows all AIC values
#Construct composite GLM for final models

mDAT1.aic <- dredge(mDAT1.glm, rank = "AIC")
mDAT1.aic <- as.data.frame(mDAT1.aic)
mDAT1.aic

write.csv(mDAT1.aic, file="all_var.csv")


mDAT.top <- glm(cull ~ hunt + petition + supp.gvt + ZoDa + Zone, data = qDAT, family=binomial(link=probit))

summary (mDAT.top)

anova(mDAT.top, mDAT1.glm, test = "F")

library(Deducer)

rocplot(mDAT.top, AUC = TRUE)

qDAT$Match <- as.numeric(qDAT$cull == qDAT$supp.gvt)