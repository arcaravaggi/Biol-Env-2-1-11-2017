library(MuMIn)

qDAT <- read.csv("qDAT.txt", header = TRUE, sep = "\t")
qDAT <- qDAT[complete.cases(qDAT),]
qDAT$ZoDa <- qDAT$zone*qDAT$data
qDAT$support <- NULL

#set max print to limit output of final model#

options(max.print=500) 

mDAT1.glm  <- glm(cull ~  ., data = qDAT)
mDAT2.glm  <- glm(cull ~  zone + data + farmer + aware + pests + data*zone, data = qDAT)
mDAT3.glm  <- glm(cull ~  zone + data + pests + threat + aware + data*zone, data = qDAT)
mDAT4.glm  <- glm(cull ~  zone + data + farmer + pests + aware + data*zone, data = qDAT)
mDAT5.glm  <- glm(cull ~  zone + data + farmer + pests + cons + data*zone, data = qDAT)

summary (mDAT1.glm)
summary (mDAT2.glm)
summary (mDAT3.glm)
summary (mDAT4.glm)
summary (mDAT5.glm)


options(na.action = "na.fail") 

#Construct composite GLM for final models

mDAT1.aic <- dredge(mDAT1.glm, rank = "AIC")
mDAT1.aic <- as.data.frame(mDAT1.aic)
mDAT1.aic

write.csv(mDAT1.aic, file="mDAT1.csv")

mDAT2.aic <- dredge(mDAT2.glm, rank = "AIC")
mDAT2.aic <- as.data.frame(mDAT2.aic)
mDAT2.aic

write.csv(mDAT2.aic, file="mDAT2.csv")

mDAT3.aic <- dredge(mDAT3.glm, rank = "AIC")
mDAT3.aic <- as.data.frame(mDAT3.aic)
mDAT3.aic

write.csv(mDAT3.aic, file="mDAT3.csv")

mDAT4.aic <- dredge(mDAT4.glm, rank = "AIC")
mDAT4.aic <- as.data.frame(mDAT4.aic)
mDAT4.aic

write.csv(mDAT4.aic, file="mDAT4.csv")

mDAT5.aic <- dredge(mDAT5.glm, rank = "AIC")
mDAT5.aic <- as.data.frame(mDAT5.aic)
mDAT5.aic

write.csv(mDAT5.aic, file="mDAT5.csv")

#GLM of top models as defined by AIC weighting
#[model] <- glm(Presence ~ [variables], data = [data frame])
#Coefficients are Beta values
#Library (car)
#Anova([glm], Type = "III", test.statistic = F)

mDAT6.glm  <- glm(cull ~  zone + data + threat, data = qDAT)
mDAT7.glm  <- glm(cull ~  zone + data + support, data = qDAT)
mDAT8.glm  <- glm(cull ~  zone + data, data = qDAT)

summary (mDAT6.glm)
summary (mDAT7.glm)
summary (mDAT8.glm)

anova(mDAT7.glm, mDAT6.glm, test = "F")


#############################################
#
# Full dataset
# 
#############################################

qDAT2 <- read.csv("qDAT2.txt", header = TRUE, sep = "\t")

qDAT2 <- na.omit(qDAT2)

library(leaps)
library(car)

attach(qDAT2)

# identify top models for n variables

model1<-regsubsets(cull~Zone+data+land+farm+seen+pests+aware+seen.euro
                  +member+supp.mgmt+threat+cons+hunt,data=qDAT2,
                  nvmax = NULL, nbest=1, force.in = NULL, force.out = NULL,
                  method = "exhaustive")
model1
summary.out <- summary(model1)
as.data.frame(summary.out$outmat)
plot(model1,scale="r2")
subsets(model1, statistic="rsq")

library(MASS)

fit <- lm(cull~Zone+data+land+farm+seen+pests+aware+seen.euro
          +member+supp.mgmt+petition+threat+cons+hunt,data=qDAT2)
step <- stepAIC(fit, direction="both")
step$anova # display results

# create top models and identify best performing model via AIC
model.null <- glm(cull~Zone+data+land+farm+seen+pests+aware+seen.euro
               +member+supp.mgmt+petition+threat+cons+hunt+Zone*data, data=qDAT2)
model1 <- glm(cull~ petition, data = qDAT2) 
model2 <- glm(cull~ petition+hunt, data = qDAT2) 
model3 <- glm(cull~ petition+hunt+supp.mgmt, data = qDAT2) 
model4 <- glm(cull~ petition+hunt+supp.mgmt+aware, data = qDAT2) 
model5 <- glm(cull~ petition+hunt+supp.mgmt+aware+cons, data = qDAT2) 

summary(model5)

anova(model.null, model5, test = "F")


# Create subset of models based on null and top models and export to calculate variable importance
model.null.aic <- dredge(model.null, rank = "AIC")
model.null.aic <- as.data.frame(model.null.aic)
model.null.aic

write.csv(model.null.aic, file="modelnull.csv")

model5.aic <- dredge(model5, rank = "AIC")
model5.aic <- as.data.frame(model5.aic)
model5.aic

write.csv(model5.aic, file="model5.csv")



# T tests between stakeholder group and variables
sink("T-tests.txt")    #write to file

t.test(data ~ land, data = qDAT2)
t.test(data ~ farm, data = qDAT2)
t.test(data ~ seen, data = qDAT2)
t.test(data ~ pests, data = qDAT2)
t.test(data ~ aware, data = qDAT2)
t.test(data ~ seen.euro, data = qDAT2)
t.test(data ~ member, data = qDAT2)
t.test(data ~ supp.mgmt, data = qDAT2)
t.test(data ~ petition, data = qDAT2)
t.test(data ~ threat, data = qDAT2)
t.test(data ~ cons, data = qDAT2)
t.test(data ~ cull, data = qDAT2)
t.test(data ~ hunt, data = qDAT2)
t.test(data ~ supp.gvt, data = qDAT2)

sink()

#Binomial test for differences between responses

binom.test(x=144, n=343, p=.56, alternative="two.sided")




#Correlation analyses

attach(qDAT2)
cor.test(cull, supp.gvt, method = "pearson")
cor.test(cull, hunt, method = "pearson")

#Best approximating model AUC

library(Deducer)
modelfit <- glm(formula=cull~ petition+hunt+supp.mgmt+aware+cons, data=qDAT2, na.action=na.omit)
rocplot(modelfit, AUC = TRUE)
summary(modelfit)

library(pROC)
prob=predict(model5,type=c("response"))
qDAT2$prob=prob
g <- roc(cull ~ prob, data = qDAT2)
plot(g)  

options(na.action = "na.omit")
