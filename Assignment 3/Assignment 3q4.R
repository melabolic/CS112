# Question 4 ####
## Finding the column numbers for WAREND2 and WAREND5
foo2 <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")

grep('pbs5l', colnames(foo2)) # 33 is the numerical version
grep('pbs5l', colnames(foo2)) # 35
grep('uncint', colnames(foo2)) # 52
foo2 <- foo2[,c(6:8, 11:16, 33, 35, 52, 99, 50, 114, 49, 63, 136, 109, 126, 48, 160, 142, 10, 108)]

# Removing columns with NA
foo2 <- foo2[c(-4, -16, -19, -47, -84, -93, -98),]

# Checking the table again
which(is.na(foo2) == TRUE)

Tr <- rep(0, length(foo2$uncint))
Tr[which(foo2$uncint != "None")] <- 1
foo2 <- cbind(foo2, Tr)
levels(foo2$pbs2l) <- c(0, 1)

######################### For 2 years: pbs5l #########################

# Running the logistic regression
glm.2l <- glm(pbs2ll ~ Tr+wartype+logcost+wardur+factnum+factnum2+trnsfcap
              +untype4+treaty+develop+exp+decade, data=foo2, family="binomial")
# to find the treatment effect, we simply need the difference between
# the mean of treatment - the mean of control
treat_2l <- foo2[which(foo2$Tr == 1),]
control_2l <- foo2[which(foo2$Tr == 0),]
pred_treat_2l <- predict(glm.2l, treat_2l, type="response")
pred_control_2l <- predict(glm.2l, control_2l, type="response")
tmt_2l <- mean(pred_treat_2l) - mean(pred_control_2l)
tmt_2l

# Running propensity-score matching
pscore_2l <- glm(Tr~wartype+logcost+wardur+factnum+factnum2+trnsfcap
                 +treaty+develop+exp+decade, data=foo2, family="binomial")
mout_2l <- Match(Tr=foo2$Tr, X=pscore_2l$fitted, M=1, replace=TRUE)
summary(mout_2l)
mb_2l <- MatchBalance(Tr~wartype+logcost+wardur+factnum+factnum2+trnsfcap
                      +treaty+develop+exp+decade, data=foo2, match.out=mout_2l,
                      nboots=500)
# Once satisfied with the output, we can then run BiasAdjust to find the regression-
# adjusted results
bias_adjust2l <- Match(Y=foo2$pbs2ll, Tr=foo2$Tr, X=pscore_2l$fitted, M=1, 
                       BiasAdjust=TRUE, replace=TRUE)
summary(bias_adjust2l)
bias_adjust2l$est; bias_adjust2l$est.noadj

# Running the genetic matching algorithm
X2 <- cbind(foo2$wartype, foo2$logcost, foo2$wardur, foo2$factnum, foo2$factnum2,
            foo2$trnsfcap, foo2$treaty, foo2$develop, foo2$exp, foo2$decade)
genout_2l <- GenMatch(Tr=foo2$Tr, X=X2, estimand="ATT", M=2, pop.size=300, 
                      max.generations=50, wait.generations=5)
genmout_2l <- Match(Tr=foo2$Tr, X=X2, estimand="ATT", Weight.matrix=genout_2l,
                    M=2, replace=TRUE)
summary(genmout_2l)
genmb_2l <- MatchBalance(Tr~wartype+logcost+wardur+factnum+factnum2+trnsfcap
                         +treaty+develop+exp+decade, data=foo2, match.out=genmout_2l,
                         nboots=500)
# Adding BiasAdjust
gen_ba_2l <- Match(Y=foo2$pbs2ll, Tr=foo2$Tr, X=X2, estimand="ATT",
                   Weight.matrix=genout_2l, M=1, BiasAdjust=TRUE, replace=TRUE)
summary(gen_ba_2l)
gen_ba_2l$est; gen_ba_2l$est.noadj

######################### For 5 years: pbs5l #########################
# Running the logistic regression
glm.5l <- glm(pbs5l ~ Tr+wartype+logcost+wardur+factnum+factnum2+trnsfcap
              +untype4+treaty+develop+exp+decade, data=foo2, family="binomial")
# to find the treatment effect, we simply need the difference between
# the mean of treatment - the mean of control
treat_5l <- foo2[which(foo2$Tr == 1),]
control_5l <- foo2[which(foo2$Tr == 0),]
pred_treat_5l <- predict(glm.5l, treat_5l, type="response")
pred_control_5l <- predict(glm.5l, control_5l, type="response")
tmt_5l <- mean(pred_treat_5l) - mean(pred_control_5l)
tmt_5l

# Running propensity-score matching
pscore_5l <- glm(Tr~wartype+logcost+wardur+factnum+factnum2+trnsfcap
                 +treaty+develop+exp+decade, data=foo2, family="binomial")
mout_5l <- Match(Tr=foo2$Tr, X=pscore_5l$fitted, M=5, caliper=0.6, replace=TRUE)
summary(mout_5l)
mb_5l <- MatchBalance(Tr~wartype+logcost+wardur+factnum+factnum2+trnsfcap
                      +treaty+develop+exp+decade, data=foo2, match.out=mout_5l,
                      nboots=500)
# Once satisfied with the output, we can then run BiasAdjust to find the regression-
# adjusted results
bias_adjust5l <- Match(Y=foo2$pbs5l, Tr=foo2$Tr, X=pscore_5l$fitted, M=5, caliper=0.6, 
                       BiasAdjust=TRUE, replace=TRUE)
summary(bias_adjust5l)
bias_adjust5l$est; bias_adjust5l$est.noadj

# Running the genetic matching algorithm
X2 <- X2
genout_5l <- GenMatch(Tr=foo2$Tr, X=X2, estimand="ATT", M=2, pop.size=300, 
                      max.generations=50, wait.generations=5)
genmout_5l <- Match(Tr=foo2$Tr, X=X2, estimand="ATT", Weight.matrix=genout_5l,
                    M=2, replace=TRUE)
summary(genmout_5l)
genmb_5l <- MatchBalance(Tr~wartype+logcost+wardur+factnum+factnum2+trnsfcap
                         +treaty+develop+exp+decade, data=foo2, match.out=genmout_5l,
                         nboots=500)
# Adding BiasAdjust
gen_ba_5l <- Match(Y=foo2$pbs5l, Tr=foo2$Tr, X=X2, estimand="ATT",
                   Weight.matrix=genout_5l, M=1, BiasAdjust=TRUE, replace=TRUE)
summary(gen_ba_5l)
gen_ba_5l$est; gen_ba_5l$est.noadj
