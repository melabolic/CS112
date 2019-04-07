# Question 2
foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")

# extract relevant columns
# Including 'logdead'
grep('logdead', colnames(foo))
foo <- foo[, c(6:8, 11:16, 99, 50, 114, 49, 63, 136, 109, 126, 48, 160, 142, 10, 108)]

# remove 2 rows with missing data (there are better ways to handle missing data)
foo <- foo[c(-19, -47), ]

# check that all missing data is gone...
which(is.na(foo) == TRUE)

### Outcome is "pbs2s3": "democracy" and "peace" within 2 years after the end of the war
### Treatment indicator is "untype4": "multidimensional peacekeeping/peacebuilding"

# Original Model
glm1 <- glm(pbs2s3~wartype+logcost+wardur+factnum+factnum2+trnsfcap
            +untype4+treaty+develop+exp+decade, data=foo, family="binomial")
summary(glm1)

# Model with new Interaction Term
glm2 <- glm(pbs2s3~wartype+logcost+wardur+factnum+factnum2+trnsfcap
            +untype4+treaty+develop+exp+decade+I(logdead*untype4), data=foo, family="binomial")
summary(glm2)

### Holding constant all variables at their means except untype4 (which indicates
### the treatment or control) and wardur
df.names<- c('wartype','logcost','wardur', 'factnum', 'factnum2','trnsfcap',
             'untype4', 'treaty','develop','exp', 'decade', 'logdead')

df.treat<- data.frame(row.names=1:320)
df.control<- data.frame(row.names=1:320)

for (i in df.names) {
  if (i == 'untype4') {
    df.treat <- cbind(df.treat, rep(1, 320))
    df.control <- cbind(df.control, rep(0, 320))
  }
  else if (i == 'wardur') {
    df.treat <- cbind(df.treat, rep(1:320))
    df.control <- cbind(df.control, rep(1:320))
  }
  else {
    df.treat <- cbind(df.treat, rep(mean(foo[,i], na.rm = TRUE), 320))
    df.control <- cbind(df.control, rep(mean(foo[,i], na.rm = TRUE), 320))
  }
}

# Setting the names of the columns in the 2 new data frames
names(df.treat) <- df.names
names(df.control) <- df.names

# View each data frame
View(df.treat)
View(df.control)

# Finding the treatment effects of the predictions made from both the logit models
glm1.pred.treat = predict(glm1, newdata = df.treat, type = "response")
glm1.pred.control = predict(glm1, newdata = df.control, type = "response")
glm1.tmteffect = glm1.pred.treat - glm1.pred.control
print(glm1.tmteffect)

glm2.pred.treat = predict(glm2, newdata = df.treat, type = "response")
glm2.pred.control = predict(glm2, newdata = df.control, type = "response")
glm2.tmteffect = glm2.pred.treat - glm2.pred.control
print(glm2.tmteffect)

# Plotting the two results
plot(df.treat$wardur, glm1.tmteffect, type = "l", lty = 3,
     main= "Causal Effect of Multidimensional UN Peacekeeping Operations",
     ylab= "Marginal Effects of UN Peacekeeping Operations",
     xlab = "Duration of war in months",
     xlim = c(0,320),
     ylim = c(0.0, 0.8))
lines(df.treat$wardur, glm2.tmteffect, type = "l", lty = 1)

legend(x = "topright", c("Original Model", "Model with Interaction Term"),
       lty = c(3,1), lwd = c(1, 1), cex = 0.5) 





