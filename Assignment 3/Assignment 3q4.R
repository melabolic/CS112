# Question 4 ####
## Finding the column numbers for WAREND2 and WAREND5
foo2 <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")

grep('pbs2l', colnames(foo2)) # 33/34
grep('pbs5l', colnames(foo2)) # 35
grep('uncint', colnames(foo2)) # 52
foo2 <- foo2[,c(6:8, 11:16, 33, 35, 52, 99, 50, 114, 49, 63, 136, 109, 126, 48, 160, 142, 10, 108)]

# Removing columns with NA
foo2 <- foo2[c(-4, -16, -19, -47, -84, -93, -98),]

# Checking the table again
which(is.na(foo2) == TRUE)

# Running the logistic regression
glm3 <- glm()

Tr <- rep(0, length(foo2$uncint))
Tr[which(foo2$uncint != "None")] <- 1
