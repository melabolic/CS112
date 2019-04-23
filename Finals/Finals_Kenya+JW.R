### Declaring the required libraries
library(Synth)

# set your working directory below:
setwd("./Finals")

# importing the dataset
load("scdata.RData")
View(scdata)

# defining the predictors
pred <- names(scdata)[c(
  7, # pop65+
  8, # pop15-	
  11, # years in current term
  12, # unified government
  13, # legislative election
  14, # pluralty rule
  15, # proportional representation
  #20, # labor productivity (annual growth rate)
  21, # health expenditure/GDP
  22, # GDP expenditure approach
  24, # taxrev %GDP, no Social Security
  25, # CO2 emissions
  31, # unemployment (World Bank)	
  34, # openness (expenditure)
  36, # PolconIII
  38, # Potrafke ideology
  39, # Majority margin
  41, #lag debt/gdp (RR)	
  42 # Rae Fractionalisation index (government)
)]

# defining the Euro12 countries
Euro12 <- c('Germany','Netherlands','Greece','Spain','Portugal','Italy',
            'Finland','France','Luxembourg','Belgium','Austria','Ireland')

### creating the synthetic control group
contr <- sort(unique(scdata$ccode[is.element(scdata$country,
                                             setdiff(scdata$country,
                                                     c(Euro12,"Euro 11","Slovenia")))]))

# excluding the countries with data constraints (missing values)
contr <- setdiff(contr, c(1111,2222,70,732,155,225,269,290,
                          310,316,317,349,355,360,366,666,
                          sort(unique(scdata$ccode[scdata$eu==0 & scdata$oecd==0]))))
country <- sort(unique(scdata$country[scdata$ccode %in% contr]))

### Creating the function that runs the entire Synthetic Control
synth_func <- function(dataset, country_string, predictors, controls, pred_dur1, pred_dur2, unit_id) {
  return(dataprep(foo = dataset[dataset$ccode %in% controls | dataset$country == country_string,],
           predictors = predictors,
           dependent = names(dataset[6]),
           unit.variable = "ccode",
           time.variable = "Year",
           treatment.identifier = unit_id,
           controls.identifier = controls,
           time.predictors.prior = pred_dur1,
           time.optimize.ssr = pred_dur2,
           unit.names.variable = "country",
           time.plot = 1983:2010))
}

graph_plot <- function(synth_output, sdata, legend1, legend2, tmt_year, type, country=NULL) {
  if (type == 1) { # plots regular graphs
    path.plot(synth.res = synth_output, 
              dataprep.res = sdata,
              Main = paste(legend1, "Debt/GDP"),
              Ylab="Debt/GDP (Nominal)",
              Xlab="Year",
              Legend=c(legend1,legend2),
              Legend.position="topleft", abline(v=tmt_year,lty="dashed"))
  }
  else if (type == 2) { # plots placebo-in-time
    path.plot(synth.res = synth_output, 
              dataprep.res = sdata,
              Main = paste("Placebo-in-time for", legend1),
              Ylab="Debt/GDP (Nominal)",
              Xlab="Year",
              Legend=c(legend1,legend2),
              Legend.position="topleft", abline(v=tmt_year,lty="dashed"))
  }
  else if (type == 3) { # plots placebo-in-space
    path.plot(synth.res = synth_output, 
              dataprep.res = sdata,
              Main = paste("Placebo-in-space for", legend1),
              Ylab="Debt/GDP (Nominal)",
              Xlab="Year",
              Legend=c(legend1,legend2),
              Legend.position="topleft", abline(v=tmt_year,lty="dashed"))
  }
  else { # plots for sensitivity test
    path.plot(synth.res = synth_output, 
              dataprep.res = sdata,
              Main = paste("Synthetic Pool without", country),
              Ylab="Debt/GDP (Nominal)",
              Xlab="Year",
              Legend=c(legend1,legend2),
              Legend.position="topleft", abline(v=tmt_year,lty="dashed"))
  }
}

#################### FOR DONOR COUNTRIES ##################

### Replicating Figure 4a
donor_data <- synth_func(scdata, "donor countries", pred, contr, c(1983:1998), c(1983:1999), 1111)

# Run the synthetic control analysis:
synth.out <- synth(data.prep.obj = donor_data, method = "BFGS")

# calculate output gaps from the results
gaps <- donor_data$Y1plot - (donor_data$Y0plot %*% synth.out$solution.w)
synth.tables <- synth.tab(dataprep.res = donor_data, synth.res=synth.out)

# Plotting the figure for the donor countries
graph_plot(synth.out, donor_data, "Donor Countries", "Synthetic Donor Countries", 1999, 1)
# Plot the gap of the Debt to GDP ratio for the Euro11 and the Synthetic control
gaps.plot(synth.res = synth.out, dataprep.res = donor_data,
          Ylab= "Gap in Debt/GDP (percentage points, 1983-2010)", Xlab="Year",
          Main=NA, abline(v=1999,lty="dashed"))

# extract Country weights from Synthetic control 
a <- cbind(synth.tables$tab.w[,1],synth.tables$tab.w[,2])
row.names(a) <- synth.tables$tab.w[,2]
dotchart(a[,1],pch=16)

### Placebo-in-time 
d.data1 <- synth_func(scdata, "donor countries", pred, contr, c(1983:1995), c(1983:1995), 1111)
# we can't really run placebos-in-time earlier than 1996 because there's no 
# data for deltaLP, or the annual growth of labor productivity. We're using
# 1997 here because it allows for 2 years before assessing the treatment compared
# to just 1 if we used 1996.

# Run the synthetic control analysis:
synth.d.out1 <- synth(data.prep.obj = d.data1, method = "BFGS")
graph_plot(synth.d.out1, d.data1, "Donor Countries", "Synthetic Donor Countries", 1995, 2)


#################### FOR RECEPIENT COUNTRIES ##################

### Replicating Figure 5a
rec_data <- synth_func(scdata, "recipient countries", pred, contr, c(1983:1998), c(1983:1999), 2222)

# Run the synthetic control analysis:
synth.out2 <- synth(data.prep.obj = rec_data, method = "BFGS")

# calculate output gaps from the results
gaps2 <- rec_data$Y1plot - (rec_data$Y0plot %*% synth.out2$solution.w)
synth.tables2 <- synth.tab(dataprep.res = rec_data, synth.res=synth.out2)

# Plotting the figure for the donor countries
graph_plot(synth.out2, rec_data, "Recipient Countries", "Synthetic Rec. Countries", 1999, 1)

# Plot the gap of the Debt to GDP ratio for the Euro11 and the Synthetic control
gaps.plot(synth.res = synth.out2, dataprep.res = rec_data,
          Ylab= "Gap in Debt/GDP (percentage points, 1983-2010)", Xlab="Year",
          Main=NA, abline(v=1999,lty="dashed"))

# extract Country weights from Synthetic control 
b <- cbind(synth.tables2$tab.w[,1],synth.tables2$tab.w[,2])
row.names(b) <- synth.tables2$tab.w[,2]
dotchart(b[,1],pch=16)

### Placebo-in-time
r.data1 <- synth_func(scdata, "recipient countries", pred, contr, c(1983,1995), c(1983,1995), 2222)
# the earliest entry for deltaLP is 1990 in this case, so we can use 1995. We can 
# also change it to 1997 for consistency purposes. Weights for countries here
# are different than the original treatment in 1999.
synth.r.out1 <- synth(data.prep.obj = r.data1, method = "BFGS")
graph_plot(synth.r.out1, r.data1, "Recipient Countries", "Synthetic Rec. Countries", 1995, 2)

### Placebo-in-space
# Using Norway as the Placebo country
for (i in contr) { 
  cntry <- unique(scdata$country[scdata$ccode == i])
  tmp <- setdiff(contr, c(i,sort(unique(scdata$ccode[scdata$eu==0 & scdata$oecd==0]))))
  ps.data <- synth_func(scdata, cntry, pred, tmp, c(1983:1998), c(1983:1999), i)
  ps.synth.out <- synth(data.prep.obj = ps.data, method = "BFGS")
  graph_plot(ps.synth.out, ps.data, cntry, paste("Synthetic",cntry), 1999, 3)
}
# This section of code literally just creates a plot for all the countries
# listed in the controls pool (contr).

############## SENSITIVITY TESTS ###############

# For donor countries
for (i in contr) {
  cntry <- unique(scdata$country[scdata$ccode == i])
  tmp <- setdiff(contr, c(i,sort(unique(scdata$ccode[scdata$eu==0 & scdata$oecd==0]))))
  ps.data <- synth_func(scdata, "donor countries", pred, tmp, c(1983:1998), c(1983:1999), 1111)
  ps.synth.out <- synth(data.prep.obj = ps.data, method = "BFGS")
  graph_plot(ps.synth.out, ps.data, "Donor Countries", "Synthetic Donor Countries", 1999, 4, cntry)
}

# For recipient countries
# Since Canada is the only real country that was used to construct the synthetic recipient
# countries, we measure the impact of removing canada.
tmp <- setdiff(contr, c(20,sort(unique(scdata$ccode[scdata$eu==0 & scdata$oecd==0]))))
ps.data <- synth_func(scdata, "recipient countries", pred, tmp, c(1983:1998), c(1983:1999), 2222)
ps.synth.out <- synth(data.prep.obj = ps.data, method = "BFGS")
synth.d.table2 <- synth.tab(dataprep.res = ps.data, synth.res=ps.synth.out)
synth.d.table2$tab.w # To see the weights of the new countries that make up the Synth group
graph_plot(ps.synth.out, ps.data, "Recipient Countries", "Synthetic Rec. Countries", 1999, 4, "Canada")

