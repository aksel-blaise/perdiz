# qualitative data for Perdiz points
library(here)
library(CGPfunctions)
library(lsr)


# read attribute data
data <- as.data.frame(read.csv("perdiz.csv", header = TRUE, as.is = TRUE))

# variables as factors
raw <- as.factor(data$raw.mat) # raw material
con <- as.factor(data$context) # burial context
temp <- as.factor(data$temporal) # temporal period
site <- as.factor(data$trinomial) # site

#####
## raw material by burial context
#comparison tables
r.tab <- table(raw, con)
ftable(r.tab)
summary(r.tab)

# effect size
cramersV(r.tab)

# number of observations
margin.table(r.tab,1) # raw material
margin.table(r.tab,2) # mortuary/no

# percentages
prop.table(r.tab)
prop.table(r.tab,1) # perc by raw.mat (read l/r)
prop.table(r.tab,2) # perc by mortuary/no (read top to bottom)

# plot cross tabs
PlotXTabs(data, context, raw.mat) # side by side
PlotXTabs(data, context, raw.mat, "stack") # stacked
PlotXTabs(data, context, raw.mat, plottype = "percent") # as percentages

#####
## raw material by temporal period
#comparison tables
r.tab2 <- table(raw, temp)
ftable(r.tab2)
summary(r.tab2)

# effect size
cramersV(r.tab2)

# number of observations
margin.table(r.tab2,1) # raw material
margin.table(r.tab2,2) # mortuary/no

# percentages
prop.table(r.tab2)
prop.table(r.tab2,1) # perc by raw.mat (read l/r)
prop.table(r.tab2,2) # perc by mortuary/no (read top to bottom)

# plot cross tabs
PlotXTabs(data, temporal, raw.mat) # side by side
PlotXTabs(data, temporal, raw.mat, "stack") # stacked
PlotXTabs(data, temporal, raw.mat, plottype = "percent") # as percentages

#####
## raw material by site
#comparison tables
r.tab3 <- table(raw, site)
ftable(r.tab3)
summary(r.tab3)

# effect size
cramersV(r.tab3)

# number of observations
margin.table(r.tab3,1) # raw material
margin.table(r.tab3,2) # mortuary/no

# percentages
prop.table(r.tab3)
prop.table(r.tab3,1) # perc by raw.mat (read l/r)
prop.table(r.tab3,2) # perc by mortuary/no (read top to bottom)

# plot cross tabs
PlotXTabs(data, trinomial, raw.mat) # side by side
PlotXTabs(data, trinomial, raw.mat, "stack") # stacked
PlotXTabs(data, trinomial, raw.mat, plottype = "percent") # as percentages
