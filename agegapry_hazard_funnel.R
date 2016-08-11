library("RSimpactCyan")
library("data.table")
# The data.table package has to be installed with install.packages("data.table", repos="http://research.edm.uhasselt.be/jori")
library("extrafont")

# This is a helper function that runs a simulation, analyzes the generated data
# and creates a plot from it: a point in the figure indicates a relationship,
# the X-axis is the age of the man when the relationship was formed and the Y-axis
# is the relationship of the woman. A line is also fitted to these formed
# relationships and is shown as a red line. The green line is just the diagonal:
# if everyone formed relationships only with people of the same age, all points
# would lie on this green line.

path = "C:/Users/lucp9040/Documents/SIMPACT_manual_examples/simptest5"
seed = -1

showAges <- function(cfg, path, seed){
  ret <- simpact.run(cfg, path, seed=seed)
  rel <- fread(ret$logrelations)
  ppl <- fread(ret$logpersons)
  numRel = dim(rel)[1]
  agesMen = list()
  agesWomen = list()
  for (r in 1:numRel){
    idm = rel$ID1[r]
    idw = rel$ID2[r]
    t = rel$FormTime[r]
    men = subset(ppl,ppl$ID == idm)
    women = subset(ppl,ppl$ID == idw)
    tobm = men$TOB
    tobw = women$TOB
    agesMen[[r]] <- t-tobm
    agesWomen[[r]] <- t-tobw
  }
  par(pty="s")
  plot(agesMen,agesWomen,type = "p",col = "blue",pty = "s", xlab = "Age man", ylab = "Age woman", xlim = c(0,100), ylim = c(0,100))
  lines(c(0,100),c(0,100),col="green") # Plot the diagonal
}

# First, we'll run a simulation with all the default settings. This uses
# the 'simple' formation hazard with settings that do not specify any 
# particular age difference.
cfg <- list()
showAges(cfg,path,seed) 

# In the following example, we're going to use the 'agegapry' hazard. To be able to
# use this, we also need to specify that the reference year is synchronized periodically.
# By default, the 'agegapry' hazard expects that this sync is performed at least yearly
# (the default value of 'formation.hazard.agegapry.maxageref.diff' is 1), so we'll set
# 'syncrefyear.interval' to 1 as well.
#
# In the hazard, we're not going to change the slope of the resulting distribution, but
# we're going to set the parameters that make the importance of the age gap term dependent
# on the age of the person. This way, we get a funnel-like shape of the distribution.
cfg <- list()
cfg["population.simtime"] <- 50
cfg["population.nummen"] <- 400
cfg["population.numwomen"] <- 400
cfg["syncrefyear.interval"] <- 1
cfg["formation.hazard.type"] <- "agegapry"
cfg["formation.hazard.agegapry.baseline"] <- 0.1
cfg["formation.hazard.agegapry.gap_factor_man_const"] <- 0
cfg["formation.hazard.agegapry.gap_factor_man_exp"] <- -0.7
cfg["formation.hazard.agegapry.gap_factor_man_age"] <- -0.06
cfg["formation.hazard.agegapry.gap_factor_woman_const"] <- 0
cfg["formation.hazard.agegapry.gap_factor_woman_exp"] <- -0.7
cfg["formation.hazard.agegapry.gap_factor_woman_age"] <- -0.06
showAges(cfg,path,seed)

# This is a simular example as before, but instead of getting less picky as they
# age, in this simulation people will put more importance in the age difference
# as they get older.
cfg <- list()
cfg["population.simtime"] <- 50
cfg["population.nummen"] <- 400
cfg["population.numwomen"] <- 400
cfg["syncrefyear.interval"] <- 1
cfg["formation.hazard.type"] <- "agegapry"
cfg["formation.hazard.agegapry.baseline"] <- 0.1
cfg["formation.hazard.agegapry.gap_factor_man_const"] <- 0
cfg["formation.hazard.agegapry.gap_factor_man_exp"] <- -0.05
cfg["formation.hazard.agegapry.gap_factor_man_age"] <- 0.03
cfg["formation.hazard.agegapry.gap_factor_woman_const"] <- 0
cfg["formation.hazard.agegapry.gap_factor_woman_exp"] <- -0.05
cfg["formation.hazard.agegapry.gap_factor_woman_age"] <- 0.03
showAges(cfg,path,seed)
