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

path = "C:/Users/lucp9040/Documents/SIMPACT_manual_examples/simptest4"
seed = -1

showAges <- function(cfg, path, seed){
  ret <- simpact.run(cfg, path, seed=seed)
  rel <- fread(ret$logrelations)
  ppl <- fread(ret$logpersons)
  numRel = dim(rel)[1]
  agesMen = list()
  agesWomen = list()
  for (r in 1:numRel){
    idm = rel$IDm[r]
    idw = rel$IDw[r]
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
  # Also perform a linear fit and plot the line
  agesM=unlist(agesMen)
  agesW=unlist(agesWomen)
  model=lm(formula = agesW ~ agesM)
  intercept=model$coefficients[1]
  slope=model$coefficients[2]
  x=c(0,100)
  y=intercept + x*slope
  lines(x,y,col="red")
  print(paste0("intercept = ",intercept))
  print(paste0("slope = ",slope))      
}

# First, we'll run a simulation with all the default settings. This uses
# the 'simple' formation hazard with settings that do not specify any 
# particular age difference.
cfg <- list()
showAges(cfg,path,seed)  

# Next, we're going to run a simulation with the 'agegap' hazard. By default
# the preferred age gap is 0, and by controlling 'formation.hazard.agegap.gap_factor_man'
# and 'formation.hazard.agegap.gap_factor_woman' we can control the width of the
# resulting distribution. Using the settings below, only relationships in which
# both partners have approximately the same age will be formed.
cfg <- list()
cfg["population.simtime"] <- 50
cfg["population.nummen"] <- 200
cfg["population.numwomen"] <- 200
cfg["formation.hazard.type"] <- "agegap"
cfg["formation.hazard.agegap.baseline"] <- 0
cfg["formation.hazard.agegap.gap_factor_man"] <- -0.3
cfg["formation.hazard.agegap.gap_factor_woman"] <- -0.3
showAges(cfg,path,seed) 

# This is nearly the same simulation as the previous one, but here the
# weight of the agegap term is different, resulting in a broader
# distribution.
cfg <- list()
cfg["population.simtime"] <- 50
cfg["population.nummen"] <- 200
cfg["population.numwomen"] <- 200
cfg["formation.hazard.type"] <- "agegap"
cfg["formation.hazard.agegap.baseline"] <- 0
cfg["formation.hazard.agegap.gap_factor_man"] <- -0.075
cfg["formation.hazard.agegap.gap_factor_woman"] <- -0.075
showAges(cfg,path,seed) 

# In the following simulation, we'll use the settings for the narrower
# distribution again, but we'll also specify a preferred age gap of 15
# years, both for men and for women. This will cause the distribution to
# be offset from the diagonal
cfg <- list()
cfg["population.simtime"] <- 50
cfg["population.nummen"] <- 200
cfg["population.numwomen"] <- 200
cfg["formation.hazard.type"] <- "agegap"
cfg["formation.hazard.agegap.baseline"] <- 0
cfg["formation.hazard.agegap.gap_factor_man"] <- -0.3
cfg["formation.hazard.agegap.gap_factor_woman"] <- -0.3
cfg["person.agegap.man.dist.fixed.value"] <- 15
cfg["person.agegap.woman.dist.fixed.value"] <- 15
showAges(cfg,path,seed) 

# Finally, using 'formation.hazard.agegap.gap_agescale_man' and
# 'formation.hazard.agegap.gap_agescale_woman', the preferred age difference
# becomes age dependent, and the slope of the resulting distribution will
# clearly be different than the one of the green line.
cfg <- list()
cfg["population.simtime"] <- 50
cfg["population.nummen"] <- 200
cfg["population.numwomen"] <- 200
cfg["formation.hazard.type"] <- "agegap"
cfg["formation.hazard.agegap.baseline"] <- 0
cfg["formation.hazard.agegap.gap_factor_man"] <- -0.3
cfg["formation.hazard.agegap.gap_agescale_man"] <- 0.3
cfg["formation.hazard.agegap.gap_factor_woman"] <- -0.3
cfg["formation.hazard.agegap.gap_agescale_woman"] <- 0.3
showAges(cfg,path,seed) 
