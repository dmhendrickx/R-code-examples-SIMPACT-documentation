library("RSimpactCyan")
library("data.table")
# The data.table package has to be installed with install.packages("data.table", repos="http://research.edm.uhasselt.be/jori")

# We'll run a simulation with 1000 men and 1000 women. To make the relation
# above easy to see, we're going to disable treatment by setting the acceptance
# threshold to zero
cfg <- list()
cfg["population.simtime"] <- 100
cfg["population.nummen"] <- 1000
cfg["population.numwomen"] <- 1000
cfg["person.art.accept.threshold.dist.type"] <- "fixed"
cfg["person.art.accept.threshold.dist.fixed.value"] <- 0 # Make sure a person never accepts treatment
r <- simpact.run(cfg, "C:/Users/lucp9040/Documents/SIMPACT_manual_examples/simptest2")

# This is the line with t_surv = C/Vsp^(-k), on a log-log plot
C = 1325.0
k = -0.49
logVL=seq(2,6,by=2)
logTS=log10(C)+k*logVL

# Read the person log from the simulation output
persons=fread(r$logpersons)

# We're only interested in people that got infected by transmission, not by
# the HIV seeding event (InfectType == 1), in persons who actually have died
# (TOD < inf) and who did not receive treatment (TreatTime == inf, which is
# actually always the case since no person will accept treatment)
filteredPersons <- subset(persons,InfectType == 1 & TOD < Inf & TreatTime == Inf)

# The survival time is the difference between the time of death and the time
# infection took place. We're going to take the logarithm of this.
# The survival time is the difference between the time of death and the time
# infection took place. We're going to take the logarithm of this.
survTime = log10(filteredPersons$TOD - filteredPersons$InfectTime)

# We're going to plot this logarithm of the survival time against the logarithm
# of the set-point viral load
SPVL = filteredPersons$log10SPVL
plot(SPVL, survTime, type = "p", col = "blue",xlim=c(2,6.5),ylim=c(0,2.5))
lines(logVL,logTS,col="green")

# Here, we'll repeat the procedure from above, but we're also going to specify that
# we only want to plot the survival time for people who died from AIDS (AIDSDeath == 1)
filteredPersons <- subset(persons,InfectType == 1 & TOD < Inf & TreatTime == Inf & AIDSDeath == 1)
survTime = log10(filteredPersons$TOD - filteredPersons$InfectTime)
SPVL = filteredPersons$log10SPVL
plot(SPVL, survTime, type = "p", col = "blue",xlim=c(2,6.5),ylim=c(0,2.5))
lines(logVL,logTS,col="green")

# In the following simulation, we're going to specify that for a specific person
# the 'x' parameter in the formula at the top should be picked from a normal
# distribution with a width of 0.1. 
cfg <- list()
cfg["population.simtime"] <- 100
cfg["population.nummen"] <- 1000
cfg["population.numwomen"] <- 1000
cfg["person.survtime.logoffset.dist.type"] <- "normal"
cfg["person.survtime.logoffset.dist.normal.mu"] <- 0
cfg["person.survtime.logoffset.dist.normal.sigma"] <- 0.1
cfg["person.art.accept.threshold.dist.type"] <- "fixed"
cfg["person.art.accept.threshold.dist.fixed.value"] <- 0 # Make sure a person never accepts treatment
r <- simpact.run(cfg, "C:/Users/lucp9040/Documents/SIMPACT_manual_examples/simptest2")

# If we now create the same plot as before, the survival times will no
# longer lie precisely on a line (in the log-log plot), but will show
# some variation
persons=fread(r$logpersons)
filteredPersons <- subset(persons,InfectType == 1 & TOD < Inf & TreatTime == Inf & AIDSDeath == 1)
survTime = log10(filteredPersons$TOD - filteredPersons$InfectTime)
SPVL = filteredPersons$log10SPVL
plot(SPVL, survTime, type = "p", col = "blue",xlim=c(2,6.5),ylim=c(0,2.5))
lines(logVL,logTS,col="green")