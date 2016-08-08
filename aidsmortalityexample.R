library("RSimpactCyan")

solveForVsp <- function(t,C,k){
  (as.double(C)/t)^(-1/k)
}

t_before = 10.0
t_after = 50.0
C = 1000
k= -0.5
Vsp_before = solveForVsp(t_before, C, k)
Vsp_after = solveForVsp(t_after, C, k)

#EXAMPLE 1: INFECTION ONLY

cfg <- list()
# To focus on the AIDS mortality event we'll just use a population that consists
# of one man
cfg["population.nummen"] <- 1
cfg["population.numwomen"] <- 0
# By default, at the start of the simulation a number of people will be infected
# with HIV. We set the fraction to 100%, so that the only member of our population
# will certainly get infected when the simulation starts.
cfg["hivseed.fraction"] <- 1
# By setting the age scale for the normal (non-AIDS) mortality event to something
# very large and by setting the simulation time to something large as well, we can
# be sure that the simulation will only stop when the one person in it dies from
# AIDS
cfg["mortality.normal.weibull.scale"] <- 1000
cfg["population.simtime"] <- 1000
# Here, we set the values of the C and k parameters that we defined earlier
cfg["mortality.aids.survtime.C"] <- C
cfg["mortality.aids.survtime.k"] <- k
# To make sure that upon infection the person gets the value of Vsp that we want,
# we use the 'usealternativeseeddist' setting which can be used to specify a distribution
# for Vsp that is used when seeding. This distribution should provide the Vsp value
# on a log10 scale. Since we want one specific value, we'll use the 'fixed' distribution.
cfg["person.vsp.model.logdist2d.usealternativeseeddist"] <- "yes"
cfg["person.vsp.model.logdist2d.alternativeseed.dist.type"] <- "fixed"
cfg["person.vsp.model.logdist2d.alternativeseed.dist.fixed.value"] <- log10(Vsp_before)
# Because we don't want treatment in this example, we could in principle just give
# the diagnosis event a very low hazard. But to make it easier for the following
# examples we'll actually use a very high hazard for diagnosis so it will happen
# very shortly after infection
cfg["diagnosis.baseline"] <- 100
# To make sure that no treatment is performed, we'll set the threshold for treatment
# to zero
cfg["monitoring.cd4.threshold"] <- 0
# On the other hand, thinking ahead again, when a person does get offered treatment
# we'd like to make sure that he accepts it. This is done by setting the ART
# accept threshold to 100%, using a 'fixed' distribution again.
cfg["person.art.accept.threshold.dist.type"] <- "fixed"
cfg["person.art.accept.threshold.dist.fixed.value"] <- 1
# Here we set the interval for the monitoring event to one year.
cfg["monitoring.interval.piecewise.left"] <- 1
cfg["monitoring.interval.piecewise.right"] <- 1
cfg["monitoring.interval.piecewise.cd4s"] <- 500
cfg["monitoring.interval.piecewise.times"] <- 1
# When a monitoring event is triggered and the person's CD4 count is below the
# threshold, he will be offered treatment. We've already made sure that the person
# will accept the treatment, and here we specify that treatment should alter the
# Vsp value to Vsp_after. This is not actually used in this first example, but
# will be in the next two.
cfg["monitoring.fraction.log_viralload"] <- log10(Vsp_after)/log10(Vsp_before)

# Now we'll execute the Simpact Cyan simulation with these settings. The `run` function returns
# an object which contains paths to the output files.
r <- simpact.run(cfg, "C:/Users/lucp9040/Documents/SIMPACT_manual_examples/simptest3")
r

# We'll display the log of all events which is stored in the file specified by `logevents`.
print(read.csv(r$logevents,header=FALSE))

# EXAMPLE 2: TREATMENT

intCfg <- list()
# This change in configuration will take place one year into the simulation
intCfg["time"] <- 0.99
# At that time the CD4 threshold is set to a very large value, so that during
# the next monitoring event the person will receive treatment
intCfg["monitoring.cd4.threshold"] <- 100000
# We're not interested in any more monitoring events, so we'll set the interval
# to 1000 years
intCfg["monitoring.interval.piecewise.left"] <- 1000
intCfg["monitoring.interval.piecewise.right"] <- 1000
intCfg["monitoring.interval.piecewise.times"] <- 1000
# In this example we do not want the person to drop out of treatment, so the
# dropout interval is set to the fixed value of 1000 years
intCfg["dropout.interval.dist.type"] <- "fixed"
intCfg["dropout.interval.dist.fixed.value"] <- 1000

# Then we run the simulation with the existing configuration and with the intervention
# configuration above

r <- simpact.run(cfg, "C:/Users/lucp9040/Documents/SIMPACT_manual_examples/simptest3",intervention = list(intCfg))

# Let's show the event log again
print(read.csv(r$logevents,header=FALSE))

# EXAMPLE 3: TREATMENT AND DROPOUT
intCfg <- list()
# This change in configuration will take place one year into the simulation
intCfg["time"] <- 0.99
# At that time the CD4 threshold is set to a very large value, so that during
# the next monitoring event the person will receive treatment
intCfg["monitoring.cd4.threshold"] <- 100000
# To make sure that the person drops out of treatment 10 years later, we fix this value
intCfg["dropout.interval.dist.type"] <- "fixed"
intCfg["dropout.interval.dist.fixed.value"] <- 10
# To prevent a person from being re-diagnoses after dropout, we'll set the
# baselline value for this hazard to a very negative value (causes a very low
# hazard)
intCfg["diagnosis.baseline"] <- -100
# We're not interested in any more monitoring events, so we'll set the interval
# to 1000 years
intCfg["monitoring.interval.piecewise.left"] <- 1000
intCfg["monitoring.interval.piecewise.right"] <- 1000
intCfg["monitoring.interval.piecewise.times"] <- 1000

r <- simpact.run(cfg, "C:/Users/lucp9040/Documents/SIMPACT_manual_examples/simptest3",intervention = list(intCfg))

# Let's show the event log again
print(read.csv(r$logevents,header=FALSE))

