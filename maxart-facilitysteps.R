library("RSimpactCyan")
library("MASS")

# First, we'll need to tell the Simpact simulation engine to use the MaxART-specific simulation
simpact.set.simulation("maxart")

# We configure the simulation to use one of the files with fake randomization data
# (which should have been saved to /tmp/maxart-randomization-fake_1.csv for this
# exact configuration to work)
cfg <- list()
cfg["facilities.randomization"] <- "C:/tmp/maxart-randomization-fake_1.csv"
r <- simpact.run(cfg, "C:/Users/lucp9040/Documents/SIMPACT_manual_examples/simptest9")
# We then read the steps as they have been written to a CSV log file
steps = read.csv(r$logsteps)
# Let's show this step data. The first line is written when the study is started (by default
# 5 years into the simulation). At that point, all facilities are in the control stage ('C').
# Then, the stage each facility is in is written at regular intervals, when new facilities enter
# the transitioning stage ('T'), or when they advance to the intervention stage ('I'). A final
# line is written when the study has ended ('Post' study).
steps
