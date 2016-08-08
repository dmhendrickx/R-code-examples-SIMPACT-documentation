library("RSimpactCyan")
library("MASS")

cfg <- list()
cfg["population.nummen"] <- 100000
cfg["population.numwomen"] <- 100000
cfg["population.maxevents"] <- 1 # We don't need an actual simulation, just the 2D locations
cfg["population.eyecap.fraction"] <- 0 # Don't initialize formation events (would not even be possible for this many people)

# We're going to sample the geographic location (which is not currently used for anything)
# from a discrete distribution, based on data in a tiff file
cfg["person.geo.dist2d.type"] <- "discrete"
cfg["person.geo.dist2d.discrete.densfile"] <- "C:/Users/lucp9040/Documents/SIMPACT_manual_examples/simptest1/up32.tiff"
cfg["person.geo.dist2d.discrete.maskfile"] <- "" # This option must be set, but we leave it blank to disable the mask
cfg["person.geo.dist2d.discrete.width"] <- 320
cfg["person.geo.dist2d.discrete.height"] <- 240

res <- simpact.run(cfg, "C:/Users/lucp9040/Documents/SIMPACT_manual_examples/simptest1")

# We're going to get the information from the persons log, in which the X and Y
# coordinate for each person will be saved

persons = read.csv(res$logpersons)

# Let's make a histogram of these coordinates, which should show that we're
# sampling from the discrete distribution that corresponds to the image shown
# above

k<-kde2d(persons$XCoord,persons$YCoord, n=100, lims=c(0,320,0,240))
image(k,col=c("blue", heat.colors(16)))

# As explained in the Simpact Cyan documentation, the default value of the following
# parameter is 'yes', to account for a difference in Y-axis orientation between images
# and regular plots. To illustrate what would happen otherwise, we'll set it to 'no'
# in the next test

cfg["person.geo.dist2d.discrete.flipy"] <- "no"

# Again run the simulation and plot the histogram

res <- simpact.run(cfg, "C:/Users/lucp9040/Documents/SIMPACT_manual_examples/simptest1")
persons = read.csv(res$logpersons)
k<-kde2d(persons$XCoord,persons$YCoord, n=100, lims=c(0,320,0,240))
image(k,col=c("blue", heat.colors(16)))



