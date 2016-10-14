library("RSimpactCyan")
library("MASS")

# First, we'll need to tell the Simpact simulation engine to use the MaxART-specific simulation
simpact.set.simulation("maxart")

# In a first simulation, we'll just initialize the number of men and women to 150,000 each.
# Relationships are disabled by setting population.eyecap.fraction to 0, to speed up initialization
# and to reduce memory requirements, and the simulation is told to stop after one event. These
# settings will already allow us to plot a histogram of the simulated person positions, which
# should then resemble the population density from the Hhohho region.
cfg <- list()
cfg["population.nummen"] <- 150000
cfg["population.numwomen"] <- 150000
cfg["population.eyecap.fraction"] <- 0
cfg["population.maxevents"] <- 1
r <- simpact.run(cfg, "C:/Users/lucp9040/Documents/SIMPACT_manual_examples/simptest7")

# We'll read the person log, and create a histogram of the X and Y positions of each person
persons = read.csv(r$logpersons)
k<-kde2d(persons$XCoord,persons$YCoord, n=100, lims=c(10,100,0,90))
image(k,col=c("white", rainbow(256)),ylim=c(90,0))
axis(1)
axis(2)

# In a second simulation, we'll do the same thing but we'll use person coordinates based on
# the full Swaziland population density information. In the 'person.geo' settings, the default
# is to use the Swaziland map, but by using a mask file only the Hhohho region is selected.
# To again obtain locations from the entire Swaziland country, this mask file is disabled in
# the settings below
cfg <- list()
cfg["population.nummen"] <- 600000
cfg["population.numwomen"] <- 600000
cfg["population.eyecap.fraction"] <- 0
cfg["population.maxevents"] <- 1
cfg["person.geo.dist2d.discrete.maskfile"] <- ""
r <- simpact.run(cfg, "C:/Users/lucp9040/Documents/SIMPACT_manual_examples/simptest7")

# Plotting a histogram of the person locations will now produce a density map that resembles
# the one from Swaziland
persons = read.csv(r$logpersons)
k<-kde2d(persons$XCoord,persons$YCoord, n=100, lims=c(0,130,0,180))
image(k,col=c("white", rainbow(256)),ylim=c(180,0))
axis(1)
axis(2)

# In a final simulation, we'll again use the same settings as the ones used to plot
# the Hhohho region above. We'll also specify that the facility GPS coordinates, translated
# such map coordinates, should be written to a file.
cfg <- list()
cfg["population.nummen"] <- 150000
cfg["population.numwomen"] <- 150000
cfg["population.eyecap.fraction"] <- 0
cfg["population.maxevents"] <- 1
cfg["facilities.outfile.facilityxypos"] <- "${SIMPACT_OUTPUT_PREFIX}facilitypositions.csv"
r <- simpact.run(cfg, "C:/Users/lucp9040/Documents/SIMPACT_manual_examples/simptest7")

# We'll again create a histogram of the X and Y positions of each person, and on this
# map we'll indicate the locations of the health care facilities used in the MaxART study
# by using red circles. 
# In a final step, we'll also draw labels for these facilities
facPos = read.csv(r$facilityxypos)
persons = read.csv(r$logpersons)
k<-kde2d(persons$XCoord,persons$YCoord, n=100, lims=c(10,100,0,90))
image(k,col=c("white", rainbow(256)),ylim=c(90,0))
points(facPos[,2],facPos[,3],type="p",pch=19,ylim=c(90,0))
text(facPos[,2], facPos[,3], facPos[,1], cex=0.6, pos=4, col="black")
axis(1)
axis(2)


