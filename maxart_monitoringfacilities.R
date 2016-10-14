library("RSimpactCyan")
library("MASS")
library("RColorBrewer")

# First, we'll need to tell the Simpact simulation engine to use the MaxART-specific simulation
simpact.set.simulation("maxart")

# Let's run a simulation with a fair amount of people, but limit the fraction of the population
# that anyone can see, to make the simulation advance faster. We'll also write the facility
# positions to a file, for plotting purposes later on
cfg <- list()
cfg["population.nummen"] <- 2000
cfg["population.numwomen"] <- 2000
cfg["population.eyecap.fraction"] <- 0.05
cfg["facilities.outfile.facilityxypos"] <- "${SIMPACT_OUTPUT_PREFIX}facilitypositions.csv"
r <- simpact.run(cfg, "C:/Users/lucp9040/Documents/SIMPACT_manual_examples/simptest8")

# Here we'll inspect the event log for monitoring events. As extra arguments on such a line,
# the place where the monitoring takes place is written. Bookkeeping is then performed so that
# the person's X and Y coordinates are associated to that particular facility.
persons = read.csv(r$logpersons)
facilityNames = read.csv(r$logsteps)[,1]
facName <- NULL
x <- NULL
y <- NULL
events = read.csv(r$logevents,check.names = FALSE,header = FALSE)
numMonitoring = 0
for (i in 1:dim(events)[1]){
  if (events[i,2] == "monitoring"){
    numMonitoring = numMonitoring + 1
    facName[numMonitoring] = as.character(events[i,14])
    personId = as.integer(events[i,4])
    personInfo = persons[persons$ID == personId,]
    x[numMonitoring] = as.double(personInfo$XCoord)
    y[numMonitoring] = as.double(personInfo$YCoord)
  }
}
facilityTreatments = data.frame(facName,x,y)

# Here we plot the facilities, together with the locations of people treated
# or monitored there.

# First we'll assign colors to facilities
cols<-brewer.pal(n=6,name="Set1")
cols<-c(cols,cols,cols[1],cols[2])
cols_facName<-cols[facilityTreatments$facName]

# Then, for each facility we'll add markers for the locations of the persons
# that are treated there, and we'll draw the location of the facility itself
# as a large circle
facPos = read.csv(r$facilityxypos)
plot(facilityTreatments$x,facilityTreatments$y,type="p",pch=19,col=cols_facName,ylim=c(90,0))
points(facPos$XCoord,facPos$YCoord,col=cols_facName,pch=19,cex=5,ylim=c(90,0))
# Let's add some labels
text(facPos[,2], facPos[,3], facPos[,1], cex=0.6, pos=4, col="black")




