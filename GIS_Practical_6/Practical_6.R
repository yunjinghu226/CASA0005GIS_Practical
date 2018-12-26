# install and load necessary packages
install.packages('spatstat')
install.packages("GISTools")

library(spatstat)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojsonio)
library(tmaptools)

# read in data
# read from geojson
EW <- geojson_read("http://geoportal.statistics.gov.uk/datasets/8edafbe3276d4b56aec60991cbddda50_2.geojson", what = "sp")
BoroughMap <- EW[grep("^E09",EW@data$lad15cd),]
qtm(BoroughMap)
# or read from local directory
BoroughMapSF <- read_shape("BoundaryDataEdinaCensus/england_lad_2011.shp", as.sf = TRUE)
BoroughMapSP <- as(BoroughMapSF, "Spatial")
qtm(BoroughMapSP)

# reprojection
BNG = "+init=epsg:27700"
BoroughMapBNG <- spTransform(BoroughMap,BNG)

#Now get the location of all Blue Plaques in the City and reproject
BluePlaques <- geojson_read("https://s3.eu-west-2.amazonaws.com/openplaques/open-plaques-london-2018-04-08.geojson", what = "sp")
WGS = "+init=epsg:4326"
BluePlaquesBNG <- spTransform(BluePlaques, BNG)

# plot with tmap setting in an interactive mode
tmap_mode("view")
tm_shape(BoroughMapBNG) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesBNG) +
  tm_dots(col = "blue")

# Then remove any errant plaque points
#first we'll remove any Plaques with the same grid reference (i.e. two points on the same location) as this will cause problems later on in the analysis...
BluePlaquesBNG <- remove.duplicates(BluePlaquesBNG)
#now just select the points inside London - thanks to Robin Lovelace for posting how to do this one, very useful!
BluePlaquesSub <- BluePlaquesBNG[BoroughMapBNG,]
#check to see that they've been removed
tmap_mode("view")
tm_shape(BoroughMapBNG) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")

# analyze the distribution within certain Borough
#pick out Horrow
Borough <- BoroughMapBNG[BoroughMapBNG@data$lad15nm=="Harrow",]

#or as an sf object:
#BoroughMapBNGSF <- st_as_sf(BoroughMapBNG)
#BoroughSF <- BoroughMapBNGSF[BoroughMapBNGSF$lad15nm=="Harrow",]

#Check to see that the correct borough has been pulled out
tm_shape(Borough) +
  tm_polygons(col = NA, alpha = 0.5)

#clip the data to our single borough
BluePlaquesSub <- BluePlaquesBNG[Borough,]
#check that it's worked
tmap_mode("view")
tm_shape(Borough) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")

# create an observation window for spatstat package to carry out its analysis within
# now set a window as the borough boundary
window <- as.owin(Borough)
plot(window)

# create a ppp(point pattern) object for the plaque points to work with spatstat since this packages doesn't work with sp or sf
BluePlaquesSub.ppp <- ppp(x=BluePlaquesSub@coords[,1],y=BluePlaquesSub@coords[,2],window=window)
# print out the ppp
plot(BluePlaquesSub.ppp,pch=16,cex=0.5, main="Blue Plaques Harrow")

# play around with plot KDE(Kernal Density Estimation) map, the size of sigma value(i.e. diameter of kernal) affects the outcome
plot(density(BluePlaquesSub.ppp, sigma = 500))
plot(density(BluePlaquesSub.ppp, sigma = 1000))


# point data analysis: see if the distribution of the points is different from a CSR(complete spatial randomness), i.e. if there is any pattern of the distribution

# Quadrat Analysis: basic CSR test but not used in real analysis, but good start to get an overview of the data and understand the Poisson distribution
#First plot the points
plot(BluePlaquesSub.ppp,pch=16,cex=0.5, main="Blue Plaques in Harrow")
#now count the points in that fall in a 6 x 6 grid overlaid across the window
plot(quadratcount(BluePlaquesSub.ppp, nx = 6, ny = 6),add=T,col="red")
# compare the data with a statistically likely CSR distribution
#run the quadrat count
Qcount<-data.frame(quadratcount(BluePlaquesSub.ppp, nx = 6, ny = 6))
#put the results into a data frame
QCountTable <- data.frame(table(Qcount$Freq, exclude=NULL))
#we don't need the last row, so remove it
QCountTable <- QCountTable[-nrow(QCountTable),]
#check the data type in the first column - if it is factor, we will need to convert it to numeric
class(QCountTable[,1])
#convert it to numeric
vect<- as.numeric(levels(QCountTable[,1]))
vect <- vect[1:6]
QCountTable[,1] <- vect

# calculate the expected values based on Poisson distribution: p(X=k) = (lambda^k*e^-lambda)/k!
#calculate the total blue plaques (Var * Freq)
QCountTable$total <- QCountTable[,1]*QCountTable[,2]
#calculate mean
sums <- colSums(QCountTable[,-1])
#and now calculate our mean Poisson parameter (lambda)
lambda <- sums[2]/sums[1]
#calculate expected using the Poisson formula from above - k is the number of blue plaques counted in a square and is found in the first column of our table...
QCountTable$Pr <- ((lambda^QCountTable[,1])*exp(-lambda))/factorial(QCountTable[,1])
#now calculate the expected counts and save them to the table
QCountTable$Expected <- round(QCountTable$Pr * sums[1],0)
QCountTable
#Compare the frequency distributions of the observed and expected point patterns
plot(c(1,5),c(0,14), type="n", xlab="Number of Blue Plaques (Red=Observed, Blue=Expected)", ylab="Frequency of Occurances")
points(QCountTable$Freq, col="Red", type="o", lwd=3)
points(QCountTable$Expected, col="Blue", type="o", lwd=3)
# Hypothesis Test: confirm the resuts using built-in function quadrat.test that runs a Chi square test
# Null Hypo: there is no complete CSR in the data
teststats <- quadrat.test(BluePlaquesSub.ppp, nx = 6, ny = 6)
teststats
# p-value = 0.1984, > 0.05, thus null hypo rejected, the data is distributed randomly
plot(BluePlaquesSub.ppp,pch=16,cex=0.5, main="Blue Plaques in Harrow")
plot(teststats, add=T, col = "red")

# conclusion: there is no spatial pattern for blue plaques in Harrow

# Ripley's K analysis: compare the observed distribution of points with the Poisson random model for a whole range of different distance radii
K <- Kest(BluePlaquesSub.ppp, correction="border")
plot(K)

# Density-Based Spatial Clustering of Application with Noise
library(raster)
library(fpc)
library(plyr)
library(OpenStreetMap)

# DBSCAN requires you to input two parameters: 1. Epsilon - this is the radius within which the algorithm with search for clusters 2. MinPts - this is the minimum number of points that should be considered a cluster
# Based on the results of the Ripley’s K analysis earlier, we can see that we are getting clustering up to a radius of around 1200m, with the largest bulge in the graph at around 700m. Therefore, 700m is probably a good place to start and we will begin by searching for clusters of at least 4 points
#first extract the points from the spatial points data frame
BluePlaquesSubPoints <- data.frame(BluePlaquesSub@coords[,1:2])
#now run the dbscan analysis
db <- fpc::dbscan(BluePlaquesSubPoints, eps = 700, MinPts = 4)
#now plot the results
plot(db, BluePlaquesSubPoints, main = "DBSCAN Output", frame = F)
plot(Borough, add=T)

# change the way of display
library(ggplot2)
#we can now add this cluster membership info back into our dataframe
BluePlaquesSubPoints$cluster <- db$cluster
#next we are going to create some convex hull polygons to wrap around the points in our clusters
#use the ddply function in the plyr package to get the convex hull coordinates from the cluster groups in our dataframe
chulls <- ddply(BluePlaquesSubPoints, .(cluster), function(df) df[chull(df$coords.x1, df$coords.x2), ])
# as 0 isn't actually a cluster (it's all points that aren't in a cluster) drop it from the dataframe
chulls <- subset(chulls, cluster>=1)
#now create a ggplot2 object from our data
dbplot <- ggplot(data=BluePlaquesSubPoints, aes(coords.x1,coords.x2, colour=cluster, fill=cluster)) 
#add the points in
dbplot <- dbplot + geom_point()
#now the convex hulls
dbplot <- dbplot + geom_polygon(data = chulls, aes(coords.x1,coords.x2, group=cluster), alpha = 0.5) 
#now plot, setting the coordinates to scale correctly and as a black and white plot (just for the hell of it)...
dbplot + theme_bw() + coord_equal()

# add the point plot to a base map
#First get the bbox in lat long for Harrow
latlong <- "+init=epsg:4326" 
BoroughWGS <-spTransform(Borough, CRS(latlong))
BoroughWGS@bbox
# convert the basemap to BNG projection
basemap<-openmap(c(51.5530613,-0.4040719),c(51.6405318,-0.2671556), zoom=NULL,"stamen-toner")
#convert the basemap to British National Grid - remember we created the BNG object right at the beginning of the practical - it's an epsg string...
basemap_bng<-openproj(basemap, projection=BNG)
autoplot(basemap_bng) + geom_point(data=BluePlaquesSubPoints, aes(coords.x1,coords.x2, colour=cluster, fill=cluster)) + geom_polygon(data = chulls, aes(coords.x1,coords.x2, group=cluster, fill=cluster), alpha = 0.5)


# Analyzing spatial autocorrelation
# read data in
library(rgdal)
LondonWards <- readOGR("LondonWardData/LondonWards.shp", layer="LondonWards")
proj4string(LondonWards) <- CRS("+init=epsg:27700")
tmap_mode("view")
tm_shape(LondonWards) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaques) +
  tm_dots(col = "blue")
# remove those points fall outside of london wards
BluePlaquesSub <- BluePlaquesBNG[LondonWards,]
tm_shape(LondonWards) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")
# create spatially referenced continuous observation of blue plaques and Avg GCSE score
res <- poly.counts(BluePlaquesSub, LondonWards)
#and add this as a column in our spatialPolygonsDataframe
LondonWards@data$BluePlaqueCount<-res
#as the wards are of different sizes, perhaps best that we calculate a density
LondonWards@data$BlueDensity <- LondonWards$BluePlaqueCount/poly.areas(LondonWards)
tm_shape(LondonWards) +
  tm_polygons("BlueDensity",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              title="Blue Plaque Density")

# from the map we can see a few wards with densely distributed plaques, so we will do the Moran's I analysis with these wards
# first define a Wij spatial weight matrix
library(spdep)
#First calculate the centroids of all Wards in London
coordsW <- coordinates(LondonWards)
plot(coordsW)
#Now we need to generate a spatial weights matrix. We'll start with a simple binary matrix of queen's case neighbours
#create a neighbours list
LWard_nb <- poly2nb(LondonWards, queen=T)
#plot them
plot(LWard_nb, coordinates(coordsW), col="red")
#add a map underneath
plot(LondonWards, add=T)
#create a spatial weights object from these weights
Lward.lw <- nb2listw(LWard_nb, style="C")
head(Lward.lw$neighbours)
#moran's I test - this tells us whether we have clustered values (close to 1) or dispersed values (close to -1)
#we will calculate for the densities rather than raw values
I_LWard_Global_Density <- moran.test(LondonWards@data$BlueDensity, Lward.lw)
I_LWard_Global_Density
#result: 0.67, we have some distinctive clustering
#Geary's C as well. This tells us whether similar values or dissimilar values are clusering
C_LWard_Global_Density <- geary.test(LondonWards@data$BlueDensity, Lward.lw)
C_LWard_Global_Density
#result: 0.40, similar values are clustering ( Geary’s C falls between 0 and 2; 1 means no spatial autocorrelation, <1 - positive spatial autocorrelation or similar values clustering, >1 - negative spatial autocorreation or dissimilar values clustering)
#Getis Ord General G. This tells us whether high or low values are clustering. If G > Expected = High values clustering; if G < expected = low values clustering
G_LWard_Global_Density <- globalG.test(LondonWards@data$BlueDensity, Lward.lw)
G_LWard_Global_Density
#result: 0.01, > expected, so high values are tending to cluster

# create hot-spot
#use the localmoran function to generate I for each ward in the city
I_LWard_Local <- localmoran(LondonWards@data$BluePlaqueCount, Lward.lw)
I_LWard_Local_Density <- localmoran(LondonWards@data$BlueDensity, Lward.lw)
#copy some of the columns (the I score (column 1) and the z-score standard deviation (column 4)) back into the LondonWards spatialPolygonsDataframe
LondonWards@data$BLocI <- I_LWard_Local[,1]
LondonWards@data$BLocIz <- I_LWard_Local[,4]
LondonWards@data$BLocIR <- I_LWard_Local_Density[,1]
LondonWards@data$BLocIRz <- I_LWard_Local_Density[,4]

# plot the map of Moran's I
#set the breaks manually based on the rule that data points >2.58 or <-2.58 standard deviations away from the mean are significant at the 99% level (<1% chance that autocorrelation not present); >1.96 - <2.58 or <-1.96 to >-2.58 standard deviations are significant at the 95% level (<5% change that autocorrelation not present). >1.65 = 90% etc.
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
#create a new diverging colour brewer palette and reverse the order using rev so higher values correspond to red
MoranColours<- rev(brewer.pal(8, "RdGy"))
#now plot on an interactive map
tm_shape(LondonWards) +
  tm_polygons("BLocIRz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, Blue Plaques in London")

# Getis Ord G
Gi_LWard_Local_Density <- localG(LondonWards@data$BlueDensity, Lward.lw)
LondonWards@data$BLocGiRz <- Gi_LWard_Local_Density
GIColours<- rev(brewer.pal(8, "RdBu"))
tm_shape(LondonWards) +
  tm_polygons("BLocGiRz",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, Blue Plaques in London")

# Similar clustering analysis on Avg GCSE score in London Wards
# Moran's I
I_LWard_Local_GCSE <- localmoran(LondonWards@data$AvgGCSE201, Lward.lw)
LondonWards@data$GCSE_LocIz <- I_LWard_Local_GCSE[,4]
tm_shape(LondonWards) +
  tm_polygons("GCSE_LocIz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, GCSE Scores")
# Getis Ord G
Gi_LWard_Local_GCSE <- localG(LondonWards@data$AvgGCSE201, Lward.lw)
LondonWards@data$GCSE_LocGiz <- Gi_LWard_Local_GCSE
tm_shape(LondonWards) +
  tm_polygons("GCSE_LocGiz",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, GCSE Scores")

### EXTENSION: plot with a faceted ggplot2 map??? AND automate function to take other variables

