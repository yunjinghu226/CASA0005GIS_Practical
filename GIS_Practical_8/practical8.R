library(tidyverse)
library(downloader)
library(rgdal)
library(sf)
library(ggplot2)
library(reshape2)
library(plotly)
library(highcharter)

library(rgeos)
library(maptools)
library(sf)
library(sfc)
library(tmap)

getwd()
LondonWards <- readOGR('NewLondonWard.shp',layer='NewLondonWard')
LondonWardsSF <- st_as_sf(LondonWards)
extradata <- read_csv("https://www.dropbox.com/s/qay9q1jwpffxcqj/LondonAdditionalDataFixed.csv?raw=1")
LondonWardsSF <- merge(LondonWardsSF, extradata, by.x = "WD11CD", by.y = "Wardcode")

#check which variables are numeric first

list1 <- as.data.frame(cbind(lapply(LondonWardsSF, class)))
list1 <- cbind(list1, seq.int(nrow(list1)))
#you will notice that there are some non-numeric columns, we want to exclue these, and drop the geometry 
LondonSub <- LondonWardsSF[,c(1:73,83:86)]

#make sure the geometry is null or we will get errors - also create some subsets so that we can see our data better
LondonSub2 <- st_set_geometry(LondonWardsSF[,c(1:3,9:27)],NULL)
LondonSub3 <- st_set_geometry(LondonWardsSF[,c(1:3,28:50)],NULL)
LondonSub4 <- st_set_geometry(LondonWardsSF[,c(1:3,51:73,85:86)],NULL)

LondonMelt2 <- melt(LondonSub2, id.vars = 1:3)
attach(LondonMelt2)
hist2 <- ggplot(LondonMelt2, aes(x=value)) + geom_histogram(aes(y = ..density..)) + geom_density(colour="red", size=1, adjust=1)
hist2 + facet_wrap(~ variable, scales="free")

LondonMelt3 <- melt(LondonSub3, id.vars = 1:3)
attach(LondonMelt3)
hist3 <- ggplot(LondonMelt3, aes(x=value)) + geom_histogram(aes(y = ..density..)) + geom_density(colour="red", size=1, adjust=1)
hist3 + facet_wrap(~ variable, scales="free")

LondonMelt4 <- melt(LondonSub4, id.vars = 1:3)
attach(LondonMelt4)
hist4 <- ggplot(LondonMelt4, aes(x=value)) + geom_histogram(aes(y = ..density..)) + geom_density(colour="red", size=1, adjust=1)
hist4 + facet_wrap(~ variable, scales="free")

hist5 <- ggplot(LondonMelt4, aes(x=log10(value))) + geom_histogram(aes(y = ..density..)) + stat_function(fun=dnorm, colour="red", size=1)
hist5 + facet_wrap(~ variable, scales="free")

londonpoint<-ggplot(LondonSub, aes(x=x.y,y=y.y))+geom_point()+coord_equal()
londonpoint

londonpoint<-ggplot(LondonSub, aes(x=x.y,y=y.y))+stat_bin2d(bins=10)
londonpoint

londonpoint<-ggplot(LondonSub, aes(x=x.y,y=y.y))+geom_point()+coord_equal()
londonpoint

londonpoint+stat_density2d(aes(fill = ..level..), geom="polygon")+scale_fill_gradient(low="blue", high="red")

attach(LondonWardsSF)
summary(AvgGCSE201)
newvar<-0
recode<-function(variable,high,medium,low){
  newvar[variable<=high]<-"High"
  newvar[variable<=medium]<-"Medium"
  newvar[variable<=low]<-"Low"
  return(newvar)
}
LondonWardsSF$GCSE_recode <- recode(AvgGCSE201,409.1,358.3,332.3)
summary(UnauthAbse)
LondonWardsSF$Unauth_recode <- recode(UnauthAbse,2.4675,1.4105,0.8215)

#Location Quotient function 1
LQ1<-function(pctVariable){
  pctVariable /mean(pctVariable)
}

#Location Quotient function 2
LQ2<-function(variable,rowtotal){
  localprop<-variable/rowtotal
  globalprop<-sum(variable)/sum(rowtotal)
  return(localprop/globalprop)
}

LondonWardsSF$Owned_recode <- LQ1(PctOwned20)
LondonWardsSF$SocialR_recode <- LQ1(PctSocialR)
LondonWardsSF$Private_recode <- LQ1(PctPrivate)
LondonWardsSF$SharedO_recode <- LQ1(PctSharedO)
LondonWardsSF$RentFree_recode <- LQ1(PctRentFre)

sfdataframe <- LondonWardsSF

LQMapper<-function(sfdataframe){
  print(colnames(sfdataframe))  
  vars<-readline("From the list above, select the variables 
                 you want to calculate location quotients for 
                 separated by spaces...")
  
  # split the string at the spaces  
  vars<-unlist(strsplit(vars, split = "\\s"))  
  # now save vars as a list
  vars<-as.list(vars)  
  
  print("looping to create new location quotient variables...")
  attach(sfdataframe)  
  for(i in 1:length(vars)){
    pctVariable<-vars[[i]]
    colvect<-which(colnames(sfdataframe)==vars[[i]])
    
    #this is a little function to calculate location quotients
    LQ<-function(pctVariable){
      pctVariable/mean(pctVariable)
    }
    #use LQ function here to create new variable in sfdataframe 
    #and save it
    v <- sfdataframe[,colvect]
    sfdataframe[,paste("LQ_",pctVariable, sep="")] <- LQ(v[[pctVariable]])    
  }
  
  #reset i as we're going to use it again in a minute
  i=0
  
  print("now entering the plotting loop")
  for(i in 1:length(vars)){
    print("I'm plotting")
    pctVariable<-paste("LQ_",vars[[i]],sep="")
    colvect<-which(colnames(sfdataframe)==paste("LQ_",vars[[i]],sep=""))
    
    #create the plot
    LQMapperPlot <- tm_shape(sfdataframe) + tm_polygons(pctVariable, 
                                                        style="jenks",
                                                        palette="Spectral",
                                                        midpoint=1,
                                                        title=pctVariable,
                                                        alpha = 0.5)
    
    LQMapperPlot
    #save the plot to a pdf and give it a name based on its variable
    tmap_save(LQMapperPlot, filename=paste(pctVariable,".png",sep=""))
    
  }  
  return(sfdataframe)  
}
LQMapper(LondonWardsSF)

LondonWardsDF <- st_set_geometry(LondonWardsSF, NULL)
#clustering 
# Create a new data frame just containing the two variables we are #interested in
mydata<-as.data.frame(LondonWardsDF[,c("PctOwned20","PctNoEngli")])
attach(mydata)
histplot <- ggplot(data=mydata, aes(x=PctOwned20))
histplot +geom_histogram()
histplot <- ggplot(data=mydata, aes(x= PctNoEngli))
histplot +geom_histogram()
# run a k-means to find 3 clusters – use 25 iterations
fit <- kmeans(mydata, 3, nstart=25) # 3 cluster solution
# get cluster means
centroid<-aggregate(mydata,by=list(fit$cluster),FUN=mean)
#print the results of the cluster groupings
centroid
p <- ggplot(mydata,aes(PctOwned20, PctNoEngli))
p+geom_point(aes(colour=factor(fit$cluster)))+geom_point(data=centroid[,2:3],aes(PctOwned20, PctNoEngli), size=7, shape=18)+ theme(legend.position="none")
mydata$cluster <- fit$cluster

#add the cluster groups to the LondonWards data frame
LondonWardsSF$cluster<-mydata$cluster
#merge the cluster results into a fortified dataframe for plotting (we made LondonGeom earlier)
#London_geom<-merge(London_geom,LondonWardsDF,by.x="id", by.y="Wardcode1")
#now map our geodeomographic classification
map <- ggplot(LondonWardsSF) + geom_sf(mapping = aes(fill=cluster))+scale_fill_continuous(breaks=c(1,2,3))
map

##what does a cross tabulation of the data look like?

chi<-chisq.test(LondonWardsSF$GCSE_recode,LondonWardsSF$unauth_recode)
#observed counts
chi$observed
#expected counts
chi$expected
#chi squared statistic
chi$statistic
#p-value
chi$p.value

# plot the histogram
varlist <- data.frame(cbind(lapply(LondonWardsSF, class)))
varlist$id <- seq(1,nrow(varlist))

qplot(AvgGCSE201, data = LondonWardsSF, geom = "histogram")
qplot(UnauthAbse, data = LondonWardsSF, gemo = "histogram")

# plot
qplot(UnauthAbse, AvgGCSE201, data = LondonWardsSF, geom = "point") + stat_smooth(method="lm", se=FALSE, size=1)
#It looks like there is a negative relationship, so can we discover exactly what this relationship is using a linear regression model (we actually fitted one above to create the blue line)
library(broom)

#to fit the linear regrssion model, use the lm() function
model1 <- lm(AvgGCSE201 ~ UnauthAbse, data = LondonWardsSF)
#write the results out into a dataframe
model1_res <- tidy(model1)

#examine the results
summary(model1)
#examine some of the diagnostic plots to see if there is any patterning in the residuals

# test spatial clustering of residuals
library(tmap)
#save the residuals into your dataframe
LondonWardsSF$model1_resids <- model1$residuals
#now plot the residuals
tmap_mode("view")
qtm(LondonWardsSF, fill = "model1_resids")

# spatial correlation using the Moran's I statistic
library(spdep)
library(sp)
library(sf)
#####
LondonWards <- as(LondonWardsSF,"Spatial")
#First calculate the centroids of all Wards in London
coordsW <- coordinates(LondonWards)
plot(coordsW)
#Now we need to generate a spatial weights matrix (remember from the lecture). We'll start with a simple binary matrix of queen's case neighbours
#create a neighbours list
LWard_nb <- poly2nb(LondonWards, queen=T)
#plot them
plot(LWard_nb, coordinates(coordsW), col="red")
#add a map underneath
plot(LondonWards, add=T)
#create a spatial weights object from these weights
Lward.lw <- nb2listw(LWard_nb, style="C")
#now run a moran's I test on the residuals
moran.test(LondonWards@data$model1_resids, Lward.lw)

# dummy vairables
# multilinear regression
p <- ggplot(LondonWardsSF, aes(x=UnauthAbse, y=AvgGCSE201))
p + geom_point(aes(colour = InnerOuter))
#first, let's make sure R is reading our InnerOuter variable as a factor
LondonWardsSF$InnerOuter <- as.factor(LondonWardsSF$InnerOuter)

#now run the model
model1_dummy <- lm(AvgGCSE201 ~ UnauthAbse + InnerOuter, data = LondonWardsSF)

summary(model1_dummy)
contrasts(LondonWardsSF$InnerOuter)
LondonWardsSF$InnerOuter <- relevel(LondonWardsSF$InnerOuter, ref="Outer")

model1_dummy <- lm(AvgGCSE201 ~ UnauthAbse + InnerOuter, data = LondonWardsSF)
summary(model1_dummy)

# check if there's any correlated independent variables
library(corrplot)
LondonWardsDF <- st_set_geometry(LondonWardsSF,NULL)
cormat <- cor(LondonWardsDF[,8:72], use="complete.obs", method="pearson")
corrplot(cormat)
# selec just a few variables...
cormat <- cor(LondonWardsDF[,c(28,60,61,71)], use="complete.obs", method="pearson")
corrplot(cormat)

# run the multilinear regression model again with new variable added in
model2_dummy <- lm(AvgGCSE201 ~ UnauthAbse + Employment + InnerOuter, data = LondonWardsSF)

summary(model2_dummy)
# can add more variables to optimize the model
### workflow: add variable and run the model,interpretate the results, write residual values to LondonWardsSF dataframe, plot the residuals to check visually for spatial autocorrelation, run a Moran's I test ot confirm the presence of otherwise of spatial autocorrelation


#Geographically Weighted Regression Models (GWR)
model_final <- lm(AvgGCSE201 ~ UnauthAbse + Employment + CarsPerHH2, data = LondonWardsSF)
summary(model_final)
LondonWardsSF$model_final_res <- model_final$residuals
plot(model_final)
qtm(LondonWardsSF, fill = "model_final_res")
LondonWards <- as(LondonWardsSF,"Spatial")
moran.test(LondonWards@data$model1_resids, Lward.lw)

library(spgwr)
GWRbandwidth <- gwr.sel(AvgGCSE201 ~ UnauthAbse + Employment + CarsPerHH2, data = LondonWards, coords=cbind(x,y),adapt=T) 
#run the gwr model
gwr.model = gwr(AvgGCSE201 ~ UnauthAbse + Employment + CarsPerHH2, data=LondonWards, coords=cbind(x,y), adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 
#print the results of the model
gwr.model
results<-as.data.frame(gwr.model$SDF)
head(results)
#attach coefficients to original dataframe
LondonWards@data$coefUnauthAbse<-results$UnauthAbse
LondonWards@data$coefEmployment<-results$Employment
LondonWards@data$coefCarsPerHH2<-results$CarsPerHH2
tm_shape(LondonWards) +
  tm_polygons(col = "coefUnauthAbse", palette = "RdBu")
tm_shape(LondonWards) +
  tm_polygons(col = "coefEmployment", palette = "RdBu")
tm_shape(LondonWards) +
  tm_polygons(col = "coefCarsPerHH2", palette = "RdBu")

# calculate standard errors
sigTest = abs(gwr.model$SDF$UnauthAbse) -2 * gwr.model$SDF$UnauthAbse_se 

LondonWards$GWRUnauthSig<-sigTest
tm_shape(LondonWards) +
  tm_polygons(col = "GWRUnauthSig", palette = "RdYlBu")


