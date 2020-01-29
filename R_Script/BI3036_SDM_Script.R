# Species distribution modeling for Melilotus albus in Alaska 
# Peter Frank 
# peterfr@stud.ntnu.no
# 2019-03-18

#Create root folders to organize workspace, only run during once
getwd()                                                          # shows the current working drive
setwd("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data")   # sets a new working drive
dir.create(path = "R_data")                                      # creates a directory for base data
dir.create(path = "R_output")                                    # creates a directory for output data

#Install the required R packages for the SDM
install.packages("dismo")     # Functions for species distribution modeling, that is, predicting entire geographic distributions form occurrences at a number of sites and the environment at these sites.
install.packages("maptools")  # Set of tools for manipulating geographic data.
install.packages("rgdal")     # Provides bindings to the 'Geospatial' Data Abstraction Library and access to projection/transformation operations.
install.packages("rJava")     # Allows creation of objects, calling methods and accessing fields.
install.packages("raster")    # Reading, writing, manipulating, analyzing and modeling of gridded spatial data.
install.packages("rgeos")     # Interface to geom
install.packages("sp")        # Utility functions are provided, e.g. for plotting data as maps, spatial selection, as well as methods for retrieving coordinates.
install.packages("sf")        # Utility functions supporting a standardized way to encode spatial vector data
install.packages("arulesViz")
install.packages("XML")
install.packages("boot")
install.packages("caret")

#Load the libraryies of the five installed packages
library("sp")
library("raster")
library("maptools")
library("rgdal")
library("rJava")
library("rgeos")
library("dismo")
library("sf")
library("arulesViz")
library("XML")
library("boot")
library("caret")

####################################RASTER STACK#####################################################################################

# Finds all the files with extension "tif" in the RasterStack_TIFF Folder
#files <- list.files(path= "~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack",
        # pattern='tif', full.names=TRUE )

#Loads in each individual raster 
dd0 = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack/dd0.tif")
  dd5 = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack/dd5.tif")
  mat = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack/mat.tif")
mcmt = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack/mcmt.tif")
  map = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack/map.tif")
  msp = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack/msp.tif")
mwmt = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack/mwmt.tif")
ppt_sm = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack/ppt_sm.tif")
ppt_wt = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack/ppt_wt.tif")
  smh = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack/smh.tif")
tave_sm = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack/tave_sm.tif")
tave_wt = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack/tave_wt.tif")
lsc = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack/lsc.tif")

#If considering lsc as a factor
#lsc_numeric = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack/lsc.tif")
#lsc = as.factor(lsc_numeric)

#Create Raster stacks for predictors
raster.stack = stack(dd5, lsc, ppt_sm)

raster.stack.clim = stack(dd0, dd5, mat, mcmt, map, msp, mwmt, ppt_sm, ppt_wt, smh, tave_sm, tave_wt)

head(raster.stack)

# Give the layers in the raster stack more meaningful names
names(raster.stack)= c('Degree Days Above 5°C',
                       'Landscape Condition Model',
                       'Mean Annual Precitipation')

# Establish geographic extent for Alaksa Study Area
max.lat <- 71 
min.lat <- 51 
max.lon <- -130
min.lon <- -175
geographic.extent <- extent(x = c(min.lon, max.lon, min.lat, max.lat))

plot(raster.stack, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat))

####################################OBSERVATION DATA#####################################################################################

# Read in the species presence data
  # It is very important to remember that R will read coordinates as it does a graph with x & y coordinates, therefore you must always put the Longitude (x) frist followed by the latitude (y)
  #MEAL_ObsData <- read.csv2("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/MEAL_ObsData.csv")
  #View(MEAL_ObsData)

#MEAL_shp = readOGR(dsn = "~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data", layer = "MEAL_ObsData")

#summary(MEAL_shp)

# Check the data to make sure it loaded correctly
summary(MEAL_ObsData)

MEAL_Data = rbind(MEAL_ObsData, MEAL_ObsTest)
MEAL_ObsData = MEAL_Data
MEAL_ObsData = MEAL_Data

# Determine geographic extent of our data
max.lat <- ceiling(max(MEAL_ObsData$latitude))
min.lat <- floor(min(MEAL_ObsData$latitude))
max.lon <- ceiling(max(MEAL_ObsData$longitude))
min.lon <- floor(min(MEAL_ObsData$longitude))
geographic.extent <- extent(x = c(min.lon, max.lon, min.lat, max.lat))

# Load the data to use for our base map
data(wrld_simpl)

# Plot the base map
plot(wrld_simpl, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE, 
     col = "grey95")

# Add the points for individual observation
points(x = MEAL_ObsData$longitude, 
       y = MEAL_ObsData$latitude, 
       col = "red", 
       pch = 20, 
       cex = 0.75)

#points(MEAL_shp, col = "blue", pch = 3, cex = 0.75)

# And draw a little box around the graph
box()

# Plot a single predictor from the Raster Stack along with ObsData
plot(raster.stack, 1, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE)

#plot(MEAL_shp, add=TRUE)

points(x = MEAL_ObsData$longitude, 
       y = MEAL_ObsData$latitude, 
       col = "blue", 
       pch = 15, 
       cex = 0.75)

####################################ESTABLISH SUB-SAMPLES#####################################################################################

# create sequences of latitude and longitude values to define the grid
SubsampleGrid <- raster(geographic.extent)

# Set the resolution of the cells in degrees
res(SubsampleGrid) <- .001  ### 1 degree = approximaly 110.5 km, .001 degrees is roughly 1km pixels 

# Create the Sub-Sample
MEAL_Subsample <- gridSample(MEAL_ObsData, SubsampleGrid, n=1)   ### This indicates that you will randomly select n=1 point within ea grid cell

# Plot the base map
plot(wrld_simpl, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE, 
     col = "grey95",
     Add = TRUE)

plot(AK_Crop, 
      axes = TRUE, 
      col = "grey95",
      border = "black",
      lwd = 0.1)

# Add the points for individual observation
points(x = MEAL_ObsData$longitude, 
       y = MEAL_ObsData$latitude, 
       col = "Dark Green", 
       pch = 20, 
       cex = .5)

# Add the points for individual observation
points(x = MEAL_Subsample$longitude, 
       y = MEAL_Subsample$latitude, 
       col = "Green", 
       pch = 20, 
       cex = 0.2)

# And draw a little box around the graph
box()

####################################CREATE BACKGROUND DATA####################################################################################
# define circles with a radius of 50 km around the subsampled points
MEAL_Background = dismo::circles(MEAL_Subsample[,c("longitude","latitude")], d=50000, lonlat=TRUE)
Background_Polys <- polygons(MEAL_Background)

      #Clip the bounding boxes to the outline of AK
      AK_Crop = readOGR(dsn = "~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data", layer = "Alaska_Crop")
     

BackgroundPolys_Clip <- crop(Background_Polys, AK_Crop) 

# Create random points within the circles established above
Background_Points = spsample(BackgroundPolys_Clip, 1000, type='random', iter=1000)

  # Create random points within the circles established above
  Background_Points_AK = spsample(AK_Crop, 1000, type='random', iter=1000)

# Plot the base map
plot(wrld_simpl, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE, 
     col = "grey95")

plot(AK_Crop, 
     axes = TRUE, 
     col = "grey95",
     border = "black",
     lwd = 0.3)

plot(BackgroundPolys_Clip, col = rgb(1, 0, 0, 0.3), border = "grey", lwd = 0.1, add = TRUE) 

# Add the points for individual observation
points(x = Background_Points, 
       col = rgb(0, 0, 0, 0.4), 
       pch = 1, 
       cex = 0.6)

points(x = Background_Points_AK, 
       col = rgb(0, 0, 0, 0.4), 
       pch = 1, 
       cex = 0.6)

# Add the points for individual observation
points(x = MEAL_Subsample$longitude, 
       y = MEAL_Subsample$latitude, 
       col = "Red", 
       pch = 20, 
       cex = 0.6)

# And draw a little box around the graph
box()

####################################EXTRACTING VALUES FROM RASTERS#################################################################################
#Extract values from the observation data
Obs_Extract = extract(raster.stack, MEAL_ObsData) 

#Take a subset of the data for test purposes
#Test_Base<-sample(nrow(Obs_Extract),round(0.75*nrow(Obs_Extract))) 
#Obs_Extract<-Obs_Extract[Test_Base,] #This samples 3/4 of the presence datarows
#Test_Extract<-Obs_Extract[-Test_Base,] #This takes the remaining 1/4  (add extra data from fieldwork here, if necessary)

Test_Extract = extract(raster.stack, MEAL_ObsTest)

#Extracts values from the background data 
Background_Extract = extract(raster.stack, Background_Points) 

    Background_Extract = as.data.frame(Background_Extract)
    Background_Extract[, 'LSC'] = as.factor(Background_Extract[,'LSC']) 

    Background_Extract_AK = extract(raster.stack, Background_Points_AK) 

    Background_Extract_AK = as.data.frame(Background_Extract_AK)
    Background_Extract_AK[, 'LSC'] = as.factor(Background_Extract_AK[,'LSC']) 

    pb = c(rep(1, nrow(Obs_Extract)), rep(0, nrow(Background_Extract_AK))) 
    pb_test = c(rep(1, nrow(Test_Extract)), rep(0, nrow(Background_Extract_AK))) 
    
    sdmdata = data.frame(cbind(pb, rbind(Obs_Extract, Background_Extract_AK))) 
    sdmdata_test = data.frame(cbind(pb_test, rbind(Test_Extract, Background_Extract_AK))) 

#Creates a vector dataset combining the observation data and background data
pb = c(rep(1, nrow(Obs_Extract)), rep(0, nrow(Background_Extract))) 
pb_test = c(rep(1, nrow(Test_Extract)), rep(0, nrow(Background_Extract))) 

#Joins observation data and background data into a single dataset
sdmdata = data.frame(cbind(pb, rbind(Obs_Extract, Background_Extract))) 
sdmdata_test = data.frame(cbind(pb_test, rbind(Test_Extract, Background_Extract))) 

# Establishes the Landscape Condition variable as catagorical rather than numeric
sdmdata[,'LSC'] = as.factor(sdmdata[,'LSC']) 
sdmdata_test[,'LSC'] = as.factor(sdmdata_test[,'LSC'])

   # sdmdata[,'LSC'] = as.numeric(sdmdata[,'LSC']) 
   # sdmdata_test[,'LSC'] = as.numeric(sdmdata_test[,'LSC'])

str((sdmdata))
str((sdmdata_test))

#Make sure these two are the same 
head(sdmdata)
head(Obs_Extract)

cor(sdmdata[2:4], use = "complete.obs")
pairs(sdmdata[c(2:4)])

panel.cor_val <- panel.cor(sdmdata)
{
  par(usr = c(0, 1, 0, 1))
  txt <- as.character(format(cor(sdmdata), digits=2))
  text(0.5, 0.5, txt, cex = 6* abs(cor(sdmdata)))
}
View(panel.cor_val)

pairs(sdmdata[2:4], upper.panel = panel.cor(sdmdata))

par(mfrow=c(2,6)) 

#MAKE PAIR PLOT AGAINST MEAL_OBSDATA

boxplot(DD5~LSC, data=Obs_Extract, xlab="Landscape Condition", ylab="Growing Degree-Days Above 5°C")
boxplot(MAP~LSC, data=Obs_Extract,xlab="Landscape Condition", ylab="Mean Annual Precipitation")
plot((DD5~MAP), data=Obs_Extract, xlab="Growing Degree-Days Above 5°C", ylab="Mean Annual Precipitation")

summary(sdmdata)
str(sdmdata)

histLSC = Obs_Extract[ , 2]
histDD5 = Obs_Extract[ , 1]
histMAP = Obs_Extract[ , 3]
hist(histLSC)
hist(histDD5)
hist(histMAP)

####################################SPECIES DISTRIBUTION MODEL#################################################################################
# Establish the model using the SDMData 
glm_MEAL<-with(sdmdata, glm(pb ~ LSC + DD5, family = binomial (link =logit)))    #AUC 0.777
summary(glm_MEAL)

glm_MEAL_CLIM<-with(sdmdata, glm(pb ~ DD5 + PPT_SM, family = binomial (link =logit)))    #AUC 0.777
summary(glm_MEAL_CLIM)

plot(glm_MEAL)
glm_MEAL

###glm_MEAL<-with(sdmdata, glm(pb ~ LSC + DD5 + I(DD5^2) + MAP, family = "binomial"))    #AUC 0.777
    ###summary(glm_MEAL)

    ###glm_MEAL<-with(sdmdata, glm(pb ~ LSC + DD5, family = "binomial"))   #AUC 0.775
    ###summary(glm_MEAL)

    ###glm_MEAL<-with(sdmdata, glm(pb ~ DD5 + MAP, family = "binomial"))   #AUC 0.66
    ###summary(glm_MEAL)

  #Establishes the model with a quadratic term for precipitation
  #glm_MEAL<-with(sdmdata, glm(pb ~ I(LSC^2) + DD5 + MAP, family = "binomial")) 
  #summary(glm_MEAL)

    # Run model selection tests to determine the best model 
    #step(glm_MEAL)
    #drop1(glm_MEAL, test="Chi")

#Test the model with the second group. Note the AUC value, and the threshold that maximises the correctly classified observations
glm_Eval<-evaluate(sdmdata_test, Background_Extract_AK, glm_MEAL) #An evaluation of the selected model (input presence data, absence data, model)
glm_Eval 

glm_Eval_Clim<-evaluate(sdmdata_test, Background_Extract, glm_MEAL_CLIM) #An evaluation of the selected model (input presence data, absence data, model)
glm_Eval_Clim

  #Test the model with the second group. Note the AUC value, and the threshold that maximises the correctly classified observations
  glm_Eval<-evaluate(sdmdata_test, Background_Extract_AK, glm_MEAL) #An evaluation of the selected model (input presence data, absence data, model)
  glm_Eval 

#Create a confusion matrix from test data 
glm_predict = predict(glm_MEAL, newdata = sdmdata_test, type = "response")
pb_test = as.factor(pb_test)
glm_predict_bi = ifelse(glm_predict > 0.88,
                        "1",
                        "0")
  
glm_predict_bi = as.factor(glm_predict_bi)
confusionMatrix(data = glm_predict_bi, reference = pb_test)

#####

glm_predict = predict(glm_MEAL_CLIM, newdata = sdmdata_test, type = "response")
pb_test = as.factor(pb_test)
glm_predict_bi = ifelse(glm_predict > 0.88,
                        "1",
                        "0")

glm_predict_bi = as.factor(glm_predict_bi)
confusionMatrix(data = glm_predict_bi, reference = pb_test)

inv.logit(3.655106)  #Optimal threshold to determine presense absense in the model 

step(glm_MEAL)

#precision recall & sensitivtiy model

# Plots the AUC curve
par(mfrow=c(1,2))
plot(glm_Eval,"ROC")
plot(glm_Eval,"TPR")

par(mfrow=c(1,2))
plot(glm_Eval_Clim,"ROC")
plot(glm_Eval_Clim,"TPR")

#Boxplots of predictions (logit scale) for presence and background data
boxplot(glm_Eval, type='response', ylab="Model prediction (logit)")  

density(glm_Eval)

threshold(glm_Eval)

glm_Eval

#Response curves
response(glm_MEAL, fun=function(x, y) predict(x, y, type='response')) 

response(glm_MEAL_CLIM, fun=function(x, y) predict(x, y, type='response')) 

#Map the glm model using the predict function
glm_Map = raster::predict(raster.stack, glm_MEAL, ext = geographic.extent, type="response")

glm_Map_Clim = raster::predict(raster.stack, glm_MEAL_CLIM, ext = geographic.extent, type="response")

par(mfrow=c(1,2))

plot(glm_Map, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE)

plot(glm_Map_Clim, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE)

plot(wrld_simpl,
     add = TRUE)

# Add original observations
points(MEAL_ObsData$longitude, MEAL_ObsData$latitude, col = "red", pch = 20, cex = 0.75,  add = TRUE)

box()

####################################SPECIES DISTRIBUTION MODEL USING DISMO#################################################################################
#Download BioClimatic variables from WorldClim
bioclim.data <- getData(name = "worldclim",  # Indicates the name of the data set we would like to download
 var = "bio",         # This tells getData that we want to download all 19 of the bioclimatic variables, rather than individual temperature or precipitation measurements
 res = 2.5,           # This is the resolution of the data we want to download; in this case, it is 2.5 arc minutes of a degree. 1 arc minuite is approximatly .01 degrees or 10 km
 path = "~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data")    # Finally, this sets the location to which the files are downloaded. In our case, it is the data folder we created at the beginning.

#Or Download single datasources from the worldlcim database, with a set 
bioclim.data = getData('worldclim', var='bio', res=0.5, lon=-170, lat=50) 

names(bioclim.data)<-c('Annual Mean Temperature',
  'Mean Diurnal Range (Mean of monthly (max temp - min temp))',
   'Isothermality (BIO2/BIO7) (* 100)',
  'Temperature Seasonality (standard deviation *100)',
   'Max Temperature of Warmest Month',
  'Min Temperature of Coldest Month',
   'Temperature Annual Range (BIO5-BIO6)',
   'Mean Temperature of Wettest Quarter',
   'Mean Temperature of Driest Quarter',
   'Mean Temperature of Warmest Quarter',
   'Mean Temperature of Coldest Quarter',
   'Annual Precipitation',
   'Precipitation of Wettest Month',
   'Precipitation of Driest Month',
   'Precipitation Seasonality (Coefficient of Variation)',
   'Precipitation of Wettest Quarter',
   'Precipitation of Driest Quarter',
   'Precipitation of Warmest Quarter',
   'Precipitation of Coldest Quarter')

plot(bioclim.data, 
 xlim = c(min.lon, max.lon),
 ylim = c(min.lat, max.lat))


# Build species distribution model
bc.model <- bioclim(x = raster.stack, p = MEAL_ObsData)

# Predict presence from model
predict.presence <- dismo::predict(object = bc.model, x = raster.stack, ext = geographic.extent)

response(bc.model)

# Plot base map
plot(wrld_simpl, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE, 
     col = "grey95")

# Add model probabilities
plot(predict.presence, add = TRUE)

# Redraw those country borders
plot(wrld_simpl, add = TRUE, border = "grey5")

# Add original observations
points(MEAL_ObsData$longitude, MEAL_ObsData$latitude, col = "red", pch = 20, cex = 0.75)

box()

####################################SPECIES DISTRIBUTION MODEL USING MAXEXNT#################################################################################
system.file("java", package="dismo")

### Paste the file ’maxent.jar’ in the ’java’ folder of the ’dismo’ package

raster.stack.maxent = stack(lsc,dd5, ppt_sm)

head(raster.stack.maxent)

MEAL_Data1 <- MEAL_Data[,c(1,2)]
MEAL_DataSpat <- SpatialPointsDataFrame(coords = MEAL_Data, data = MEAL_Data)

?maxent

MEAL_MaxEnt = maxent(raster.stack.maxent, MEAL_DataSpat, ngb = 1000, factors = LSC)

response(MEAL_MaxEnt)

MEAL_MaxEnt_Map<-predict(MEAL_MaxEnt, raster.stack)

plot(MEAL_MaxEnt_Map,col=rev(heat.colors(100)),main="MaxEnt")

points(MEAL_ObsTest, pch=20, col=1)

#####################################EXPORT SPATIAL DATA TO GIS###############################################################################################
Background_Points= as_Spatial(c(Background_Points))

writeSpatialShape(Background_Points, "Background_Points")

writeOGR(obj=Background_Points, dsn ="~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data", layer="Background_Points", driver="ESRI Shapefile")

writeOGR(BackgroundPolys_Clip, dsn ="~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data", layer="Background_Polys_Clip", driver="ESRI Shapefile")

writeRaster(glm_Map, filename = 'glmMap', format = "GTiff")

writeRaster(SubsampleGrid, filename = 'SampleGrid', format = "GTiff")

write.csv(MEAL_Subsample)


