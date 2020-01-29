#Loads in each individual raster 
dd0 = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack_Full/dd0.tif")
dd5 = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack_Full/dd5.tif")
mat = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack_Full/mat.tif")
mcmt = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack_Full/mcmt.tif")
map = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack_Full/map.tif")
msp = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack_Full/msp.tif")
mwmt = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack_Full/mwmt.tif")
ppt_sm = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack_Full/ppt_sm.tif")
ppt_wt = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack_Full/ppt_wt.tif")
smh = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack_Full/smh.tif")
tave_sm = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack_Full/tave_sm.tif")
tave_wt = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack_Full/tave_wt.tif")
lsc_dis_km = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack/LSC_DIS_KM.tif")



#If considering lsc as a factor
#lsc_numeric = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack/lsc.tif")
#lsc = as.factor(lsc_numeric)

#Create Raster stacks for predictors

raster.stack.clim = stack(dd5, msp, smh, tave_sm, lsc_dis_km)

head(raster.stack)

# Give the layers in the raster stack more meaningful names
names(raster.stack)= c('Degree Days Above 5°C',
                       'Landscape Condition',
                       'Summer (Jun to Aug) Precipitation (mm)')

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


raster.stack.clim = stack(dd0, dd5, mat, mcmt, map, msp, ppt_sm, ppt_wt, smh, tave_sm, tave_wt)

#Extract values from the observation data
Obs_Extract_Clim = extract(raster.stack.clim, MEAL_Develop) 
Test_Extract_Clim = extract(raster.stack.clim, MEAL_Test)

#Extracts values from the background data 
Background_Extract_AK_Clim = extract(raster.stack.clim, Background_Points_AK) 

#Background_Extract_AK_Clim = as.data.frame(Background_Extract_AK_Clim)

#Creates a vector dataset combining the observation data and background data
pb_Clim = c(rep(1, nrow(Obs_Extract_Clim)), rep(0, nrow(Background_Extract_AK_Clim))) 
pb_test_Clim = c(rep(1, nrow(Test_Extract_Clim)), rep(0, nrow(Background_Extract_AK_Clim))) 

#Joins observation data and background data into a single dataset
sdmdata_Clim = data.frame(cbind(pb, rbind(Obs_Extract_Clim, Background_Extract_AK_Clim))) 
sdmdata_test_Clim = data.frame(cbind(pb_test, rbind(Test_Extract_Clim, Background_Extract_AK_Clim))) 

str((sdmdata_Clim))
str((sdmdata_test_Clim))

#Make sure these two are the same 
head(sdmdata_Clim)
head(Obs_Extract_Clim)

cor(sdmdata_Clim[2:12], use = "complete.obs")
pairs(sdmdata_Clim[c(2:12)])

cor.data = cor(sdmdata_Clim[2:4], use = "complete.obs")
cor.data

ggpairs(sdmdata_Clim[c(2:7)])

####################################SPECIES DISTRIBUTION MODEL#################################################################################
# Establish the model using the SDMData 
glm_MEAL_CLIM = with(sdmdata_Clim, glm(pb ~ DD5 + MAT + MAP + MSP + SMH + TAVE_SM, family = binomial (link =logit)))
summary(glm_MEAL_CLIM)

glm_MEAL_CLIM = with(sdmdata_Clim, glm(pb ~ DD5 + MAT + MAP, family = binomial (link =logit)))
summary(glm_MEAL_CLIM)

plot(glm_MEAL_CLIM)

step(glm_MEAL_CLIM)
drop1(glm_MEAL_CLIM, test="Chi")

#Test the model with the second group. Note the AUC value, and the threshold that maximises the correctly classified observations
glm_Eval_Clim<-evaluate(sdmdata_test_Clim, Background_Extract_AK_Clim, glm_MEAL_CLIM) #An evaluation of the selected model (input presence data, absence data, model)
glm_Eval_Clim

inv.logit(1.792084)  #Optimal threshold to determine presense absense in the model 

#Create a confusion matrix from test data 
glm_predict_Clim = predict(glm_MEAL_CLIM, newdata = sdmdata_test_Clim, type = "response")
pb_test_Clim = as.factor(pb_test_Clim)
glm_predict_bi_Clim = ifelse(glm_predict_Clim > 0.857,
                        "1",
                        "0")

glm_predict_bi_Clim = as.factor(glm_predict_bi_Clim)
confusionMatrix(data = glm_predict_bi_Clim, reference = pb_test_Clim)

#precision recall & sensitivtiy model

# Plots the AUC curve
par(mfrow=c(1,2))
plot(glm_Eval_Clim,"ROC")
plot(glm_Eval_Clim,"TPR")

#Boxplots of predictions (logit scale) for presence and background data
boxplot(glm_Eval_Clim, type='response', ylab="Model prediction (logit)")  

density(glm_Eval_Clim)

threshold(glm_Eval_Clim)

#Response curves
response(glm_MEAL_CLIM, fun=function(x, y) predict(x, y, type='response')) 

#Map the glm model using the predict function
glm_Map_Clim = raster::predict(raster.stack.clim, glm_MEAL_CLIM, ext = geographic.extent, type="response")

plot(glm_Map_Clim, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE)

plot(wrld_simpl,
     add = TRUE)

# Add original observations
points(MEAL_ObsData$longitude, MEAL_ObsData$latitude, col = "red", pch = 20, cex = 0.75,  add = TRUE)

box()