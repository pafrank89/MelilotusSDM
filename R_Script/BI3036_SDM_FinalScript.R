# Species distribution modeling for Melilotus albus in Alaska 
# Peter Frank 
# peterfr@stud.ntnu.no
# 2019-05-01

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
install.packages("ggplot2")            
install.packages("GGally")
install.packages("cowplot")
install.packages("AICcmodavg") 

#Load the libraryies of the five installed packages
library("readr")
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
library("ggplot2")                   
library("GGally")
library("cowplot")
library("AICcmodavg")

####################################RASTER STACK#####################################################################################
#Loads in each individual raster 

  dd5 = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack/dd5.tif")
  msp = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack/msp.tif")
  shm = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack_Full/smh.tif")
  tave_sm = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack_Full/tave_sm.tif")
  #lsc = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack/lsc.tif")
  #lsc_dis = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack/LSC_DIS.tif")
  lsc_dis = raster("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack/LSC_DIS_KM.tif")
  
#Create Raster stacks for predictors
raster.stack = stack(dd5, msp, shm, tave_sm, lsc_dis)

# Give the layers in the raster stack more meaningful names
names(raster.stack)= c("Degree Days Above 5°C",
                       "Distance to Low Landscape Condition (m)",
                       "Summer (Jun to Aug) Precipitation (mm)")

# Establish geographic extent for Alaksa Study Area
max.lat <- 71 
min.lat <- 51 
max.lon <- -130
min.lon <- -175
geographic.extent <- extent(x = c(min.lon, max.lon, min.lat, max.lat))

plot(raster.stack, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat))

# Load the data to use for our base map
data(wrld_simpl)

AK_Crop = readOGR(dsn = "~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data", layer = "Alaska_Crop")

####################################OBSERVATION DATA#####################################################################################
#Upload CSV file from the working directory
MEAL_ObsData <- read_csv("~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data/RasterStack/MEAL_ObsData.csv")
View(MEAL_ObsData)

# Check the data to make sure it loaded correctly
summary(MEAL_ObsData)

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

# And draw a little box around the graph
box()

# Plot a single predictor from the Raster Stack along with ObsData
plot(raster.stack, 1, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE)

points(x = MEAL_ObsData$longitude, 
       y = MEAL_ObsData$latitude, 
       col = "blue", 
       pch = 15, 
       cex = 0.75)

####################################CREATE PSEUDO-ABSENSE DATA####################################################################################
#Create 1000 pseudo-absense points randomly placed across the study area
Background_Points_AK = spsample(AK_Crop, 1000, type='random', iter=1000)

# Plot the base map
plot(AK_Crop, 
     axes = TRUE, 
     col = "grey95",
     border = "black",
     lwd = 0.3)

# Add the points for individual observation
points(x = Background_Points_AK, 
       col = rgb(0, 0, 0, 0.4), 
       pch = 1, 
       cex = 0.6)

# Add the points for individual observation
points(x = MEAL_ObsData$longitude, 
       y = MEAL_ObsData$latitude, 
       col = "Red", 
       pch = 20, 
       cex = 0.6)

# And draw a box around the graph
box()

####################################EXTRACTING VALUES FROM RASTERS#################################################################################
#Take a subset of the data for test purposes, this is done before the extract to ensure that the split remains constant across models
Test_Base = sample(nrow(MEAL_ObsData),round(0.75*nrow(MEAL_ObsData))) 
MEAL_Develop = MEAL_ObsData[Test_Base,] #This samples 3/4 of the presence datarows
MEAL_Test = MEAL_ObsData[-Test_Base,] #This takes the remaining 1/4  

Obs_Extract = extract(raster.stack, MEAL_Develop)
Test_Extract = extract(raster.stack, MEAL_Test)

#Extracts values from the background data 
Background_Extract = extract(raster.stack, Background_Points_AK) 

    #Background_Extract = as.data.frame(Background_Extract)
    #Background_Extract[, 'LSC'] = as.factor(Background_Extract[,'LSC']) 

#Creates a vector dataset combining the observation data and background data
pb = c(rep(1, nrow(Obs_Extract)), rep(0, nrow(Background_Extract))) 
pb_test = c(rep(1, nrow(Test_Extract)), rep(0, nrow(Background_Extract))) 
    
#Joins observation data and background data into a single dataset
sdmdata = data.frame(cbind(pb, rbind(Obs_Extract, Background_Extract))) 
sdmdata_test = data.frame(cbind(pb_test, rbind(Test_Extract, Background_Extract))) 

## Make sure the sdm dataframe is using the correct data type for each variable
str((sdmdata))
str((sdmdata_test))

#Make sure these two are the same and there are no errors
head(sdmdata)
head(Obs_Extract)

cor.data = cor(sdmdata[2:4], use = "complete.obs")
cor.data

ggpairs(sdmdata[c(2:6)])

#################################### CLIMATE MODEL SELECTION #################################################################################
#Develops a model selection table based on a set of climate models
glm.model = list()
glm.model[[1]] = with(sdmdata, glm(pb ~ DD5 + MSP, family = binomial(link = "logit")))
glm.model[[2]] = with(sdmdata, glm(pb ~ DD5 + SMH, family = binomial(link = "logit")))
glm.model[[3]] = with(sdmdata, glm(pb ~ TAVE_SM + MSP, family = binomial(link = "logit"))) 
glm.model[[4]] = with(sdmdata, glm(pb ~ DD5, family = binomial(link = "logit")))
glm.model[[5]] = with(sdmdata, glm(pb ~ MSP, family = binomial(link = "logit")))
glm.model[[6]] = with(sdmdata, glm(pb ~ TAVE_SM, family = binomial(link = "logit")))
glm.model[[7]] = with(sdmdata, glm(pb ~ SMH, family = binomial(link = "logit")))

Mod_NAME = c("One", "Two", "Three", "Four", "Five", "Six", "Seven")

aictab(cand.set = glm.model, modnames = NULL, second.ord = TRUE, nobs = NULL, sort = TRUE, c.hat = 1)

#Analyze Model 1 
glm.model1 = with(sdmdata, glm(pb ~ DD5 + MSP, family = binomial(link = "logit")))
summary(glm.model1)

      glm.eval1 = evaluate(sdmdata_test, Background_Extract, glm.model1) 
      glm.eval1
      
      inv.logit(1.248009)  #Input max TPR+TNR to determine the optimal threshold for presense absense in the model
    
          glm_predict = predict(glm.model1, newdata = sdmdata_test, type = "response")
          pb_test = as.factor(pb_test)
          glm_predict_bi = ifelse(glm_predict > 0.776955,
                                "1",
                                "0")
        
          glm_predict_bi = as.factor(glm_predict_bi)
          confusionMatrix(data = glm_predict_bi, reference = pb_test)

              glm.map1 = raster::predict(raster.stack, glm.model1, ext = geographic.extent, type="response")
            
              plot(glm.map1, 
                  xlim = c(min.lon, max.lon),
                  ylim = c(min.lat, max.lat),
                  axes = TRUE)
              
              plot(glm.map1,
                   xlim = c(min.lon, max.lon),
                   ylim = c(min.lat, max.lat),
                   axes = TRUE,
                   xlab=expression("Longitude "*degree),
                   ylab=expression("Latitude "*degree),
                   main="Climate Model")
    
              points(MEAL_ObsData$longitude, MEAL_ObsData$latitude, col = "red", pch = 20, cex = 0.75,  add = TRUE)
              
              box()
              
#Analyze Model 2  
glm.model2 = with(sdmdata, glm(pb ~ DD5 + SMH, family = binomial(link = "logit")))

    glm.eval2 = evaluate(sdmdata_test, Background_Extract, glm.model2) 
    glm.eval2

        glm_predict = predict(glm.model2, newdata = sdmdata_test, type = "response")
        pb_test = as.factor(pb_test)
        glm_predict_bi = ifelse(glm_predict > 0.75,
                                "1",
                                "0")
        
        glm_predict_bi = as.factor(glm_predict_bi)
        confusionMatrix(data = glm_predict_bi, reference = pb_test)

            glm.map2 = raster::predict(raster.stack, glm.model2, ext = geographic.extent, type="response")
            
            plot(glm.map2, 
                 xlim = c(min.lon, max.lon),
                 ylim = c(min.lat, max.lat),
                 axes = TRUE)

#Analyze Model 3            
glm.model3 = with(sdmdata, glm(pb ~ TAVE_SM + MSP, family = binomial(link = "logit"))) 

      glm.eval3 = evaluate(sdmdata_test, Background_Extract, glm.model3) 
      glm.eval3

          glm_predict = predict(glm.model3, newdata = sdmdata_test, type = "response")
          pb_test = as.factor(pb_test)
          glm_predict_bi = ifelse(glm_predict > 0.75,
                                  "1",
                                  "0")
          
          glm_predict_bi = as.factor(glm_predict_bi)
          confusionMatrix(data = glm_predict_bi, reference = pb_test)

                glm.map3 = raster::predict(raster.stack, glm.model3, ext = geographic.extent, type="response")
                
                plot(glm.map3, 
                     xlim = c(min.lon, max.lon),
                     ylim = c(min.lat, max.lat),
                     axes = TRUE)

#Analyze Model 4
glm.model4 = with(sdmdata, glm(pb ~ DD5, family = binomial(link = "logit")))

        glm.eval4 = evaluate(sdmdata_test, Background_Extract, glm.model4) 
        glm.eval4

            glm_predict = predict(glm.model4, newdata = sdmdata_test, type = "response")
            pb_test = as.factor(pb_test)
            glm_predict_bi = ifelse(glm_predict > 0.75,
                                    "1",
                                    "0")
            
            glm_predict_bi = as.factor(glm_predict_bi)
            confusionMatrix(data = glm_predict_bi, reference = pb_test)

                  glm.map4 = raster::predict(raster.stack, glm.model4, ext = geographic.extent, type="response")
                  
                  plot(glm.map4, 
                       xlim = c(min.lon, max.lon),
                       ylim = c(min.lat, max.lat),
                       axes = TRUE)


#Analyze Model 5
glm.model5 = with(sdmdata, glm(pb ~ MSP, family = binomial(link = "logit")))

        glm.eval5 = evaluate(sdmdata_test, Background_Extract, glm.model5) 
        glm.eval5

            glm_predict = predict(glm.model5, newdata = sdmdata_test, type = "response")
            pb_test = as.factor(pb_test)
            glm_predict_bi = ifelse(glm_predict > 0.75,
                                    "1",
                                    "0")
            
            glm_predict_bi = as.factor(glm_predict_bi)
            confusionMatrix(data = glm_predict_bi, reference = pb_test)

                  glm.map5 = raster::predict(raster.stack, glm.model5, ext = geographic.extent, type="response")
                  
                  plot(glm.map5, 
                       xlim = c(min.lon, max.lon),
                       ylim = c(min.lat, max.lat),
                       axes = TRUE)

#Analyze Model 6
glm.model6 = with(sdmdata, glm(pb ~ TAVE_SM, family = binomial(link = "logit")))

        glm.eval6 = evaluate(sdmdata_test, Background_Extract, glm.model6) 
        glm.eval6

              glm_predict = predict(glm.model6, newdata = sdmdata_test, type = "response")
              pb_test = as.factor(pb_test)
              glm_predict_bi = ifelse(glm_predict > 0.75,
                                      "1",
                                      "0")
                
              glm_predict_bi = as.factor(glm_predict_bi)
              confusionMatrix(data = glm_predict_bi, reference = pb_test)

                    glm.map6 = raster::predict(raster.stack, glm.model4, ext = geographic.extent, type="response")
                      
                    plot(glm.map6, 
                           xlim = c(min.lon, max.lon),
                           ylim = c(min.lat, max.lat),
                           axes = TRUE)

#Analyze Model 7
glm.model7 = with(sdmdata, glm(pb ~ SMH, family = binomial(link = "logit")))

        glm.eval7 = evaluate(sdmdata_test, Background_Extract, glm.model7) 
        glm.eval7

              glm_predict = predict(glm.model7, newdata = sdmdata_test, type = "response")
              pb_test = as.factor(pb_test)
              glm_predict_bi = ifelse(glm_predict > 0.75,
                                        "1",
                                        "0")
                
               glm_predict_bi = as.factor(glm_predict_bi)
               confusionMatrix(data = glm_predict_bi, reference = pb_test)

                   glm.map7 = raster::predict(raster.stack, glm.model7, ext = geographic.extent, type="response")
                    
                   plot(glm.map7, 
                         xlim = c(min.lon, max.lon),
                         ylim = c(min.lat, max.lat),
                         axes = TRUE)

#################################### MULTI-VARIATE MODEL SELECTION #################################################################################
#Develops a model selection table based on a set of multi-variate models                      
glm.model2 = list()
glm.model2[[1]] = with(sdmdata, glm(pb ~ LSC_DIS_KM + DD5 + MSP, family = binomial(link = "logit")))
glm.model2[[2]] = with(sdmdata, glm(pb ~ LSC_DIS_KM + DD5, family = binomial(link = "logit")))
glm.model2[[3]] = with(sdmdata, glm(pb ~ LSC_DIS_KM + MSP, family = binomial(link = "logit"))) 
glm.model2[[4]] = with(sdmdata, glm(pb ~ LSC_DIS_KM + TAVE_SM + MSP, family = binomial(link = "logit")))
glm.model2[[5]] = with(sdmdata, glm(pb ~ LSC_DIS_KM + TAVE_SM, family = binomial(link = "logit")))
glm.model2[[6]] = with(sdmdata, glm(pb ~ LSC_DIS_KM + DD5 + SMH, family = binomial(link = "logit")))
glm.model2[[7]] = with(sdmdata, glm(pb ~ LSC_DIS_KM + SMH, family = binomial(link = "logit"))) 
glm.model2[[8]] = with(sdmdata, glm(pb ~ LSC_DIS_KM, family = binomial(link = "logit")))
                      
Mod_NAME = c("One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight")
                      
aictab(cand.set = glm.model2, modnames = NULL, second.ord = TRUE, nobs = NULL, sort = TRUE, c.hat = 1)
                      
#Analyze Model 2.1    
glm.model2.1 = with(sdmdata, glm(pb ~ LSC_DIS_KM + DD5 + MSP, family = binomial(link = "logit")))

    glm.eval2.1 = evaluate(sdmdata_test, Background_Extract, glm.model2.1) 
    glm.eval2.1
    
        glm_predict = predict(glm.model2.1, newdata = sdmdata_test, type = "response")
        pb_test = as.factor(pb_test)
        glm_predict_bi = ifelse(glm_predict > 0.75,
                                "1",
                                "0")
        
        glm_predict_bi = as.factor(glm_predict_bi)
        confusionMatrix(data = glm_predict_bi, reference = pb_test)
        
            glm.map2.1 = raster::predict(raster.stack, glm.model2.1, ext = geographic.extent, type="response")
            
            plot(glm.map2.1, 
                 xlim = c(min.lon, max.lon),
                 ylim = c(min.lat, max.lat),
                 axes = TRUE)

#Analyze Model 2.2  
glm.model2.2 = with(sdmdata, glm(pb ~ LSC_DIS_KM + DD5, family = binomial(link = "logit")))

    glm.eval2.2 = evaluate(sdmdata_test, Background_Extract, glm.model2.2) 
    glm.eval2.2
    
        glm_predict = predict(glm.model2.2, newdata = sdmdata_test, type = "response")
        pb_test = as.factor(pb_test)
        glm_predict_bi = ifelse(glm_predict > 0.75,
                                "1",
                                "0")
        
        glm_predict_bi = as.factor(glm_predict_bi)
        confusionMatrix(data = glm_predict_bi, reference = pb_test)
        
            glm.map2.2 = raster::predict(raster.stack, glm.model2.2, ext = geographic.extent, type="response")
            
            plot(glm.map2.2, 
                 xlim = c(min.lon, max.lon),
                 ylim = c(min.lat, max.lat),
                 axes = TRUE)

#Analyze Model 2.3
glm.model2.3 = with(sdmdata, glm(pb ~ LSC_DIS_KM + MSP, family = binomial(link = "logit"))) 

    glm.eval2.3 = evaluate(sdmdata_test, Background_Extract, glm.model2.3) 
    glm.eval2.3
    
        glm_predict = predict(glm.model2.3, newdata = sdmdata_test, type = "response")
        pb_test = as.factor(pb_test)
        glm_predict_bi = ifelse(glm_predict > 0.75,
                                "1",
                                "0")
        
        glm_predict_bi = as.factor(glm_predict_bi)
        confusionMatrix(data = glm_predict_bi, reference = pb_test)
        
            glm.map2.3 = raster::predict(raster.stack, glm.model2.3, ext = geographic.extent, type="response")
            
            plot(glm.map2.3, 
                 xlim = c(min.lon, max.lon),
                 ylim = c(min.lat, max.lat),
                 axes = TRUE)

#Analyze Model 2.4
glm.model2.4 = with(sdmdata, glm(pb ~ LSC_DIS_KM + TAVE_SM + MSP, family = binomial(link = "logit")))

    glm.eval2.4 = evaluate(sdmdata_test, Background_Extract, glm.model2.4) 
    glm.eval2.4
    
        glm_predict = predict(glm.model2.4, newdata = sdmdata_test, type = "response")
        pb_test = as.factor(pb_test)
        glm_predict_bi = ifelse(glm_predict > 0.75,
                                "1",
                                "0")
        
        glm_predict_bi = as.factor(glm_predict_bi)
        confusionMatrix(data = glm_predict_bi, reference = pb_test)
        
            glm.map2.4 = raster::predict(raster.stack, glm.model2.4, ext = geographic.extent, type="response")
            
            plot(glm.map2.4, 
                 xlim = c(min.lon, max.lon),
                 ylim = c(min.lat, max.lat),
                 axes = TRUE)

#Analyze Model 2.5
glm.model2.5 = with(sdmdata, glm(pb ~ LSC_DIS_KM + TAVE_SM, family = binomial(link = "logit")))

    glm.eval2.5 = evaluate(sdmdata_test, Background_Extract, glm.model2.5) 
    glm.eval2.5
    
        glm_predict = predict(glm.model2.5, newdata = sdmdata_test, type = "response")
        pb_test = as.factor(pb_test)
        glm_predict_bi = ifelse(glm_predict > 0.75,
                                "1",
                                "0")
        
        glm_predict_bi = as.factor(glm_predict_bi)
        confusionMatrix(data = glm_predict_bi, reference = pb_test)
        
            glm.map2.5 = raster::predict(raster.stack, glm.model2.5, ext = geographic.extent, type="response")
            
            plot(glm.map2.5, 
                 xlim = c(min.lon, max.lon),
                 ylim = c(min.lat, max.lat),
                 axes = TRUE)

#Analyze Model 2.6  
glm.model2.6 = with(sdmdata, glm(pb ~ LSC_DIS_KM + DD5 + SMH, family = binomial(link = "logit")))
summary(glm.model2.6)

    glm.eval2.6 = evaluate(sdmdata_test, Background_Extract, glm.model2.6) 
    glm.eval2.6
    
    inv.logit(2.265676)  #Input max TPR+TNR to determine the optimal threshold for presense absense in the model
    
        glm_predict = predict(glm.model2.6, newdata = sdmdata_test, type = "response")
        pb_test = as.factor(pb_test)
        glm_predict_bi = ifelse(glm_predict > 0.9059942,
                                "1",
                                "0")
        
        glm_predict_bi = as.factor(glm_predict_bi)
        confusionMatrix(data = glm_predict_bi, reference = pb_test)
        
            glm.map2.6 = raster::predict(raster.stack, glm.model2.6, ext = geographic.extent, type="response")
            
            plot(glm.map2.6, 
                 xlim = c(min.lon, max.lon),
                 ylim = c(min.lat, max.lat),
                 axes = TRUE)
            
            plot(glm.map2.6,
                 xlim = c(min.lon, max.lon),
                 ylim = c(min.lat, max.lat),
                 axes = TRUE,
                 xlab=expression("Longitude "*degree),
                 ylab=expression("Latitude "*degree),
                 main="Anthropogenic Model")

            points(MEAL_ObsData$longitude, MEAL_ObsData$latitude, col = "red", pch = 20, cex = 0.75,  add = TRUE)
            
            box()
            
            
#Analyze Model 2.7
glm.model2.7 = with(sdmdata, glm(pb ~ LSC_DIS_KM + SMH, family = binomial(link = "logit"))) 

    glm.eval2.7 = evaluate(sdmdata_test, Background_Extract, glm.model2.7) 
    glm.eval2.7
    
        glm_predict = predict(glm.model2.7, newdata = sdmdata_test, type = "response")
        pb_test = as.factor(pb_test)
        glm_predict_bi = ifelse(glm_predict > 0.75,
                                "1",
                                "0")
        
        glm_predict_bi = as.factor(glm_predict_bi)
        confusionMatrix(data = glm_predict_bi, reference = pb_test)
        
            glm.map2.7 = raster::predict(raster.stack, glm.model2.7, ext = geographic.extent, type="response")
            
            plot(glm.map2.7, 
                 xlim = c(min.lon, max.lon),
                 ylim = c(min.lat, max.lat),
                 axes = TRUE)

#Analyze Model 2.8
glm.model2.8 = with(sdmdata, glm(pb ~ LSC_DIS_KM, family = binomial(link = "logit")))
                      
    glm.eval2.8 = evaluate(sdmdata_test, Background_Extract, glm.model2.7) 
    glm.eval2.8                     
    
        glm_predict = predict(glm.model2.8, newdata = sdmdata_test, type = "response")
        pb_test = as.factor(pb_test)
        glm_predict_bi = ifelse(glm_predict > 0.75,
                                "1",
                                "0")
        
        glm_predict_bi = as.factor(glm_predict_bi)
        confusionMatrix(data = glm_predict_bi, reference = pb_test)
        
            glm.map2.8 = raster::predict(raster.stack, glm.model2.8, ext = geographic.extent, type="response")
            
            plot(glm.map2.8, 
                 xlim = c(min.lon, max.lon),
                 ylim = c(min.lat, max.lat),
                 axes = TRUE)               
                
####################################ASSESS BEST DISTRIBUTION MODEL#################################################################################
# Plots the AUC curve
par(mfrow=c(1,2))    
plot(glm.eval1,"ROC")
plot(glm.eval1,"TPR")

par(mfrow=c(1,2))    
plot(glm.eval2.6,"ROC")
boxplot(glm.eval2.6, type='response', ylab="Model prediction (logit)")  
plot(glm.eval2.6,"TPR")

#Boxplots of predictions (logit scale) for presence and background data
par(mfrow=c(1,2))   
boxplot(glm.eval1, type='response', ylab="Model prediction (logit)")  
density(glm.eval1)

threshold(glm.eval1)

#Response curves
response(glm.model1, fun=function(x, y) predict(x, y, type='response'))  

response(glm.model2.6, fun=function(x, y) predict(x, y, type='response'))

response(glm.model2.1, fun=function(x, y) predict(x, y, type='response'))

#Fancy Response curves
      ggLSC = ggplot(sdmdata, aes(x = LSC_DIS_KM, y = pb)) + geom_point() + 
        stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE)
      ggDD5 = ggplot(sdmdata, aes(x = DD5, y = pb)) + geom_point() + 
        stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE)
      ggTAVE_SM = ggplot(sdmdata, aes(x = TAVE_SM, y = pb)) + geom_point() + 
        stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE)
      ggMSP = ggplot(sdmdata, aes(x = MSP, y = pb)) + geom_point() + 
        stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE)
      ggSMH = ggplot(sdmdata, aes(x = SMH, y = pb)) + geom_point() + 
        stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE)
      
      plot_grid(ggDD5, ggTAVE_SM, ggMSP, ggSMH, ggLSC, labels = "AUTO")

##################################### MODEL AVERAGING ###############################################################################################
# Stack the two model outputs together and rename them
models = stack(glm.map1, glm.map2.6)
names(models) <- c("Landscape Condition", "Climate")
plot(models)

# A weighted average of the two models based on their adjusted AUC score
auc <- list((0.8046286-0.5^2), (0.7107288-0.5^2))
model_average <- weighted.mean(models, auc)
plot(model_average, main='average score')


#####################################EXPORT SPATIAL DATA TO GIS###############################################################################################
Background_Points= as_Spatial(c(Background_Points_AK))

writeSpatialShape(Background_Points_AK, "Background_Points")

writeOGR(obj=Background_Points_AK, dsn ="~/Desktop/NTNU Masters/BI3036/Semester Project/R_Data", layer="Background_Points", driver="ESRI Shapefile")

writeRaster(glm.map1, filename = 'glmMap1', format = "GTiff", overwrite=TRUE)

writeRaster(glm.map2.6, filename = 'glmMap2.6', format = "GTiff", overwrite=TRUE)

writeRaster(model_average, filename = 'glmAvg', format = "GTiff")

write.csv(MEAL_Develop, file = 'MEAL_Develop')
write.csv(MEAL_Test, file = 'MEAL_Test')
