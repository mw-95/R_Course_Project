# This Script creates Training Point Samples from provided Polygons
# and randomly reassignes a fraction of them to a wrong class.
# This is done to test the Sensitivity of the supervised Classification Algorithms
# provided by the function "superClass()" in RStoolbox.

# Originally written by Marius Witt in January 2020, latest Version: March 2020
# Code is good to go!

library(rstudioapi)
library(dplyr)
library(sp)
library(sf)
library(raster)
library(rgdal)

#########################################################################################################

###################################################
#                                                 #
#           !!! Important Note !!!                #
#                                                 # 
#   !Save the script and the files you want to    #
#   use it on (imagery, shapefiles) in the same   # 
#   folder on your PC, otherwise the script will  #
#   fail!                                         #
#                                                 #
###################################################

wd <- dirname(getSourceEditorContext()$path)
setwd(wd) 

BRCK <- "Test_Data.tif" # Brick of Remote Sensing Imagery
S2Brick <- brick(BRCK)

#Setup Part, not needed if you just want to run the Code with the provided Data
# For Classification, we need S2 Imagery
#files.list <- list.files("./",pattern="60m.tif")

#rasterlist <- list()

#for (i in 1:length(files.list)){
#  rasterlist[[i]] <- raster(files.list[i]) 
#}

#S2Brick <- brick(rasterlist)
#writeRaster(S2Brick,"S2Brick_1.tif")
#rm(rasterlist)

########################################################

# Sampling Random Points

# Training Data Polygons
vec <- readOGR("./TrainingPoly.shp")
# Validation Data Polygons
val <- readOGR("./ValidationData.shp")
# Number of Samples per Landcover Class
numsamps <- 50
# Name of the Class Column (in the Dataframe)
ColNam <- "Class" 


# Sampling the TrainingPoints
uniqueClass <- as.character(unique(vec$Class))
for (i in 1:length(uniqueClass)) {
  class_data <- vec[vec$Class==uniqueClass[i],]
  classpts <- spsample(class_data, type="random",n=numsamps)
  if(i == 1){
  xy <- classpts
} else {
  xy <- rbind(xy,classpts)
}
}

rm(i)

# Creating a SPDF from the Training Points
temp <- over(x=xy, y=vec)
TrainVals <- SpatialPointsDataFrame(xy@coords,temp,proj4string = xy@proj4string)

# Sampling the Validation Points
uniqueClass <- as.character(unique(val$Class))
for (i in 1:length(uniqueClass)) {
  class_data <- val[val$Class==uniqueClass[i],]
  classpts <- spsample(class_data, type="random",n=numsamps)
  if(i == 1){
    pq <- classpts
  } else {
    pq <- rbind(pq,classpts)
  }
}

rm(i)


# Creating a SPDF from the Validation Points
temp2 <- over(x=pq, y=val)
Valid <- SpatialPointsDataFrame(pq@coords,temp2,proj4string = xy@proj4string)


#######################################################

# Calculate the Total Number of Samples
TotalSamps <- (length(uniqueClass)*numsamps)
# Create a sequence of the same length
WrongSeq <- seq(1,TotalSamps,1)

# Create a vector with the Fractions of randomly reassigned Data
WrongData <- seq(0.025,0.5,0.025)

# Randomly select the training points, whose class designations are going to be replaced
WrongSamp <- list()

for (i in 1:length(WrongData)){
  WrongSamp[[i]] <- sample(WrongSeq,(WrongData[i]*TotalSamps))
}

rm(i)

# Reassign training points with wrong class designations
ReasData <- list()

for (i in 1:length(WrongSamp)){
  tmp <- WrongSamp[[i]]
  for (q in 1:length(tmp)){
    tmp3 <- uniqueClass[!uniqueClass %in% TrainVals@data[tmp[q],ColNam]]
    TrainVals[tmp[q],ColNam] <- sample(tmp3,1)
  }  
  ReasData[[i]] <- TrainVals
  # Recreate the unspoiled TrainVals again, else the result will be biased
  TrainVals <- SpatialPointsDataFrame(xy@coords,temp,proj4string = xy@proj4string)
}

rm(i)
rm(q)

# Prepend the unspoiled Data to the list of spoiled Data
ReasData <- append(ReasData,TrainVals,0)

# Prepend a 0 to the fractions of Faulty Data
WrongData <- append(WrongData,0,0)

# Cleanup
rm(WrongSeq)
rm(TotalSamps)
rm(WrongSamp)
######################################################################################

# Load RStoolbox for the next step
library(RStoolbox)
library(ggplot2)
library(lattice)

Ergebnis <- list()
Ergebnis.list <- list()
nRep <- 3    # Number of Repetitions of the Classification
mod <- "mlc" # Classification scheme used. Can be either "mlc" (Maximum Likelihood) or "rf" (Random Forest).
             # RF classifaction slows down the Code cosiderably.

# Run the Classification nRep times for each fraction of faulty Data

######################################################################################
#                                                                                    #
#! Runtime of the Classification with the Presets (and supplied Data) ca. 2:30 Mins !#
#                                                                                    #
######################################################################################                                                                                    

for(q in 1:nRep){
for(i in 1:length(ReasData)){
sc <- superClass(S2Brick,trainData = ReasData[[i]],valData = Valid,responseCol = ColNam, model = mod)
Ergebnis[[i]] <-  sc$modelFit[[1]] # we are only interested in the Classifications accuracy and dump the rest 
}
  Ergebnis.list[[q]] <- Ergebnis
}
######################################################################################

df.temp <- data.frame()
df.list <- list()

#Create a List of plottable Dataframes for every repeat nRep

for(g in 1:nRep){
 TEMP <- Ergebnis.list[[g]]
  for(q in 1:length(Ergebnis)){
    for(i in 1:3){
      df.temp[q,i] <- TEMP[[(q)]][1,i]
  }
 }
  df.list[[g]] <- df.temp
}

#Cleanup
rm(g)
rm(q)
rm(i)

# Append the corresponding Data fractions to the Dataframe
for (i in 1:nRep){
 df.list[[i]][,4] <- WrongData 
}


# Bind all the Dataframes in df.list into one plottable Dataframe
plot_df <- bind_rows(df.list)

######################################################################################
# Plot the Results using the ggplot2

library(ggpmisc) # Used to display R² and the Regression Formula
library(glue) # Used for the Subtitle

my.formula <- y ~ x
p <- ggplot(data = plot_df, aes(x = V4, y = V1)) +
  geom_smooth(method = "lm", se=TRUE, color="blue", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               label.x = "right", label.y = "top",
               parse = TRUE) +         
  geom_point() + labs(title = "Response of the overall classification accuracy to increasing fractions of wrong data using the superClass function", subtitle = "Based on CLC 2018 with 12 Classes and 60m Resolution Sentinel-2 images",
                      caption = glue("Used classification: {mod}")) +
  xlab("Fraction of wrong data") + ylab("Overall accuracy") + ylim(min(plot_df$V1)-0.02,max(plot_df$V1)+0.02)
p

# It should show a strong linearity between the fraction of wrong Data and
# the Overall Accuracy of the Classification Output.
