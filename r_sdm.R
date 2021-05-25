# Load Packages
library(dismo)
library(dplyr)
library(ggplot2)
library(raster)
library(gtools)
library(rJava)
library(geosphere)
library(sf)
library(rgdal)

##############################################################################
# get_species_bioclim = function(){
#  return(1)
# }
# data = get_species_bioclim()
##############################################################################

na_extent = extent(-135, -50, 20, 60)
bioclim_vars = c('bio1', 'bio2', 'bio3', 'bio4', 'bio5',
                 'bio6', 'bio7', 'bio8', 'bio9', 'bio10',
                 'bio11', 'bio12', 'bio13', 'bio14', 'bio15',
                 'bio16', 'bio17', 'bio18', 'bio19')
  
tplot = function(type, obj, color = "") {
  if (type == 'coord'){
    points(obj[c("Longitude", "Latitude")],
           pch  = "+", 
           cex = 0.5, 
           col = color)
  }
  else if (type == 'raster'){
    plot(obj, 
         ext = na_extent)
  }
  else if (type == 'shape'){
    plot(obj,
         add = TRUE)
  }
}



# Setting up global variables and loading data
species_name = "Dreissena polymorpha"
setwd('./data')
bioclim_data_future = stack("wc2.1_5m_bioc_MIROC6_ssp585_2021-2040.tif")
#bioclim_data_future_245 = stack("wc2.1_5m_bioc_BCC-CSM2-MR_ssp585_2021-2040.tif")
#bioclim_data_future_370 = stack("wc2.1_5m_bioc_BCC-CSM2-MR_ssp585_2021-2040.tif")
#bioclim_data_future_585 = stack("wc2.1_5m_bioc_BCC-CSM2-MR_ssp585_2021-2040.tif")

names(bioclim_data_future) = bioclim_vars

species_data = read.csv("~/Desktop/Invasive_species_modeling/data/Dreissena_polymorpha_before2000.csv")
species_data = subset(species_data, select = -after2000)
species_locations = select(species_data, Longitude, Latitude)

setwd("./wc2.1_5m_bio")

rastlist = mixedsort(list.files(path = ".", pattern='.tif$', all.files=TRUE, full.names=FALSE))
bioclim_data_current = stack(rastlist)

names(bioclim_data_current) = bioclim_vars

# Generating Species Absence Points
absence_num = (dim(species_data)[1])*0.4
absence_points = randomPoints(bioclim_data_current, absence_num, species_locations, ext=na_extent, extf=1, excludep=TRUE)
absence_points = as.data.frame(absence_points)
absence_points = absence_points %>% rename(Longitude=x,Latitude=y)
absence_points = cbind(Species = species_name, absence_points)
absence_points['Occur'] = 0

# Update species occurence data w/ absence points and bioclimate data
species_data = bind_rows(species_data, absence_points)
species_locations = select(species_data, Longitude, Latitude)
species_bioclim_locations = extract(bioclim_data_current, species_locations)
species_data = cbind(species_data, species_bioclim_locations)

#map("world", "USA", xlim=c(-135, -50), ylim=c(20, 60), col="gray90", fill=TRUE)

#plot(bioclim_data_current)

# fit maximum entropy model to get bioclimate variable contribution 
fold <- kfold(species_locations, k=5)
occtest <- species_locations[fold == 1, ]
occtrain <- species_locations[fold != 1, ]

me <- maxent(bioclim_data_current, occtrain)
plot(me)

# visualizing absence/occurrence relations to 2 most important bioclimate variables
ggplot(species_data,
       mapping = aes(x = bio14, y = bio4, color = Occur)) +
  geom_point()

# setting up logistic regression model for the 2 most important bioclimate variable
logistic_regr_model = glm(Occur ~ bio14 + bio4,
                           family = binomial(link = "logit"),
                           data = species_data)
summary(logistic_regr_model)

# evaluate lrm accuracy by plotting ROC curve
presence_data = filter(species_data, Occur == 1)
absence_data = filter(species_data, Occur == 0)

evaluation = evaluate(presence_data, absence_data, logistic_regr_model)
plot(evaluation, 'ROC')

# predict, threshold, and visualize using current bioclimate data 
predictions = predict(bioclim_data_current,
                      logistic_regr_model,
                      type = 'response')
tr = threshold(evaluation, stat = 'prevalence')

plot(predictions > 0.5, ext = na_extent)
writeRaster(predictions, '~/Desktop/Invasive_species_modeling/results/Aegilops_Triuncialis_current.tif')
#points(presence_data[c("Longitude", "Latitude")], pch  = "+", cex = 0.5, col = 'red')
setwd('..')
us_state = readOGR(dsn="./us_states_5m", layer="cb_2018_us_state_5m")
plot(us_state, 
     add = TRUE,)
#points(absence_data[c("Longitude", "Latitude")], pch  = "+", cex = 0.5, col = 'red')


# forecast using future bioclimate data
forcasts = predict(bioclim_data_future,
                   logistic_regr_model,
                   type = 'response')

plot(forcasts > 0.5, ext = na_extent)
plot(predictions - forcasts, ext = na_extent)
plot(bioclim_data_future)
