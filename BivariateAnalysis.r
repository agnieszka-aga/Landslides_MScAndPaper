# Histograms -  landslide cells distribution in feature classes

library(tidyverse)
library(binr)
library(ggplot2)
library(gtools)

DataSet <- read.delim("D:/.../DataSet_beforeScaling2.txt", sep = ",")

### 1. The dataset was splited into two datasets with categorical and continuous variables to make the computation faster

	# Due to memory problems this step was done in IBM SPSS Modeler
	# Possible solution in R:
	
		#DataSet_Cat <- subset(DataSet, select=c("Landforms", "Climate_zones_null", "Landuse_null", "Irrigated_areas_null", "Aspect_N", "Forest", "Geology_null", "Soils_null", "ls_inventory1", "Aspect_S", "Volcanoes", "ls_inventory2", "Vegetation_null", "Temperature_new3_null", "Precipitation_null", "cut_fill_flowdir"))
		#write_delim(DataSet_Cat, "D:/.../DataSet_Cat.txt")

		#DataSet_Cont <- subset(DataSet, select=c("Topographic.Position.Index", "Slope", "mo_utmc30_cut_saga", "Sky.View.Factor", "Topographic.Wetness.Index_null", "Faults_all_eucdist", "railroads_all_eucdist", "rivers_all_eucdist", "ls_inventory1", "roads_all_eucdist", "cut_curvature_saga", "ls_inventory2", "Plan.Curvature", "Profile.Curvature", "Terrain.Ruggedness.Index..TRI.", "LS.Factor_null", "fill_flowdir_flowacc", "Stream.Power.Index", "Total.Catchment.Area", "Faults_allnew_eucdist"))
		#write_delim(DataSet_Cont, "D:/.../DataSet_Cat.txt")


### 2. Dataset preparation - dataset with continuous variables

DataSet <- read.delim("D:/.../DataSet_beforeScaling_Cont.txt") 
colnames(DataSet)
str(DataSet)

#head(DataSet$Sky.View.Factor)
#min(DataSet$Sky.View.Factor)


## a. Classification of continuous parameters

DataSetClass <- DataSet %>% 
	mutate(TPI_class=cut(DataSet$TPI, breaks=c(min(DataSet$TPI), -0.0001, 0.0001, max(DataSet$TPI)), 
	labels=c("lower", "flat", "higher"), include.lowest=TRUE, right=FALSE)) # lower = min to -0.0001, flat = -0.0001 to 0.0001, higher = 0.0001 to max

	mutate(slope_class=cut(DataSet$Slope, breaks=c(min(DataSet$Slope), 0.175, 0.35, 0.525, 0.70, 0.875, 1.05, 1.225, max(DataSet$Slope)), 
	labels=c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80"), include.lowest=TRUE, right=FALSE)) # breaks in rad, labels in degrees, 70-80 = 70-max

	mutate(Elevation_class=cut(DataSet$Elevation_saga, breaks=c(min(DataSet$Elevation_saga), 500, 700, 900, 1100, 1300, 
			1500, 1700, 1900, 2100, 2300, 2500, 2700, 2900, 3100, 3300, 3500, 3700, 3900, max(DataSet$Elevation_saga)), 
	labels=c("300-500", "500-700", "700-900", "900-1100", "1100-1300", "1300-1500", "1500-1700", "1700-1900", "1900-2100", 
			"2100-2300", "2300-2500", "2500-2700", "2700-2900", "2900-3100", "3100-3300", "3300-3500", "3500-3700", "3700-3900", "3900-4100"), include.lowest=TRUE, right=FALSE)) # 300-500 = min(385) - 500, 3900-4100 = 3900 - max(4070)
	
	mutate(SkyViewFactor_class=cut(DataSet$Sky_View_Factor, breaks=c(min(DataSet$Sky_View_Factor), 0.2, 0.4, 0.6, 0.8, max(DataSet$Sky_View_Factor)), 
	labels=c("0-0.2", "0.2-0.4", "0.4-0.6", "0.6-0.8", "0.8-1.0"), include.lowest=TRUE, right=FALSE)) # min = 0.142
	
	mutate(TWI_class=cut(DataSet$TWI_null, breaks=c(min(DataSet$TWI_null), 6, 11, 16, 21, max(DataSet$TWI_null)), 
	labels=c("1-6", "6-11", "11-16", "16-21", "21-26"), include.lowest=TRUE, right=FALSE)) # max = 25.8
	
	mutate(Faults_class=cut(DataSet$Faults_all_eucdist, breaks=c(min(DataSet$Faults_all_eucdist), 500, 1000, 2000, 3000, 5000, 
			7500, 10000, 15000, 20000, 30000, 50000, max(DataSet$Faults_all_eucdist)), 
	labels=c("0-500", "500-1000", "1000-2000", "2000-3000", "3000-5000", "5000-7500", "7500-10000", "10000-15000", "15000-20000", 
			"20000-30000", "30000-50000", "50000-70000"), include.lowest=TRUE, right=FALSE)) # max = 66126 m (?)
			
	mutate(Railroads_class=cut(DataSet$railroads_all_eucdist, breaks=c(min(DataSet$railroads_all_eucdist), 500, 1000, 2000, 3000, 5000, 
			7500, 10000, 15000, 20000, 30000, 50000, max(DataSet$railroads_all_eucdist)), 
	labels=c("0-500", "500-1000", "1000-2000", "2000-3000", "3000-5000", "5000-7500", "7500-10000", "10000-15000", "15000-20000", 
			"20000-30000", "30000-50000", "50000-70000"),include.lowest=TRUE, right=FALSE)) # max = 68412 m 

	mutate(Rivers_class=cut(DataSet$rivers_all_eucdist, breaks=c(min(DataSet$rivers_all_eucdist), 500, 1000, 2000, 3000, 4000, 
			5000, 6000, 7000, 80000, 90000, max(DataSet$rivers_all_eucdist)), 
	labels=c("0-500", "500-1000", "1000-2000", "2000-3000", "3000-4000", "4000-5000", "5000-6000", "6000-7000", "7000-8000", 
			"8000-9000", "9000-10000"),include.lowest=TRUE, right=FALSE)) # max = 9489 m 
	
	mutate(Roads_class=cut(DataSet$roads_all_eucdist, breaks=c(min(DataSet$roads_all_eucdist), 500, 1000, 2000, 3000, 5000, 
			7500, 10000, 15000, max(DataSet$roads_all_eucdist)), 
	labels=c("0-500", "500-1000", "1000-2000", "2000-3000", "3000-5000", "5000-7500", "7500-10000", "10000-15000", "15000-20000"), include.lowest=TRUE, right=FALSE)) # max = 16078 m 
	
	mutate(Curvature_class=cut(DataSet$cut_curvature_saga, breaks=c(min(DataSet$cut_curvature_saga), -0.0001, 0.0001, max(DataSet$cut_curvature_saga)), 
	labels=c("convex", "flat", "concave"), include.lowest=TRUE, right=FALSE)) # convex = min to -0.0001, flat = -0.0001 to 0.0001, concave = 0.0001 to max; min: -30,22, max: 38,44
	
	mutate(PlanCurv_class=cut(DataSet$Plan_Curvature, breaks=c(min(DataSet$Plan_Curvature), -0.00001, 0.00001, max(DataSet$Plan_Curvature)), 
	labels=c("concave", "flat", "convex"), include.lowest=TRUE, right=FALSE)) # concave = min to -0.00001, flat = -0.00001 to 0.00001, convex = 0.00001 to max; min: -0,038, max: 0,045
	
	mutate(ProfileCurv_class=cut(DataSet$Profile_Curvature, breaks=c(min(DataSet$Profile_Curvature), -0.00001, 0.00001, max(DataSet$Profile_Curvature)), 
	labels=c("concave", "flat", "convex"), include.lowest=TRUE, right=FALSE)) # concave = min to -0.00001, flat = -0.00001 to 0.00001, convex = 0.00001 to max; min: -0,041, max: 0,045
	
	mutate(TRI_class=cut(DataSet$TRI, breaks=c(min(DataSet$TRI), 20, 40, 60, 80, max(DataSet$TRI)), 
	labels=c("0-20", "20-40", "40-60", "60-80", "80-100"), include.lowest=TRUE, right=FALSE)) #  80-100 = 80-max(94)
	
	mutate(LSF_class=cut(DataSet$LSFactor_null, breaks=c(min(DataSet$LSFactor_null), 5, 10, 20, 40, 60, 80, max(DataSet$LSFactor_null)), 
	labels=c("0-5", "5-10", "10-20", "20-40", "40-60", "60-80", "80-190"),include.lowest=TRUE, right=FALSE)) #  80-190 = 80-max(187)
	
	mutate(FlowAcc_class=cut(DataSet$fill_flowdir_flowacc, breaks=c(min(DataSet$fill_flowdir_flowacc), 100, 500, 5000, 10000, 20000, 
			50000, 500000, 1000000, 5000000, 10000000, max(DataSet$fill_flowdir_flowacc)), 
	labels=c("0-100", "100-500", "500-5000", "5000-10000", "10000-20000", "20000-50000", "50000-500000", "500000-1Mi", "1Mi-5Mi", 
			"5Mi-10Mi", "10Mi-15Mi"), include.lowest=TRUE, right=FALSE)) # max = 12961395
	
	mutate(SPI_class=cut(DataSet$SPI, breaks=c(min(DataSet$SPI), 100, 500, 5000, 10000, 20000, 
			50000, 500000, 1000000, 5000000, 10000000, 100000000, 500000000, max(DataSet$SPI)), 
	labels=c("0-100", "100-500", "500-5000", "5000-10000", "10000-20000", "20000-50000", "50000-500000", "500000-1Mi", "1Mi-5Mi", 
			"5Mi-10Mi", "10Mi-100Mi", "100Mi-500Mi", "500Mi-900Mi"), include.lowest=TRUE, right=FALSE)) # max = 812259328

	mutate(TCA_class=cut(DataSet$Total_Catchment_Area, breaks=c(min(DataSet$Total_Catchment_Area), 1600, 3000, 5000, 25000, max(DataSet$Total_Catchment_Area)), 
	labels=c("900-1600", "1600-3000", "3000-5000", "5000-25000", "25000-1Mld"), include.lowest = TRUE, right = FALSE))


	
# Check	
#head(DataSet$TPI_class, 20)
#table(DataSet$TPI_class) # Output: lower 15472338 (data points), flat 142456 (data point) higher 15414376 (data points / cells)

write_delim(DataSetClass, "D:/.../DataSet_Cont_Class.txt")

## b. Create a subset with landslide cells (cell value = 1)

# ls_inventory1 -> ls = landslides

DataSetLS <- subset(DataSetClass, DataSetClass$ls_inventory1==1 | DataSetClass$ls_inventory2==1) # 1665613 cells, 37 variables
write_delim(DataSetLS, "D:/.../DataSetLS_ContClass.txt")

DataSetLS_inventory1 <- subset(DataSetClass, DataSetClass$ls_inventory1==1) # 626847 cells (626879) diff: 32; 37 variables
write_delim(DataSetLS_inventory1, "D:/.../DataSetLS_ContClass_inventory1.txt")

DataSetLS_inventory2 <- subset(DataSetClass, DataSetClass$ls_inventory2==1) # 1342977 cells (1343156) diff: 179 cells; 37 variables
write_delim(DataSetLS_inventory2, "D:/.../DataSetLS_ContClass_inventory2.txt")

### 3. Dataset preparation - dataset with categorical variables

DataSet <- read.delim("D:/.../DataSet_beforeScaling_Cat.txt") # 16 variables

## a. Replace aspect values with 1 

## When aspect N > 0 => aspect S = 0 & when aspect S > 0 => aspect N = 0 
## -> Replacing continuous aspect values with 1 will simplify the results: 1 = Aspect IS N (S), 0 = aspect IS NOT N (S) 
## (since only this general information is needed)

DataSet_Cat_AspectNTo1 <- DataSet_Cat %>% 
	mutate(AspectNto1 = replace(DataSet_Cat$Aspect_N, DataSet_Cat$Aspect_N > 0, 1)) 

DataSet_Cat_AspectTo1 <- DataSet_Cat_AspectNto1 %>%
	mutate(AspectSto1 = replace(DataSet_Cat_AspectNTo1$Aspect_S, DataSet_Cat_AspectNTo1$Aspect_S > 0, 1))

write_delim(DataSet_Cat_AspectTo1, "D:/.../DataSet_Cat_AspectTo1.txt")

## b. Create a subset with landslide cells (cell value = 1)

DataSetLS_Cat <- subset(DataSet_Cat_AspectTo1, DataSet_Cat_AspectTo1$ls_inventory1==1 | DataSet_Cat_AspectTo1$ls_inventory2==1) # 1665613 cells,
write_delim(DataSetLS_Cat, "D:/.../DataSetLS_Cat.txt")

DataSetLS_Cat_inventory1 <- subset(DataSet_Cat_AspectTo1, DataSet_Cat_AspectTo1$ls_inventory1==1) # 626847 cells (626879) diff: 32;
write_delim(DataSetLS_Cat_inventory1, "D:/.../DataSetLS_Cat_inventory1.txt")

DataSetLS_Cat_inventory2 <- subset(DataSet_Cat_AspectTo1, DataSet_Cat_AspectTo1$ls_inventory2==1) # 1342977 cells (1343156) diff: 179 cells
write_delim(DataSetLS_Cat_inventory2, "D:/.../DataSetLS_Cat_inventory2.txt")


### 4. Bar plots (histograms)

## a. Classified continuous parameters

# Parameters: TPI_class, slope_class, Elevation_class, SkyViewFactor_class, TWI_class, Faults_class, Railroads_class, Rivers_class, Roads_class, Curvature_class
# 			PlanCurv_class, ProfileCurv_class, TRI_class, LSF_class, FlowAcc_class, SPI_class, TCA_class          


dataSetLS_Class_inventory2 <- read.delim("D:/.../DataSetLS_ContClass_inventory2.txt", sep=" ")
dataSetLS_Class_inventory1 <- read.delim("D:/.../DataSetLS_ContClass_inventory1.txt", sep=" ")

table(DataSetLS_Class_inventory2$TPI_class)

# par(mfrow = c(2, 2))


for (i in 21:ncol(dataSetLS_Class_inventory1)){
 print(ggplot(dataSetLS_Class_inventory1, aes(dataSetLS_Class_inventory1[,i])) + geom_bar(color="darkblue", fill="darkblue") 
 + xlab(colnames(dataSetLS_Class_inventory1[i])) + ylab("LS cells") + geom_text(aes(label=..count..), stat = "count", vjust = - 0.8)
 + labs(title = "Landslides inventory 1") + scale_x_discrete(limits=c(mixedsort(unique(dataSetLS_Class_inventory1[,i])))) + theme_minimal())}

for (i in 21:ncol(dataSetLS_Class_inventory2)){
 print(ggplot(dataSetLS_Class_inventory2, aes(dataSetLS_Class_inventory2[,i])) + geom_bar(color="darkred", fill="darkred") 
 + xlab(colnames(dataSetLS_Class_inventory2[i])) + ylab("LS cells") + geom_text(aes(label=..count..), stat = "count", vjust = - 0.8) 
 + labs(title = "Landslides inventory 2") + scale_x_discrete(limits=c(mixedsort(unique(dataSetLS_Class_inventory2[,i])))) + theme_minimal())}


## b. Categorical parameters

# Parameters: AspectNto1, aspectSto1, Climate zones, Forest, Geology, Irrigated Areas, Landforms, Land use, precipitation, 
#				soils, temperature, vegetation, volcanoes


dataSetLS_Cat_inventory2 <- read.delim("D:/.../DataSetLS_Cat_inventory2.txt", sep=" ")
dataSetLS_Cat_inventory1 <- read.delim("D:/.../DataSetLS_Cat_inventory1.txt", sep=" ")

# Drop original aspect data 
dataSetLS_Cat_inventory1A <- subset(dataSetLS_Cat_inventory1, select=-c(Aspect_N, Aspect_S))

for (i in 1:ncol(dataSetLS_Cat_inventory1A)){
 print(ggplot(dataSetLS_Cat_inventory1A, aes(dataSetLS_Cat_inventory1A[,i])) + geom_bar(color="darkblue", fill="darkblue") 
 + xlab(colnames(dataSetLS_Cat_inventory1A[i])) + ylab("LS cells") + geom_text(aes(label=..count..), stat = "count", vjust = - 0.8) 
 + labs(title = "Landslides inventory 1") + scale_x_discrete(limits=c(mixedsort(unique(dataSetLS_Cat_inventory1A[,i])))) + theme_minimal())}

# Drop original aspect data
dataSetLS_Cat_inventory2A <- subset(dataSetLS_Cat_inventory2, select=-c(Aspect_N, Aspect_S))

for (i in 1:ncol(dataSetLS_Cat_inventory2A)){
 print(ggplot(dataSetLS_Cat_inventory2A, aes(dataSetLS_Cat_inventory2A[,i])) + geom_bar(color="darkred", fill="darkred") 
 + xlab(colnames(dataSetLS_Cat_inventory2A[i])) + ylab("LS cells") + geom_text(aes(label=..count..), stat = "count", vjust = - 0.8) 
 + labs(title = "Landslides inventory 2") + scale_x_discrete(limits=c(mixedsort(unique(dataSetLS_Cat_inventory2A[,i])))) + theme_minimal())}


### Two datasets (inventory2 + inventory1) in one plot
# ggplot(NULL, aes(TWI_class)) + geom_bar(data=DataSetLS_Class_inventory2, col="blue", fill="blue") + geom_bar(DataSetLS_Class_inventory1, color="red", fill="red")



### This is not a part of the code: Memory check (due to problems with the memory limit)

# Check memory limit
memory.size()
memory.limit()

# Check the maximum amount of memory
memory.size(max=TRUE)

# Memory currently in use
memory.size(max=FALSE)

# Increasing / requesting new memory limit
memory.limit(size) # size = memory limit, for instance 24000