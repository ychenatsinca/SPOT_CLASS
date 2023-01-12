# Revision for the program structure
# Date: 2023-01-12
# Refined as R function for the bash submission/jobs in bash mode with Rscript arguments 
#  
# Step-1: SPOT classification for each grid (tree while loop)
# Step-2: Merge each grids classification result into one raster (mergeraster_Taiwan while loop)
# Step-3: Pond detection and calculate pond size (ponddetect while loop)
#
# Now I'm working on the Step-1 for the statistics of single grid box classification 
#
fun.lu.type <- function (xmin=235, xmax=236, ymin=210, ymax=211, wrk_yr=2015, aoi_reg = c("taipei"))
# start the fun.lu.type
{
#aoi_reg="XY_ID"
#wrk_yr=2015
#xmin=235;xmax=235;ymin=210;ymax=210
#load libraries  
library(raster)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(rgdal)
#set AOI region 
print(aoi_reg)
   #set AOI with specific region
   if (aoi_reg == "TAIPEI") {
       xmin=234;xmax=237; ymin=209; ymax=212
     }else if (aoi_reg=="TAOYUAN") {
               xmin=231; xmax=234; ymin=208; ymax=210 
     }else if (aoi_reg=="NORTH") { 
               xmin=224; xmax=240; ymin=205; ymax=212
     }else if (aoi_reg=="CENTRAL") {
               xmin=224; xmax=230; ymin=195; ymax=204
     }else if (aoi_reg=="EAST") {
               xmin=231; xmax=240; ymin=195; ymax=204
     }else if (aoi_reg=="SOUTH") {
               xmin=224; xmax=240; ymin=185; ymax=194
     }else if (aoi_reg=="TAIWAN"){
               xmin=224; xmax=240; ymin=185; ymax=212
     }else {
       xmin=xmin;xmax=xmax;ymin=ymin;ymax=ymax
     } 
   
#else if(aoi_reg=="TAOYUAN") {xmin=231; xmax=234; ymin=208; ymax=210}
#   else if(aoi_reg=="TAIWAN")  {xmin=224; xmax=240; ymin=185; ymax=212}
#   else if(aoi_reg=="NORTH")   {xmin=224; xmax=240; ymin=205; ymax=212}
#   else if(aoi_reg=="CENTRAL") {xmin=224; xmax=240; ymin=195; ymax=204}
#   else if(aoi_reg=="SOUTH")   {xmin=224; xmax=240; ymin=185; ymax=194}
 
#--------------------#classification decision tree#--------------------#
tree <- TRUE
while (tree==TRUE){
# set up xminmax yminmax for area of interest;
# any pixel within this extent will be run through classification code
#Taipei
# xmin <- 235
# xmax <- 235
# ymin <- 210
# ymax <- 210
  
#taoyuan
# xmin <- 231
# xmax <- 234
# ymin <- 208
# ymax <- 210
  
#taiwan
#  xmin <- 224
#  xmax <- 240
#  ymin <- 185
#  ymax <- 212

  #create allaoi vector to store all aoi
  allaoi <- vector()
  for (x in xmin:xmax){
    for (y in ymin:ymax){
      #append aoi to allaoi
      allaoi <- append(allaoi, paste(x,"_",y,sep=""))
    }
  }
  str_yr <- min(wrk_yr)
  end_yr <- max(wrk_yr)
  
  #classification algorithm for future images
  # test for year 2013 
  for (yr in str_yr:end_yr){ #doesn't iterate yr 2018 because when iterating 2017, 2018 will be counted in
    print(yr)
    
    #delete aoi grid index if there are no images within the grid
    for (aoi in 1:length(allaoi)){
      #direct to directory based on year
      directory <- paste("/lfs/home/ychen/Satellite/SPOT_CSRSR/grid_box/",yr,"/",sep="")
      #create list of SPOT image under directory that satisfy condition
      aoi_images <- dir(path = directory, pattern = "SPOT", all.files = FALSE,
                        full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE)
      #use all images that satisfies aoi location
      aoi_images <- aoi_images[substr(aoi_images,start=23,stop=29)==allaoi[aoi]] 
      #set allaoi[aoi] to 0 if the aoi does not contain SPOT images within the "/lfs/home/ychen/Satellite/SPOT_CSRSR/grid_box/" inventory
      if (length(aoi_images)==0){
        allaoi[aoi] <- 0 #set as 0 first and not delete the item, so i won't mess up the index in the aoi_images list
      }
    }
    #remove aoi's that are set as 0 (meaning these aoi does not contain SPOT images within inventory)
    allaoi <- allaoi[allaoi!=0]
    
    for (aoi in 1:length(allaoi)){
      print(aoi)
      if (yr==2017){ #separate if statement for years 2017 2018 to bind two year's images to classify
        #direct to directory of 2017 images
        directory <- c("/lfs/home/ychen/Satellite/SPOT_CSRSR/grid_box/2017/")
        #create list of SPOT image under directory that satisfy condition
        aoi_images <- dir(path = directory, pattern = "SPOT", all.files = FALSE,
                          full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE)
        #use all images that satisfies aoi location
        aoi_images <- aoi_images[substr(aoi_images,start=23,stop=29)==allaoi[aoi]] 

        #direct to directory of 2018 images
        directory <- c("/lfs/home/ychen/Satellite/SPOT_CSRSR/grid_box/2018/")
        #create list of SPOT image under directory that satisfy condition
        temp_images <- dir(path = directory, pattern = "SPOT", all.files = FALSE,
                           full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE)
        #use all images that satisfies aoi location
        temp_images <- temp_images[substr(temp_images,start=23,stop=29)==allaoi[aoi]]
        
        #bind 2017 and 2018 image list to together be classified
        aoi_images <- c(aoi_images,temp_images)
        
        #set default extent xmin xmax ymin ymax to 0
        extent_xmin <- 0
        extent_xmax <- 0
        extent_ymin <- 0
        extent_ymax <- 0
        row <- 0
        col <- 0
        #loop among all items to find min of xmin, max of xmax, min of ymin, and max of ymax -
        # to later use these extents to create universal raster extent for all SPOT images within this aoi to extend to
        for (item in 1:length(aoi_images)){
          if (substr(aoi_images[item],start=7,stop=10)==2017){
            testlayer <- raster(x=paste("/lfs/home/ychen/Satellite/SPOT_CSRSR/grid_box/2017/",aoi_images[item],"/",aoi_images[item],".1.bsq.ers",sep=""))
          }else if (substr(aoi_images[item],start=7,stop=10)==2018){
            testlayer <- raster(x=paste("/lfs/home/ychen/Satellite/SPOT_CSRSR/grid_box/2018/",aoi_images[item],"/",aoi_images[item],".1.bsq.ers",sep=""))
          }
          if (item==1){
            extent_xmin <- extent(testlayer)[1]
            extent_xmax <- extent(testlayer)[2]
            extent_ymin <- extent(testlayer)[3]
            extent_ymax <- extent(testlayer)[4]
            row <- dim(testlayer)[1]
            col <- dim(testlayer)[2]
          }else{
            extent_xmin <- min(extent_xmin, extent(testlayer)[1])
            extent_xmax <- max(extent_xmax, extent(testlayer)[2])
            extent_ymin <- min(extent_ymin, extent(testlayer)[3])
            extent_ymax <- max(extent_ymax, extent(testlayer)[4])
            row <- max(row, dim(testlayer)[1])
            col <-max(col, dim(testlayer)[2])
          }
        } #end of item-loop
        #create extent object that holds xmin xmax ymin ymax values that you want to set future rasters 
        #to (this is to create an universal extent for all images under this aoi so later these rasters can be stacked)
        extent <- extent(extent_xmin,extent_xmax,extent_ymin,extent_ymax)
        #create empty vector t to store each images' raster extent within the aoi
        #loop to find each images' raster extent within the aoi
        t <- rep(NA,length(aoi_images))
        for (item in 1:length(aoi_images)){
          if (substr(aoi_images[item],start=7,stop=10)==2017){
            band <- raster(x=paste("/lfs/home/ychen/Satellite/SPOT_CSRSR/grid_box/2017/",aoi_images[item],"/",aoi_images[item],".1.bsq.ers",sep=""))
          }else if (substr(aoi_images[item],start=7,stop=10)==2018){
            band <- raster(x=paste("/lfs/home/ychen/Satellite/SPOT_CSRSR/grid_box/2018/",aoi_images[item],"/",aoi_images[item],".1.bsq.ers",sep=""))
          }
          band <- extend(band, extent, value=NA)
          t[item] <- extent(band)[1]
        }
        #find majority raster's extent after extending
        xmin <- as.numeric(names(which.max(table(t)))) 
        #loop to find rasters in the list aoi_images that when extended, does not match the majority's extent therefore cannot be stacked in the future
        for (item in 1:length(aoi_images)){
          if (substr(aoi_images[item],start=7,stop=10)==2017){
            band <- raster(x=paste("/lfs/home/ychen/Satellite/SPOT_CSRSR/grid_box/2017/",aoi_images[item],"/",aoi_images[item],".1.bsq.ers",sep=""))
          }else if (substr(aoi_images[item],start=7,stop=10)==2018){
            band <- raster(x=paste("/lfs/home/ychen/Satellite/SPOT_CSRSR/grid_box/2018/",aoi_images[item],"/",aoi_images[item],".1.bsq.ers",sep=""))
          }
          band <- extend(band, extent, value=NA)
          if (extent(band)[1]!=xmin){
            aoi_images[item] <- 0 #set as 0 first and not delete the item, so i won't mess up the index in the aoi_images list
          }
        }
        #remove aoi's that are set as 0 (meaning these images within the aoi has different extent therefore cannot be stacked with other rasters)
        aoi_images <- aoi_images[aoi_images!=0]
        
        #create empty dataframe rasterallpoints to later store each pixels' monthly median NDVI & NIR value (_N stores NDVI values, _NIR stores NIR values)
        rasterallpoints <- data.frame("index"=1:(row*col),
                                      "January_N"=NA,
                                      "February_N"=NA,
                                      "March_N"=NA,
                                      "April_N"=NA,
                                      "May_N"=NA,
                                      "June_N"=NA,
                                      "July_N"=NA,
                                      "August_N"=NA,
                                      "September_N"=NA,
                                      "October_N"=NA,
                                      "November_N"=NA,
                                      "December_N"=NA,
                                      "January_NIR"=NA,
                                      "February_NIR"=NA,
                                      "March_NIR"=NA,
                                      "April_NIR"=NA,
                                      "May_NIR"=NA,
                                      "June_NIR"=NA,
                                      "July_NIR"=NA,
                                      "August_NIR"=NA,
                                      "September_NIR"=NA,
                                      "October_NIR"=NA,
                                      "November_NIR"=NA,
                                      "December_NIR"=NA)
        colnames(rasterallpoints) <- c("index","Jan_N","Feb_N","Mar_N","Apr_N","May_N","Jun_N","Jul_N","Aug_N","Sep_N","Oct_N","Nov_N","Dec_N",
                                               "Jan_NIR","Feb_NIR","Mar_NIR","Apr_NIR","May_NIR","Jun_NIR","Jul_NIR","Aug_NIR","Sep_NIR","Oct_NIR","Nov_NIR","Dec_NIR")
        
        #loop among 12 months to retrieve monthly median NDVI & NIR values for aoi
        for (month in 1:12){ #12 months
          print(month)
          #create strings ex. for 2015 january "201501" to search in aoi_images list of available images for aoi for the year+month
          if (1<=month & month<=9){
            string_month <- paste("0",as.character(month),sep="")
          }else{
            string_month <- as.character(month)
          }
          #subset aoi_images to get those that satisfy year and month
          subset_aoiimages <- aoi_images[substr(aoi_images,start=11,stop=12)==string_month]
          #if there are no available images for the month (aka length(subset_aoiimages)==0), set the points' median NDVI & NIR value for the month as NA
          if (length(subset_aoiimages)==0){
            rasterallpoints[,month+1] <- NA #ex. NDVI january column is the 1+1=2 2nd column
            rasterallpoints[,month+13] <- NA #ex. NIR january column is the 1+13=14 14th column
            next
          }else{
            #loop to stack images with same month and aoi to calculate median NDVI & NIR
            for (image in 1:length(subset_aoiimages)){
              if (substr(subset_aoiimages[image],start=7,stop=10)==2017){
                #loop to retrieve rasters for all 4 bands
                for (band in 1:4){
                  #assign SPOT image band digital count values to variables band1-4
                  assign(paste("band",band,sep=""),  raster(x=paste("/lfs/home/ychen/Satellite/SPOT_CSRSR/grid_box/2017/",
                                                            subset_aoiimages[image],"/",subset_aoiimages[image],".",band,".bsq.ers",sep="")))
                } #end of band-loop
              }else if (substr(subset_aoiimages[image],start=7,stop=10)==2018){
                #loop to retrieve rasters for all 4 bands
                for (band in 1:4){
                  #assign SPOT image band digital count values to variables band1-4
                  assign(paste("band",band,sep=""),  raster(x=paste("/lfs/home/ychen/Satellite/SPOT_CSRSR/grid_box/2018/",
                                                            subset_aoiimages[image],"/",subset_aoiimages[image],".",band,".bsq.ers",sep="")))
                } #end of band-loop
              }
              #create tempNDVIlayer to get NDVI raster created from band math
              tempNDVIlayer <- (band4-band3)/(band4+band3)
              #extend created NDVI raster and NIR raster to the universal extent for the images of the aoi with NA values
              tempNDVIlayer <- extend(tempNDVIlayer, extent, value=NA)
              band4 <- extend(band4, extent, value=NA)
              #stack all images under the month together; resulting NDVIstack & NIRstack 
              #raster stack holds NDVI & NIR value of all the images available under the year and month of that aoi
              if (image==1){
                NDVIstack <- tempNDVIlayer
                NIRstack <- band4
              }else{
                NDVIstack <- stack(NDVIstack, tempNDVIlayer)
                NIRstack <- stack(NIRstack, band4)
              }
            } #end of image-loop
            #if NDVIstack has more than one layer (aka more than one available image for the month), perform median among all layers
            if (dim(NDVIstack)[3]!=1){
              #convert from RasterLayer to RasterBrick
              NDVIstack <- brick(NDVIstack)
              #perform median among the raster stack of all images under the month together
              medianNDVI_ofmonth <- calc(NDVIstack, median,na.rm=T) #ignore error, still produces results
            }else if (dim(NDVIstack)[3]==1){ 
                     #if NDVIstack only has one layer,
                     # meaning there is only one available image for the month,
                     # don't need to perform median; the NDVI will be counted for the month
              medianNDVI_ofmonth <- NDVIstack
            }
              #if NIRstack has more than one layer 
              #(aka more than one available image for the month), perform median among all layers
            if (dim(NIRstack)[3]!=1){
              #convert from RasterLayer to RasterBrick
              NIRstack <- brick(NIRstack)
              #perform median among the raster stack of all images under the month together
              medianNIR_ofmonth <- calc(NIRstack, median,na.rm=T) #ignore error, still produces results
            }else if (dim(NIRstack)[3]==1){ 
              #if NIRstack only has one layer, meaning there is only one available image for the month, 
              # don't need to perform median; the NIR will be counted for the month
              medianNIR_ofmonth <- NIRstack
            }
            #convert median NDVI & NIR value raster for the month to a dataframe
            medianNDVI <- as.data.frame(medianNDVI_ofmonth)
            medianNIR <- as.data.frame(medianNIR_ofmonth)
            #set the points' median NDVI & NIR value for the month as the median NDVI & NIR values within dataframe
            rasterallpoints[,month+1] <- medianNDVI #ex. NDVI january column is the 1+1=2 2nd column
            rasterallpoints[,month+13] <- medianNIR #ex. NIR january column is the 1+13=14 14th column
          }
        } #end of month-loop
      
      }else{
        #direct to directory based on year
        directory <- paste("/lfs/home/ychen/Satellite/SPOT_CSRSR/grid_box/",yr,"/",sep="")
        #create list of SPOT image under directory that satisfy condition
        aoi_images <- dir(path = directory, pattern = "SPOT", all.files = FALSE,
                          full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE)
        #use all images that satisfies aoi location
        aoi_images <- aoi_images[substr(aoi_images,start=23,stop=29)==allaoi[aoi]] 
        
        #set default extent xmin xmax ymin ymax to 0
        extent_xmin <- 0
        extent_xmax <- 0
        extent_ymin <- 0
        extent_ymax <- 0
        row <- 0
        col <- 0
        # loop among all items to find min of xmin, max of xmax, min of ymin, 
        # and max of ymax - to later use these extents to create universal 
        # raster extent for all SPOT images within this aoi to extend to
        for (item in 1:length(aoi_images)){
          testlayer <- raster(x=paste(directory,"/",aoi_images[item],"/",aoi_images[item],".1.bsq.ers",sep=""))
          if (item==1){
            extent_xmin <- extent(testlayer)[1]
            extent_xmax <- extent(testlayer)[2]
            extent_ymin <- extent(testlayer)[3]
            extent_ymax <- extent(testlayer)[4]
            row <- dim(testlayer)[1]
            col <- dim(testlayer)[2]
          }else{
            extent_xmin <- min(extent_xmin, extent(testlayer)[1])
            extent_xmax <- max(extent_xmax, extent(testlayer)[2])
            extent_ymin <- min(extent_ymin, extent(testlayer)[3])
            extent_ymax <- max(extent_ymax, extent(testlayer)[4])
            row <- max(row, dim(testlayer)[1])
            col <-max(col, dim(testlayer)[2])
          }
        } #end of item-loop
        # create extent object that holds xmin xmax ymin ymax values that you want to set future rasters to 
        # (this is to create an universal extent for all images under this aoi so later these rasters can be stacked)
        extent <- extent(extent_xmin,extent_xmax,extent_ymin,extent_ymax)
        # create empty vector t to store each images' raster extent within the aoi
        t <- rep(NA,length(aoi_images))
        #loop to find each images' raster extent within the aoi
        for (item in 1:length(aoi_images)){
          band <-  raster(x=paste(directory,"/",aoi_images[item],"/",aoi_images[item],".1.bsq.ers",sep=""))
          band <- extend(band, extent, value=NA)
          t[item] <- extent(band)[1]
        }
        xmin <- as.numeric(names(which.max(table(t)))) #find majority raster's extent after extending
        #loop to find rasters in the list aoi_images that when extended, does not match the majority's extent therefore cannot be stacked in the future
        for (item in 1:length(aoi_images)){
          band <-  raster(x=paste(directory,"/",aoi_images[item],"/",aoi_images[item],".1.bsq.ers",sep=""))
          band <- extend(band, extent, value=NA)
          if (extent(band)[1]!=xmin){
            aoi_images[item] <- 0 #set as 0 first and not delete the item, so i won't mess up the index in the aoi_images list
          }
        }
        #remove aoi's that are set as 0 (meaning these images within the aoi has different extent therefore cannot be stacked with other rasters)
        aoi_images <- aoi_images[aoi_images!=0]
        
        #create empty dataframe rasterallpoints to later store each pixels' monthly median NDVI & NIR value (_N stores NDVI values, _NIR stores NIR values)
        rasterallpoints <- data.frame("index"=1:(row*col),
                                      "January_N"=NA,
                                      "February_N"=NA,
                                      "March_N"=NA,
                                      "April_N"=NA,
                                      "May_N"=NA,
                                      "June_N"=NA,
                                      "July_N"=NA,
                                      "August_N"=NA,
                                      "September_N"=NA,
                                      "October_N"=NA,
                                      "November_N"=NA,
                                      "December_N"=NA,
                                      "January_NIR"=NA,
                                      "February_NIR"=NA,
                                      "March_NIR"=NA,
                                      "April_NIR"=NA,
                                      "May_NIR"=NA,
                                      "June_NIR"=NA,
                                      "July_NIR"=NA,
                                      "August_NIR"=NA,
                                      "September_NIR"=NA,
                                      "October_NIR"=NA,
                                      "November_NIR"=NA,
                                      "December_NIR"=NA)
        colnames(rasterallpoints) <- c("index","Jan_N","Feb_N","Mar_N","Apr_N","May_N","Jun_N","Jul_N","Aug_N","Sep_N","Oct_N","Nov_N","Dec_N",
                                       "Jan_NIR","Feb_NIR","Mar_NIR","Apr_NIR","May_NIR","Jun_NIR","Jul_NIR","Aug_NIR","Sep_NIR","Oct_NIR","Nov_NIR","Dec_NIR")
        
        #loop among 12 months to retrieve monthly median NDVI & NIR values for aoi
        for (month in 1:12){ #12 months
          #create strings ex. for 2015 january "201501" to search in aoi_images list of available images for aoi for the year+month
          print(month)
          if (1<=month & month<=9){
            string_month <- paste("0",as.character(month),sep="")
          }else{
            string_month <- as.character(month)
          }
          #subset aoi_images to get those that satisfy year and month
          subset_aoiimages <- aoi_images[substr(aoi_images,start=7,stop=12)==paste(yr,string_month,sep="")]
          #if there are no available images for the month (aka length(subset_aoiimages)==0), set the points' median NDVI & NIR value for the month as NA
          if (length(subset_aoiimages)==0){
            rasterallpoints[,month+1] <- NA #ex. NDVI january column is the 1+1=2 2nd column
            rasterallpoints[,month+13] <- NA #ex. NIR january column is the 1+13=14 14th column
            next
          }else{
            #loop to stack images with same month and aoi to calculate median NDVI & NIR
            for (image in 1:length(subset_aoiimages)){
              #loop to retrieve rasters for all 4 bands
              for (band in 1:4){
                #assign SPOT image band digital count values to variables band1-4
                assign(paste("band",band,sep=""),  raster(x=paste(directory,"/",subset_aoiimages[image],"/",subset_aoiimages[image],".",band,".bsq.ers",sep="")))
              } #end of band-loop
              #create tempNDVIlayer to get NDVI raster created from band math
              tempNDVIlayer <- (band4-band3)/(band4+band3)
              #extend created NDVI raster and NIR raster to the universal extent for the images of the aoi with NA values
              tempNDVIlayer <- extend(tempNDVIlayer, extent, value=NA)
              band4 <- extend(band4, extent, value=NA)
              # stack all images under the month together; resulting NDVIstack & NIRstack 
              # raster stack holds NDVI & NIR value of all the images available under the year and month of that aoi
              if (image==1){
                NDVIstack <- tempNDVIlayer
                NIRstack <- band4
              }else{
                NDVIstack <- stack(NDVIstack, tempNDVIlayer)
                NIRstack <- stack(NIRstack, band4)
              }
            } # end of image-loop
              # if NDVIstack has more than one layer (aka more than one available image for the month), perform median among all layers
            if (dim(NDVIstack)[3]!=1){
              # convert from RasterLayer to RasterBrick
              NDVIstack <- brick(NDVIstack)
              # perform median among the raster stack of all images under the month together
              medianNDVI_ofmonth <- calc(NDVIstack, median,na.rm=T) #ignore error, still produces results
            }else if (dim(NDVIstack)[3]==1){
              # if NDVIstack only has one layer, meaning there is only one available image for the month, 
              # don't need to perform median; the NDVI will be counted for the month
              medianNDVI_ofmonth <- NDVIstack
            }
              #if NIRstack has more than one layer (aka more than one available image for the month), perform median among all layers
            if (dim(NIRstack)[3]!=1){
              #convert from RasterLayer to RasterBrick
              NIRstack <- brick(NIRstack)
              #perform median among the raster stack of all images under the month together
              medianNIR_ofmonth <- calc(NIRstack, median,na.rm=T) #ignore error, still produces results
            }else if (dim(NIRstack)[3]==1){ 
              #if NIRstack only has one layer, meaning there is only one available image for the month, 
              #don't need to perform median; the NIR will be counted for the month
              medianNIR_ofmonth <- NIRstack
            }
            #convert median NDVI & NIR value raster for the month to a dataframe
            medianNDVI <- as.data.frame(medianNDVI_ofmonth)
            medianNIR <- as.data.frame(medianNIR_ofmonth)
            #set the points' median NDVI & NIR value for the month as the median NDVI & NIR values within dataframe
            rasterallpoints[,month+1] <- medianNDVI #ex. NDVI january column is the 1+1=2 2nd column
            rasterallpoints[,month+13] <- medianNIR #ex. NIR january column is the 1+13=14 14th column
          }
        } #end of month-loop
      } #end of if else statement
      
      #calculate max, min of median NDVI from Jan to Dec for all points stored in new column "max_N", "min_N"
      rasterallpoints["max_N"] <-apply(X=rasterallpoints[2:13], MARGIN=1, FUN=max,na.rm=T) #margin=1 meaning retrieving max by row
      rasterallpoints["min_N"] <-apply(X=rasterallpoints[2:13], MARGIN=1, FUN=min,na.rm=T) #margin=1 meaning retrieving min by row
      #calculate variance of median NDVI from Jan to Dec for all points stored in new column "variance_N"
      rasterallpoints["variance_N"] <-apply(X=rasterallpoints[2:13], MARGIN=1, FUN=var,na.rm=T) #margin=1 meaning retrieving variance by row
      #calculate median of median NIR from Jan to Dec for all points stored in new column "median_NIR"
      rasterallpoints["median_NIR"] <-apply(X=rasterallpoints[14:25], MARGIN=1, FUN=median,na.rm=T)
      #calculate NDVI max/NDVI variance ratio from Jan to Dec for all points stored in new column "maxvar_ratio"
      rasterallpoints["maxvar_ratio"] <- rasterallpoints["max_N"]/rasterallpoints["variance_N"]
      
      #--------------------#classification based on decision tree#--------------------#
      #classify as forest=1, builtup=2, water=3, agri=4, unknown=5, unclassified=6
      #duplicate rasterallpoints dataframe for further usage
      allpoints <- rasterallpoints
      allpoints["class"] <- NA
      #thresholds
      allpoints$class[allpoints$max_N<=0.03] <- 3
      
      allpoints$class[allpoints$max_N>0.03 & allpoints$max_N<=0.19 & allpoints$median_NIR<=350] <- 3
      allpoints$class[allpoints$max_N>0.03 & allpoints$max_N<=0.19 & allpoints$median_NIR>350] <- 2
      
      allpoints$class[allpoints$max_N>0.19 & allpoints$max_N<=0.39 & allpoints$variance_N>=0.001 & allpoints$variance_N<=0.006] <- 4
      allpoints$class[allpoints$max_N>0.19 & allpoints$max_N<=0.39 & (allpoints$variance_N<0.001 | allpoints$variance_N>0.006)] <- 5 #unknown land type
      
      allpoints$class[allpoints$max_N>0.39 & allpoints$maxvar_ratio<=19.19] <- 4
      allpoints$class[allpoints$max_N>0.39 & allpoints$maxvar_ratio>19.19] <- 1
      
      #classify points that are not classified from above decision tree as unclassified land type 6
      allpoints$class[is.na(allpoints$class)==TRUE] <- 6
      
      #output dataframe as raster
      #duplicate medianNDVI_ofmonth raster created from testloop while loop to copy dimension, extent, and projection
      outputraster<- medianNDVI_ofmonth
      values(outputraster) <- NA
      outputraster <- replace(outputraster,allpoints$index,allpoints$class)
      
      #plot classification results with unknown as grey
      colors <- c("#439c6e","#e86d5f","#8bd2e8","#f0d86e","#999999","#383838") #dark green for forest, red for builtup, blue for water, orange for agri, grey for unknown, black for unclassified
      #subset color palette to the available classified values (ex. sometimes an image doesn't have unknown land type, so we omit grey from the color palette)
      colors <- colors[sort(unique(outputraster))]
     
      #create structured folder  
      image_path <- paste("/lfs/home/ychen/scripts/R/Rscripts/SPOT_CLASS/vivian_code/results",sep="")
      dir.create(image_path)  
      # image_path <- paste("/lfs/home/ychen/scripts/R/Rscripts/SPOT_CLASS/vivian_code/results/",yr,sep="")
      # dir.create(image_path) 
      # image_path <- paste("/lfs/home/ychen/scripts/R/Rscripts/SPOT_CLASS/vivian_code/results/",yr,"/all",sep="")
      # dir.create(image_path) 


      if (yr==2017){
        writeRaster(outputraster,paste(image_path,"/2017and2018_",allaoi[aoi],".tif",sep=""),overwrite=TRUE)

       # png(paste(image_path,"/2017and2018_",allaoi[aoi],".png",sep=""),
       #      width = 1080, height = 1080, units = "px")
      }else{
        writeRaster(outputraster,paste(image_path,yr,"_",allaoi[aoi],".tif",sep=""),overwrite=TRUE)
       
       # png(paste(image_path,yr,"_",allaoi[aoi],".png",sep=""),
       #     width = 1080, height = 1080, units = "px")
      }
     # plot(outputraster,
     #      col=colors)
     # dev.off()
      
      print(paste("please chech the image file:", image_path,yr,"_",allaoi[aoi],"_Cn0_",tunning_cn0,".png",sep="") )
      # print current time
      print(Sys.time())
 
      #clear up variables "allpoints" and "rasterallpoints"
      rm(allpoints)
      rm(rasterallpoints)
      # clean the memory 
      #gc()
    } #end of aoi-for loop
  } #end of yr-for loop
  
  tree <- FALSE
  } #end of tree while loop

 #return()
## end of function fun.lu.type 
}


#========== Set the script to Auto RUN=========================== 
# Satrt the funtion byt the specific arguments 
# If you want to submit the file by queue to obelix/300T,.etc. serve rname), you need to apply the 
# following lines, which allowed this R-script can be called under the shell/bash script
# with the arguments sending by specific batch jobs  
#
args<-commandArgs(TRUE)
print(args)
fun.lu.type(args[1], args[2], args[3], args[4], args[5], args[6]) 
#
#========== End ================================================



