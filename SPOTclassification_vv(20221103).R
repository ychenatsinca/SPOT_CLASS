library(raster)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(rgdal)

#--------------------#classification decision tree#--------------------#
tree2 <- FALSE
while (tree2==TRUE){
  #set up xminmax yminmax for area of interest; any pixel within this extent will be run through classification code
  #taipei
  # xmin <- 234
  # xmax <- 237
  # ymin <- 209
  # ymax <- 212
  
  #taoyuan
  # xmin <- 231
  # xmax <- 234
  # ymin <- 208
  # ymax <- 210
  
  #taiwan
  xmin <- 224
  xmax <- 240
  ymin <- 185
  ymax <- 212
  
  #create allaoi vector to store all aoi
  allaoi <- vector()
  for (x in xmin:xmax){
    for (y in ymin:ymax){
      #append aoi to allaoi
      allaoi <- append(allaoi, paste(x,"_",y,sep=""))
    }
  }
  
  #delete aoi grid index if there are no images within the grid (ex. 231_210 is iterated in loop into allaoi list but actually does not exist within taiwan aoi images)
  for (yr in 2015:2015){
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
  }
  #remove aoi's that are set as 0 (meaning these aoi does not contain SPOT images within inventory)
  allaoi <- allaoi[allaoi!=0]
  
  #classification algorithm for future images
  for (yr in c(2013,2014,2015,2016,2017,2019,2020,2021,2022)){ #doesn't iterate yr 2018 because when iterating 2017, 2018 will be counted in
    print(yr)
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
        #loop among all items to find min of xmin, max of xmax, min of ymin, and max of ymax - to later use these extents to create universal raster extent for all SPOT images within this aoi to extend to
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
        #create extent object that holds xmin xmax ymin ymax values that you want to set future rasters to (this is to create an universal extent for all images under this aoi so later these rasters can be stacked)
        extent <- extent(extent_xmin,extent_xmax,extent_ymin,extent_ymax)
        #create empty vector t to store each images・ raster extent within the aoi
        #loop to find each images・ raster extent within the aoi
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
        
        #create empty dataframe rasterallpoints to later store each pixels' monthly median NDVI value (_N stores NDVI values, _G stores GRVI values)
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
                                      "January_G"=NA,
                                      "February_G"=NA,
                                      "March_G"=NA,
                                      "April_G"=NA,
                                      "May_G"=NA,
                                      "June_G"=NA,
                                      "July_G"=NA,
                                      "August_G"=NA,
                                      "September_G"=NA,
                                      "October_G"=NA,
                                      "November_G"=NA,
                                      "December_G"=NA)
        colnames(rasterallpoints) <- c("index","Jan_N","Feb_N","Mar_N","Apr_N","May_N","Jun_N","Jul_N","Aug_N","Sep_N","Oct_N","Nov_N","Dec_N","Jan_G","Feb_G","Mar_G","Apr_G","May_G","Jun_G","Jul_G","Aug_G","Sep_G","Oct_G","Nov_G","Dec_G")
        
        #loop among 12 months to retrieve monthly median NDVI & GRVI values for aoi
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
          #if there are no available images for the month (aka length(subset_aoiimages)==0), set the points' median NDVI value for the month as NA
          if (length(subset_aoiimages)==0){
            rasterallpoints[,month+1] <- NA #ex. NDVI january column is the 1+1=2 2nd column
            rasterallpoints[,month+13] <- NA #ex. GRVI january column is the 1+13=14 14th column
            next
          }else{
            #loop to stack images with same month and aoi to calculate median NDVI&GRVI
            for (image in 1:length(subset_aoiimages)){
              if (substr(subset_aoiimages[image],start=7,stop=10)==2017){
                #loop to retrieve rasters for all 4 bands
                for (band in 1:4){
                  #assign SPOT image band digital count values to variables band1-4
                  assign(paste("band",band,sep=""),  raster(x=paste("/lfs/home/ychen/Satellite/SPOT_CSRSR/grid_box/2017/",subset_aoiimages[image],"/",subset_aoiimages[image],".",band,".bsq.ers",sep="")))
                } #end of band-loop
              }else if (substr(subset_aoiimages[image],start=7,stop=10)==2018){
                #loop to retrieve rasters for all 4 bands
                for (band in 1:4){
                  #assign SPOT image band digital count values to variables band1-4
                  assign(paste("band",band,sep=""),  raster(x=paste("/lfs/home/ychen/Satellite/SPOT_CSRSR/grid_box/2018/",subset_aoiimages[image],"/",subset_aoiimages[image],".",band,".bsq.ers",sep="")))
                } #end of band-loop
              }
              #create tempNDVIlayer & tempGRVIlayer to get NDVI & GRVI raster created from band math
              tempNDVIlayer <- (band4-band3)/(band4+band3)
              tempGRVIlayer <- (band4)/(band2)
              #extend created NDVI & GRVI raster to the universal extent for the images of the aoi with NA values
              tempNDVIlayer <- extend(tempNDVIlayer, extent, value=NA)
              tempGRVIlayer <- extend(tempGRVIlayer, extent, value=NA)
              #stack all images under the month together; resulting NDVIstack & GRVIstack raster stack holds NDVI & GRVI value of all the images available under the year and month of that aoi
              if (image==1){
                NDVIstack <- tempNDVIlayer
                GRVIstack <- tempGRVIlayer
              }else{
                NDVIstack <- stack(NDVIstack, tempNDVIlayer)
                GRVIstack <- stack(GRVIstack, tempGRVIlayer)
              }
            } #end of image-loop
            #if NDVIstack has more than one layer (aka more than one available image for the month), perform median among all layers
            if (dim(NDVIstack)[3]!=1){
              #convert from RasterLayer to RasterBrick
              NDVIstack <- brick(NDVIstack)
              #perform median among the raster stack of all images under the month together
              medianNDVI_ofmonth <- calc(NDVIstack, median,na.rm=T) #ignore error, still produces results
            }else if (dim(NDVIstack)[3]==1){ #if NDVIstack only has one layer, meaning there is only one available image for the month, don't need to perform median; the NDVI will be counted for the month
              medianNDVI_ofmonth <- NDVIstack
            }
            #if GRVIstack has more than one layer (aka more than one available image for the month), perform median among all layers
            if (dim(GRVIstack)[3]!=1){
              #convert from RasterLayer to RasterBrick
              GRVIstack <- brick(GRVIstack)
              #perform median among the raster stack of all images under the month together
              medianGRVI_ofmonth <- calc(GRVIstack, median,na.rm=T) #ignore error, still produces results
            }else if (dim(GRVIstack)[3]==1){ #if GRVIstack only has one layer, meaning there is only one available image for the month, don't need to perform median; the GRVI will be counted for the month
              medianGRVI_ofmonth <- GRVIstack
            }
            #convert median NDVI & GRVI value raster for the month to a dataframe
            medianNDVI <- as.data.frame(medianNDVI_ofmonth)
            medianGRVI <- as.data.frame(medianGRVI_ofmonth)
            #set the points' median NDVI & GRVI value for the month as the median NDVI & GRVI values within dataframe
            rasterallpoints[,month+1] <- medianNDVI #ex. NDVI january column is the 1+1=2 2nd column
            rasterallpoints[,month+13] <- medianGRVI #ex. GRVI january column is the 1+13=14 14th column
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
        #loop among all items to find min of xmin, max of xmax, min of ymin, and max of ymax - to later use these extents to create universal raster extent for all SPOT images within this aoi to extend to
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
        #create extent object that holds xmin xmax ymin ymax values that you want to set future rasters to (this is to create an universal extent for all images under this aoi so later these rasters can be stacked)
        extent <- extent(extent_xmin,extent_xmax,extent_ymin,extent_ymax)
        #create empty vector t to store each images・ raster extent within the aoi
        t <- rep(NA,length(aoi_images))
        #loop to find each images・ raster extent within the aoi
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
        
        #create empty dataframe rasterallpoints to later store each pixels' monthly median NDVI value (_N stores NDVI values, _G stores GRVI values)
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
                                      "January_G"=NA,
                                      "February_G"=NA,
                                      "March_G"=NA,
                                      "April_G"=NA,
                                      "May_G"=NA,
                                      "June_G"=NA,
                                      "July_G"=NA,
                                      "August_G"=NA,
                                      "September_G"=NA,
                                      "October_G"=NA,
                                      "November_G"=NA,
                                      "December_G"=NA)
        colnames(rasterallpoints) <- c("index","Jan_N","Feb_N","Mar_N","Apr_N","May_N","Jun_N","Jul_N","Aug_N","Sep_N","Oct_N","Nov_N","Dec_N","Jan_G","Feb_G","Mar_G","Apr_G","May_G","Jun_G","Jul_G","Aug_G","Sep_G","Oct_G","Nov_G","Dec_G")
        
        #loop among 12 months to retrieve monthly median NDVI & GRVI values for aoi
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
          #if there are no available images for the month (aka length(subset_aoiimages)==0), set the points' median NDVI value for the month as NA
          if (length(subset_aoiimages)==0){
            rasterallpoints[,month+1] <- NA #ex. NDVI january column is the 1+1=2 2nd column
            rasterallpoints[,month+13] <- NA #ex. GRVI january column is the 1+13=14 14th column
            next
          }else{
            #loop to stack images with same month and aoi to calculate median NDVI&GRVI
            for (image in 1:length(subset_aoiimages)){
              #loop to retrieve rasters for all 4 bands
              for (band in 1:4){
                #assign SPOT image band digital count values to variables band1-4
                assign(paste("band",band,sep=""),  raster(x=paste(directory,"/",subset_aoiimages[image],"/",subset_aoiimages[image],".",band,".bsq.ers",sep="")))
              } #end of band-loop
              #create tempNDVIlayer & tempGRVIlayer to get NDVI & GRVI raster created from band math
              tempNDVIlayer <- (band4-band3)/(band4+band3)
              tempGRVIlayer <- (band4)/(band2)
              #extend created NDVI & GRVI raster to the universal extent for the images of the aoi with NA values
              tempNDVIlayer <- extend(tempNDVIlayer, extent, value=NA)
              tempGRVIlayer <- extend(tempGRVIlayer, extent, value=NA)
              #stack all images under the month together; resulting NDVIstack & GRVIstack raster stack holds NDVI & GRVI value of all the images available under the year and month of that aoi
              if (image==1){
                NDVIstack <- tempNDVIlayer
                GRVIstack <- tempGRVIlayer
              }else{
                NDVIstack <- stack(NDVIstack, tempNDVIlayer)
                GRVIstack <- stack(GRVIstack, tempGRVIlayer)
              }
            } #end of image-loop
            #if NDVIstack has more than one layer (aka more than one available image for the month), perform median among all layers
            if (dim(NDVIstack)[3]!=1){
              #convert from RasterLayer to RasterBrick
              NDVIstack <- brick(NDVIstack)
              #perform median among the raster stack of all images under the month together
              medianNDVI_ofmonth <- calc(NDVIstack, median,na.rm=T) #ignore error, still produces results
            }else if (dim(NDVIstack)[3]==1){ #if NDVIstack only has one layer, meaning there is only one available image for the month, don't need to perform median; the NDVI will be counted for the month
              medianNDVI_ofmonth <- NDVIstack
            }
            #if GRVIstack has more than one layer (aka more than one available image for the month), perform median among all layers
            if (dim(GRVIstack)[3]!=1){
              #convert from RasterLayer to RasterBrick
              GRVIstack <- brick(GRVIstack)
              #perform median among the raster stack of all images under the month together
              medianGRVI_ofmonth <- calc(GRVIstack, median,na.rm=T) #ignore error, still produces results
            }else if (dim(GRVIstack)[3]==1){ #if GRVIstack only has one layer, meaning there is only one available image for the month, don't need to perform median; the GRVI will be counted for the month
              medianGRVI_ofmonth <- GRVIstack
            }
            #convert median NDVI & GRVI value raster for the month to a dataframe
            medianNDVI <- as.data.frame(medianNDVI_ofmonth)
            medianGRVI <- as.data.frame(medianGRVI_ofmonth)
            #set the points' median NDVI & GRVI value for the month as the median NDVI & GRVI values within dataframe
            rasterallpoints[,month+1] <- medianNDVI #ex. NDVI january column is the 1+1=2 2nd column
            rasterallpoints[,month+13] <- medianGRVI #ex. GRVI january column is the 1+13=14 14th column
          }
        } #end of month-loop
      } #end of if else statement
      
      #calculate max, min, range of median NDVI from Jan to Dec for all points stored in new column "max_N", "min_N"
      rasterallpoints["max_N"] <-apply(X=rasterallpoints[2:13], MARGIN=1, FUN=max,na.rm=T) #margin=1 meaning retrieving max by row
      rasterallpoints["min_N"] <-apply(X=rasterallpoints[2:13], MARGIN=1, FUN=min,na.rm=T) #margin=1 meaning retrieving min by row
      #calculate variance of median NDVI from Jan to Dec for all points stored in new column "variance_N"
      rasterallpoints["variance_N"] <-apply(X=rasterallpoints[2:13], MARGIN=1, FUN=var,na.rm=T) #margin=1 meaning retrieving variance by row
      #calculate max of median GRVI from Jan to Dec for all points stored in new column "max_G"
      rasterallpoints["max_G"] <-apply(X=rasterallpoints[14:25], MARGIN=1, FUN=max,na.rm=T)
      
      #--------------------#classification based on decision tree#--------------------#
      #classify as forest=1, builtup=2, water=3, agri=4, unknown=5
      #duplicate rasterallpoints dataframe for further usage
      allpoints <- rasterallpoints
      allpoints["class"] <- NA
      #thresholds
      allpoints$class[allpoints$max_N<=0.03] <- 3
      
      allpoints$class[allpoints$max_N>0.03 & allpoints$max_N<=0.11 & allpoints$variance_N<=0.003] <- 2
      allpoints$class[allpoints$max_N>0.03 & allpoints$max_N<=0.11 & allpoints$variance_N>0.003] <- 3
      
      allpoints$class[allpoints$max_N>0.11 & allpoints$max_N<=0.19 & allpoints$max_G<=1.1] <- 3
      allpoints$class[allpoints$max_N>0.11 & allpoints$max_N<=0.19 & allpoints$max_G>1.1] <- 2
      
      allpoints$class[allpoints$max_N>0.19 & allpoints$max_N<0.39 & allpoints$variance_N>=0.013 & allpoints$variance_N<=0.089] <- 4
      allpoints$class[allpoints$max_N>0.19 & allpoints$max_N<0.39 & (allpoints$variance_N<0.013 | allpoints$variance_N>0.089)] <- 2
      
      allpoints$class[allpoints$max_N>=0.39 & allpoints$max_N<=0.74 & allpoints$variance_N<=0.02] <- 1
      allpoints$class[allpoints$max_N>=0.39 & allpoints$max_N<=0.74 & allpoints$variance_N>0.02] <- 4
      
      allpoints$class[allpoints$max_N>0.74 & allpoints$variance_N<=0.03] <- 1
      allpoints$class[allpoints$max_N>0.74 & allpoints$variance_N>0.03] <- 4
      
      #classify points that are not classified from above decision tree as unknown land type 5
      allpoints$class[is.na(allpoints$class)==TRUE] <- 5 
      
      #output dataframe as raster
      #duplicate medianNDVI_ofmonth raster created from testloop while loop to copy dimension, extent, and projection
      outputraster<- medianNDVI_ofmonth
      values(outputraster) <- NA
      outputraster <- replace(outputraster,allpoints$index,allpoints$class)
      
      #plot classification results with unknown as grey
      colors <- c("#439c6e","#e86d5f","#8bd2e8","#f0d86e","#999999") #dark green for forest, red for builtup, blue for water, orange for agri, grey for unknown
      #subset color palette to the available classified values (ex. sometimes an image doesn't have unknown land type, so we omit grey from the color palette)
      colors <- colors[sort(unique(outputraster))]
      if (yr==2017){
        png(paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/all/2017and2018_",allaoi[aoi],"_taiwanclassification.png",sep=""))
        # png(paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taoyuan/all/",yr,"/2017and2018_",allaoi[aoi],"_taoyuanclassification.png",sep=""))
      }else{
        png(paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/all/",yr,"_",allaoi[aoi],"_taiwanclassification.png",sep=""))
        # png(paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taoyuan/all/",yr,"/",yr,"_",allaoi[aoi],"_taoyuanclassification.png",sep=""))
      }
      plot(outputraster,
           col=colors)
      dev.off()
      
      #create raster of classification result
      if (yr==2017){
        writeRaster(outputraster,paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/all/2017and2018_",allaoi[aoi],"_taiwanclassification.tif",sep=""))
        # writeRaster(outputraster,paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taoyuan/all/",yr,"/2017and2018_",allaoi[aoi],"_taoyuanclassification.tif",sep=""))
      }else{
        writeRaster(outputraster,paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/all/",yr,"_",allaoi[aoi],"_taiwanclassification.tif",sep=""))
        # writeRaster(outputraster,paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taoyuan/all/",yr,"/",yr,"_",allaoi[aoi],"_taoyuanclassification.tif",sep=""))
      }
      
      #clear up variables "allpoints" and "rasterallpoints"
      rm(allpoints)
      rm(rasterallpoints)
    } #end of aoi-for loop
  } #end of yr-for loop
  
  tree2 <- FALSE
} #end of tree2 while loop

#--------------------#for Taiwan: merge classification results from all aoi#--------------------#
#merge by first by longitude (into 17 strips), then merge 1-5th strip, 6-10th strip, and 11-17th strip, then merge the three into Taiwan
mergeraster_taiwan <- FALSE
while (mergeraster_taiwan==TRUE){
  
  for (yr in c(2013,2014,2015,2016,2017,2019,2020,2021,2022)){ #doesn't iterate yr 2018 because when iterating 2017, 2018 will be counted in
    print(yr)
    if (yr==2017){
      #-----#merge by first by longitude (into 17 strips)#-----#
      #direct to directory of all aoi's classification rasters for the year
      directory <- c(paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/all/",sep="")) #2017and2018 combined classification results will be stored under 2017 folder
      #create list of classification rasters under directory that satisfy condition
      rasters <- dir(path = directory, all.files = FALSE,pattern = "\\.tif$",
                     full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE)
      #for classification results rasters for years 2017&2018, the aoi info is stored at 13-19th character in file name
      rasters <- substr(rasters,start=13,stop=19) 
      #create a vector of all longitudes within Taiwan
      longitude <- 224:240
      
      #iterate among longitude list to merge each longitude's aoi classification rasters together
      for (long in longitude){
        print(long)
        #iterate among longitude list to merge each longitude's aoi classification rasters together
        images <- rasters[substr(rasters,start=1,stop=3)==long]
        #iterate among aoi classification rasters with same longitude to merge them together as single raster
        for (item in 1:length(images)){
          inputraster<- raster(x=paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/all/2017and2018_",images[item],"_taiwanclassification.tif",sep=""))
          #merge classification results to be plotted later
          if (item ==1){
            entireraster <- inputraster
          }else{
            entireraster <- merge(entireraster, inputraster)
          }
        } #end of item loop
        #plot longitude-merged classification results, with unknown as grey
        colors <- c("#439c6e","#e86d5f","#8bd2e8","#f0d86e","#999999") #dark green for forest, red for builtup, blue for water, orange for agri, grey for unknown
        #subset color palette to the available classified values (ex. sometimes an image doesn't have unknown land type, so we omit grey from the color palette)
        colors <- colors[sort(unique(entireraster))]
        png(paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/longitude/2017and2018_",long,"_taiwanclassification.png",sep=""))
        plot(entireraster,
             col = colors)
        dev.off()
        #create raster of the merged classified images
        writeRaster(entireraster,paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/longitude/2017and2018_",long,"_taiwanclassification.tif",sep=""))
        #remove variable "entireraster" to clear variable
        rm(entireraster)
      } #end of long loop
      
      #-----#merge 1-5th strip, 6-10th strip, and 11-17th strip#-----#
      #direct to directory of all longitude's classification rasters for the year
      directory <- c(paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/longitude/",sep=""))
      #create list of longitude-merged classification rasters under directory that satisfy condition
      rasters <- dir(path = directory, all.files = FALSE,pattern = "\\.tif$",
                     full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE)
      #for longitude-merged classification results rasters for years 2017&2018, the aoi's longitude info is stored at 13-15th character in file name
      rasters <- substr(rasters,start=13,stop=15) 
      
      #iterate among longitude list to merge first five longitude-merged classification rasters together
      for (item in 1:5){
        inputraster<- raster(x=paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/longitude/2017and2018_",rasters[item],"_taiwanclassification.tif",sep=""))
        #merge classification results to be plotted later
        if (item ==1){
          entireraster <- inputraster
        }else{
          entireraster <- merge(entireraster, inputraster)
        }
      } #end of item loop
      #plot merged 1-5th longitude classification results, with unknown as grey
      colors <- c("#439c6e","#e86d5f","#8bd2e8","#f0d86e","#999999") #dark green for forest, red for builtup, blue for water, orange for agri, grey for unknown
      #subset color palette to the available classified values (ex. sometimes an image doesn't have unknown land type, so we omit grey from the color palette)
      colors <- colors[sort(unique(entireraster))]
      png(paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/three/2017and2018_taiwanclassification_0105.png",sep=""))
      plot(entireraster,
           col = colors)
      dev.off()
      #create raster of the merged classified images
      writeRaster(entireraster,paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/three/2017and2018_taiwanclassification_0105.tif",sep=""))
      #remove variable "entireraster" to clear variable
      rm(entireraster)
      
      #iterate among longitude list to merge 6-10th longitude-merged classification rasters together
      for (item in 6:10){
        inputraster<- raster(x=paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/longitude/2017and2018_",rasters[item],"_taiwanclassification.tif",sep=""))
        #merge classification results to be plotted later
        if (item ==6){
          entireraster <- inputraster
        }else{
          entireraster <- merge(entireraster, inputraster)
        }
      } #end of item loop
      #plot merged 6-10th longitude classification results, with unknown as grey
      colors <- c("#439c6e","#e86d5f","#8bd2e8","#f0d86e","#999999") #dark green for forest, red for builtup, blue for water, orange for agri, grey for unknown
      #subset color palette to the available classified values (ex. sometimes an image doesn't have unknown land type, so we omit grey from the color palette)
      colors <- colors[sort(unique(entireraster))]
      png(paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/three/2017and2018_taiwanclassification_0610.png",sep=""))
      plot(entireraster,
           col = colors)
      dev.off()
      #create raster of the merged classified images
      writeRaster(entireraster,paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/three/2017and2018_taiwanclassification_0610.tif",sep=""))
      #remove variable "entireraster" to clear variable
      rm(entireraster)
      
      #iterate among longitude list to merge last seven longitude-merged classification rasters together
      for (item in 11:17){
        inputraster<- raster(x=paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/longitude/2017and2018_",rasters[item],"_taiwanclassification.tif",sep=""))
        #merge classification results to be plotted later
        if (item ==11){
          entireraster <- inputraster
        }else{
          entireraster <- merge(entireraster, inputraster)
        }
      } #end of item loop
      #plot merged 11-17th longitude classification results, with unknown as grey
      colors <- c("#439c6e","#e86d5f","#8bd2e8","#f0d86e","#999999") #dark green for forest, red for builtup, blue for water, orange for agri, grey for unknown
      #subset color palette to the available classified values (ex. sometimes an image doesn't have unknown land type, so we omit grey from the color palette)
      colors <- colors[sort(unique(entireraster))]
      png(paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/three/2017and2018_taiwanclassification_1117.png",sep=""))
      plot(entireraster,
           col = colors)
      dev.off()
      #create raster of the merged classified images
      writeRaster(entireraster,paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/three/2017and2018_taiwanclassification_1117.tif",sep=""))
      #remove variable "entireraster" to clear variable
      rm(entireraster)
      
      #-----#merge the three merged rasters into Taiwan#-----#
      firstraster<- raster(x=paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/three/2017and2018_taiwanclassification_0105.tif",sep=""))
      secondraster <- raster(x=paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/three/2017and2018_taiwanclassification_0610.tif",sep=""))
      thirdraster <- raster(x=paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/three/2017and2018_taiwanclassification_1117.tif",sep=""))
      #merge classification results to be plotted later
      entireraster <- merge(firstraster, secondraster)
      entireraster <- merge(entireraster, thirdraster)
      #plot classification results, with unknown as grey
      colors <- c("#439c6e","#e86d5f","#8bd2e8","#f0d86e","#999999") #dark green for forest, red for builtup, blue for water, orange for agri, grey for unknown
      #subset color palette to the available classified values (ex. sometimes an image doesn't have unknown land type, so we omit grey from the color palette)
      colors <- colors[sort(unique(entireraster))]
      png(paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/2017and2018_taiwanclassification.png",sep=""))
      plot(entireraster,
           col = colors)
      dev.off()
      #create raster of the merged classified images
      writeRaster(entireraster,paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/2017and2018_taiwanclassification.tif",sep=""))
      #remove variable "entireraster" to clear variable
      rm(entireraster)
      
    } else{ #for years other than 2017&2018
      #-----#merge by first by longitude (into 17 strips)#-----#
      #direct to directory of all aoi's classification rasters for the year
      directory <- c(paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/all/",sep=""))
      #create list of classification rasters under directory that satisfy condition
      rasters <- dir(path = directory, all.files = FALSE,pattern = "\\.tif$",
                     full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE)
      #for classification results rasters for years other than 2017&2018, the aoi info is stored at 6-12th character in file name
      rasters <- substr(rasters,start=6,stop=12) 
      #create a vector of all longitudes within Taiwan
      longitude <- 224:240
      
      #iterate among longitude list to merge each longitude's aoi classification rasters together
      for (long in longitude){
        print(long)
        #iterate among longitude list to merge each longitude's aoi classification rasters together
        images <- rasters[substr(rasters,start=1,stop=3)==long]
        #iterate among aoi classification rasters with same longitude to merge them together as single raster
        for (item in 1:length(images)){
          inputraster<- raster(x=paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/all/",yr,"_",images[item],"_taiwanclassification.tif",sep=""))
          #merge classification results to be plotted later
          if (item ==1){
            entireraster <- inputraster
          }else{
            entireraster <- merge(entireraster, inputraster)
          }
        } #end of item loop
        #plot longitude-merged classification results, with unknown as grey
        colors <- c("#439c6e","#e86d5f","#8bd2e8","#f0d86e","#999999") #dark green for forest, red for builtup, blue for water, orange for agri, grey for unknown
        #subset color palette to the available classified values (ex. sometimes an image doesn't have unknown land type, so we omit grey from the color palette)
        colors <- colors[sort(unique(entireraster))]
        png(paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/longitude/",yr,"_",long,"_taiwanclassification.png",sep=""))
        plot(entireraster,
             col = colors)
        dev.off()
        #create raster of the merged classified images
        writeRaster(entireraster,paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/longitude/",yr,"_",long,"_taiwanclassification.tif",sep=""))
        #remove variable "entireraster" to clear variable
        rm(entireraster)
      } #end of long loop
      
      #-----#merge 1-5th strip, 6-10th strip, and 11-17th strip#-----#
      #direct to directory of all longitude's classification rasters for the year
      directory <- c(paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/longitude/",sep=""))
      #create list of longitude-merged classification rasters under directory that satisfy condition
      rasters <- dir(path = directory, all.files = FALSE,pattern = "\\.tif$",
                     full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE)
      #for classification results rasters for years other than 2017&2018, the aoi's longitude info is stored at 6-8th character in file name
      rasters <- substr(rasters,start=6,stop=8) 

      #iterate among longitude list to merge first five longitude-merged classification rasters together
      for (item in 1:5){
        inputraster<- raster(x=paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/longitude/",yr,"_",rasters[item],"_taiwanclassification.tif",sep=""))
        #merge classification results to be plotted later
        if (item ==1){
          entireraster <- inputraster
        }else{
          entireraster <- merge(entireraster, inputraster)
        }
      } #end of item loop
      #plot merged 1-5th longitude classification results, with unknown as grey
      colors <- c("#439c6e","#e86d5f","#8bd2e8","#f0d86e","#999999") #dark green for forest, red for builtup, blue for water, orange for agri, grey for unknown
      #subset color palette to the available classified values (ex. sometimes an image doesn't have unknown land type, so we omit grey from the color palette)
      colors <- colors[sort(unique(entireraster))]
      png(paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/three/",yr,"_taiwanclassification_0105.png",sep=""))
      plot(entireraster,
           col = colors)
      dev.off()
      #create raster of the merged classified images
      writeRaster(entireraster,paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/three/",yr,"_taiwanclassification_0105.tif",sep=""))
      #remove variable "entireraster" to clear variable
      rm(entireraster)
      
      #iterate among longitude list to merge 6-10th longitude-merged classification rasters together
      for (item in 6:10){
        inputraster<- raster(x=paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/longitude/",yr,"_",rasters[item],"_taiwanclassification.tif",sep=""))
        #merge classification results to be plotted later
        if (item ==6){
          entireraster <- inputraster
        }else{
          entireraster <- merge(entireraster, inputraster)
        }
      } #end of item loop
      #plot merged 6-10th longitude classification results, with unknown as grey
      colors <- c("#439c6e","#e86d5f","#8bd2e8","#f0d86e","#999999") #dark green for forest, red for builtup, blue for water, orange for agri, grey for unknown
      #subset color palette to the available classified values (ex. sometimes an image doesn't have unknown land type, so we omit grey from the color palette)
      colors <- colors[sort(unique(entireraster))]
      png(paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/three/",yr,"_taiwanclassification_0610.png",sep=""))
      plot(entireraster,
           col = colors)
      dev.off()
      #create raster of the merged classified images
      writeRaster(entireraster,paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/three/",yr,"_taiwanclassification_0610.tif",sep=""))
      #remove variable "entireraster" to clear variable
      rm(entireraster)
      
      #iterate among longitude list to merge last seven longitude-merged classification rasters together
      for (item in 11:17){
        inputraster<- raster(x=paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/longitude/",yr,"_",rasters[item],"_taiwanclassification.tif",sep=""))
        #merge classification results to be plotted later
        if (item ==11){
          entireraster <- inputraster
        }else{
          entireraster <- merge(entireraster, inputraster)
        }
      } #end of item loop
      #plot merged 11-17th longitude classification results, with unknown as grey
      colors <- c("#439c6e","#e86d5f","#8bd2e8","#f0d86e","#999999") #dark green for forest, red for builtup, blue for water, orange for agri, grey for unknown
      #subset color palette to the available classified values (ex. sometimes an image doesn't have unknown land type, so we omit grey from the color palette)
      colors <- colors[sort(unique(entireraster))]
      png(paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/three/",yr,"_taiwanclassification_1117.png",sep=""))
      plot(entireraster,
           col = colors)
      dev.off()
      #create raster of the merged classified images
      writeRaster(entireraster,paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/three/",yr,"_taiwanclassification_1117.tif",sep=""))
      #remove variable "entireraster" to clear variable
      rm(entireraster)
      
      #-----#merge the three merged rasters into Taiwan#-----#
      firstraster<- raster(x=paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/three/",yr,"_taiwanclassification_0105.tif",sep=""))
      secondraster <- raster(x=paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/three/",yr,"_taiwanclassification_0610.tif",sep=""))
      thirdraster <- raster(x=paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/three/",yr,"_taiwanclassification_1117.tif",sep=""))
      #merge classification results to be plotted later
      entireraster <- merge(firstraster, secondraster)
      entireraster <- merge(entireraster, thirdraster)
      #plot classification results, with unknown as grey
      colors <- c("#439c6e","#e86d5f","#8bd2e8","#f0d86e","#999999") #dark green for forest, red for builtup, blue for water, orange for agri, grey for unknown
      #subset color palette to the available classified values (ex. sometimes an image doesn't have unknown land type, so we omit grey from the color palette)
      colors <- colors[sort(unique(entireraster))]
      png(paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/",yr,"_taiwanclassification.png",sep=""))
      plot(entireraster,
           col = colors)
      dev.off()
      #create raster of the merged classified images
      writeRaster(entireraster,paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taiwan/",yr,"/",yr,"_taiwanclassification.tif",sep=""))
      #remove variable "entireraster" to clear variable
      rm(entireraster)
    } #end of if else statement
  } #end of yr-loop
  
  mergeraster_taiwan <- FALSE
} #end of mergeraster_taiwan while loop

#--------------------#pond detection of merged TIF#--------------------#
ponddetect <- FALSE
while (ponddetect==TRUE){
  
  for (yr in c(2013,2014,2015,2016,2017,2019,2020,2021,2022)){ #count 2017&2018 combined classification as 2017
    print(yr)
    #import previously merged classified results of the iterated year
    if (yr==2017){
      taoyuanraster<- raster(x="/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taoyuan/9aoi_merge/2017and2018_taoyuanclassification.tif")
    }else{
      taoyuanraster<- raster(x=paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taoyuan/9aoi_merge/",yr,"_taoyuanclassification.tif",sep=""))
    }
    #read in shapefile of Taiwan city borders
    shapefileborder <- readOGR(
      dsn= "/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/sourcefiles/TWNshapefile.shp" ,
      verbose=FALSE
    )
    crs(shapefileborder) <- crs(taoyuanraster)
    #subset shapefile to only taoyuan 
    shapefileborder <- subset(shapefileborder, shapefileborder$countyid=="68000001")
    #clip taoyuanraster raster with taoyuan shapefile border
    taoyuanpond <- mask(taoyuanraster, shapefileborder)
    
    #create a matrix of index to attach to taoyuanpond raster to give index value
    indexmatrix <- matrix(1:(dim(taoyuanpond)[1]*dim(taoyuanpond)[2]),nrow=dim(taoyuanpond)[1],ncol=dim(taoyuanpond)[2],byrow = TRUE)
    #convert matrix to raster
    indexraster <- raster(indexmatrix)
    #set coordinate system and extent of index raster just like taoyuanpond classification raster
    crs(indexraster) <- crs(taoyuanpond)
    extent(indexraster) <- extent(extent(taoyuanpond)[1],extent(taoyuanpond)[2],
                                  extent(taoyuanpond)[3],extent(taoyuanpond)[4])
    #stack taoyuanpond classification raster with index raster just created
    rasterstack <- stack(taoyuanpond,indexraster)
    #change raster layers' names
    names(rasterstack) <- c("ltype","index")
    
    #calculate total area of land classified as water of clipped classification raster
    temp_taoyuanpond <- as.data.frame(taoyuanpond)
    temp_taoyuanpond <- temp_taoyuanpond[!is.na(temp_taoyuanpond)]
    taoyuanpond_water <- subset(temp_taoyuanpond, temp_taoyuanpond==3)
    print(length(taoyuanpond_water)*6*6)
    
    #import taoyuan pond centroids coordinates csv
    centroidpoints <- read.csv("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/sourcefiles/centroid.csv", header=T, encoding = "UTF-8",sep = ",")
    coordinates(centroidpoints)= ~ TWD97X + TWD97Y
    crs(centroidpoints) <- crs(taoyuanpond)
    #extract value of taoyuanpond raster (land type and index) through overlapping point of centroid
    rasValue <- raster::extract(rasterstack, centroidpoints)
    #create combinePointValue dataframe that stores the centroid coordinates and extracted index+landtype value from taoyuanpond raster
    combinePointValue <- cbind(as.data.frame(centroidpoints),rasValue)
    #create new pondarea column to combinePointValue dataframe
    combinePointValue["pondarea"] <- NA
    
    #find neighboring water pixels of pond centroids
    boxrange<-75 
    #use 75 because on Taoyuan gov website it says largest pond is 198900 m2, which is 198900/6/6=5525 pixels, which in a matrix square of 75 pixels per side (75*75=5625)
    box <- (75-1)/2
    #the purpose of this pixel for-loop is to iterate among the pond centroids to see how many water pixels are neighboring within the 75x75 square surrounding pond centroid
    for (pixel in 1:length(combinePointValue$index)){ #length(combinePointValue$index)=223
      #if extracted land type of centroid is NA value, skip
      if (is.na(combinePointValue$ltype[pixel])){
        next
      }else{
        #convert index value to raster row and column value
        raster_row <- (combinePointValue$index[pixel]%/%(dim(taoyuanpond)[2]))+1
        raster_col <- combinePointValue$index[pixel]-((combinePointValue$index[pixel]%/%(dim(taoyuanpond)[2]))*dim(taoyuanpond)[2])
        #clip a 75x75 matrix with center at centroid; the min max functions in the extent function is to deal with situations where centroid is close to border therefore cannot extend 75x75 matrix outwards
        clipmatrix <- crop(rasterstack, extent(rasterstack, max(1,raster_row-box), min(dim(taoyuanpond)[1],raster_row+box), max(1,raster_col-box), min(dim(taoyuanpond)[2],raster_col+box))) #row min, row max, col min, col max
        #iterate pixels within the clipped 75x75 box to see how many water pixels and filter out water pixels that don't have adjacent water pixels (aka stand alone by itself)
        #choose to filter out water pixels that don't have adjacent water pixels because it may be just noise or misclassification
        for (row in 1:boxrange){
          for (col in 1:boxrange){
            if (is.na(clipmatrix[row,col][1])){
              next
            }else if (clipmatrix[row,col][1]==3){
              #templist evaluating left right top bottom pixels of clipmatrix[row,col][1] to see if they are also water ltype
              #solve issues regarding border pixels - templist[1]=0 means that there is no left pixels, clipmatrix[row,col][1] is at left border of matrix
              #                                       templist[2]=boxrange+1 means that there is no right pixels, clipmatrix[row,col][1] is at right border of matrix
              #                                       templist[3]=0 means that there is no top pixels, clipmatrix[row,col][1] is at top border of matrix
              #                                       templist[4]=boxrange+1 means that there is no bottom pixels, clipmatrix[row,col][1] is at bottom border of matrix
              templist<-c(max(0,col-1),min(boxrange+1,col+1),max(0,row-1),min(boxrange+1,row+1)) #left right top bottom
              #if templist[1]!=0 that there is left neighboring pixel, get land type of the left pixel
              if (templist[1]!=0){ templist[1]<- clipmatrix[row,templist[1]][1] }
              #if templist[2]!=(boxrange+1) that there is right neighboring pixel, get land type of the right pixel
              if (templist[2]!=(boxrange+1)){ templist[2]<-clipmatrix[row,templist[2]][1] }
              #if templist[3]!=0 that there is top neighboring pixel, get land type of the top pixel
              if (templist[3]!=0){ templist[3]<-clipmatrix[templist[3],col][1] }
              #if templist[4]!=(boxrange+1) that there is bottom neighboring pixel, get land type of the bottom pixel
              if (templist[4]!=(boxrange+1)){ templist[4]<-clipmatrix[templist[4],col][1] }
              #set templist[1]/templist[3] as 0 is there are no left/top neighbor pixels
              templist[c(1,3)][templist[c(1,3)]==0]<- 0
              #set templist[2]/templist[4] as 0 is there are no right/bottom neighbor pixels
              templist[c(2,4)][templist[c(2,4)]==boxrange+1]<- 0
              #count the number water pixels surrounding the iterated pixel; if there is no surrounding water pixels, the center pixel will not be counted as pond
              if ((3 %in% templist)==FALSE){
                clipmatrix[row,col][1]<-NA #set those pixels that don't contain neighboring water pixels as NA
              } #end of if statement
            } #end of else-if statement
          } #end of col-loop
        } #end of row-loop
        #create classifiedlandtype dataframe out of the land type column of clipmatrix dataframe to later count how many water pixels are in it to calculate pond size
        classifiedlandtype <- as.data.frame(clipmatrix$ltype)
        #count the number of occurrence of 3 within the clipped 75x75 matrix to see how many water pixels are counted as pond and *6*6 to get total area of pond (pixel resolution is 6x6m)
        combinePointValue$pondarea[pixel] <- (sum(classifiedlandtype==3,na.rm=TRUE))*6*6
        clipmatrix <- as.data.frame(clipmatrix)
        #subset clipmatrix to only those have water land type (later to be converted to pond pixels)
        tempmatrix <- subset(clipmatrix, clipmatrix$ltype==3)
        #bind all counted pond pixels into single dataframe "wholelist" that contains all pond pixels index
        if (exists("wholelist")==FALSE){ #set the first tempmatrix created as wholelist since it is empty
          wholelist <- tempmatrix
        }else{ #stack the following pixel's tempmatrix created to wholelist since it is not empty anymore
          wholelist <- rbind(wholelist,tempmatrix)
        }
      } #end of else-if statement
    } #end of pixel-loop
    #--------------------#
    #create pondareas vectors storing the pond areas detected in each year's rasters
    pondareas <- combinePointValue[!is.na(combinePointValue$pondarea), 6] #only output the pondarea column to pondareas dataframe
    pondareas <- pondareas[pondareas!=0]
    #print sum of detected pond areas for the year
    print(sum(pondareas))
    
    #following if else statement results in allyear_pondareas dataframe holding the pond areas of each year
    if (exists("allyear_pondareas")==FALSE){ #set the first pondareas list created as allyear_pondareas since it is empty
      #choose to create empty list with list length identical as "combinePointValue" then fill in each pond areas of the year, so that when cbind() future pondareas df to allyear_pondareas df, there won't be length not matching errors
      allyear_pondareas <- rep(NA,dim(combinePointValue)[1])
      allyear_pondareas[1:length(pondareas)] <- pondareas
    }else{ #bind the following pondareas list created to allyear_pondareas dataframe since it is not empty anymore
      temp_pondareas <- rep(NA,dim(combinePointValue)[1])
      temp_pondareas[1:length(pondareas)] <- pondareas
      allyear_pondareas <- cbind(allyear_pondareas, temp_pondareas)
    }
    #using the index of pond pixels to replace the land type of taoyuanpond raster of same index from 3 to 5 (reclassify from water to pond)
    #reclassify original classification results with 5 indicating ponds (before:forest: 1; builtup: 2; water: 3; agri: 4; now: forest: 1; builtup: 2; water: 3; agri: 4; pond: 5)
    taoyuanpond <- replace(taoyuanpond,wholelist$index,5) #there will be difference between pondareas and the actual amount of pixels replaced as pond type 5 because there are duplicated counts in pondareas
    #transfer water pixels to pond pixels under the scenario that the pond is only one pixel standing by its own that is deleted in the above clipmatrix for-loop
    subsetdf <- combinePointValue[!is.na(combinePointValue$ltype)&combinePointValue$ltype==3,]
    taoyuanpond <- replace(taoyuanpond,subsetdf$index,5)
    
    #create raster of reclassified taoyuan image (forest: 1; builtup: 2; water: 3; agri: 4; pond: 5)
    if (yr==2017){
      writeRaster(taoyuanpond,"/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taoyuan/9aoi_merge/pond_clips/2017and2018_taoyuanclassification_pond5.tif")
    }else{
      writeRaster(taoyuanpond,paste("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taoyuan/9aoi_merge/pond_clips/",yr,"_taoyuanclassification_pond5.tif",sep=""))
    }
    
    rm(wholelist) #clear variable so "if (exists("wholelist")==FALSE)" if statement can be used again for next year
  } #end of yr-loop
  
  #export allyear_pondareas dataframe to csv for future usage
  write.csv(allyear_pondareas,"/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taoyuan/9aoi_merge/pond_clips/taoyuan_2013to2022pondarea.csv", row.names = FALSE)
  
  #plot density plot of pond area size distribution for 2013 to 2022
  options(scipen=0.5)
  png("/data1/home/vivianlin0921/R_Scripts/PCA(forWFH)/plots/classificationresults/taoyuan/9aoi_merge/pond_clips/taoyuan_2013to2022pondarea_densityplot_2018combined.png")
  #set plottext for xaxis labels
  plottext <- c("2013","2014","2015","2016","2017-2018","2019","2020","2021","2022")
  #set density plot line colors for each year
  colors <- c("#080808","#7d7d7d","#dbdbdb","#080808","#7d7d7d","#dbdbdb","#080808","#7d7d7d","#dbdbdb") #black, grey, light grey
  #find ylim min and max for density plot
  ylim_min <- min(density(allyear_pondareas[,1],na.rm=T)$y,density(allyear_pondareas[,2],na.rm=T)$y,density(allyear_pondareas[,3],na.rm=T)$y,density(allyear_pondareas[,4],na.rm=T)$y,density(allyear_pondareas[,5],na.rm=T)$y,density(allyear_pondareas[,6],na.rm=T)$y,density(allyear_pondareas[,7],na.rm=T)$y,density(allyear_pondareas[,8],na.rm=T)$y,density(allyear_pondareas[,9],na.rm=T)$y)
  ylim_max <- max(density(allyear_pondareas[,1],na.rm=T)$y,density(allyear_pondareas[,2],na.rm=T)$y,density(allyear_pondareas[,3],na.rm=T)$y,density(allyear_pondareas[,4],na.rm=T)$y,density(allyear_pondareas[,5],na.rm=T)$y,density(allyear_pondareas[,6],na.rm=T)$y,density(allyear_pondareas[,7],na.rm=T)$y,density(allyear_pondareas[,8],na.rm=T)$y,density(allyear_pondareas[,9],na.rm=T)$y)
  
  #plot density plot of 2013
  plot(density(allyear_pondareas[,1],na.rm=T),
       col = colors[1], lwd = 2, lty=1,
       ylim=c(ylim_min, ylim_max),
       xlab=expression("pond area (m"^2*")"),
       main="2013~2022 Density Plot of Taoyuan Pond Area")
  #plot density plot of 2014,2015 since they have same plotting line type lty=1 (same with 2013)
  for (i in 2:3){
    lines(density(allyear_pondareas[,i],na.rm=T), lwd = 2, lty=1, col = colors[i])
  }
  #plot density plot of 2016,2017&2018,2019 since they have same plotting line type lty=2
  for (i in 4:6){
    lines(density(allyear_pondareas[,i],na.rm=T), lwd = 2, lty=2, col = colors[i])
  }
  #plot density plot of 2020,2021,2022 since they have same plotting line type lty=3
  for (i in 7:9){
    lines(density(allyear_pondareas[,i],na.rm=T), lwd = 2, lty=3, col = colors[i])
  }
  legend("topright", plottext,
         lty = c(1,1,1,2,2,2,3,3,3),
         col = colors, cex=1)
  dev.off()
  
  rm(allyear_pondareas) #clear variable
  
  ponddetect <- FALSE
} #end of ponddetect while loop
