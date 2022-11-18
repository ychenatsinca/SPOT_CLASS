

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
