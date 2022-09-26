########################################################
### Function to write raster stacks as NETCDF FILE   ###
### Rakesh Kumar Singh 23 Sep 2022                   ###
### Inputs:                                          ###
### fname <- Filename for output NETCDF file         ###
### odata <- Output raster stack                     ###
### varnames <- Name of the image layers             ###
### dname <- Name of the dataset                     ###
### vunit <- Unit of the Dataset                     ###
########################################################
writeNC <- function(fname,odata,varnames,dname,vunit) {
  
  if (!require(raster)) 
    stop("raster package is missing")
  if (!require(ncdf4)) 
    stop("ncdf4 package is missing")
  if (!require(oceanmap)) 
    stop("oceanmap package is missing")
  
  message(paste0("Saving rasters to file:",fname))
  # Getting lat and long for the rasters
  lon <- seq(xmin(odata),xmax(odata),xres(odata))
  lon<-lon[-length(lon)]
  lat <- seq(ymin(odata),ymax(odata),yres(odata))
  lat<-lat[-length(lat)]
  nx <- length(lon)
  ny <- length(lat)
  
  
  # define dimensions
  londim <- ncdim_def("Longitude","degrees_east",as.double(lon))
  latdim <- ncdim_def("Latitude","degrees_north",as.double(lat))
  rm(lat,lon)
  
  # define variables
  fillvalue <- -999.0
  zname <- varnames
  var_def <- sapply(1:length(zname), FUN = function(x){ncvar_def(zname[[x]],
                                                                 vunit,list(londim,latdim),
                                                                 fillvalue,dname,prec="single")},
                    simplify = FALSE, USE.NAMES = TRUE)
  rm(vunit, londim, latdim)
  
  # Replacing NAs with fillvalue
  message(paste0("Replacing NAs with ", fillvalue))
  idx <- sapply(names(odata), FUN = function(x) {is.na(raster::values(odata[[x]]))},
                simplify = FALSE, USE.NAMES = TRUE)
  for (i in 1:nlayers(odata))
    raster::values(odata[[i]])[idx[[i]]] <- fillvalue
  rm(idx, i, fillvalue)
  
  
  # create netCDF file and put arrays
  ncout <- nc_create(fname,var_def,force_v4=TRUE)
  message(paste0(fname," is created"))
  
  # put variables
  dummy <- sapply(1:length(zname), FUN = function(x){ncvar_put(ncout,var_def[[x]], raster2matrix(odata[[x]]),
                                                               start=c(1,1), count=c(nx,ny))},
                  simplify = FALSE, USE.NAMES = TRUE)
  message(paste0("Variables are written to ",fname))
  rm(dummy,zname, var_def, odata, nx, ny)
  
  # put additional attributes into dimension and data variables
  message("Writing attributes: Latitude and Longitude")
  ncatt_put(ncout,"Longitude","axis","X") #,verbose=FALSE) #,definemode=FALSE)
  ncatt_put(ncout,"Latitude","axis","Y")
  
  # add global attributes
  message("Writing global attributes")
  ncatt_put(ncout,0,"Title",substr(fname,1,nchar(fname)-3))
  ncatt_put(ncout,0,"Institution","Universit? du Qu?bec ? Rimouski")
  ncatt_put(ncout,0,"Image Source","Ocean Biology Processing Group (OBPG), NASA-GSFC")
  #ncatt_put(ncout,0,"Reference",ref_article)
  ncatt_put(ncout,0,"History",paste("Rakesh Kumar Singh", date(), sep=", "))
  ncatt_put(ncout,0,"Software","SeaDASv8")
  
  # close the file, writing data to disk
  nc_close(ncout)
  message(paste0("Rasters saved to ",fname," successfully."))
}
