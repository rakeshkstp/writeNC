writeNC <- function(fname,odata,varnames,dname,vunit) {
  
  if (!require(terra)) 
    stop("terra package is missing")
  if (!require(ncdf4)) 
    stop("ncdf4 package is missing")
 # if (!require(oceanmap)) 
#    stop("oceanmap package is missing")
  
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
  idx <- sapply(1:nlyr(odata), FUN = function(x) {is.na(terra::values(odata[[x]]))},
                simplify = FALSE, USE.NAMES = TRUE)
  for (i in 1:nlyr(odata))
    terra::values(odata[[i]])[idx[[i]]] <- fillvalue
  rm(idx, i, fillvalue)
  
  
  # create netCDF file and put arrays
  ncout <- nc_create(fname,var_def,force_v4=TRUE)
  message(paste0(fname," is created"))
  
  # put variables
  dummy <- sapply(1:length(zname), FUN = function(x){ncvar_put(ncout,var_def[[x]], matrix(values(flip(odata[[x]])), nx, ny, byrow = FALSE),
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
  ncatt_put(ncout,0,"Institution","Centre for Remote Imaging, Sensing and Processing (CRISP), National University of Singapore")
  ncatt_put(ncout,0,"Image Source","NASA-Ocean Biology Distributed Active Archive Center (OBDAAC)")
  ncatt_put(ncout,0,"Reference","Singh, R.K., Vader, A., Mundy, C.J., Søreide, J.E., Iken, K., Dunton, K.H., Castro de la Guardia, L., Sejr, M.K., Bélanger, S., 2022. Satellite-Derived Photosynthetically Available Radiation at the Coastal Arctic Seafloor. Remote Sens. 14, 5180. https://doi.org/10.3390/rs14205180")
  ncatt_put(ncout,0,"History",paste("Rakesh Kumar Singh", date(), sep=", "))
  ncatt_put(ncout,0,"Processing Software","SeaDASv8")
  
  # close the file, writing data to disk
  nc_close(ncout)
  message(paste0("Rasters saved to ",fname," successfully."))
}