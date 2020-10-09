# rstToWKT

  # Huriel Reichel
  # Copyright (c) 2020 STDL, Swiss Territorial Data Lab

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# This will transform any raster, if not too big or dense, in a CSV file 
# with WKT coordinates. Pay attention to the five arguments and how they 
# need to be called. 

# rgb and elev_bool must always be called as either TRUE of FALSE, and
# this is defining in which loop we will be working in. And remember, if 
# you're working only with elevation raster, set its path with elev, and not
# with input_raster.

rstToWKT <- function(input_raster, #whole path with " "
                       output_wkt, #whole path with " " 
                       rgb, #TRUE of FALSE
                       elev_bool, #TRUE of FALSE
                       elev){  

#installing and loading required libraries
if(!require(rgdal)){
  install.packages("rgdal")
  library(rgdal)
}
if(!require(sf)){
  install.packages("sf")
  library(sf)
}
  if(!require(raster)){
    install.packages("sf")
    library(sf)
  }
  
#if raster is a RGB one only
  if (elev_bool == FALSE && rgb == TRUE ){
    #reading the raster file
    rst <- readGDAL(input_raster)
    #checking projection
    sf <- spTransform(rst, CRS("+proj=longlat +ellps=WGS84"))
    print("Projection system is or has been transformed to WGS84")
    cb <- data.frame(cbind(sf@coords, sf$band1, sf$band2, sf$band3))
    cb <- cb[complete.cases(cb), ]
    colnames(cb) <- c("x", "y", "R", "G", "B")
    sf <- st_as_sf(cb, coords = c("x", "y"), crs = 4326)
    st_write(sf, output_wkt, layer_options = "GEOMETRY=AS_WKT", overwrite = TRUE)
    print("RGB only")
   
#if raster is RGB and you have an elevation raster too
  } else if (elev_bool == TRUE && rgb == TRUE) {
    #reading the raster file
    rst <- readGDAL(input_raster)
    #checking projection
    sf <- spTransform(rst, CRS("+proj=longlat +ellps=WGS84"))
    print("Projection system is or has been transformed to WGS84")
    dem = readGDAL(elev)
    dem <- spTransform(dem, CRS("+proj=longlat +ellps=WGS84"))
    dem <- as.data.frame(dem)
    dem <- rasterFromXYZ(dem[, c("x", "y", "band1")])
    ext <- extract(dem, sf)
    cb <- data.frame(cbind(sf@coords, ext, sf$band1, sf$band2, sf$band3))
    cb <- cb[complete.cases(cb), ]
    colnames(cb) <- c("x", "y", "MSL", "R", "G", "B")
    sf <- st_as_sf(cb, coords = c("x", "y"), crs = 4326)
    st_write(sf, output_wkt, layer_options = "GEOMETRY=AS_WKT", overwrite = TRUE)
    print("RGB and elevation")

#if only elevation raster is available
  } else if (elev_bool ==TRUE && rgb == FALSE) {
    #reading the raster file
    rst <- readGDAL(elev)
    #checking projection
    sf <- spTransform(rst, CRS("+proj=longlat +ellps=WGS84"))
    print("Projection system is or has been transformed to WGS84")
    cb <- data.frame(cbind(sf@coords, sf$band1))
    cb <- cb[complete.cases(cb), ]
    colnames(cb) <- c("x", "y", "MSL")
    sf <- st_as_sf(cb, coords = c("x", "y"), crs = 4326)
    st_write(sf, output_wkt, layer_options = "GEOMETRY=AS_WKT", overwrite = TRUE)  
    print("elevation only")

#if nothing else, check for errors
  } else {
    print("Error possible by reading, please check data and arguments")
  }
}


