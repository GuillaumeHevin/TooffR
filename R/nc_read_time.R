# Copyright 2024 Guillaume Hévin (guillaume.hevin@protonmail.com)*1
#                     
# *1   INRAE, France
#
# This file is part of TooffR_Package R package.
#
# TooffR_Package R package is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# TooffR_Package R package is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with TooffR_Package R package.
# If not, see <https://www.gnu.org/licenses/>.


#' @title Read time variable in a netCDF File
#'
#' @description
#' Will read the "time" variable in a netcdf file, and transform in a POSIXc vector using the units attribute of the time variable.\cr
#' 
#' @usage nc_read_time(nc_file)
#' 
#' @param netcdf_file object created by [ncdf4::nc_open()]
#'
#' @return vector with all dates in POSIXc format
#' 
#' @details
#' The units attribute must be in the following format: ":TIME: since :DATE:".\cr
#' :TIME: can have the following values:
#'  - days    -> :DATE: format by default is %Y-%m-%d
#'  - hours   -> :DATE: format by default is %Y-%m-%d %H:%M:%S
#'  - seconds -> :DATE: format by default is %Y-%m-%d %H:%M:%S
#' 
#' If the attribute "date_format" exist, it will be use for :DATE: format.
#' 
#' @seealso [ncdf4::nc_open()]
#' 
#' @examples
#' 
#' #Exemple with no "date_format" attributes
#' file <- "path\to\netCDFfile"
#' nc_file <- nc_open(file)
#' 
#' nc_file
#'#>      1 variables (excluding dimension variables):
#'#>        double Bm[time,gid]   (Chunking: [4018,100])  
#'#>        units: m
#'#>        
#'#>      2 dimensions:
#'#>        time  Size:4018 
#'#>          units: days since 2010-08-01
#'#>          long_name: time
#'#>        gid  Size:33612 
#'#>          units: Segments_ID
#'#>          long_name: gid
#'
#' time <- nc_read_time(nc_file)
#' time
#'#>    [1] "2010-08-01 GMT" "2010-08-02 GMT" "2010-08-03 GMT" "2010-08-04 GMT"
#'#>    [5] "2010-08-05 GMT" "2010-08-06 GMT" "2010-08-07 GMT" "2010-08-08 GMT"
#'#>    [9] "2010-08-09 GMT" "2010-08-10 GMT" "2010-08-11 GMT" "2010-08-12 GMT"
#'#>    ...
#'
#'
#' #Exemple with "date_format" attributes
#' file <- "path\to\netCDFfile"
#' nc_file <- nc_open(file)
#' 
#' nc_file
#'#>      1 variables (excluding dimension variables):
#'#>        float data[time,gid]   (Contiguous storage)  
#'#>        variable_name: Tw_NFS
#'#>          units: celsius_degrees
#'#>          long_name: water temperature at the final node
#'#>      
#'#>      2 dimensions:
#'#>        gid  Size:1 
#'#>          units: segments_IDs
#'#>        time  Size:96432 
#'#>          units: hours since 01/08/2010 00:00:00
#'#>          date_format: %d/%m/%Y %H:%M:%S
#'        
#'        
#' time <- nc_read_time(nc_file)
#' 
#' time
#'#>    [1] "2010-08-01 00:00:00 GMT" "2010-08-01 01:00:00 GMT"
#'#>    [3] "2010-08-01 02:00:00 GMT" "2010-08-01 03:00:00 GMT"
#'#>    [5] "2010-08-01 04:00:00 GMT" "2010-08-01 05:00:00 GMT"
#'#>    ...


nc_read_time <- function(netcdf_file) {
  
  #get units attribute of the variable----
  time_var_unit <- ncatt_get(netcdf_file,'time','units')$value
  
  if (time_var_unit == 0) {
    stop('The netCDF must have a dimension named "time" with the attributes "units"')
  }
  
  #get date format in the units
  format_var <- ncatt_get(netcdf_file,'time','date_format')$value
  if (format_var == 0) {
    format_default = TRUE
  } else {
    format_default = FALSE
  }
  
  #get time variable
  time <- ncvar_get(netcdf_file,'time')
  
  #get info in units
  time_var_unit_split <- unlist(str_split(time_var_unit,' since '))
  
  time_units <- time_var_unit_split[1]
  date_units <- time_var_unit_split[2]
  
  #Create the date vector
  if (time_units == 'hours') {
    if (format_default) {
      format = '%Y-%m-%d %H:%M:%S'
    }else{
      format = format_var
    }
    
    time_start <- strptime(date_units,format = format, tz = 'GMT')
    time_date <- hours(time) + time_start
    
  }else if (time_units == 'days') {
    if (format_default) {
      format = '%Y-%m-%d'
    }else{
      format = format_var
    }
    
    time_start <- strptime(date_units,format = format, tz = 'GMT')
    time_date <- days(time) + time_start
    
  }else if (time_units == 'seconds') {
    if (format_default) {
      format = '%Y-%m-%d %H:%M:%S'
    }else{
      format = format_var
    }
    
    time_start <- strptime(date_units,format = format, tz = 'GMT')
    time_date <- seconds(time) + time_start
    
  }else {
    
    stop(paste("time units",time_units,"not recognize"))
  }
  
  
  return(as.POSIXct(time_date))
}


