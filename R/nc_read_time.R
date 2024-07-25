
#' @title Read time variable in a netCDF File
#'
#' @description
#' Will read the "time" variable in a netcdf file, and transform in a POSIXc vector using the units of the time variable
#' 
#' @usage nc_read_time(filename)
#' @param netcdf_file Path to the netCDF file
#'
#' @return vector with all dates in POSIXc format
#' @export
#' @examples
#' 
#' time <- nc_read_time("path\to\netCDFfile")
#' 
nc_read_time <- function(netcdf_file) {
  
  time_var <- ncatt_get(netcdf_file,'time','units')$value
  format_var <- ncatt_get(netcdf_file,'time','date_format')$value
  
  time <- ncvar_get(netcdf_file,'time')
  
  time_var_split <- unlist(str_split(time_var,' since '))
  
  time_units <- time_var_split[1]
  if (time_units == 'hours'){
    if (format_var == 0) {
      format = '%Y-%m-%d %H:%M:%S'
    }else{
      format = format_var
    }
    time_start <- strptime(time_var_split[2],format = format, tz = 'GMT')
    
    time_date <- hours(time) + time_start
  }else if (time_units == 'days'){
    if (format_var == 0) {
      format = '%Y-%m-%d'
    }else{
      format = format_var
    }
    time_start <- strptime(time_var_split[2],format = format, tz = 'GMT')
    
    time_date <- days(time) + time_start
  }else if (time_units == 'seconds'){
    if (format_var == 0) {
      format = '%Y-%m-%d %H:%M:%S'
    }else{
      format = format_var
    }
    time_start <- strptime(time_var_split[2],format = format, tz = 'GMT')
    
    time_date <- seconds(time) + time_start
  }else{
    stop(paste("time units",time_units,"not recognize"))
  }
  
  
  return(as.POSIXct(time_date))
}

