
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


#' Convert date string to POSIXct
#' @description
#' Easy to use string date to POSIXct convertor, with string dates in format Y-m-d or Y-m-d H:M:S.
#' Any other format will result return NA.
#' Timezone set for the conversion is 'GMT'
#' 
#' @usage text_toDate(date)
#' 
#' @param date date to convert as a string, with format Y-m-d or Y-m-d H:M:S
#'
#' @return date as POSIXct
#' @export
#' 
#' @examples
#' date_POSIXct <- text_toDate('2012-12-26')
#'   [1] "2012-12-26 GMT"
#' 
#' text_toDate(c('2012-12-26 12:00:00','1995-01-16 20:00:00'))
#'   [1] "2012-12-26 12:00:00 GMT" "1995-01-16 20:00:00 GMT"
text_toDate <- function(date) {
  
  #find if there is any hours in text
  if (any(stringr::str_detect(date,':')))
  {
    format_date <- '%Y-%m-%d %T'
  }else{
    format_date <- '%Y-%m-%d'
  }
  
  #convert to POSIXct
  date_ok <- as.POSIXct(date, format = format_date, tz = 'GMT' )
  
  return(date_ok)
  
}
