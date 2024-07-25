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



#' Convert secondes to human readable time
#' @description
#' Convert a number of seconds in the format "days, hours min sec" to be easly readable by humain
#' 
#' @param secondes number of seconds to convert
#'
#' @return character in format "days hours minuts secondes"
#' @export
#'
#' @examples
#' sec2time(361)
#'   [1] "6m 1s"
#' 
#' sec2time(c(18956, 900163, 23, 637))
#'   [1] "5h 15m 56s"   "10 days, 10h 2m 43s"   "23s"   "10m 37s"  
sec2time <- function(secondes) {
  
  
  if (length(secondes) > 1) {
    str_time_all <-  rep(NA, length(secondes))
  }
  
  for (i in 1:length(secondes)) {
    sec = secondes[i]
    
    if (is.na(sec)){
      str_time <- ''
    }else{
      days <- as.integer(sec %/% 86400)
      remainder <- sec %% 86400
      
      hours <- as.integer(remainder %/% 3600)
      remainder <- remainder %% 3600
      
      minutes <- as.integer(remainder %/% 60)
      seconds <- as.integer(remainder %% 60)
      
      d_str <- if (days > 0) paste0(days, " day", ifelse(days == 1, "", "s"), ", ") else ""
      h_str <- if (hours > 0) paste0(hours, "h ") else ""
      m_str <- if (minutes > 0) paste0(minutes, "m ") else ""
      s_str <- paste0(seconds, "s")
      
      if (sec >= 86400) {
        str_time <- paste0(d_str, h_str, m_str, s_str)
      } else if (sec >= 3600) {
        str_time <- paste0(h_str, m_str, s_str)
      } else if (sec >= 60) {
        str_time <- paste0(m_str, s_str)
      } else {
        str_time <- s_str
      }
    }
    
    
    if (length(secondes) > 1) {
      str_time_all[i] <- str_time
    }else{
      str_time_all <- str_time
    }
  }
  return(str_time_all)
  
}