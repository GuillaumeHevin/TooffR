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


#' @title Easy progress bar
#' @description Progress bar easy to use to check progress of loops
#' 
#' @usage Chargement_command_window(size, text = '', minimal = FALSE)
#' 
#' @param size Number of total elements in the loop
#' @param text Text to add before the progress bar (default = '')
#' @param minimal if FALSE, a progress bar will appear, if TRUE only the percentage is shown
#' 
#' @examples
#'  
#' #simple loop
#' Chargement_Command_window(100)
#' for(i in 1:100)
#'  {
#'    Chargement_Command_window()
#'    Sys.sleep(0.15)
#'  }
#' 
#' 
#' #Use in double loops without progress bar with a descriptive text
#' Chargement_Command_window(6*7, text = 'Waiting...', minimal = T)
#' for (j in 1:6)
#' {
#'  for(i in 1:7)
#'  {
#'    Chargement_Command_window()
#'    Sys.sleep(0.15)
#'  }
#' }


Chargement_Command_window <- function(size, text = '', minimal = FALSE) {
  interval_trait <- 5
  
  if (nargs() > 0)
  {
    if (nchar(text) > 30)
    {
      text <- sprintf(' %-30s',paste0(substr(text,1,10),'...',substr(text,nchar(text)-16,nchar(text))))
    }else if (!text == ''){
      text <- sprintf(' %-30s',text)
    }
    CCW_I <<- 0
    CCW_TXT <<- text
    CCW_NBRE <<- size
    CCW_MINIMAL <<- minimal
    
    #Initialize print
    percent <- (CCW_I / CCW_NBRE) * 100
    
    if (!CCW_MINIMAL){
      if (percent == 100){fleche <- '='}else{fleche <- '>'}
      Nb_trait <- max(c(percent %/% interval_trait -1,0))
      empty <- 100/interval_trait - Nb_trait -1
      bar <- paste0(' [',strrep('=', Nb_trait),fleche,strrep(' ', empty),']')
    }else{
      bar <- ''
    }
    
    cat(paste0("\r",CCW_TXT,bar, sprintf(' %3s%%',sprintf('%.0f',percent))))
    
  }else{
    if (!exists('CCW_I'))
    {
      stop('Chargement_Command_window needs initialisation or size argument is to small compared to the loop')
    }
    CCW_I <<- CCW_I +1
    
    percent <- (CCW_I / CCW_NBRE) * 100
    
    if (!CCW_MINIMAL){
      if (percent == 100){fleche <- '='}else{fleche <- '>'}
      Nb_trait <- max(c(percent %/% interval_trait -1,0))
      empty <- 100/interval_trait - Nb_trait -1
      bar <- paste0(' [',strrep('=', Nb_trait),fleche,strrep(' ', empty),']')
    }else{
      bar <- ''
    }
    
    cat(paste0("\r",CCW_TXT,bar, sprintf(' %3s%%',sprintf('%.0f',percent))))
    
    if (percent == 100){
      rm(CCW_I,CCW_TXT,CCW_NBRE,CCW_MINIMAL, envir = .GlobalEnv)
      cat('\n')
    }
  }
}
























