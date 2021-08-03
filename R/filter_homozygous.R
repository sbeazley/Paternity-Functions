#########################################################################
#
# Package: Paternity-Functions
#
# File: filter_homozygous.R
# Contains: filter_homozygous
#
# Written by Samuel Beazley
#
# First version: May-2021
# Last update: 2-Aug-2021
#
#########################################################################
#'
#' Test parentage of individual
#'
#' Given a vector of possible parents and a dataframe, the function returns an edited dataframe that only allows rows that have at least one case of 0x0 and 4x4 cross within the parent duos.
#'
#' @param parents a vector with strings related to the name of the suspected parents
#' @param data the dataframe from which the data is from
#'
#' @return A dataframe only containing rows with at least 0x0 or 4x4 cross
#'
#' @examples
#'
#' filter_homozygous(parents = c("W6511.1R","VillettaRose","W9914.1R"),
#'            data = potato.data)
#'
#' @export
#'

filter_homozygous <- function(parents,data)
{
  rows = c()
  for(i in 1:length(data)){
    vec0 = c()
    vec4 = c()
    for(parent in parents){
      vec0 <- c(vec0, (data[i,][[parent]] == 0))
    }
    if(sum(vec0) >= 2){ #more than two 0 values
      rows <- c(rows, i)
    }
    for(parent in parents){
      vec4 <- c(vec4, (data[i,][[parent]] == 4))
    }
    if(sum(vec4) >= 2){ #more than two 4 values
      rows <- c(rows, i)
    }
  }
  return(data[rows,])
}
