#########################################################################
#
# Package: Paternity
#
# File: paternity.R
# Contains: paternity
#
# Written by Samuel Beazley and Rodrigo Amadeu
#
# First version: March-2021
# Last update: 5-Apr-2021
#
# License: GPL-3
#
#########################################################################

#' Paternity by Exclusion Test
#'
#' Performs paternity by exclusion based on a matrix three entries (trio: mom, individual, dad), returns the proportion of mismatch cases and the total cases checked.
#'
#' @param x matrix of values to test. The matrix should be 3 columns with the progeny being tested in the second column.
#'
#' @return A vector with proportion of cases that has a mismatch a.k.a. pedigree conflict.
#'
#' @examples
#' data(potato.data)
#' trio1 = potato.data[,c("W6511.1R", "W15268.5R", "VillettaRose")]
#' trio2 = potato.data[,c("W6511.1R", "W15268.5R", "W9914.1R")]
#' paternity(trio1) #more likely to be this trio
#' paternity(trio2)
#'
#' @import dplyr
#' @importFrom magrittr "%>%"
#'
#' @export

paternity <- function(x)
{
  mcount=sum((x[,1]==0)*(x[,3]==0)*(x[,2]==0))+sum((x[,1]==4)*(x[,3]==4)*(x[,2]==4))
  total=sum((x[,1]==0)*(x[,3]==0)+(x[,1]==4)*(x[,3]==4))

  return(c(1-mcount/total, total))
}
