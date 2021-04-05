#########################################################################
#
# Package: Paternity
#
# File: functions.R
# Contains: paternity
#
# Written by Samuel Beazley
#
# First version: March-2021
# Last update: 5-Apr-2021
#
#########################################################################
#' @import dplyr
#' @importFrom magrittr "%>%"
#'
#'
#' Paternity by Exclusion Test
#'
#' @param x matrix of values to test. The matrix should be 3 columns with the progeny being tested in the second column.
#' @return A vector with proportion of cases that has a mismatch a.k.a. pedigree conflict.
#'
#' @export

paternity <- function(x)
{
  y <- dplyr::as_tibble(x) #converting matrix to tibble

  colnames(y) <- c("Mom", "Progeny", "Dad") #labeling columns

  subset <- y %>% dplyr::filter((Mom == 0 & Dad == 0) | (Mom == 4 & Dad == 4))
  #subsetting when Mom and Dad are both 0 or 4

  total <- dim(subset)[1] #total number of times Mom and Dad are both 0 or 4

  mismatch <- (subset %>% filter(Mom != Progeny))
  #subsetting when Progeny is not equal to both Mom and Dad

  mcount = dim(mismatch)[1] #counting number of mismatched

  p <- (mcount / total) #proportion of mismatched

  return(p)
}
