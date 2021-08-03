#########################################################################
#
# Package: Paternity-Functions
#
# File: proportion.R
# Contains: proportion
#
# Written by Samuel Beazley and Rodrigo Amadeu
#
# First version: March-2021
# Last update: 2-Aug-2021
#
#########################################################################
#'
#' Test parentage of individual
#'
#' Given individual and a vectors of possible parents, function returns dataframe of proportion of pedigree conflict with each possible trio
#'
#' @param parents a vector with strings related to the name of the suspected parents
#' @param individual a string value with the individual name you are testing
#' @param data the dataframe from which the data is from
#'
#' @return A dataframe of different combinations of parents and individual with the proportion of pedigree conflicts in each trio
#'
#' @examples
#' data(potato.data)
#' proportion(parents = c("W6511.1R","VillettaRose","W9914.1R"),
#'            individual = "W15268.1R",
#'            data = potato.data)
#'
#' @export

proportion <- function(parents, individual, data)
{
  DF <- data.frame()

  for(indiv in individuals)
  {
    table <- gtools::combinations(n = length(parents), r = 2, repeats.allowed = F, v = parents) #unique combinations of parents
    table <- cbind(table, rep(indiv, dim(table)[1]) ) #creating table of parents to test

    vec <- c() #initializing vector
    vec2 <- c()

    for(i in 1:dim(table)[1])
    {
      tmp <- paternity(cbind(data[[ table[i,1] ]], data[[ table[i,3] ]], data[[ table[i,2] ]]))
      vec <- cbind(vec,tmp[1])
      vec2 <- cbind(vec2,tmp[2])
    }

    table <- cbind(table, t(vec2), t(vec)) #adding statistic column
    colnames(table) <- c("Parent1", "Parent2", "Individual", "N", "Statistic") #labelling columns


    DF <- rbind(DF, as.data.frame(subset(table, select = c("Parent1", "Parent2", "Individual", "N", "Statistic")))) #final dataframe

  }

  return(DF)
}
