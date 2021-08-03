#########################################################################
#
# Package:
#
# File: selection.R
# Contains: selection
#
# Written by Rodrigo Amadeu and Samuel Beazley
#
# First version: 17-June-2021
# Last update: 2-Aug-2021
#
#########################################################################
#'
#' Test parentage of individual
#'
#' The function returns a vector of rows that have the most evidence for parentage testing
#'
#' @param parents a vector with strings related to the name of the suspected parents
#' @param initial the number of initial rows to select
#' @param total the total number of rows to select
#' @param data the dataframe from which the data is from
#' @param pedigree a dataframe containing the pedigree data
#' @param method the algorithm method used to check each row (default is forward)
#'
#' @return an optimized vector of rows with the most convincing evidence of identifying the correct parent
#'
#' @examples
#' selection(parents = c("A", "B", "C", "D"),
#'           initial = 10,
#'           total = 48,
#'           data = geno_filtered1,
#'           pedigree = geno_pedigree,
#'           method = "forward")
#'
#' @export
#'

selection <- function(parents, initial, total, data, pedigree, method = "forward")
{

  rownames(geno_filtered1)=1:nrow(geno_filtered1) #fixing rownames
  select.markers = as.numeric(names(sort(apply(geno_filtered1[,c("A","B","C","D")],1,var),decreasing = TRUE))[1:initial])
  candidate.markers = c(1:nrow(geno_filtered1))[-select.markers]
  geno_filtered1_init = geno_filtered1[select.markers,]
  init = proportion(parents = c("A", "B", "C", "D"), individual = geno_pedigree[,1], data = geno_filtered1_init)
  correct.assign = rep(0,length(candidate.markers))

  rownames(data)=1:nrow(data) #fixing rownames
  select.markers = as.numeric(names(sort(apply(data[,parents],1,var),decreasing = TRUE))[1:initial])
  candidate.markers = c(1:nrow(data))[-select.markers]

  for(i in 1:(total-length(select.markers))){

    for(j in 1:length(candidate.markers)){
      print(j)
      set2try = c(select.markers,candidate.markers[j])
      init = proportion(parents = parents, individual = pedigree[,1], data = data[set2try,])

      ## Same chunk of code as before
      direct.min = init %>% group_by(Individual) %>% slice(which.min(Statistic)) %>% arrange(Individual)
      reverse.min = init[nrow(init):1,] %>% group_by(Individual) %>% slice(which.min(Statistic)) %>% arrange(Individual)
      for(i in 1:nrow(direct.min)){
        if(prod(direct.min[i,1:3] == reverse.min[i,1:3])){ #check if it is non-unique minimum
          RealParents = geno_pedigree[match(direct.min$Individual[i],geno_pedigree[,1]),2:3]
          CheckParents = match(c(direct.min$Parent1[i],direct.min$Parent2[i]),RealParents)
          if(length(which(is.na(CheckParents)))==0)
            correct.assign[j]=correct.assign[j]+1
        }
      }
    }

    select.markers = c(select.markers,candidate.markers[which.max(correct.assign)])
    candidate.markers = candidate.markers[-which.max(correct.assign)] #remove from the candidates
    correct.assign = rep(0,length(candidate.markers))
    print(select.markers)

  }

  return(select.markers)
}
