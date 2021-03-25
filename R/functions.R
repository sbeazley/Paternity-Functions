

#' @export
#' @import dplyr
#' @importFrom magrittr "%>%"
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

#' @export
proportion <- function(parents, individual, data)
{
  table <- gtools::combinations(n = length(parents), r = 2, repeats.allowed = F, v = parents) #unique combinations of parents
  table <- cbind(table, rep(individual, dim(table)[1]) ) #creating table of parents to test

  vec <- c() #initializing vector

  for(i in 1:dim(table)[1])
  {
    vec <- cbind(vec, paternity(cbind(data[[ table[i,1] ]], data[[ table[i,3] ]], data[[ table[i,2] ]]) )) #vector of statistic values
  }

  table <- cbind(table, t(vec)) #adding statistic column
  colnames(table) <- c("Parent1", "Parent2", "Individual", "Statistic") #labelling columns

  DF <- as.data.frame(subset(table, select = c("Parent1", "Parent2", "Statistic"))) #final dataframe

  return(DF)
}
