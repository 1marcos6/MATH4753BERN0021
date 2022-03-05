#' Title
#'
#' @param df is the frame of data to be passed to the function
#' @param SPECIES is the subset of the data, the SPECIES, to analyze
#'
#' @return
#' @export
#'
#' @examples
#' myddt(read.csv("/Users/marcosbernier/Dev/stats/MATH4753BERN0021/projects/project1/DDT.csv"),"CCATFISH")
myddt <- function(df,SPECIES)
{
  library(ggplot2) ## Import ggplot for plots

  library(dplyr) ## Import dpylr for other functions

  cond = (df$SPECIES == SPECIES) ## Create true or false condition evaulating input string SPECIES to SPECIES of dataset
  newf <- df %>% filter({{cond}})  ## Create new data frame containing only datapoints of SPECIES SPECIES, supplied by function arg

  g = ggplot(newf, aes(x = LENGTH, y = WEIGHT)) + geom_point(aes(col = RIVER)) +
    geom_smooth(formula = y~x +I(x^2), method = "lm") + ggtitle("Marcos Bernier Project 1")   ## Create dot plot with quad curve of subsetted data. Datapoints are color coded by river.
  print(g) ## Print plot

  write.csv(newf,paste("LvsWfor",paste(SPECIES,".csv",sep=""),sep="")) ## Write subsetted data to csv file

  listof3 = list(df,newf,(table(df$RIVER)/length(df$RIVER))) ## Create list of 3 requested tables

  names(listof3) = c("Complete dataframe", "Subsetted dataframe", "Relative freq of RIVERS before subset")  ## Name previously added list elements accordingly

  print(listof3) ## Print list of 3.
}
