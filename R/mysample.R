#' Sample Generator
#'
#' @param n the population size
#' @param iter the amount of samples to make
#' @param time the amount of time to wait before releasing a new sample
#'
#' @return There is no return but through plot output.
#' @export
#'
#' @examples
mysample=function(n, iter=10,time=0.5){
  for( i in 1:iter){
    #make a sample
    s=sample(1:10,n,replace=TRUE)
    # turn the sample into a factor
    sf=factor(s,levels=1:10)
    #make a barplot
    barplot(table(sf)/n,beside=TRUE,col=rainbow(10),
            main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim=c(0,0.2)
    )

    #release the table
    Sys.sleep(time)
  }
}
