mybirthday <- function(n)
{

  # One problem is the size of the numbers
  # Use log to make calculation manageable
  # Below is log(P(AC))
  logpAc = lfactorial(365)-lfactorial(365-(1:n))-(1:n)*log(365)
  pAc=exp(logpAc)

  pA = 1-pAc

    # place names on the vector
    names(pA) = 1:(n-1)

    # find the differences in the probabilities
    dpA <-  diff(pA)
  names(dpA) = 1:(n-1)

  # various roundings
  pA2 <- round(pA,2)
  dpA4 <- round(dpA,4)

  # make a matrix holding (n-1) X 2 values
  mat = matrix(c(pA2[-n],dpA4), nr=n-1, nc=2, byrow=TRUE,#c
                 dimnames=list("Number in room"=1:(n-1), c("pA", "diff")))

  # Make a data frame out of the matrix
  df <- as.data.frame(mat)

  # plots

  colh = pA/max(pA) # number between 0,1

  # notice how we call the function - also rgb color
  plotrix::pie3D(x = pA,
                 col = rgb(colh,1-colh^2,0),
                 radius = 1.2,
                 labels = 1:n, #d
                   labelcex = 0.7,
                 explode = 0.6)


  barplot(height = pA,
          col = rgb(colh,1-colh^2,0),
          ylim = c(0,1),
          cex.axis = 0.8,
          las = 1 #e
  )

  title(main = "pA Vs n",
        sub = "Number in room",
        xlab = "n",
        ylab = "Prob. of A")


  colh <- dpA/max(dpA)

  barplot(height = dpA,
          col = rgb(colh,1-colh^2,0),
          ylim = c(0,max(1)), #e
            cex.axis = 0.8,
            las = 2
          )

          title(main = "dpA Vs n",
                sub = "Number in room",
                xlab = "n-1",
                ylab = "Delta Prob. of A")

          # invisible (list will be seen if caught in object)
          invisible(list(df = df, pA = pA2, dpA = dpA4))
}

obj <- mybirthday(32)

