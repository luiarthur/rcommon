plotInPlot <-
function(minor.plot,coords="topright",scale=1/3,stay=F,bg.col=rgb(0,0,0,0)) {
  # stay: acts like 'add' in the 'plot' function. The default is FALSE.
  # coords = x1,y1,x2,y2
  # minor.plot is a function with no parameters that plots the smaller plot
  mar <- x1 <- x2 <- y1 <- y2 <- NULL
  rx1 <- rx2 <- ry1 <- ry2 <- NULL
  s <- par("usr")
  if (is.numeric(coords)) {
    x1 <- coords[1]; x2 <- coords[2]
    y1 <- coords[3]; y2 <- coords[4]
  } else if (coords=="topright"){
    x1 <- s[1] + (s[2]-s[1]) * (1-scale)
    x2 <- s[2] - (s[2]-s[1]) * .01
    y1 <- s[3] + (s[4]-s[3]) * (1-scale)
    y2 <- s[4]
    mar <- c(.1,.1,1,.1)
  } else if (coords=="bottomright") {
    x1 <- s[1] + (s[2]-s[1]) * (1-scale)
    x2 <- s[2] - (s[2]-s[1]) * .01
    y1 <- s[3] + (s[4]-s[3]) *.05
    y2 <- y1 + (s[4]-s[3]) * (scale)
    mar <- c(1,.1,1,.1)
  } else if (coords=="topleft") {
    x1 <- s[1] + (s[2]-s[1]) * .05
    x2 <- x1 + (s[2]-s[1]) * (scale)
    y1 <- s[3] + (s[4]-s[3]) * (1-scale)
    y2 <- s[4]
    mar <- c(.1,1,1,.1)
  }else if (coords=="bottomleft") {
    x1 <- s[1] + (s[2]-s[1]) * .05
    x2 <- x1 + (s[2]-s[1]) * (scale)
    y1 <- s[3] + (s[4]-s[3]) *.05
    y2 <- y1 + (s[4]-s[3]) * (scale)
    mar <- c(1,1,1,.1)
  }

  mfg <- par()$mfg

  opts <- par(no.readonly=T)
    par(fig = c(grconvertX(c(x1,x2),from="user",to="ndc"),
                grconvertY(c(y1,y2),from="user",to="ndc")),
        mar = mar, new = TRUE)
    minor.plot()
  par(opts)

  if (!stay) {
    row.num <- mfg[1]
    col.num <- mfg[2]
    last.row <- mfg[3]
    last.col <- mfg[4]

    if (col.num < last.col) {
      mfg[2] <- mfg[2] + 1
    } else {
      if (row.num < last.row) {
        mfg[1] <- mfg[1] + 1
      } else {
        mfg[1] <- 1
      }
      mfg[2] <- 1
    }
  }

  par(mfg=mfg)
}
