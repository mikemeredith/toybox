
# Display pictures of frogs calling vs silent with probability p.

SpringPeeperCalls <- function(n=10, p=0.75) {

  # Load images
  imagePath <- system.file("extdata", package="toybox")
  springPeeper_calling <- jpeg::readJPEG(file.path(imagePath,
    "Spring_peeper_calling.jpg"), native=TRUE)
  springPeeper_silent <- jpeg::readJPEG(file.path(imagePath,
    "Spring_peeper_silent.jpg"), native=TRUE)

  locBell <- options(locatorBell = FALSE) ; on.exit(options(locBell))
  result <- rep(NA, n)
  plot(c(1,3), 1:2, type='n', main="",
      xlab="", ylab="", xaxt='n', yaxt='n')
  lim <- par()
  text(2, 1.95, "click to start", cex=3)
  tmp <- locator(1)

  for(i in 1:n) {
    plot(c(1,3), 1:2, type='n', main="Did you hear it?",
      xlab="", ylab="", xaxt='n', yaxt='n')
    Sys.sleep(0.5)
    if(runif(1) < p) {
      rasterImage(springPeeper_calling, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
      text(2, 1.95, "calling", col='white', cex=3)
      result[i] <- 1
    } else {
      rasterImage(springPeeper_silent, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
      text(2, 1.95, "silent", col='white', cex=3)
      result[i] <- 0
    }
    tmp <- locator(1)
    if(is.null(tmp))
      break
  }
  plot(c(1,3), 1:2, type='n', main="Did you hear it?",
      xlab="", ylab="", xaxt='n', yaxt='n')
  if(i == n) {
    text(2, 1.95, "finished", cex=3)
  } else {
    text(2, 1.95, "interrupted", cex=3)
  }

  return(invisible(result))
}
