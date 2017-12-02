\name{SpringPeeperCalls}
\alias{SpringPeeperCalls}

\title{
Simulation of spring peeper calls
}
\description{
Imagine we plan to conduct a large-scale occupancy survey for spring peeper frogs using aural surveys, ie, we go to ponds and wetlands in the evening and listen for spring peeper calls. For an optimal design, we need an approximate estimate of the probability of hearing calls given the frogs are present. So we identify a number of ponds near our lab where we know the frogs are present, and we go and listen, recording the proportion of ponds where we hear frogs. 

This function displays a series of pictures of spring peepers calling or not calling (silent) with a given probability that they are calling.
}
\usage{
SpringPeeperCalls(n=10, p=0.75)

}
\arguments{
 \item{n}{
integer, the number of visits to ponds to simulate
}
  \item{p}{
the probability of hearing frogs at a specific pond
}
}
\details{
When the function is called, a plot appears with an empty rectangle. Click on the plot to begin, then on the plot repeatedly to advance to the next image. The sequence can be interrupted by right-clicking on the plot and selecting 'stop'.
}
\value{
Returns invisibly a 1/0 vector of length \code{n} where 1 indicates detection, 0 non-detection. If the sequence is interrupted, the remaining places are filled with NAs.
}

\references{
MacKenzie, D I; J D Nichols; G B Lachman; S Droege; J A Royle; C A Langtimm. 2002. Estimating site occupancy rates when detection probabilities are less than one. \emph{Ecology} 83:2248-2255.

}

\author{
Mike Meredith
}
\examples{
if(interactive()) {
res <- SpringPeeperCalls()
res
}
}
