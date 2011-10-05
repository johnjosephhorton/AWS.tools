\name{stopCluster}
\alias{stopCluster}
\alias{terminateCluster}
\title{
  Stop or terminate instances in a cluster.
}
\description{
  Stop or terminate the machine instances in a cluster object.
}
\usage{
stopCluster(cluster)
terminateCluster(cluster)
}
\arguments{
  \item{cluster}{a cluster object describing the reservationID and
    machine instances composing the cluster}
}
\value{
  a dataframe listing the termination status of the instances in the cluster.
}
\references{
  http://docs.amazonwebservices.com/AWSEC2/latest/CommandLineReference
}
\author{
Whit Armstrong
}


\seealso{
  \code{\link{startCluster}}
}
\examples{\dontrun{
library(aws.tools)
cl <- startCluster(ami="ami-a531fccc",key="my-ec2-key",instance.count=10,instance.type="t1.micro",verbose=FALSE)
stopCluster(cl)
}}
\keyword{utilities}
