\name{startCluster}
\alias{startCluster}
\title{
  Start an EC2 cluster.
}
\description{
  Start a group of instances which serve as a compute cluster.
}
\usage{
startCluster(ami, key, instance.count, instance.type, verbose = FALSE)
}
\arguments{
  \item{ami}{machine-id}
  \item{key}{securtiy key}
  \item{instance.count}{number of instances requested}
  \item{instance.type}{type of instance requested}
  \item{verbose}{print verbose output}
}
\value{
  a cluster object encompassing the reservationID and instanceID's
}
\references{
  http://docs.amazonwebservices.com/AWSEC2/latest/CommandLineReference
}
\author{
  Whit Armstrong
}
\examples{\dontrun{
library(aws.tools)
cl <- startCluster(ami="ami-a531fccc",key="my-ec2-key",instance.count=10,instance.type="t1.micro",verbose=FALSE)
stopCluster(cl)
}}
\keyword{utilities}
