\name{ec2stop.instance}
\alias{ec2stop.instance}
\title{
  Stop selected running instances.
}
\description{
  Stop selected running instances.
  The INSTANCE parameter is an instance ID to stop.
}
\usage{
ec2stop.instance(instance.id)
}
\arguments{
  \item{instance.id}{the instance to stop}
}
\value{
  a vector describing the termination status of the instance.
}
\references{
http://docs.amazonwebservices.com/AWSEC2/latest/CommandLineReference
}
\author{
Whit Armstrong
}
\keyword{utilities}
