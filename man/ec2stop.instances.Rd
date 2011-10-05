\name{ec2stop.instances}
\alias{ec2stop.instances}
\alias{ec2terminate.instances}
\title{
  Stop selected running instances.
}
\description{
  Stop selected running instances.
  The INSTANCE parameter is an instance ID to stop.
}
\usage{
ec2stop.instances(instance.ids)
ec2terminate.instances(instance.ids)
}
\arguments{
  \item{instance.ids}{the instances to stop}
}
\value{
  a vector describing the termination status of the instances.
}
\references{
http://docs.amazonwebservices.com/AWSEC2/latest/CommandLineReference
}
\author{
Whit Armstrong
}
\keyword{utilities}
