\name{ec2stop.reservation}
\alias{ec2stop.reservation}
\title{
  Stop all the instances in a reservation.
}
\description{
  Stop all the instances in a reservation.
}
\usage{
ec2stop.reservation(reservation.id)
}
\arguments{
  \item{reservation.id}{the reservation.id to be stopped.}
}
\value{
  a dataframe containing the termination status of the instances.
}
\references{
http://docs.amazonwebservices.com/AWSEC2/latest/CommandLineReference
}
\author{
Whit Armstrong
}
\keyword{utilities}
