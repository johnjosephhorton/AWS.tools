\name{ec2din}
\alias{ec2din}
\title{
  Describe instances.
}
\description{
  List and describe your instances
  The INSTANCE parameter is the instance ID(s) to describe.
  If unspecified all your instances will be returned.
}
\usage{
ec2din(instance = NULL, filters = NULL, verbose = FALSE)
}
\arguments{
  \item{instance}{the instanceID to describe}
  \item{filters}{filters to apply to the command}
  \item{verbose}{show the command to be executed}
}
\value{a dataframe enumerating the details of your instances}
\references{
http://docs.amazonwebservices.com/AWSEC2/latest/CommandLineReference
}
\author{
Whit Armstrong
}
\examples{\dontrun{
library(aws.tools)
cl <- startCluster(ami="ami-a531fccc",key="my-ec2-key",instance.count=10,instance.type="t1.micro",verbose=FALSE)
ec2din()
stopCluster(cl)
}}

\keyword{utilities}
