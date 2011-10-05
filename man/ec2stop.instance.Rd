\name{ec2stop.instance}
\alias{ec2stop.instance}
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ec2stop.instance(instance.id)
}
\arguments{
  \item{instance.id}{
%%     ~~Describe \code{instance.id} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
http://docs.amazonwebservices.com/AWSEC2/latest/CommandLineReference
}
\author{
Whit Armstrong
}
}


\seealso{
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(instance.id) {
    cmd <- paste("ec2-stop-instances",instance.id)
    res <- system(cmd,intern=TRUE)
    res
  }
}
\keyword{utilities}
