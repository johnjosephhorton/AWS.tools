\name{ec2din}
\alias{ec2din}
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ec2din(filters = NULL, verbose = TRUE)
}
\arguments{
  \item{filters}{
%%     ~~Describe \code{filters} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
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
function(filters=NULL,verbose=TRUE) {
    aws.cmd <- paste("ec2-describe-instances",
                     paste("--filter",filters))
    if(verbose) {
        cat("ec2din, using this cmd:\n")
        print(aws.cmd)
    }

    system(aws.cmd,inern=TRUE)
  }
}
\keyword{utilities}
