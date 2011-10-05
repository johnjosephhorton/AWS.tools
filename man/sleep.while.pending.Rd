\name{sleep.while.pending}
\alias{sleep.while.pending}
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sleep.while.pending(reservation.id, sleep.time = 1, verbose = TRUE)
}
\arguments{
  \item{reservation.id}{
%%     ~~Describe \code{reservation.id} here~~
}
  \item{sleep.time}{
%%     ~~Describe \code{sleep.time} here~~
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
function(reservation.id,sleep.time=1,verbose=TRUE) {
    while(pending.instance(reservation.id)) {
        if(verbose) { cat(".") }
        Sys.sleep(sleep.time)
    }
    if(verbose) { cat("\n") }
  }
}
\keyword{utilities}
