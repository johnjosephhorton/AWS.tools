\name{instances.from.reservation}
\alias{instances.from.reservation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
instances.from.reservation(reservation.id, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{reservation.id}{
%%     ~~Describe \code{reservation.id} here~~
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
%% ~put references to the literature/web site here ~
}
\author{
Whit Armstrong
}
\note{
%%  ~~further notes~~
}


\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(reservation.id,verbose=TRUE) {
    ec2din(paste("ec2din","reservation-id=",reservation.id,sep=""),verbose=verbose)

    res <- system(cmd,intern=TRUE)
    instances <- res[grep("^INSTANCE",res),]
    instances.to.dataframe(instances)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
