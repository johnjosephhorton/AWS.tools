\name{startCluster}
\alias{startCluster}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
startCluster(ami, key, instance.count, instance.type, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ami}{
%%     ~~Describe \code{ami} here~~
}
  \item{key}{
%%     ~~Describe \code{key} here~~
}
  \item{instance.count}{
%%     ~~Describe \code{instance.count} here~~
}
  \item{instance.type}{
%%     ~~Describe \code{instance.type} here~~
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(ami,key,instance.count,instance.type,verbose=TRUE) {
    cmd <- paste("ec2-run-instances",
                 ami,
                 "--show-empty-fields",
                 "--key",key,
                 "--instance-count",instance.count,
                 "--instance-type",instance.type)

    if(verbose) {
        cat("using this cmd:\n")
        print(cmd)
    }
    res <- system(cmd,intern=TRUE)
    reservation <- strsplit(res[[1]],split="\t")[[1]][-1]
    sleep.while.pending(reservation[1])
    instances <- instances.from.reservation(reservation[1])
    ans <- list(reservation=reservation,instances=instances)
    class(ans) <- "ec2.cluster"
    ans
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
