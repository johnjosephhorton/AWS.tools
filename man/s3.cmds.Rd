\name{s3.del}
\alias{s3.del}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
s3.del(bucket, bucket.location = "US", human.readable.sizes = TRUE, list.md5 = FALSE, verbose = FALSE, debug = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bucket}{
%%     ~~Describe \code{bucket} here~~
}
  \item{bucket.location}{
%%     ~~Describe \code{bucket.location} here~~
}
  \item{human.readable.sizes}{
%%     ~~Describe \code{human.readable.sizes} here~~
}
  \item{list.md5}{
%%     ~~Describe \code{list.md5} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{debug}{
%%     ~~Describe \code{debug} here~~
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
function(bucket,bucket.location="US",human.readable.sizes=TRUE,list.md5=FALSE,verbose=FALSE,debug=FALSE) {
    check.bucket(bucket)
    s3.cmd <- paste("s3cmd del",
                    bucket,
                    paste("--bucket-location",bucket.location),
                    "--no-progress",
                    ifelse(verbose,"--verbose",""),
                    ifelse(debug,"--debug",""),
                    ifelse(human.readable.sizes,"--human-readable-sizes",""),
                    ifelse(list.md5,"--list-md5","")
                    )
    system(s3.cmd,intern=TRUE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
\name{s3.du}
\alias{s3.du}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
s3.du(bucket, bucket.location = "US", human.readable.sizes = TRUE, list.md5 = FALSE, verbose = FALSE, debug = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bucket}{
%%     ~~Describe \code{bucket} here~~
}
  \item{bucket.location}{
%%     ~~Describe \code{bucket.location} here~~
}
  \item{human.readable.sizes}{
%%     ~~Describe \code{human.readable.sizes} here~~
}
  \item{list.md5}{
%%     ~~Describe \code{list.md5} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{debug}{
%%     ~~Describe \code{debug} here~~
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
function(bucket,bucket.location="US",human.readable.sizes=TRUE,list.md5=FALSE,verbose=FALSE,debug=FALSE) {
    check.bucket(bucket)
    s3.cmd <- paste("s3cmd du",
                    bucket,
                    paste("--bucket-location",bucket.location),
                    "--no-progress",
                    ifelse(verbose,"--verbose",""),
                    ifelse(debug,"--debug",""),
                    ifelse(human.readable.sizes,"--human-readable-sizes",""),
                    ifelse(list.md5,"--list-md5","")
                    )
    system(s3.cmd,intern=TRUE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
\name{s3.get}
\alias{s3.get}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
s3.get(bucket, bucket.location = "US", verbose = FALSE, debug = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bucket}{
%%     ~~Describe \code{bucket} here~~
}
  \item{bucket.location}{
%%     ~~Describe \code{bucket.location} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{debug}{
%%     ~~Describe \code{debug} here~~
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
function(bucket,bucket.location="US",verbose=FALSE,debug=FALSE) {
    check.bucket(bucket)
    x.serialized <- tempfile()
    s3.cmd <- paste("s3cmd get",
                    bucket,
                    x.serialized,
                    paste("--bucket-location",bucket.location),
                    "--no-progress",
                    ifelse(verbose,"--verbose",""),
                    ifelse(debug,"--debug","")
                    )
    res <- system(s3.cmd,intern=TRUE)
    ans <- readRDS(x.serialized)
    unlink(x.serialized)
    ans
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
\name{s3.info}
\alias{s3.info}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
s3.info(bucket, bucket.location = "US", human.readable.sizes = TRUE, list.md5 = FALSE, verbose = FALSE, debug = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bucket}{
%%     ~~Describe \code{bucket} here~~
}
  \item{bucket.location}{
%%     ~~Describe \code{bucket.location} here~~
}
  \item{human.readable.sizes}{
%%     ~~Describe \code{human.readable.sizes} here~~
}
  \item{list.md5}{
%%     ~~Describe \code{list.md5} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{debug}{
%%     ~~Describe \code{debug} here~~
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
function(bucket,bucket.location="US",human.readable.sizes=TRUE,list.md5=FALSE,verbose=FALSE,debug=FALSE) {
    check.bucket(bucket)
    s3.cmd <- paste("s3cmd info",
                    bucket,
                    paste("--bucket-location",bucket.location),
                    "--no-progress",
                    ifelse(verbose,"--verbose",""),
                    ifelse(debug,"--debug",""),
                    ifelse(human.readable.sizes,"--human-readable-sizes",""),
                    ifelse(list.md5,"--list-md5","")
                    )
    system(s3.cmd,intern=TRUE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
\name{s3.la}
\alias{s3.la}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
s3.la(bucket.location = "US", human.readable.sizes = TRUE, list.md5 = FALSE, verbose = FALSE, debug = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bucket.location}{
%%     ~~Describe \code{bucket.location} here~~
}
  \item{human.readable.sizes}{
%%     ~~Describe \code{human.readable.sizes} here~~
}
  \item{list.md5}{
%%     ~~Describe \code{list.md5} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{debug}{
%%     ~~Describe \code{debug} here~~
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
function(bucket.location="US",human.readable.sizes=TRUE,list.md5=FALSE,verbose=FALSE,debug=FALSE) {
    s3.cmd <- paste("s3cmd la",
                    paste("--bucket-location",bucket.location),
                    "--no-progress",
                    ifelse(verbose,"--verbose",""),
                    ifelse(debug,"--debug",""),
                    ifelse(human.readable.sizes,"--human-readable-sizes",""),
                    ifelse(list.md5,"--list-md5","")
                    )
    system(s3.cmd,intern=TRUE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
\name{s3.ls}
\alias{s3.ls}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
s3.ls(bucket = NULL, bucket.location = "US", human.readable.sizes = TRUE, list.md5 = FALSE, verbose = FALSE, debug = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bucket}{
%%     ~~Describe \code{bucket} here~~
}
  \item{bucket.location}{
%%     ~~Describe \code{bucket.location} here~~
}
  \item{human.readable.sizes}{
%%     ~~Describe \code{human.readable.sizes} here~~
}
  \item{list.md5}{
%%     ~~Describe \code{list.md5} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{debug}{
%%     ~~Describe \code{debug} here~~
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
function(bucket=NULL,bucket.location="US",human.readable.sizes=TRUE,list.md5=FALSE,verbose=FALSE,debug=FALSE) {
    if(!is.null(bucket)) {
        check.bucket(bucket)
    }
    s3.cmd <- paste("s3cmd ls",
                    ifelse(bucket,verbose,""),
                    paste("--bucket-location",bucket.location),
                    "--no-progress",
                    ifelse(verbose,"--verbose",""),
                    ifelse(debug,"--debug",""),
                    ifelse(human.readable.sizes,"--human-readable-sizes",""),
                    ifelse(list.md5,"--list-md5","")
                    )
    system(s3.cmd,intern=TRUE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
\name{s3.mb}
\alias{s3.mb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
s3.mb(bucket, bucket.location = "US", verbose = FALSE, debug = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bucket}{
%%     ~~Describe \code{bucket} here~~
}
  \item{bucket.location}{
%%     ~~Describe \code{bucket.location} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{debug}{
%%     ~~Describe \code{debug} here~~
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
function(bucket,bucket.location="US",verbose=FALSE,debug=FALSE) {
    check.bucket(bucket)
    s3.cmd <- paste("s3cmd mb",
                    bucket,
                    paste("--bucket-location",bucket.location),
                    "--no-progress",
                    ifelse(verbose,"--verbose",""),
                    ifelse(debug,"--debug","")
                    )
    system(s3.cmd,intern=TRUE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
\name{s3.put}
\alias{s3.put}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
s3.put(x, bucket, bucket.location = "US", verbose = FALSE, debug = FALSE, encrypt = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{bucket}{
%%     ~~Describe \code{bucket} here~~
}
  \item{bucket.location}{
%%     ~~Describe \code{bucket.location} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{debug}{
%%     ~~Describe \code{debug} here~~
}
  \item{encrypt}{
%%     ~~Describe \code{encrypt} here~~
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
function(x,bucket,bucket.location="US",verbose=FALSE,debug=FALSE,encrypt=FALSE) {
    check.bucket(bucket)
    x.serialized <- tempfile()
    saveRDS(x,x.serialized)
    s3.cmd <- paste("s3cmd put",
                    x.serialized,
                    bucket,
                    ifelse(encrypt,"--encrypt",""),
                    paste("--bucket-location",bucket.location),
                    "--no-progress",
                    ifelse(verbose,"--verbose",""),
                    ifelse(debug,"--debug","")
                    )
    res <- system(s3.cmd,intern=TRUE)
    unlink(x.serialized)
    res
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
\name{s3.rb}
\alias{s3.rb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
s3.rb(bucket, recursive = FALSE, force = FALSE, bucket.location = "US", verbose = FALSE, debug = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bucket}{
%%     ~~Describe \code{bucket} here~~
}
  \item{recursive}{
%%     ~~Describe \code{recursive} here~~
}
  \item{force}{
%%     ~~Describe \code{force} here~~
}
  \item{bucket.location}{
%%     ~~Describe \code{bucket.location} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{debug}{
%%     ~~Describe \code{debug} here~~
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
function(bucket,recursive=FALSE,force=FALSE,bucket.location="US",verbose=FALSE,debug=FALSE) {
    check.bucket(bucket)
    s3.cmd <- paste("s3cmd rb",
                    bucket,
                    "--no-progress",
                    ifelse(verbose,"--verbose",""),
                    ifelse(debug,"--debug",""),
                    ifelse(recursive,"--recursive",""),
                    ifelse(force,"--force","")
                    )
    system(s3.cmd,intern=TRUE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
