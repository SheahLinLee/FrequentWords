\name{FrequentWords}
\alias{FrequentWords}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FrequentWords
}
\description{
FrequentWords is a package to find the most frequently occuring string ('Pattern') of length 'k' in a character vector 'Text'. It is developed for use in genomics to identify the most frequent occuring k-mer(s) in 'Text'. 
}
\usage{
FrequentWords(Text, k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Text}{A character vector containing one string}
  \item{k}{An integer for the length of 'Pattern' to be found (the k in k-mers)}
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
text <- c("ACGTGTACGTGT")
k <- 12

FrequentWords(Text=text, k=k)
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
