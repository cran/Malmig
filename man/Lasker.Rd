\name{Lasker}
\alias{Lasker}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculates the Lasker kinship coeffcient }
\description{
  ``Lasker''calculates the Lasker kinship coefficient starting from isonymy data.
}
\usage{
Lasker(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ is a matrix where the N rows correspond to the surnames present in the whole population and the M columns are the subpopulations }
}
\details{
  The use of ``Lasker'' could be problematic, because different people are likely to arrange isonymy data in different ways on their computers. We decided for a matrix format for the isonymy data; the function would originally accept data in a different format and then convert it internally, but this would be a problem for people with data arranged in a different format. In the end we decided to leave all data arrangement outside the function and write a verbose explanation in the info for the dataset "surnames" so it would be clear for the user how ``Lasker'' works.
}
\value{
  Returns a square symmetric kinship matrix.
}
\references{ Lasker, G.W. 1977. A coefficient of relationship by isonymy: A method for estimating the genetic relationship between populations. Hum. Biol. 49:489-493. }
\author{ Federico C. F. Calboli and Alessio Boattini \email{alessio.boattini2@unibo.it} }
\note{
    ...
  }
\seealso{ \code{\link{Phi}} for the Malecot kinship model, \code{\link{surnames}} for an explanation on how to generate the correct input dataframe }
\examples{
data(surnames)
surnames #a made-up dateset

# you can see that the surnames are arranged as the _rows_ and the populations are the _columns_
# the use of the function ``Lasker'' just turns this data into a kinship matrix

lask.kin <- Lasker(surnames)
lask.kin
}
\keyword{ array }
\keyword{ methods }
\keyword{ manip }
