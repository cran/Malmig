\name{R}
\alias{R}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculates a Conditional Kinship matrix }
\description{
  The function R calculates a R conditional kinship matrix starting from a kinship matrix obtained by the applicatio of the Malecot migration model on a colum stochastic migration matrix.
}
\usage{
R(PHI, N)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{PHI}{ \code{PHI} is a square and symmetrical kinship matrix, possibly the output of the function Phi }
  \item{N}{ \code{N} is the effectiove population vector }
}
\details{
  Much more useful than the Phi matrix, the conditional kinship R matrix is the basis for further analysis by means of Mantel tests, Procrustes rotations and cluster analysis.
}
\value{
  Returns a square symmetrical matrix.
}
\references{ Jorde, L. B. 1982. The genetic structure of the Utah mormons: migration analysis. Human Biology 54(3): 583-597. }
\author{ Federico C. F. Calboli \email{f.calboli@ucl.ac.uk} }
\note{ ... }
\seealso{ \code{\link{Phi}} for the calculation of "absolute" kinship values }
\examples{
# using Swedlund data again...
data(S); data(P); data(N)
x<-Mal.eq(S,P,N)
phi<-Phi(S,P,N,x)
cond.mat<-R(phi,N)
cond.mat
}
\keyword{ array }
\keyword{ manip }
