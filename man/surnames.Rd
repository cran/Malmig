\name{surnames}
\alias{surnames}
\docType{data}
\title{ Isonymy matrix data }
\description{
  An isonymy matrix raw dataset, with three populations and ten surnames.
}
\usage{data(surnames)}
\format{
  A 10 rows by 3 columns dataset 
}
\details{
  NB: we found that the best spreadsheet handling surnames containing spaces (e.g. "de Iorio") is GNUMERIC, because it has a text export feature that allows to put  brakets ("") to the left and right of every cell content, so that composite surnames are read as a string. The resulting text file is easily imported by read.table()
}
\source{
  Alessio Boattini. Dummy dataset generated for testing and example purposes
}
\examples{
data(surnames)
# NB. How did we produce the "surnames" dataset?
# the original data [note, the following is a crop] was arranged as:

#Population	Surname
#	Uno	alfa
#	Uno	beta
#	Uno	alfa
#	Uno	gamma
#	Uno	delta
#	Uno	alfa
#	Uno	delta
#	Uno	alfa
#	Uno	alfa
#	Due	chi
#	Due	chi
#	Due	chi
#	Due	chi
#	Due	chi
#	Due	teta
#	Due	beta
#	Due	chi
#	Due	gamma
#	Due	teta
#	Due	teta
#	Tre	lambda
#	Tre	alfa
#	Tre	chi
#	Tre	chi
#	Tre	alfa
#	Tre	alfa
#	Tre	alfa
#       ..........

# This arrangement does not necessarily reflect the way other people would arrange their data;
# so it had to be transformed by t(table(data)) to get the "surnames" dataset in its present and "correct" form
}
\keyword{datasets}
