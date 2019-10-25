\name{make.ishihara}
\alias{make.ishihara}
\title{Make "ishihara" object class from a PNG file with transparent background}
\usage{
make.ishihara(png, hidden=NULL)
}
\arguments{
	\item{png}{object of class \code{"array"} from \code{png::readPNG}.}
	\item{hidden}{optionally, the number that is hidden in the Ishihara plate.}
}
\description{
	Function to convert a PNG image to an object of class \code{"ishihara"}.
}
\details{
	The input object should be a PNG image with a transparent background. The user then must click within each of the circles of the Ishihara plate. \code{make.ishihara} detects the color & boundary limits of each circle.
}
\value{
	Object of class \code{"ishihara"}.
}
\references{
	References here.
}
\author{Liam Revell \email{liam.revell@umb.edu}}
\keyword{color-vision}

