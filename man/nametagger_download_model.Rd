% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nametagger.R
\name{nametagger_download_model}
\alias{nametagger_download_model}
\title{Download a Nametag model}
\usage{
nametagger_download_model(
  language = c("english-conll-140408", "czech-cnec-140304"),
  model_dir = tempdir()
)
}
\arguments{
\item{language}{Language model to download, 'english-conll-140408' (default) or 'czech-cnec-140304'}

\item{model_dir}{a path where the model will be downloaded to.}
}
\value{
an object of class nametagger
}
\description{
Download a Nametag model. Note that models have licence CC-BY-SA-NC. 
More details at \url{https://ufal.mff.cuni.cz/nametag/1}.
}
\examples{
\donttest{
model <- nametagger_download_model("english-conll-140408", model_dir = tempdir())
model <- nametagger_download_model("czech-cnec-140304", model_dir = tempdir())
}
}
\references{
\url{http://ufal.mff.cuni.cz/nametag/users-manual}
 \url{https://lindat.mff.cuni.cz/repository/xmlui/handle/11234/1-3118}
 \url{https://lindat.mff.cuni.cz/repository/xmlui/handle/11858/00-097C-0000-0023-7D42-8}
}
