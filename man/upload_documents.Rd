% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/index.R
\name{upload_documents}
\alias{upload_documents}
\title{Upload documents}
\usage{
upload_documents(
  index,
  documents,
  columns = NULL,
  chunk_size = 100L,
  max_tries = 5L,
  verbose = TRUE,
  credentials = NULL
)
}
\arguments{
\item{index}{The index name to create.}

\item{documents}{A data frame with columns title, text, date, and
optional other columns.}

\item{columns}{An optional list with data types, e.g. list(author =
"keyword").}

\item{chunk_size}{Uploads are broken into chunks to prevent errors.
Smaller chunks are less error-prone, but this also makes the
upload slower.}

\item{max_tries}{In case something goes wrong, how often should the
function retry to send the documents?}

\item{verbose}{Should a progress bar be printed during upload.}

\item{credentials}{The credentials to use. If not given, uses last
login information.}
}
\description{
Upload documents
}
