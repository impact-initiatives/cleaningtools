% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_cleaning_template.R
\name{create_cleaning_template}
\alias{create_cleaning_template}
\title{Create a project folder with a cleaning template}
\usage{
create_cleaning_template(folder_path, write_template_files, ...)
}
\arguments{
\item{folder_path}{Path for the new folder}

\item{write_template_files}{TRUE/FALSE if to write template files}

\item{...}{Extra information collected from the RStudio wizard}
}
\value{
Folder with a copy of the cleaning_template_folder
}
\description{
Create a project folder with a cleaning template
}
\examples{
\dontrun{
create_cleaning_template("path/to/folder")
}
}
