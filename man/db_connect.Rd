\name{db_connect}
\alias{db_connect}
\title{db_connect}
\usage{
db_connect(database = NULL, user_db = "shiny_mysql")
}
\arguments{
\item{database}{The name of the SQL database of the server you are connecting to. If left blank, this will connect to the default database specified in the config file.}
\item{user_db}{The name of the config profile that you are connecting to. This will default to "shiny_mysql" by default.}
}
\description{
Connects to a database specified in the \code{\link{config}} file and returns a database object.
}
\examples{
db_connect(database="tacos", user_db="nacho_man")
}
