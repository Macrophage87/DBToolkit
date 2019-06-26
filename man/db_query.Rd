\name{db_query}
\title{Database Query}
\arguments{
\item{host}{Hostname or IP address of the database, localhost for a local database.}
\item{port}{The port for the database, usually 3306}

}
\description{
In order to run the app you need a config.yml file in your home directory, or otherwise defined by \code{options("config_path"=path)} argument. This connects to the
}
\examples{
default:
  shiny_mysql:
    host: database.example.com
    port: 3306
    database: example
    username: shiny
    password: 8cL*9UfSzQ^scP
}
