\name{config}
\title{Configuration}
\arguments{
\item{host}{Hostname or IP address of the database, localhost for a local database.}
\item{port}{The port for the database, usually 3306}
\item{database}{The database name you wish to default to.}
\item{username}{Your username}
\item{password}{Your password}
}
\description{
In order to run the app you need a config.yml file in your home directory, or otherwise defined by options("config_path"=path) argument. Remember not to add this file to any github repositories.
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
