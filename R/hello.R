# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


db_connect <-
  function(database = NULL, user_db = "shiny_mysql") {

    conf_path<-getOption("config_path")

    if(is.null(conf_path)){
      cred <- config::get(user_db)
    }else{
        cred <- config::get(user_db, file = conf_path)
        }

    return(
      RMariaDB::dbConnect(
        drv = RMariaDB::MariaDB(),
        driver = "MySQL Driver",
        username = cred$username,
        password = cred$password,
        dbname = ifelse(is.null(database),cred$database,database),
        host  = cred$host,
        port = cred$port
      )
    )

  }


db_query <-function(...,
                    params = NULL,
                    database = NULL,
                    user_db = "shiny_mysql") {
  con <- db_connect(database = database, user_db = user_db)

  query <- glue::glue_sql(..., .con = con)

  res <- RMariaDB::dbSendQuery(con, query)
  if (!is.null(params)) {
    RMariaDB::dbBind(res, params)
  }
  opt <- RMariaDB::dbFetch(res) %>% data.table::as.data.table()
  dbClearResult(res)
  dbDisconnect(con)

  return(opt)
}


db_statement <-  function(...,
                          params = NULL,
                          database = NULL,
                          user_db = "shiny_mysql",
                          increment = FALSE) {
  con <- db_connect(database = database, user_db = user_db)

  statement <- glue::glue_sql(..., .con = con)

  res <- RMariaDB::dbSendStatement(con, statement)
  if (!is.null(params)) {
    RMariaDB::dbBind(res, params)
  }
  rows <- RMariaDB::dbGetRowsAffected(res)
  RMariaDB::dbClearResult(res)

  if(increment==TRUE){
    rows<-RMariaDB::dbGetQuery("SELECT LAST_INSERT_ID();", conn = con)
  }

  RMariaDB::dbDisconnect(con)

  return(rows)
}

db_write <-  function(data, table_name,
                          database = NULL,
                          user_db = "shiny_mysql",
                          overwrite=FALSE,
                          append = FALSE
                          ) {

  con <- db_connect(database = database, user_db = user_db)

  RMariaDB::dbWriteTable(con, table_name, data, overwrite = overwrite, append = append)

  RMariaDB::dbDisconnect(con)

  return(rows)
}
