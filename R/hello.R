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

    require(RMariaDB)
    return(
      dbConnect(
        drv = MariaDB(),
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
  require(glue)
  require(data.table)
  require(magrittr)
  con <- db_connect(database = database, user_db = user_db)

  query <- glue_sql(..., .con = con)

  res <- dbSendQuery(con, query)
  if (!is.null(params)) {
    dbBind(res, params)
  }
  opt <- dbFetch(res) %>% as.data.table()
  dbClearResult(res)
  dbDisconnect(con)

  return(opt)
}


db_statement <-  function(...,
                          params = NULL,
                          database = NULL,
                          user_db = "shiny_mysql",
                          increment = FALSE) {
  require(glue)
  require(data.table)
  con <- db_connect(database = database, user_db = user_db)

  statement <- glue_sql(..., .con = con)

  res <- dbSendStatement(con, statement)
  if (!is.null(params)) {
    dbBind(res, params)
  }
  rows <- dbGetRowsAffected(res)
  dbClearResult(res)

  if(increment==TRUE){
    rows<-dbGetQuery("SELECT LAST_INSERT_ID();", conn = con)
  }

  dbDisconnect(con)

  return(rows)
}

db_write <-  function(data, table_name,
                          database = NULL,
                          user_db = "shiny_mysql",
                          overwrite=FALSE,
                          append = FALSE
                          ) {
  require(data.table)
  con <- db_connect(database = database, user_db = user_db)

  dbWriteTable(con, table_name, data, overwrite = overwrite, append = append)

  dbDisconnect(con)

  return(rows)
}

