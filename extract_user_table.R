Quser <- paste("SELECT [id]
               ,[first_name]
               ,[last_name]
               ,[email]
               ,[username]
               ,[status]
               ,[date_last_login]
               FROM [synergygdw].[dbo].[crms_user]" , sep = '')
#fname <- paste(paste('COMM', client, year, sep = '_'),'.csv', sep = '')
DUser <- dplyr::tbl_df(RODBC::sqlQuery(dbhandle, Quser))

