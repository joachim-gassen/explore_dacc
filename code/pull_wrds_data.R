# --- Header -------------------------------------------------------------------
# (C) Joachim Gassen 2019, gassen@wiwi.hu-berlin.de
# See LICENSE file for details 
#
# This code pulls data from WRDS 
# ------------------------------------------------------------------------------

library(rstudioapi)
library(RPostgres)
library(DBI)

# --- Define functions ---------------------------------------------------------

save_wrds_data <- function(df, fname) {
  if(file.exists(fname)) {
    file.rename(fname,
                paste0(substr(fname, 1, nchar(fname) - 4), 
                       "_",
                       format(file.info(fname)$mtime, 
                              "%Y-%m-%d_%H_%M_%S"),
                       ".RDS"))
  }
  saveRDS(df, fname)
}

# --- Connect to WRDS ----------------------------------------------------------

# The dialog below only works if you use RStudio (> 1.1.67+)
# If you don't I am sure that you will find a way around the problem ;-)

user <- showPrompt(title = "WRDS Username", 
                   message = "Please enter your WRDS username: ",
                   default = "")
if (is.null(user)) stop("Sorry. Need access to WRDS to download data.")

wrds <- dbConnect(Postgres(),
                  host = 'wrds-pgdata.wharton.upenn.edu',
                  port = 9737,
                  user = user,
                  password = askForPassword("Please enter your WRDS password: "),
                  sslmode = 'require',
                  dbname = 'wrds')

message("Logged on to WRDS ...")

# --- Specify filters and variables --------------------------------------------

dyn_vars <- c("gvkey", "conm", "cik", "fyear", "datadate", "indfmt", "sich",
              "consol", "popsrc", "datafmt", "curcd", "curuscn", "fyr", 
              "act", "ap", "aqc", "aqs", "acqsc", "at", "ceq", "che", "cogs", 
              "csho", "dlc", "dp", "dpc", "dt", "dvpd", "exchg", "gdwl", "ib", 
              "ibc", "intan", "invt", "lct", "lt", "ni", "capx", "oancf", 
              "ivncf", "fincf", "oiadp", "pi", "ppent", "ppegt", "rectr", "sale", 
              "seq", "txt", "xint", "xsga", "costat", "mkvalt", "prcc_f",
              "recch", "invch", "apalch", "txach", "aoloch",
              "gdwlip", "spi", "wdp", "rcp")

dyn_var_str <- paste(dyn_vars, collapse = ", ")

stat_vars <- c("gvkey", "loc", "sic", "spcindcd", "ipodate", "fic")
stat_var_str <- paste(stat_vars, collapse = ", ")

cs_filter <- "consol='C' and (indfmt='INDL' or indfmt='FS') and datafmt='STD' and popsrc='D'"
ts_filter <- "fyear>1986"

rest_var <- c("res_notif_key", "company_fkey", "best_edgar_ticker", 
              "res_accounting", "res_fraud", "res_adverse", 
              "res_improves", "res_begin_date", "res_begin_date_num", "res_begin_aud_fkey", 
              "res_begin_aud_name", "res_end_date", "res_end_date_num", "res_end_aud_fkey", 
              "res_end_aud_name", "res_period_aud_fkey", "res_period_aud_names", 
              "res_aud_letter", "res_other", "res_cler_err",
              "res_sec_invest", "res_board_app", "eventdate_aud_fkey", 
              "eventdate_aud_name")

rest_var_str <- paste(rest_var, collapse = ", ")


# --- Pull US data -------------------------------------------------------------

message("Pulling dynamic compustat data ... ", appendLF = FALSE)
res <- dbSendQuery(wrds, paste(
  "select", 
  dyn_var_str, 
  "from COMP.FUNDA",
  "where", cs_filter,
  "and", ts_filter))

wrds_us_dynamic <- dbFetch(res, n=-1)
dbClearResult(res)
message("done!")

message("Pulling static static data ... ", appendLF = FALSE)
res2<-dbSendQuery(wrds, paste(
  "select ", stat_var_str, "from COMP.COMPANY"))

wrds_us_static <- dbFetch(res2,n=-1)
dbClearResult(res2)
message("done!")

wrds_us <- merge(wrds_us_static, wrds_us_dynamic, by="gvkey")
save_wrds_data(wrds_us, "data/cstat_us_sample.RDS")

message("Pulling restatement data ... ", appendLF = FALSE)
res <- dbSendQuery(wrds, paste(
  "select", 
  rest_var_str, 
  "from audit.auditnonreli"))

rest_data <- dbFetch(res, n=-1)
dbClearResult(res)
message("done!")

saveRDS(rest_data, "data/rest_data.RDS")

dbDisconnect(wrds)
message("Disconnected from WRDS")
