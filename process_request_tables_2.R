#### Connect (SSH) to vestec02.img.local and start the DB using this command:
####    cd /opt/vestlon/run; java -jar vestlon-0.0.1-SNAPSHOT.jar > /opt/vestlon/run/logs/stdout.txt &
#### Upload data to https://vestlontest.img.cas.cz/
#### "Reload configuration" is required when new values are added to the DB
#### Requests table viewer: https://tesla.img.cas.cz/shiny/ccp/Request/

library(blastula)
library(cowsay)
library(googlesheets)
library(janitor)
library(magrittr, include.only = "%<>%")
library(naniar)
library(pander)
panderOptions('table.split.table', Inf)
library(RMySQL)
library(sendmailR)
library(tidyverse)

ON_TESLA <- grepl("tesla", R.utils::System$getHostname())

if(!ON_TESLA){
  message('You\'re not connected to tesla.img.cas.cz. Connection to SQL DB is required and only possible on Tesla. This script will not work.')
} else {message('SUCCESS! Connection to tesla.img.cas.cz detected.')}


con <- dbConnect(RMySQL::MySQL(), dbname = "requests")
query <- paste(
  "SELECT d.REQUEST_ID, d.VALUE_TEXT, d.VALUE_DATA_POINT, d.VALUE_CATEGORY, ",
  "d.INCREMENT_VALUE, r.ID_REQUEST, r.ID_SUBREQUEST, r.DATE, p.DESCRIPTION",
  "FROM PARAMETER_REQUEST as p, REQUEST as r, REQUEST_DATA as d ",
  "WHERE p.PARAMETER_STABLE_ID=d.PARAMETER_STABLE_ID and d.REQUEST_ID=r.ID"
)
 # query <- paste(
 #   "SELECT d.*, ",
 #   "r.*",
 #  "FROM PARAMETER_REQUEST as p, REQUEST as r, REQUEST_DATA as d ",
 #   "WHERE p.PARAMETER_STABLE_ID=d.PARAMETER_STABLE_ID and d.REQUEST_ID=r.ID"
 # )
df_1 <- DBI::dbGetQuery(con, query)
dbDisconnect(con)

#### This loads manual modifications in the request tables that were done using https://tesla.img.cas.cz/shiny/ccp/Request/
comments <- read.csv("/home/shiny/html/ccp/Request/comments.csv", check.names = F) %>%
  dplyr::select(ID_REQUEST, ID_SUBREQUEST, Completed, `Completed Date`) %>%
  dplyr::filter(!is.na(Completed))

df_2 <- df_1 %>%
  mutate(INCREMENT_VALUE = INCREMENT_VALUE %>% 
           as.character %>% 
           dplyr::if_else(condition = is.na(.), true = NA_character_, false = paste0("CCP_REQ_011_001_00", .))
  ) %>% 
  mutate(INCREMENT_AMOUNT = INCREMENT_VALUE %>% str_replace(pattern = "011", replacement = "012")) %>% 
  mutate(VALUE_DATA_POINT = VALUE_DATA_POINT %>% as.character()) %>%  
  mutate(VALUE_TEXT = VALUE_TEXT %>% as.character()) %>% 
  mutate(VALUE_CATEGORY = VALUE_CATEGORY %>% as.character()) %>% 
  pivot_longer(cols = c(VALUE_DATA_POINT, VALUE_TEXT, VALUE_CATEGORY), names_to = "TYPE", values_to = "VALUE") %>%
  drop_na(VALUE) %>%
  dplyr::select(-TYPE) %>%
  group_by(ID_REQUEST, DESCRIPTION, INCREMENT_AMOUNT, VALUE) %>% # remove duplicates
  distinct() %>%                                                 # remove duplicates
  ungroup() %>% 
  pivot_wider(names_from = DESCRIPTION, values_from = VALUE)

df_sub_1 <- df_2 %>% dplyr::select(-c(INCREMENT_AMOUNT, INCREMENT_VALUE, "Procedure amount", "Procedure name"))

df_sub_2 <- df_2 %>% 
  dplyr::select(REQUEST_ID, ID_REQUEST, ID_SUBREQUEST, INCREMENT_AMOUNT, INCREMENT_VALUE, "Procedure amount", "Procedure name") %>%
  pivot_longer(cols = c("Procedure amount", "Procedure name"), values_to = "VALUE", names_to = "TYPE") %>% 
  drop_na(VALUE) %>%
  dplyr::select(-TYPE) %>%
  mutate(VALUE = VALUE %>% as.numeric) %>% 
  drop_na(VALUE) %>% 
  dplyr::select(-INCREMENT_VALUE) %>% 
  pivot_wider(names_from = INCREMENT_AMOUNT, values_from = VALUE)

df_sub_3 <- df_2 %>% 
  dplyr::select(REQUEST_ID, ID_REQUEST, ID_SUBREQUEST, INCREMENT_AMOUNT, INCREMENT_VALUE, "Procedure amount", "Procedure name") %>%
  pivot_longer(cols = c("Procedure amount", "Procedure name"), values_to = "VALUE", names_to = "TYPE") %>% 
  drop_na(VALUE) %>%
  dplyr::select(-TYPE) %>%
  mutate(VALUE = VALUE %>% dplyr::if_else(condition = is.na(as.numeric(.)), true = ., false = NA_character_)) %>% 
  drop_na(VALUE) %>% 
  dplyr::select(-INCREMENT_AMOUNT) %>% 
  pivot_wider(names_from = INCREMENT_VALUE, values_from = VALUE)

df <- df_sub_1 %>% 
  full_join(df_sub_2) %>% 
  full_join(df_sub_3) %>% 
  full_join(comments, by = c("ID_REQUEST", "ID_SUBREQUEST")) %>%
  dplyr::select(DATE, ID_REQUEST, ID_SUBREQUEST, `Initial Req Number`, `Requestor (PI)`,
                Requestor_Institution, `Comunication requestor`,
                Project, `CCP Responsible`, Completed.x, `Completed Date`, 
                CCP_REQ_011_001_001,	CCP_REQ_012_001_001,	CCP_REQ_011_001_002,
                CCP_REQ_012_001_002,	CCP_REQ_011_001_003,	CCP_REQ_012_001_003,
                CCP_REQ_011_001_004,	CCP_REQ_012_001_004,	CCP_REQ_011_001_005,
                CCP_REQ_012_001_005,	CCP_REQ_011_001_006,	CCP_REQ_012_001_006)
  
##### Google Spreadsheets
links_list <- list(
  biochemistry = "https://docs.google.com/spreadsheets/d/14hwO8QQNyOdVE1kFlMPs-Muq1F0lzR3fC1PcWaLmBHU/edit#gid=0",
  bioimaging = "https://docs.google.com/spreadsheets/d/1ZkeKzFxrkJ_bt2eyP5vnxnJgNW73iZClYYIA7DIDLbU/edit#gid=0",
  cardio = "https://docs.google.com/spreadsheets/d/1VkM0kGjwNiUyp0Nviq1IoplYV_KWRIsg_Al1PAyy3F8/edit#gid=0",
  hearing = "https://docs.google.com/spreadsheets/d/16RE4ZkCbubjBFLwaV3j6kSd0RIRTP9ANspsILNLPgKQ/edit#gid=0",
  histology = "https://docs.google.com/spreadsheets/d/1AOW71xbpQWO5kyFDwySyiDnMkv_VzzUuzzzMV46baYE/edit#gid=0",
  histopathology = "https://docs.google.com/spreadsheets/d/1AOW71xbpQWO5kyFDwySyiDnMkv_VzzUuzzzMV46baYE/edit#gid=0",
  immunology = "https://docs.google.com/spreadsheets/d/12_Mlp-fJYqex45tM67NshqWWnGzggBtKUxib2fyyQyg/edit#gid=0",
  # infectious = "https://docs.google.com/spreadsheets/d/1W62MWfrXs2KP5cgHAGPkeAD4GZCev9Uoib0GnIXrbvY/edit#gid=0",
  lungs = "https://docs.google.com/spreadsheets/d/1AVvtF9S20gZPmXVO1zkSu1Rnyrm6gG7dKoM85JXAn2M/edit#gid=0",
  metabolism = "https://docs.google.com/spreadsheets/d/1aSgonAurBWUIGR5-qNMz-FP3KJHOaJJkghBx2MJ9nF8/edit#gid=0",
  metabolomics = "https://docs.google.com/spreadsheets/d/1NWsuCLnbpu0Gy7n2j-eP6r59RizvN6VwKPcVanPWKng/edit#gid=0",
  neuro = "https://docs.google.com/spreadsheets/d/1yMN4rRqq4WJhjGD3hDJ2a6ySc4RcVqt29mGP-jCavmg/edit#gid=0",
  pdx = "https://docs.google.com/spreadsheets/d/1HJo3lcdQiC1eaqWwZXYrDBi6NcewLl-Xls_e6YWzn_k/edit#gid=0",
  preclinics = "https://docs.google.com/spreadsheets/d/1zC1sTTx7IUZSk5v0o_xASGmQngSLmgIH51lfiIAHD0U/edit#gid=0",
  statistics = "https://docs.google.com/spreadsheets/d/1lwT8els-aWHPk81gxr1fJMgVrPbNVg6OoX-zc1xZetc/edit#gid=0",
  vision = "https://docs.google.com/spreadsheets/d/1qKqN7X2m5jw1m45oF1aJFZbsLzmj3So5sskF73AGs9I/edit#gid=0"
)

column_names <- c(
  "CCP_REQ_NUMBER",
  "CCP_REQ_SUBNUMBER",
  "CCP_REQ_DATE",
  "CCP_REQ_001_001",
  "CCP_REQ_002_001",
  "CCP_REQ_003_001",
  "CCP_REQ_004_001",
  "CCP_REQ_005_001",
  "CCP_REQ_014_001",
  "CCP_REQ_015_001",
  "CCP_REQ_010_001",
  "CCP_REQ_009_001",
  "CCP_REQ_013_001",
  "CCP_REQ_011_001_001",
  "CCP_REQ_012_001_001",
  "CCP_REQ_011_001_002",
  "CCP_REQ_012_001_002",
  "CCP_REQ_011_001_003",
  "CCP_REQ_012_001_003",
  "CCP_REQ_011_001_004",
  "CCP_REQ_012_001_004",
  "CCP_REQ_011_001_005",
  "CCP_REQ_012_001_005",
  "CCP_REQ_011_001_006",
  "CCP_REQ_012_001_006"
)

col_dict <- list(
  CCP_REQ_NUMBER = c(column = "Unit Req number", formal = "CCP_REQ_NUMBER"),
  CCP_REQ_SUBNUMBER = c(column = "Unit subReq number", formal = "CCP_REQ_SUBNUMBER"),
  CCP_REQ_DATE = c(column = "Initial date", formal = "CCP_REQ_DATE"),
  CCP_REQ_001_001 = c(column = "Initial Req Number", formal = "CCP_REQ_001_001"),
  CCP_REQ_002_001 = c(column = "Requestor (PI)", formal = "CCP_REQ_002_001"),
  CCP_REQ_003_001 = c(column = "Requestor_Institution", formal = "CCP_REQ_003_001"),
  CCP_REQ_004_001 = c(column = "Comunication requestor", formal = "CCP_REQ_004_001"),
  CCP_REQ_005_001 = c(column = "Project", formal = "CCP_REQ_005_001"),
  CCP_REQ_014_001 = c(column = "Alias", formal = "CCP_REQ_014_001"),
  CCP_REQ_015_001 = c(column = "Quote", formal = "CCP_REQ_015_001"),
  CCP_REQ_010_001 = c(column = "CCP processing responsible", formal = "CCP_REQ_010_001"),
  CCP_REQ_009_001 = c(column = "Supposed finished date", formal = "CCP_REQ_009_001"),
  CCP_REQ_013_001 = c(column = "Completed", formal = "CCP_REQ_013_001"),
  CCP_REQ_011_001_001 = c(column = "Procedure", formal = "CCP_REQ_011_001_001"),
  CCP_REQ_012_001_001 = c(column = "Amount", formal = "CCP_REQ_012_001_001"),
  CCP_REQ_011_001_002 = c(column = "Procedure", formal = "CCP_REQ_011_001_002"),
  CCP_REQ_012_001_002 = c(column = "Amount", formal = "CCP_REQ_012_001_002"),
  CCP_REQ_011_001_003 = c(column = "Procedure", formal = "CCP_REQ_011_001_003"),
  CCP_REQ_012_001_003 = c(column = "Amount", formal = "CCP_REQ_012_001_003"),
  CCP_REQ_011_001_004 = c(column = "Procedure", formal = "CCP_REQ_011_001_004"),
  CCP_REQ_012_001_004 = c(column = "Amount", formal = "CCP_REQ_012_001_004"),
  CCP_REQ_011_001_005 = c(column = "Procedure", formal = "CCP_REQ_011_001_005"),
  CCP_REQ_012_001_005 = c(column = "Amount", formal = "CCP_REQ_012_001_005"),
  CCP_REQ_011_001_006 = c(column = "Procedure", formal = "CCP_REQ_011_001_006"),
  CCP_REQ_012_001_006 = c(column = "Amount", formal = "CCP_REQ_012_001_006")
) %>% bind_rows()

col_dict_t <- col_dict %>% 
  t() %>% 
  as.data.frame() %>% 
  purrr::set_names(column_names)

unit_head_email_df <- data.frame(
  unit = character(),
  head_email = character()
)

unit_head_email_df <- unit_head_email_df %>% add_row(unit = "biochemistry", head_email = "juraj.labaj@img.cas.cz")
# unit_head_email_df <- unit_head_email_df %>% add_row(unit = "biochemistry", head_email = "labajj@img.cas.cz")
unit_head_email_df <- unit_head_email_df %>% add_row(unit = "bioimaging", head_email = "michaela.prochazkova@img.cas.cz")
# unit_head_email_df <- unit_head_email_df %>% add_row(unit = "bioimaging", head_email = "prochazkovam@img.cas.cz")
unit_head_email_df <- unit_head_email_df %>% add_row(unit = "cardio", head_email = "zuzana.nichtova@img.cas.cz")
# unit_head_email_df <- unit_head_email_df %>% add_row(unit = "cardio", head_email = "nichtovaz@img.cas.cz")
unit_head_email_df <- unit_head_email_df %>% add_row(unit = "hearing", head_email = "jiri.lindovsky@img.cas.cz")
# unit_head_email_df <- unit_head_email_df %>% add_row(unit = "hearing", head_email = "lindovskyj@img.cas.cz")
unit_head_email_df <- unit_head_email_df %>% add_row(unit = "histology", head_email = "vendula.novosadova@img.cas.cz")
unit_head_email_df <- unit_head_email_df %>% add_row(unit = "histopatology", head_email = "sarka.suchanova@img.cas.cz")
unit_head_email_df <- unit_head_email_df %>% add_row(unit = "immunology", head_email = "jana.balounova@img.cas.cz")
# unit_head_email_df <- unit_head_email_df %>% add_row(unit = "infectious", head_email = "dominik.arbot@img.cas.cz")
unit_head_email_df <- unit_head_email_df %>% add_row(unit = "lungs", head_email = "vaclav.zatecka@img.cas.cz")
unit_head_email_df <- unit_head_email_df %>% add_row(unit = "metabolism", head_email = "david.pajuelo-reguera@img.cas.cz")
unit_head_email_df <- unit_head_email_df %>% add_row(unit = "metabolomics", head_email = "karel.chalupsky@img.cas.cz")
unit_head_email_df <- unit_head_email_df %>% add_row(unit = "neuro", head_email = "kateryna.pysanenko@img.cas.cz")
unit_head_email_df <- unit_head_email_df %>% add_row(unit = "pdx", head_email = "silvia.novais@img.cas.cz")
unit_head_email_df <- unit_head_email_df %>% add_row(unit = "preclinics", head_email = "gizela.koubkova@img.cas.cz")
unit_head_email_df <- unit_head_email_df %>% add_row(unit = "statistics", head_email = "vendula.novosadova@img.cas.cz")
unit_head_email_df <- unit_head_email_df %>% add_row(unit = "vision", head_email = "marcela.palkova@img.cas.cz")

new_req_df_list <- purrr::map(seq_along(links_list), function(i) {
  print(i)
  links_list[[i]] %>% 
    gsheet::gsheet2tbl(sheetid = 0) %>% 
    dplyr::slice(-c(1, 2)) %>% 
    dplyr::select(1:11,  13:26) %>%
    purrr::set_names(column_names) %>% 
    dplyr::mutate(CCP_REQ_DATE = CCP_REQ_DATE %>% lubridate::ymd()) %>% 
    drop_na(CCP_REQ_NUMBER) %>% 
    dplyr::mutate(unit = rep(names(links_list)[i], n()))
  }
)

new_req_df <- new_req_df_list %>% bind_rows() 

# new_req_df <- new_req_df %>% mutate(CCP_REQ_SUBNUMBER = CCP_REQ_SUBNUMBER %>% str_sub(start = 1, end = 1))

new_req_df <- new_req_df %>% mutate_at(c("CCP_REQ_012_001_001", "CCP_REQ_012_001_002", "CCP_REQ_012_001_003", "CCP_REQ_012_001_004", "CCP_REQ_012_001_005", "CCP_REQ_012_001_006"), .funs = . %>% str_replace_all(pattern = ",", replacement = "."))

new_req_df <- new_req_df %>% mutate_at(c("CCP_REQ_011_001_001", "CCP_REQ_011_001_002", "CCP_REQ_011_001_003", "CCP_REQ_011_001_004", "CCP_REQ_011_001_005", "CCP_REQ_011_001_006"), .funs = . %>% str_replace_all(pattern = "á", replacement = "<e1>"))
new_req_df <- new_req_df %>% mutate_at(c("CCP_REQ_011_001_001", "CCP_REQ_011_001_002", "CCP_REQ_011_001_003", "CCP_REQ_011_001_004", "CCP_REQ_011_001_005", "CCP_REQ_011_001_006"), .funs = . %>% str_replace_all(pattern = "ž", replacement = "<9e>"))
# new_req_df <- new_req_df %>% mutate_at(c("CCP_REQ_011_001_001", "CCP_REQ_011_001_002", "CCP_REQ_011_001_003", "CCP_REQ_011_001_004", "CCP_REQ_011_001_005", "CCP_REQ_011_001_006"), .funs = . %>% str_replace_all(pattern = "ž", replacement = "<fd>"))

new_req_df <- new_req_df %>% mutate(CCP_REQ_NUMBER = CCP_REQ_NUMBER %>% str_replace_all(pattern = "_", replacement = "-"))

new_req_df <- new_req_df %>% separate(col = "CCP_REQ_NUMBER", into = c("Unit", "Year", "Count"), sep = "-")
new_req_df <- new_req_df %>% mutate(Count = Count %>% str_sub(start = 1, end = 3))
new_req_df <- new_req_df %>% unite(Unit:Count, col = "CCP_REQ_NUMBER", sep = "-")

new_req_df <- new_req_df %>% inner_join(unit_head_email_df)

new_req_df <- new_req_df %>% 
  mutate(CCP_REQ_NUMBER = CCP_REQ_NUMBER %>% str_replace_all(pattern = "\\_", replacement = "-")) 

long_req_number_df <- new_req_df %>% 
  dplyr::filter(str_length(CCP_REQ_SUBNUMBER) > 2) %>% 
  dplyr::select(CCP_REQ_NUMBER, CCP_REQ_SUBNUMBER, unit, head_email) %>% 
  pivot_longer(cols = c("CCP_REQ_SUBNUMBER"), names_to = "column", values_to = "Unit subReq number")

missing_df_a <- new_req_df %>% 
  mutate(CCP_REQ_DATE = CCP_REQ_DATE %>% as.character) %>% 
  dplyr::filter(is.na(CCP_REQ_SUBNUMBER)) %>% 
  dplyr::select(CCP_REQ_NUMBER, CCP_REQ_SUBNUMBER, CCP_REQ_DATE, CCP_REQ_002_001, CCP_REQ_003_001, CCP_REQ_004_001, unit, head_email) %>% 
  pivot_longer(cols = c("CCP_REQ_SUBNUMBER", "CCP_REQ_DATE", "CCP_REQ_002_001", "CCP_REQ_003_001", "CCP_REQ_004_001"), names_to = "missing_data") %>%
  dplyr::filter(is.na(value)) %>% 
  left_join(col_dict, by = c("missing_data" = "formal")) %>% 
  dplyr::group_by(CCP_REQ_NUMBER, unit, head_email) %>% 
  dplyr::summarise(column = paste(unique(column), collapse = ", "))

missing_df_b <- new_req_df %>% 
  mutate(CCP_REQ_DATE = CCP_REQ_DATE %>% as.character) %>% 
  dplyr::select(CCP_REQ_NUMBER, CCP_REQ_SUBNUMBER, CCP_REQ_DATE, CCP_REQ_002_001, CCP_REQ_003_001, CCP_REQ_004_001, unit, head_email) %>% 
  pivot_longer(cols = c("CCP_REQ_DATE", "CCP_REQ_002_001", "CCP_REQ_003_001", "CCP_REQ_004_001"), names_to = "missing_data") %>%
  dplyr::filter(is.na(value)) %>% 
  left_join(col_dict, by = c("missing_data" = "formal")) %>% 
  dplyr::group_by(CCP_REQ_NUMBER, unit, head_email) %>% 
  dplyr::summarise(column = paste(unique(column), collapse = ", "), CCP_REQ_SUBNUMBER = paste(unique(CCP_REQ_SUBNUMBER), collapse = ", "))

missing_df_c <- new_req_df %>% 
  mutate(CCP_REQ_DATE = CCP_REQ_DATE %>% as.character) %>% 
  dplyr::filter(CCP_REQ_013_001 = TRUE) %>% 
  dplyr::select(CCP_REQ_NUMBER, CCP_REQ_SUBNUMBER, CCP_REQ_DATE, CCP_REQ_011_001_001, CCP_REQ_012_001_001, CCP_REQ_011_001_002, CCP_REQ_012_001_002, CCP_REQ_011_001_003, CCP_REQ_012_001_003, CCP_REQ_011_001_004, CCP_REQ_012_001_004, CCP_REQ_011_001_005, CCP_REQ_012_001_005, CCP_REQ_011_001_006, CCP_REQ_012_001_006, unit, head_email) %>% 
  # gather(key = "entry", value = "value", -CCP_REQ_NUMBER, -CCP_REQ_SUBNUMBER, -CCP_REQ_DATE, -unit, -head_email) %>% 
  pivot_longer(cols = c("CCP_REQ_011_001_001", "CCP_REQ_011_001_002", "CCP_REQ_011_001_003", "CCP_REQ_011_001_004", "CCP_REQ_011_001_005", "CCP_REQ_011_001_006"), names_to = "missing_data") %>%
  dplyr::rename(entry = value) %>%
  pivot_longer(cols = c("CCP_REQ_012_001_001", "CCP_REQ_012_001_002", "CCP_REQ_012_001_003", "CCP_REQ_012_001_004", "CCP_REQ_012_001_005", "CCP_REQ_012_001_006"), names_to = "missing_value") %>%
  # group_by(missing_data, missing_value) %>% 
  dplyr::filter(missing_data == "CCP_REQ_011_001_001" & missing_value == "CCP_REQ_012_001_001" | missing_data == "CCP_REQ_011_001_002" & missing_value == "CCP_REQ_012_001_002" | missing_data == "CCP_REQ_011_001_003" & missing_value == "CCP_REQ_012_001_003" | missing_data == "CCP_REQ_011_001_004" & missing_value == "CCP_REQ_012_001_004" | missing_data == "CCP_REQ_011_001_005" & missing_value == "CCP_REQ_012_001_005" | missing_data == "CCP_REQ_011_001_006" & missing_value == "CCP_REQ_012_001_006") %>% 
  dplyr::filter(!is.na(entry)) %>% 
  dplyr::filter(is.na(value)) %>% 
  left_join(col_dict, by = c("missing_data" = "formal")) %>% 
  dplyr::group_by(CCP_REQ_NUMBER, CCP_REQ_SUBNUMBER, unit, head_email) %>% 
  dplyr::summarise(column = paste(unique(missing_value), collapse = ", ")) # because they are all named "Procedure"

missing_df <- missing_df_a %>% full_join(missing_df_b) %>% full_join(missing_df_c)
missing_df <- missing_df %>% dplyr::rename("Unit subReq number" = CCP_REQ_SUBNUMBER)
missing_df <- missing_df %>% ungroup()
missing_df <- missing_df %>% full_join(long_req_number_df)

missing_df_list <- missing_df %>% split(f = .$unit)

if(nrow(missing_df) > 0){
  for (i in seq_along(missing_df_list)){
    to <- c(missing_df_list[[i]]$head_email[1], "vendula.novosadova@img.cas.cz", "trufenc@img.cas.cz")
    from <- "noreply@tesla.img.local"
    subject <- "Missing information on request table"
    date_time <- add_readable_time()
    
    missing_content <- mime_part(paste('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0
Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
</head>
<body><pre>', paste(pander_return(missing_df_list[[i]] %>% dplyr::select(-c(unit, head_email)), style = "multiline"), collapse = '\n'), '</pre></body>
</html>'))
    
    missing_content[["headers"]][["Content-Type"]] <- "text/html"
    
    body <- list(paste0(
      "Hello :),

The following request(s) is(are) missing information:"), 
      
      missing_content,
      
      paste0(
        "

We need them properly filled in to update the request database."), 
  
  # say("
  # Whenever a `Procedure` is filled, the `Amount` must also be filled
  # In addition, the value in columns CCP_REQ_NUMBER and CCP_REQ_001_001 must be in the format XXX-YYYY-NNN, where XXX is the three-letter CCP unit code, YYYY is the year, and NNN is a three-digit number. 
  # Furthermore, the field 'Unit subReq number' should not contain more than two letters.
  # 
  # Otherwise, this message will reache you repeatedly :)", names(animals)[runif(n = 1, min = 1, max = length(names(animals)))]),

  "
  Whenever a `Procedure` is filled, the `Amount` must also be filled
  In addition, the value in columns CCP_REQ_NUMBER and CCP_REQ_001_001 must be in the format XXX-YYYY-NNN, where XXX is the three-letter CCP unit code, YYYY is the year, and NNN is a three-digit number. 
  Furthermore, the field 'Unit subReq number' should not contain more than two letters.
  \n
  Otherwise, this message will reach you again and again :)",
  
  
  paste0("
  Kind regards,
  Carlos
  \n
  Email sent on ", date_time," from R in tesla." 
      )
  )
    
    sendmail(
      from = from, 
      to = to, 
      subject = subject, 
      msg = body, 
      control = list(smtpServer = "ironport.img.cas.cz")
    )
  }
}

started_new_req_v <- setdiff(new_req_df %>% unite(c(CCP_REQ_NUMBER, CCP_REQ_SUBNUMBER, CCP_REQ_013_001), col = "REQ_SUB", sep = "_") %>% pull(REQ_SUB),
                     df %>% unite(c(ID_REQUEST, ID_SUBREQUEST, Completed.x), col = "REQ_SUB", sep = "_") %>% pull(REQ_SUB)
                     )

started_new_req_v <- started_new_req_v[started_new_req_v %>% str_which(pattern = "FALSE")]

completed_new_req_v <- setdiff(
  new_req_df %>% 
    unite(c(CCP_REQ_NUMBER, CCP_REQ_SUBNUMBER, CCP_REQ_001_001, CCP_REQ_002_001, CCP_REQ_003_001, CCP_REQ_004_001, CCP_REQ_013_001, CCP_REQ_011_001_001, CCP_REQ_012_001_001, CCP_REQ_011_001_002, CCP_REQ_012_001_002, CCP_REQ_011_001_003, CCP_REQ_012_001_003, 
            CCP_REQ_011_001_004, CCP_REQ_012_001_004, CCP_REQ_011_001_005, CCP_REQ_012_001_005, CCP_REQ_011_001_006, CCP_REQ_012_001_006), col = "REQ_SUB", sep = "_") %>% 
    pull(REQ_SUB),
  df %>% 
    unite(c(ID_REQUEST, ID_SUBREQUEST, "Initial Req Number", "Requestor (PI)", "Requestor_Institution", "Comunication requestor", Completed.x, CCP_REQ_011_001_001, CCP_REQ_012_001_001, CCP_REQ_011_001_002, CCP_REQ_012_001_002, CCP_REQ_011_001_003, CCP_REQ_012_001_003, 
                 CCP_REQ_011_001_004, CCP_REQ_012_001_004, CCP_REQ_011_001_005, CCP_REQ_012_001_005, CCP_REQ_011_001_006, CCP_REQ_012_001_006), col = "REQ_SUB", sep = "_") %>% pull(REQ_SUB)
  )

completed_new_req_v <- completed_new_req_v[completed_new_req_v %>% str_which(pattern = "TRUE")]

# duplicated_entries_v <- df %>% unite("ID_REQUEST":"ID_SUBREQUEST", col = "entry", sep = "_") %>% pull(entry) %>% .[df %>% unite("ID_REQUEST":"ID_SUBREQUEST", col = "entry", sep = "_") %>% pull(entry) %>% duplicated() %>% which()]
# new_duplicated_entries <- completed_new_req_v %>% str_split_fixed(pattern = "_", n = 15) %>% as.data.frame() %>% unite("V1":"V2", col = "entry", sep = "_") %>% pull(entry)
# intersect(duplicated_entries_v, new_duplicated_entries)

req_df_started <- new_req_df %>%
  dplyr::filter(str_length(CCP_REQ_SUBNUMBER) <= 2) %>%
  unite(c(CCP_REQ_NUMBER, CCP_REQ_SUBNUMBER, CCP_REQ_013_001), col = "REQ_SUB", sep = "_") %>%
  dplyr::filter(REQ_SUB %in% started_new_req_v) %>%
  separate(col = REQ_SUB, into = c("CCP_REQ_NUMBER", "CCP_REQ_SUBNUMBER", "CCP_REQ_013_001"), sep = "_") %>%
  dplyr::select(names(new_req_df)) %>%
  dplyr::select("CCP_REQ_NUMBER":"CCP_REQ_013_001", "unit", "head_email")

req_df_completed <- new_req_df %>%
  drop_na(CCP_REQ_013_001) %>%
  dplyr::filter(str_length(CCP_REQ_SUBNUMBER) <= 2) %>%
  unite(c(CCP_REQ_NUMBER, CCP_REQ_SUBNUMBER, CCP_REQ_001_001, CCP_REQ_002_001, CCP_REQ_003_001, CCP_REQ_004_001, CCP_REQ_013_001, CCP_REQ_011_001_001, CCP_REQ_012_001_001, CCP_REQ_011_001_002, CCP_REQ_012_001_002, CCP_REQ_011_001_003, CCP_REQ_012_001_003, 
          CCP_REQ_011_001_004, CCP_REQ_012_001_004, CCP_REQ_011_001_005, CCP_REQ_012_001_005, CCP_REQ_011_001_006, CCP_REQ_012_001_006), col = "REQ_SUB", sep = "_") %>%
  dplyr::filter(REQ_SUB %in% completed_new_req_v) %>%
  separate(col = REQ_SUB, into = c("CCP_REQ_NUMBER", "CCP_REQ_SUBNUMBER", "CCP_REQ_001_001", "CCP_REQ_002_001", "CCP_REQ_003_001", "CCP_REQ_004_001", "CCP_REQ_013_001", "CCP_REQ_011_001_001", "CCP_REQ_012_001_001", "CCP_REQ_011_001_002", "CCP_REQ_012_001_002", "CCP_REQ_011_001_003", "CCP_REQ_012_001_003", 
                                   "CCP_REQ_011_001_004", "CCP_REQ_012_001_004", "CCP_REQ_011_001_005", "CCP_REQ_012_001_005", "CCP_REQ_011_001_006", "CCP_REQ_012_001_006"), sep = "_") %>%
  dplyr::select(names(new_req_df)) %>%
  dplyr::select("CCP_REQ_NUMBER":"CCP_REQ_012_001_006", "unit", "head_email") %>%
  # dplyr::filter(CCP_REQ_NUMBER %>% str_detect(pattern = "2022", negate = TRUE)) %>% 
  replace_with_na_all(condition = ~.x %in% c("NA")) 

req_df_list <- list(
  started = req_df_started,
  completed = req_df_completed
)

req_df_list <- req_df_list %>% 
  purrr::map(dplyr::filter, !is.na(CCP_REQ_DATE))

req_df_list <- req_df_list %>% 
  purrr::map(mutate, CCP_REQ_DATE = CCP_REQ_DATE %>% as.character())

req_df_list <- req_df_list %>% 
  purrr::map(function(x) x %>% drop_na(CCP_REQ_SUBNUMBER) %>% 
               drop_na(CCP_REQ_002_001) %>% 
               drop_na(CCP_REQ_003_001) %>% 
               drop_na(CCP_REQ_004_001))
  
req_df_list <- req_df_list %>% 
  purrr::map(function(x) x %>% dplyr::filter(CCP_REQ_SUBNUMBER != "") %>% 
  dplyr::filter(CCP_REQ_002_001 != "") %>% 
  dplyr::filter(CCP_REQ_003_001 != "") %>% 
  dplyr::filter(CCP_REQ_004_001 != "") %>% 
  dplyr::filter(CCP_REQ_DATE != ""))

req_df_list <- req_df_list %>% 
  purrr::map(dplyr::select, -c(unit, head_email))

headers_started <- col_dict_t %>% dplyr::select(1:13)
headers_completed <- col_dict_t
  
req_df_list$started <- headers_started %>% bind_rows(req_df_list$started)
req_df_list$completed <- headers_started %>% bind_rows(req_df_list$completed)
req_df_list$completed[1, ] <- headers_completed[1, ]
req_df_list$completed[2, ] <- headers_completed[2, ]
req_df_list$started <- req_df_list$started[, 1:13]

req_df_list$completed <- req_df_list$completed %>% 
  mutate_at(.vars = colnames(.) %>% str_which(pattern = "CCP_REQ_012_001*"), .funs = . %>% str_replace(pattern = ",", replacement = "."))

req_df_list <- req_df_list %>% purrr::map(mutate_all, tidyr::replace_na, replace = "")

req_df_list <- req_df_list %>% purrr::map(function(x) x %>% purrr::set_names(c("0", "CCP_REQ_001", rep(" ", ncol(.)-2))))

purrr::map(seq_along(req_df_list), function(i) {
  req_df_list[[i]] %>%
    xlsx::write.xlsx(file = paste0("/home/trufenc/PRIMUS/data/83_BIOINFORMATICS/Kadu/new_ccp_request_files/request_tables_", names(req_df_list)[i], "_entries_", lubridate::now(), ".xlsx"), sheetName = "Sheet1",
                     col.names = TRUE, row.names = FALSE, append = FALSE)
}
)

# purrr::map(seq_along(req_df_list), function(i) {
#   req_df_list[[i]] %>%
#     xlsx::write.xlsx(file = paste0("/home/trufenc/PRIMUS/data/83_BIOINFORMATICS/Kadu/request_tables_", names(req_df_list)[i], "_entries_1_", lubridate::today(), ".xlsx"), sheetName = "Sheet1",
#                      col.names = TRUE, row.names = FALSE, append = FALSE)
# }
# )

# purrr::map(seq_along(req_df_list), function(i) {
#   req_df_list[[i]] %>%
#     xlsx::write.xlsx(file = paste0("/home/trufenc/PRIMUS/data/83_BIOINFORMATICS/Kadu/request_tables_", names(req_df_list)[i], "_entries_2_", lubridate::today(), ".xlsx"), sheetName = "Sheet1",
#                      col.names = TRUE, row.names = FALSE, append = FALSE)
# }
# )
# 
# purrr::map(seq_along(req_df_list), function(i) {
#   req_df_list[[i]] %>%
#     xlsx::write.xlsx(file = paste0("/home/trufenc/PRIMUS/data/83_BIOINFORMATICS/Kadu/request_tables_", names(req_df_list)[i], "_entries_3_", lubridate::today(), ".xlsx"), sheetName = "Sheet1",
#                      col.names = TRUE, row.names = FALSE, append = FALSE)
# }
# )

# library(cronR)
# cmd <- cron_rscript("/home/trufenc/PRIMUS/data/83_BIOINFORMATICS/Kadu/process_request_table/process_request_tables_2.R")
# cron_add(command = cmd,
#          frequency = "daily",
#          # at = "7AM",
#          at = c("8AM", "12PM", "6PM"),
#          days_of_week = c(1, 2, 3, 4, 5),
#          id = "process_request_tables")
