

create_ard_	R Documentation
Create ARD and relative objects
Description
Create ARD and relative objects

Usage
create_ard_freq(
  indata,
  subj_indata,
  denom_indata = NULL,
  subj_by_var = "usubjid",
  grp_var = "",
  subj_grp_id = "",
  subj_grp_var = "",
  denom_grp_var = "",
  method = "default",
  operation = "n_percent",
  idkey = "",
  trakdata = "TrakData.csv"
)

create_ard_summary(
  indata,
  subj_indata,
  analysis_var,
  subj_by_var = "usubjid",
  grp_var = "",
  subj_grp_id = "",
  subj_grp_var = "",
  operation = "default",
  idkey = "",
  trakdata = "TrakData.csv"
)
Arguments
indata	
the main input data for ARD, e.g. filtered adlb

subj_indata	
a subject level data to calculate big N, and also denominator if denom_indata is not given, e.g. filtered adsl

denom_indata	
a data to calculate denominator, e.g. adlb with non-missing post-baseline value

subj_by_var	
one character string storing by variable(s) to join indata and subj_indata

grp_var	
one character string storing group variable(s) from indata. Different variables must be split by a space.

subj_grp_id	
one character string storing "grp_id" from util/metadata/subj_grp_def.xlsx. Different IDs must be split by a space.

subj_grp_var	
one character string storing group variable(s) from subj_indata. Different variables must be split by a space.

denom_grp_var	
one character string storing group variable(s) from denom_indata. Different variables must be split by a space.

method	
available only for create_ard_freq(). one character string specifying calculate method(s). Different methods must be split by a space.

default count frequency by hierarchical levels.

shift count frequency for shift table. Total is created for all groups. "#Missing" is created in the last two groups for USUBJID in subj_indata but not in indata by previous groups.

max#var var is a group variable in indata. Example value: max#aesev. Only the max value of varn (if exists) or var (if varn not exist) within other group variables is included for frequency count.

operation	
available only for create_ard_freq(). one character string specifying calculate operation(s). Different operations must be split by a space.

n_percent count of patients and percentage

n_demon_percent count of patients and percentage, combined with denominator

n count of patients

event count of data records

EAIR EAIR

idkey	
column idkey in PDT

trakdata	
trakdata file name

analysis_var	
one character string storing variable(s) to calculate descriptive statistics. Different variables must be split by a space.

Details
Four steps to generate RTF output by using the function.

Prepare input data frame. It may include to filter data, simply manipulate, rename variable and combine/merge datasets.

Call create_ard_* function.

Post processing the ARD data frame generated in step 2, for RTF generation. It may include to filter the rows needed, transpose data once or twice, simply manipulate, arrange record order, select variables needed and adjust variable sequence.

Generate RTF via package reporter. See details in TFL Generation | R Trainings.

Value
Each function returns a list containing

ard ARD data frame. See the data structure in util/documentation/ard_specification.xlsx.

subj_N a data frame storing big N for table column head in RTF

shell the base shell for RTF creation

title title for RTF creation

footnote footnotes for RTF creation

Functions
create_ard_freq(): For frequency relative calculation, including patient count, percentage, record count, EAIR.

create_ard_summary(): For descriptive statistics calculation, such as mean, SD.

Examples
see example programs in pgm/saf/example_t_*.R



format_ard_mix	R Documentation
Format ARD data to fit mix table
Description
Format ARD data to fit mix table

Usage
format_ard_mix(
  indata,
  main_grp_var = "grp1",
  idkey = "",
  trakdata = "TrakData.csv"
)
Arguments
indata	
a list of one or multiple objects returned from create_ard_*

main_grp_var	
the main group variable to display before each group

idkey	
column idkey in PDT

trakdata	
trakdata file name

Value
Each function returns a list containing

ard ARD data frame. See the data structure in util/documentation/ard_specification.xlsx.

subj_N a data frame storing big N for table column head in RTF

shell the base shell for RTF creation

title title for RTF creation

footnote footnotes for RTF creation

Examples
see example programs in pgm/saf/example_t_mix.R




















# Information Head -------------------------------------------------------------
#            FILENAME : funcs_ars.R
#   PROGRAM DEVELOPER : Carlos Pang (pangcha)
#                DATE : 2025-12-18
#    PROJECT/STUDY/RA : CSRTRAIN/CSRTRAINCN/csr_20
#         DESCRIPTION : Create Objects as per CDISC ARS.
#                       See usage in util/documentation/create_ard_help.html.
#            PLATFORM : R 4.3.1
#               NOTES :
#  PROGRAMMING MODIFICATIONS HISTORY
#  DATE        PROGRAMMER           DESCRIPTION
#  __________  ___________________  _________________________________
#
#_______________________________________________________________________________




#' Create ARD and relative objects
#'
#' @param indata the main input data for ARD, e.g. filtered adlb
#' @param subj_indata a subject level data to calculate big N, and also denominator if `denom_indata` is not given, e.g. filtered adsl
#' @param denom_indata a data to calculate denominator, e.g. adlb with non-missing post-baseline value
#' @param analysis_var one character string storing variable(s) to calculate descriptive statistics. Different variables must be split by a space.
#' @param subj_by_var one character string storing by variable(s) to join indata and subj_indata
#' @param grp_var one character string storing group variable(s) from `indata`. Different variables must be split by a space.
#' @param subj_grp_var one character string storing group variable(s) from `subj_indata`. Different variables must be split by a space.
#' @param subj_grp_id one character string storing "grp_id" from util/metadata/subj_grp_def.xlsx. Different IDs must be split by a space.
#' @param denom_grp_var one character string storing group variable(s) from `denom_indata`. Different variables must be split by a space.
#' @param method available only for `create_ard_freq()`. one character string specifying calculate method(s).
#' Different methods must be split by a space.
#' -   `default` count frequency by hierarchical levels.
#' -   `shift` count frequency for shift table. Total is created for all groups.
#'             When both of last two groups' factor levels contain "#Missing" level,
#'             "#Missing" will also include `usubjid` in `subj_indata` but not in `indata`.
#' -   `max#var` *var* is a group variable in `indata`. Example value: `max#aesev`. Only the max value of
#'               *varn* (if exists) or *var* (if *varn* not exist) within other group variables is included for frequency count.
#' @param operation one character string specifying calculate operation(s). Different operations must be split by a space.
#'
#' Operation acceptable for `create_ard_freq()`.
#' -   `n_percent` count of patients and percentage
#' -   `n_demon_percent` count of patients and percentage, combined with denominator
#' -   `n` count of patients
#' -   `event` count of data records
#' -   `EAIR` EAIR
#'
#' Operation acceptable for `create_ard_summary()`.
#' -   `default` calculate n, mean, SD, median, Q1, Q3, min, max
#' -   `sum` calculate sum
#' @param idkey column `idkey` in PDT
#' @param trakdata trakdata file name
#'
#' @return Each function returns a list containing
#' -   `ard` ARD data frame. See the data structure in util/documentation/ard_specification.xlsx.
#' -   `subj_N` a data frame storing big N for table column head in RTF
#' -   `shell` the base shell for RTF creation
#' -   `title` title for RTF creation
#' -   `footnote` footnotes for RTF creation
#'
#' @details
#' Four steps to generate RTF output by using the function.
#' 1.  Prepare input data frame. It may include to filter data, simply manipulate, rename variable and combine/merge datasets.
#' 2.  Call create_ard_* function.
#' 3.  Post processing the ARD data frame generated in step 2, for RTF generation.
#' It may include to filter the rows needed, transpose data once or twice, simply manipulate,
#' arrange record order, select variables needed and adjust variable sequence.
#' 4.  Generate RTF via package `reporter`. See details in [TFL Generation | R Trainings](https://posit-connect-proddit.eu.novartis.net/content/3484/tfl-generation.html#reporter-generate-rtf).
#'
#' Functions imported from funcs_general.R are `round2()`, `format2()`.
#'
#' @examples see example programs in pgm/saf/example_t_*.R
#' @name create_ard_
NULL





#' Check whether variables from `grp_var` all exist in `vars`.
#' Return an error containing `indata_name` if any miss.
#' Return vectorized `grp_var` if all exist.
#'
#' @param grp_var a character string storing grouping variables
#' @param vars a character vector storing variables in `indata_name`
#' @param indata_name a character string storing indata parameter name or file name
#'
#' @return vectorized `grp_var`
#' @import tidyverse
check_grp_var <- function(grp_var,vars,indata_name) {
  grp_var_name <- deparse(substitute(grp_var))
  if (!is.character(grp_var)) stop(sprintf("%s must be a character of variables.",grp_var_name))
  grp_var <- str_trim(grp_var)
  if (grp_var=="") return(character(0))
  grp_var_vec <- str_split_1(grp_var,pattern='\\s+')
  var_not_exist <- base::setdiff(grp_var_vec,vars)
  if (length(var_not_exist)>0) stop(sprintf("%s do not exist in %s.",
                                            str_c(var_not_exist,collapse=', '),
                                            indata_name))
  return(grp_var_vec)
}


#' Rename `var_vec` in `indata` to `prefix`"grp"*n*
#'
#' @param indata a data frame
#' @param var_vec a character vector storing variables to be renamed
#' @param start an integer storing the starting of *n* in `prefix`"grp"*n*
#' @param prefix an character string storing the prefix in `prefix`"grp"*n*
#'
#' @return renamed data frame
#' @import tidyverse
rename_grp_var <- function(indata,var_vec,start=1,prefix='') {
  if (length(var_vec)==0) return(indata)
  var_name <- c(var_vec,paste0(var_vec,'n'))
  var_name2 <- intersect(var_name,names(indata))
  grp_seq <- seq(start,length.out=length(var_vec))
  grp_name <- c(paste0(prefix,'grp',grp_seq),paste0(prefix,'grp',grp_seq,'n'))
  grp_name2 <- grp_name[var_name %in% var_name2]

  indata |> rename_with(~grp_name2,all_of(var_name2)) |>
    mutate(!!!setNames(var_vec,paste0(prefix,'grp',grp_seq,'_var')))
}



#' Shape input data frame
#'
#' @param indata parameter `indata` from `create_ard_*`
#' @param subj_indata parameter `subj_indata` from `create_ard_*`
#' @param grp_var parameter `grp_var` from `create_ard_*`
#' @param subj_grp_var parameter `subj_grp_var` from `create_ard_*`
#' @param subj_grp_id parameter `subj_grp_id` from `create_ard_*`
#' @param analysis_var parameter `analysis_var` from `create_ard_*`
#'
#' @return a list of different objects for `create_ard_*` to use
#' @import tidyverse
shape_input <- function(indata,subj_indata,denom_indata,subj_by_var,grp_var,subj_grp_id,subj_grp_var,denom_grp_var,analysis_var) {
  #check input parameter ---------------------
  if (!is.data.frame(indata)) stop("indata must be a data frame.")
  if (!is.data.frame(subj_indata)) stop("subj_indata must be a data frame.")

  grp_var_vec <- check_grp_var(grp_var,names(indata),'indata')
  analysis_var_vec <- check_grp_var(analysis_var,names(indata),'indata')
  subj_by_var_vec <- check_grp_var(subj_by_var,names(subj_indata),'subj_indata')
  subj_grp_var_vec <- check_grp_var(subj_grp_var,names(subj_indata),'subj_indata')

  if (!is.null(denom_indata)) {
    if (!is.data.frame(denom_indata)) stop("denom_indata must be a data frame.")
    denom_grp_var_vec <- check_grp_var(denom_grp_var,names(denom_indata),'denom_indata')
  } else {
    denom_grp_var_vec <- NULL
  }

  #shape subj_indata ---------------------
  grp_def <- readxl::read_excel(paste0(st$rprtdsm,'subj_grp_def.xlsx'),skip=1) |>
    mutate(across(where(is.character),~if_else(trimws(.)=="",NA_character_,.)))
  grp_def_var <- grp_def |> distinct(indata_var) |> filter(!is.na(indata_var)) |> pull()
  subj_grp_id_vec <- check_grp_var(subj_grp_id,grp_def$grp_id,'subj_grp_def.xlsx')

  subj_indata_sel2 <- subj_indata |>
    select(usubjid,any_of(c(grp_def_var,paste0(subj_grp_var_vec,'n'),subj_grp_var_vec,subj_by_var_vec,'trtsdt','censor_dt'))) |>
    rename_grp_var(subj_grp_var_vec,start=length(subj_grp_id_vec)+1,prefix='subj_')
  subj_indata_loop1 <- subj_indata_sel2

  # merge info from subj_grp_def.xlsx
  dum_subj_datas1 <- list()
  for (seq_num in 1:length(subj_grp_id_vec)) {
    grp_id_1 <- subj_grp_id_vec[seq_num]
    grp_vec <- filter(grp_def,grp_id==grp_id_1)$grp |> str_split_1("\\|")
    grp_vec_miss <- grp_vec[!(grp_vec %in% grp_def$grp)]
    if (length(grp_vec_miss)>0) stop(sprintf("grp %s is not defined in subj_grp_def.xlsx.",str_c(grp_vec_miss,collapse=', ')))
    indata_var_1 <- filter(grp_def,grp_id==grp_id_1)$indata_var
    grp_def1 <- grp_def |> filter(grp %in% grp_vec,is.na(grp_id))
    if (seq_num==1) grp_def1 <- grp_def1 |> mutate(grp=paste(grp,grp_label,sep="#"))
    if (!is.na(indata_var_1)) grp_def1 <- grp_def1 |>
      mutate(indata_filter=str_replace_all(indata_filter,"\\bindata_var\\b",indata_var_1))

    grp_varname <- paste0('subj_grp',seq_num)
    grpn_varname <- paste0('subj_grp',seq_num,'n')
    subj_indata_loop2 <- grp_def1 |> select(grp,grpn,indata_filter) |>
      pmap(~ subj_indata_loop1 |> filter( !!rlang::parse_expr(..3) ) |>
             mutate(!!grpn_varname:=..2,
                    !!grp_varname:=..1) ) |>
      bind_rows() |>
      mutate(!!sym(sprintf("subj_grp%s_id",seq_num)):=grp_id_1)
    subj_indata_loop1 <- subj_indata_loop2

    grp_def2 <- grp_def1 |> select(grpn,grp) |> rename(!!grpn_varname:=grpn,!!grp_varname:=grp) |>
      mutate(!!sym(sprintf("subj_grp%s_id",seq_num)):=grp_id_1)
    dum_subj_datas1[length(dum_subj_datas1)+1] <- list(grp_def2)
  }

  #arrange variable sequence
  grp_var_all <- c(as.vector(rbind(paste0("grp",1:9,"_var"),paste0("grp",1:9,"n"),paste0("grp",1:9))),
                   as.vector(rbind(paste0("subj_grp",1:9,"_id"),paste0("subj_grp",1:9,"_var"),
                                   paste0("subj_grp",1:9,"n"),paste0("subj_grp",1:9))))

  subj_indata_sel4 <-  subj_indata_loop1 |> select(any_of(c(grp_var_all,subj_by_var_vec,'trtsdt','censor_dt')))

  #create dummy data frame to cover all values of subj_grpx
  dum_subj_datas2 <- (length(subj_grp_id_vec)+(1:length(subj_grp_var_vec))) |>
     map(~subj_indata_sel2 |>
          distinct(pick(any_of(c(paste0("subj_grp",.x,"_id"),
                                 paste0("subj_grp",.x,"_var"),
                                 paste0("subj_grp",.x,"n"),
                                 paste0("subj_grp",.x))))))

  dum_subj_datas4 <- c(dum_subj_datas1,dum_subj_datas2) |> reduce(dplyr::cross_join) |>
    mutate(operation='bigN',operationn=1)

  #create subj_N
  subj_N2 <- subj_indata_sel4 |> count(across(-any_of(c('usubjid','trtsdt','censor_dt'))),name="val") |>
    dplyr::right_join(dum_subj_datas4) |>
    mutate(val=if_else(is.na(val),0,val),
           valc=sprintf("%s\nN=%s",str_split_i(subj_grp1,'#',2),format2(val,0)),
           subj_grp1=str_split_i(subj_grp1,'#',1))

  dum_subj_data <- dum_subj_datas4 |> mutate(subj_grp1=str_split_i(subj_grp1,'#',1))

  subj_indata_sel <- subj_indata_sel4 |> mutate(subj_grp1=str_split_i(subj_grp1,'#',1))

  subj_N <- subj_N2 |> pivot_wider(id_cols=-all_of(c('subj_grp1n','val')),names_from=subj_grp1,values_from=valc) |>
    select(-operation,-operationn)


  #shape indata ---------------------------
  indata_sel2 <- indata |> select(any_of(c(subj_by_var_vec,'apcsnum','astdt',paste0(grp_var_vec,'n'),grp_var_vec,analysis_var_vec)))
  indata_sel3 <- rename_grp_var(indata_sel2,grp_var_vec)
  indata_factor <- indata_sel3 |> distinct(pick(where(is.factor)))
  indata_sel4 <- indata_sel3 |> mutate(across(where(is.factor),as.numeric,.names="{.col}n"),
                                       across(where(is.factor),as.character))

  if (analysis_var!="") indata_sel4 <- pivot_longer(indata_sel4,cols=all_of(analysis_var_vec),
                                                    names_to="analysis_var",values_to='analysis_val') |>
    mutate(analysis_varn=setNames(1:length(analysis_var_vec),analysis_var_vec)[analysis_var])

  indata_sel <- indata_sel4 |> dplyr::inner_join(subj_indata_sel,by=subj_by_var_vec,relationship="many-to-many") |>
    select(any_of(c(grp_var_all,'apcsnum','analysis_var','analysis_varn','analysis_val','astdt')),usubjid)

  #shape denom_indata ---------------------------
  if (!is.null(denom_indata)) {
    denom_indata_sel2 <- denom_indata |> select(any_of(c(subj_by_var_vec,paste0(grp_var_vec,'n'),grp_var_vec)))
    denom_indata_sel3 <- rename_grp_var(denom_indata_sel2,denom_grp_var_vec)
    #denom_indata_factor <- denom_indata_sel3 |> distinct(pick(where(is.factor)))
    denom_indata_sel4 <- denom_indata_sel3 |>
      mutate(across(where(is.factor),as.numeric,.names="{.col}n"),
             across(where(is.factor),as.character))

    denom_sum <- denom_indata_sel4 |> dplyr::inner_join(subj_indata_sel,by=subj_by_var_vec,relationship="many-to-many") |>
      group_by(pick(any_of(grp_var_all))) |> summarise(denom=n_distinct(usubjid)) |> ungroup()
  } else {
    denom_sum <- subj_N2 |> select(-operation,-operationn,-valc) |> rename(denom=val)
  }

  # return a list ------------------
  mget(c('subj_by_var_vec','grp_var_vec','subj_grp_id_vec','subj_grp_var_vec','analysis_var_vec',
         'indata_sel','indata_factor','subj_indata_sel','subj_N2','subj_N','denom_sum',
         'grp_var_all','dum_subj_data'))
}



# get info from trakdata.csv & return results ------------------------------------
#' return a list containing ard and relative objects for `create_ard_*`
#'
#' @param ard ARD data frame
#' @param subj_N  a data frame storing big N for table column head in RTF
#' @param idkey idkey column `idkey` in PDT
#' @param trakdata trakdata file name
#'
#' @return a list containing ard and relative objects
return_ard <- function(ard,subj_N,idkey,trakdata) {
  idkey <- str_trim(idkey)
  if (idkey!="") {
    report <- create_report2(idkey,trakdata=trakdata)
    shell <- report$shell
    title <- report$title
    footnote <- report$footnote

    ard <- ard |> mutate(trakdata=trakdata,idkey=idkey) |>
      relocate(trakdata,idkey)
    if (!interactive() | (is.null(st$interactive_write_ard) || st$interactive_write_ard)) {
      write_rds(ard,paste0(dirname(shell$file_path),"/",idkey,'_ard.rds'))
    }
    return(mget(c('ard','subj_N','shell','title','footnote')))
  } else {
    return(mget(c('ard','subj_N')))
  }
}







#' calculate default descriptive statistics
#'
#' @param indata input data frame
#' @param indata_var a vector storing variable names of indata
#'
#' @return calculated result
create_ard2_description_stat <- function(indata, indata_var) {
  sum1 <- indata |> group_by(pick(all_of(indata_var))) |>
    summarise(n=sum(!is.na(analysis_val)),
              Mean=mean(analysis_val,na.rm=T),
              SD=sd(analysis_val,na.rm=T),
              Median=median(analysis_val,na.rm=T),
              Q1=quantile(analysis_val,.25,type=2,na.rm=T),
              Q3=quantile(analysis_val,.75,type=2,na.rm=T),
              Min={if (all(is.na(analysis_val))) NA else min(analysis_val,na.rm=T)},
              Max={if (all(is.na(analysis_val))) NA else max(analysis_val,na.rm=T)}) |>
    ungroup()

  sum2 <- sum1 |> pivot_longer(n:Max,names_to="operation",values_to='val') |>
    mutate(apcsnum1=case_when(operation %in% c('Mean','Median','Q1','Q3')~1,
                              operation %in% c('SD')~2,
                              operation %in% c('Min','Max')~0,
                              TRUE~0),
           valc_temp=format2(val,if_else(is.na(apcsnum),0,apcsnum)+apcsnum1),
           valc=case_when(is.na(val)~'NE',
                          operation=='n'~as.character(val),
                          is.na(apcsnum)~NA_character_,
                          TRUE~valc_temp)) |>
    select(-valc_temp)

  sum2 |> pivot_wider(id_cols=all_of(indata_var),names_from=operation,values_from=valc) |>
    mutate(`Mean (SD)`=sprintf("%s (%s)",Mean,SD),
           `Q1 - Q3`=sprintf("%s - %s",Q1,Q3),
           `Min - Max`=sprintf("%s - %s",Min,Max)) |>
    select(any_of(indata_var),`Mean (SD)`:`Min - Max`) |>
    pivot_longer(`Mean (SD)`:`Min - Max`,names_to="operation",values_to="valc") |>
    bind_rows(sum2) |>
    mutate(operationn=case_when(operation=='n'~20,
                                operation=='Mean'~31,
                                operation=='SD'~32,
                                operation=='Mean (SD)'~39,
                                operation=='Min'~41,
                                operation=='Q1'~42,
                                operation=='Median'~43,
                                operation=='Q3'~44,
                                operation=='Max'~45,
                                operation=='Q1 - Q3'~48,
                                operation=='Min - Max'~49
    )) |>
    select(all_of(indata_var),operationn,operation,val,valc)
}







#' calculate sum
#'
#' @param indata input data frame
#' @param indata_var a vector storing variable names of indata
#'
#' @return calculated result
create_ard2_sum <- function(indata, indata_var) {
  indata |> group_by(pick(all_of(indata_var))) |>
    summarise(val=sum(analysis_val)) |> ungroup() |>
    mutate(valc=format2(val,if_else(is.na(apcsnum),0,apcsnum)),
           operationn=51,
           operation='Sum') |>
    select(all_of(indata_var),operationn,operation,val,valc)
}







#' @describeIn create_ard_ For descriptive statistics calculation, such as mean, SD.
#' @export
create_ard_summary <- function(indata,subj_indata,analysis_var,
                               subj_by_var='usubjid',
                               grp_var="",subj_grp_id="",subj_grp_var="",
                               operation="default",
                               idkey="",trakdata='TrakData.csv') {
  # basic prepare ---------------------------------
  check_numeric <- indata |> select(any_of(str_split_1(analysis_var,'\\s'))) |> map_lgl(is.numeric) |> all()
  if (!check_numeric) stop("All variables in analysis_var must be numeric.")
  if (!'apcsnum' %in% names(indata)) stop("apcsnum must exist in indata for rounding.")
  if (!is.numeric(indata$apcsnum)) stop("apcsnum in indata must be numeric.")

  grp_var <- str_trim(grp_var)
  if (grp_var!="") {
    diff_apcsnum <- indata |> group_by(across(all_of(str_split_1(grp_var,pattern='\\s+')))) |> filter(n_distinct(apcsnum)>1) |> nrow()
  } else {
    diff_apcsnum <- indata |> filter(n_distinct(apcsnum)>1) |> nrow()
  }
  if (diff_apcsnum>0) stop(sprintf("apcsnum in indata is not unique by %s.",grp_var))

  si <- shape_input(indata,subj_indata,NULL,subj_by_var,grp_var,subj_grp_id,subj_grp_var,NULL,analysis_var)
  indata_var <- c(str_subset(names(si$indata_sel),'^(subj_)?grp'),'apcsnum','analysis_varn','analysis_var')

  # calculate results ---------------------------------
  sum1 <- tibble()
  if (str_detect(operation,'\\bdefault\\b')) sum1 <- create_ard2_description_stat(si$indata_sel,indata_var) |> bind_rows(sum1)
  if (str_detect(operation,'\\bsum\\b')) sum1 <- create_ard2_sum(si$indata_sel,indata_var) |> bind_rows(sum1)
  sum2 <- bind_rows(si$subj_N2, sum1) |> select(all_of(indata_var),operationn,operation,val,valc)

  # sort and return results -------------------------------------------
  expr1 <- sprintf("sum4 <- sum2[order(%s,method='radix',na.last=FALSE),]",str_c("sum2$",c(indata_var,'operationn'),collapse=','))
  eval(parse(text=expr1))

  ard <- sum4 |> mutate(method='descriptive statistics',.before=operationn)
  return_ard(ard,si$subj_N,idkey,trakdata)
}




#' calculate count of patients and percentage
#'
#' @param indata input data frame
#' @param denom_sum a data frame storing denominator
#'
#' @return calculated result
create_ard2_n_percent <- function(indata, denom_sum) {
  indata |> summarise(val.nsubj=n_distinct(usubjid)) |> ungroup() |>
    dplyr::inner_join(denom_sum) |>
    mutate(val.percent=val.nsubj/denom*100,
           valc.nsubj=format2(val.nsubj,0),valc.percent=format2(val.percent,1),
           valc.n_percent=sprintf("%s (%s)",valc.nsubj,valc.percent)) |>
    select(-denom) |>
    pivot_longer(cols = starts_with('val'), values_drop_na = TRUE,
                 names_to = c(".value", "operation"), names_sep = "\\.") |>
    mutate(operationn=case_when(operation=='n_percent'~21.1,
                                operation=='nsubj'~21,
                                operation=='percent'~25
                                )) |>
    relocate(operationn,.before=operation)
}





#' calculate count of patients and percentage, and combine with denom
#'
#' @param indata input data frame
#' @param denom_sum a data frame storing denominator
#'
#' @return calculated result
create_ard2_n_denom_percent <- function(indata, denom_sum) {
  indata |> summarise(val.nsubj=n_distinct(usubjid)) |> ungroup() |>
    dplyr::right_join(denom_sum) |>
    mutate(across(matches("^grp\\d_totalfl$"),~"#Not_total"),
           across(matches("^grp\\d_totalfl2$"),~FALSE),
           val.nsubj=if_else(is.na(val.nsubj),0,val.nsubj),
           val.percent=val.nsubj/denom*100,
           valc.nsubj=format2(val.nsubj,0),valc.percent=format2(val.percent,1),
           valc.n_percent=sprintf("%s (%s)",valc.nsubj,valc.percent),
           val.denom=denom,
           valc.denom=format2(denom,0),
           valc.n_denom_percent=sprintf("%s/%s (%s)",valc.nsubj,valc.denom,valc.percent)) |>
    select(-denom) |>
    pivot_longer(cols = starts_with('val'), values_drop_na = TRUE,
                 names_to = c(".value", "operation"), names_sep = "\\.") |>
    mutate(operationn=case_when(operation=='n_percent'~21.1,
                                operation=='n_denom_percent'~21.2,
                                operation=='nsubj'~21,
                                operation=='denom'~24,
                                operation=='percent'~25)) |>
    relocate(operationn,.before=operation)
}





#' calculate count of patients
#'
#' @param indata input data frame
#'
#' @return calculated result
create_ard2_n <- function(indata) {
  indata |> summarise(val=n_distinct(usubjid)) |> ungroup() |>
    mutate(valc=format2(val,0),operation='nsubj',operationn=21) |>
    relocate(operationn,operation,.before=val)
}



#' calculate count of data records
#'
#' @param indata input data frame
#'
#' @return calculated result
create_ard2_event <- function(indata) {
  indata |> summarise(val=n()) |> ungroup() |>
    mutate(operationn=101,operation='count_event',valc=format2(val,2)) |>
    relocate(val,.before=valc)
}



#' calculate EAIR
#'
#' @param indata input data frame
#' @param subj_indata subject level data frame
#'
#' @return calculated result
create_ard2_eair <- function(indata,subj_indata) {
  indata_grp_vars <- group_vars(indata)
  subj_indata2 <- indata |> ungroup() |> distinct(pick(matches("(^grp\\d)"))) |>
    dplyr::cross_join(subj_indata)

  indata3 <- indata |> group_by(pick(all_of(c(indata_grp_vars,"usubjid")))) |> arrange(astdt) |> slice(1) |>
    dplyr::right_join(subj_indata2, relationship="many-to-many") |> ungroup() |>
    mutate(exp_dur=as.numeric(if_else(is.na(astdt),censor_dt-trtsdt+1,astdt-trtsdt+1))) |>
    group_by(pick(all_of(indata_grp_vars))) |>
    summarise(val.Exposure=sum(exp_dur)/365.25,val.EAIR=sum(!is.na(grp1))/val.Exposure) |>
    ungroup() |>
    mutate(valc.Exposure=format2(val.Exposure,1),valc.EAIR=format2(val.EAIR,1)) |>
    pivot_longer(cols = starts_with('val'), values_drop_na = TRUE,
                 names_to = c(".value", "operation"), names_sep = "\\.") |>
    mutate(operationn=case_when(operation=='Exposure'~111,
                                operation=='EAIR'~121)) |>
    relocate(operationn,.before=operation)
}




#' @describeIn create_ard_ For frequency relative calculation, including patient count, percentage,
#'             record count, EAIR.
#' @export
create_ard_freq <- function(indata,subj_indata,denom_indata=NULL,
                            subj_by_var='usubjid',
                            grp_var="",subj_grp_id="",subj_grp_var="",denom_grp_var="",
                            method='default',
                            operation='n_percent',
                            idkey="",trakdata='TrakData.csv') {
  # basic prepare ---------------------------------
  operation_na <- str_split_1(operation,pattern='\\s+') |>
    base::setdiff(c('n_percent','n_denom_percent','event','event','eair','n'))
  if (length(operation_na)>0) stop(sprintf("operation not available: %s",str_c(operation_na,collapse=', ')))

  method_na <-  str_split_1(method,pattern='\\s+') |>
    base::setdiff(c('shift','default')) |>
    str_subset("^max#",negate=TRUE)
  if (length(method_na)>0) stop(sprintf("method not available: %s",str_c(method_na,collapse=', ')))

  if (str_detect(operation,'\\beair\\b')) {
    if (!all(c("trtsdt","censor_dt") %in% names(subj_indata))) stop("Variable trtsdt and/or censor_dt don't exist in subj_indata.")
    if (!"astdt" %in% names(indata)) stop("Variable astdt doesn't exist in indata.")
  }

  if (str_detect(method,'\\bmax#')) {
    max_grp_var <- method |> str_extract("\\b(?<=max#)\\w+\\b")
    if (!max_grp_var %in% names(indata)) stop(sprintf("Variable %s from method 'max#' doesn't exist in indata.",max_grp_var))
    grp_var <- grp_var |> str_replace_all(sprintf("\\b%s\\b",max_grp_var),"") |>
      paste(max_grp_var)
  }

  si <- shape_input(indata,subj_indata,denom_indata,subj_by_var,grp_var,subj_grp_id,subj_grp_var,denom_grp_var,analysis_var='')
  grp_all <- str_subset(names(si$indata_sel),'^grp\\d$')
  len_grp <- length(grp_all)

  ## create records for missing -------------------------
  if (str_detect(method,'\\bshift\\b') &
      any(levels(si$indata_factor[[paste0("grp",len_grp-1)]])=="#Missing") &
      any(levels(si$indata_factor[[paste0("grp",len_grp  )]])=="#Missing")) {
    sh_grp_var <- list()
    sh_grpn_val <- numeric()
    for (id in 1:2) {
      sh_grp_var[[id]] <- sprintf(c("grp%s","grp%sn"),len_grp-2+id)
      indata_missing <- si$indata_sel |> filter(!!sym(sh_grp_var[[id]][1])=='#Missing')
      if ( sh_grp_var[[id]][1] %in% names(si$indata_factor) ) {
        sh_grpn_val[id] <- which(levels(si$indata_factor[[sh_grp_var[[id]][1]]])=="#Missing")
      } else if (!sh_grp_var[[id]][2] %in% names(si$indata_sel) | nrow(indata_missing)==0) {
        sh_grpn_val[id] <- -Inf
      } else {
        sh_grpn_val[id] <- indata_missing |> pull(!!sym(sh_grp_var[[id]][2])) |> nth(1)
      }
    }

    indata_dum_subj <- si$indata_sel |> select(matches('^grp')) |>
      mutate(across(all_of(c(sh_grp_var[[1]][1],sh_grp_var[[2]][1])),~"#Missing"),
             across(any_of(sh_grp_var[[1]][2]),~sh_grpn_val[1]),
             across(any_of(sh_grp_var[[2]][2]),~sh_grpn_val[2])) |>
      rename_with(~paste0(.,"_dummy"),
                  all_of(unlist(sh_grp_var))) |>
      distinct() |>
      expand_grid(distinct(si$subj_indata_sel, pick(all_of(si$subj_by_var_vec))))

    indata1 <- si$indata_sel |> mutate(dummy=FALSE) |>
      left_join(indata_dum_subj) |>
      mutate(!!!setNames(map(str_glue("if_else(is.na(dummy), {var}_dummy, {var})",
                                      var=unlist(sh_grp_var)),rlang::parse_expr),
                         unlist(sh_grp_var) )) |>
      select(!matches('dummy'))
  } else indata1 <- si$indata_sel

  ## exclude records for max ---------------------------
  if (str_detect(method,'\\bmax#')) {
    max_grp_var <- grp_all[length(grp_all)]
    max_grp_varn <- paste0(max_grp_var,'n')
    max_grp_var2 <- c(max_grp_varn,max_grp_var)
    indata1 <- indata1 |> mutate(max_grp_origin=!!sym(max_grp_var))
    if (max_grp_varn %in% names(indata1)) indata1[['max_grp_originn']] <- indata1[[max_grp_varn]]

    indata1 <- indata1 |> group_by(pick(all_of(c(base::setdiff(grp_all,max_grp_var),'usubjid')))) |>
      arrange(across(any_of(c('max_grp_originn','max_grp_origin')))) |> slice(n()) |> ungroup()
  }

  ## create records for total -------------------------
  #len_grp <- 5
  grp_totalfl_vec <- sprintf("grp%s_totalfl",1:len_grp)
  grp_totalfl1 <- c('#Total','#Not_total') |> list() |> rep(len_grp) |> setNames(grp_totalfl_vec)

  grp_totalfl2 <- expand_grid(!!!grp_totalfl1) |> rowwise() |>
    mutate(grp_totalfl_all=str_c(c_across(all_of(grp_totalfl_vec)),collapse ='')) |>
    ungroup()

  if (str_detect(method,'\\bshift\\b')) {
    grp_totalfl3 <- grp_totalfl2
  } else if (str_detect(method,'\\bmax#')) {
    grp_totalfl3 <- grp_totalfl2 |>
      filter(str_detect(grp_totalfl_all, '^(#Not_total)*(#Total)*$|#Total#Not_total$'))
  } else {
    grp_totalfl3 <- grp_totalfl2 |>
      filter(str_detect(grp_totalfl_all, '^(#Not_total)*(#Total)*$'))
  }

  grp_totalfl <- grp_totalfl3 |> select(-grp_totalfl_all) |>
    mutate(!!sym(sprintf("grp%s_totalfl",len_grp+1)):='#Total') |>
    mutate(!!!setNames(map(str_glue("if_else(grp{id+1}_totalfl!='#Total', FALSE, TRUE)",
                                    id=1:len_grp),rlang::parse_expr),
                       paste0(grp_totalfl_vec,'2') )) |>
    select(-all_of(sprintf("grp%s_totalfl",len_grp+1)))

  indata3 <- indata1 |> cross_join(grp_totalfl) |>
    mutate(!!!setNames(map(str_glue("if_else({grp_totalfl_vec}=='#Total','#Total',{grp_all})"),
                           rlang::parse_expr),
                       grp_all))

  grp_nonmissn_id <- names(si$indata_sel) |> str_subset('^grp\\dn$') |> str_replace_all('^grp|n$','')
  if (length(grp_nonmissn_id)>0) {
    indata3 <- indata3 |> mutate(!!!setNames(
      map(str_glue("if_else(grp{grp_nonmissn_id}=='#Total',-Inf,grp{grp_nonmissn_id}n)"),rlang::parse_expr),
      str_glue("grp{grp_nonmissn_id}n")))
  }

  indata_final <- indata3 |> group_by(pick(matches("^(subj_)?grp")))

  # calculate results ---------------------------------
  ard1 <- tibble()
  if (str_detect(operation,'\\bn_percent\\b')) ard1 <- create_ard2_n_percent(indata_final, si$denom_sum) |> bind_rows(ard1)
  if (str_detect(operation,'\\bn_denom_percent\\b')) ard1 <- create_ard2_n_denom_percent(indata_final, si$denom_sum) |> bind_rows(ard1)
  if (str_detect(operation,'\\bn\\b')) ard1 <- create_ard2_n(indata_final) |> bind_rows(ard1)
  if (str_detect(operation,'\\bevent\\b')) ard1 <- create_ard2_event(indata_final) |> bind_rows(ard1)
  if (str_detect(operation,'\\beair\\b')) ard1 <- create_ard2_eair(indata_final,si$subj_indata_sel) |> bind_rows(ard1)

  ## merge dummy records -------------------------------------
  dum_ard1 <- ard1 |> select(-any_of(matches("^subj_"))) |> select(-val,-valc) |>
    filter(if_all(matches("^grp\\d$"),~ .=="#Total")) |>
    distinct(pick(everything())) |>
    cross_join(select(si$dum_subj_data,-operation,-operationn))

  ard3 <- ard1 |> dplyr::full_join(dum_ard1) |>
    mutate(val=if_else(is.na(valc),0,val),
           valc=if_else(is.na(valc),"0",valc))

  if (str_detect(method,'\\bshift\\b')) {
    sh_grp_var <- list()
    dum_shift <- list()
    for (id in 1:2) {
      sh_grp_var[[id]] <- sprintf(c("grp%sn","grp%s","grp%s_var","grp%s_totalfl"), len_grp-2+id)
      if ( sh_grp_var[[id]][2] %in% names(si$indata_factor) ) {
        sh_grp_levels <- si$indata_factor[[sh_grp_var[[id]][2]]] |> levels()
        dum_shift[[id]] <- tibble(
          !!sym(sh_grp_var[[id]][1]):=c(-Inf,1:length(sh_grp_levels)),
          !!sym(sh_grp_var[[id]][2]):=c('#Total',sh_grp_levels),
          !!sym(sh_grp_var[[id]][3]):=ard3[[sh_grp_var[[id]][3]]][1],
          !!sym(sh_grp_var[[id]][4]):=c('#Total',rep('#Not_total',length(sh_grp_levels))))
      } else {
        dum_shift[[id]] <- ard3 |> distinct(pick(any_of(sh_grp_var[[id]])))
      }
    }

    dum_shift3 <- ard3 |>
      select(-any_of(matches("^subj_|_totalfl2")),
             -any_of(unlist(sh_grp_var)),
             -val,-valc) |>
      distinct(pick(everything())) |>
      cross_join(select(si$dum_subj_data,-operation,-operationn)) |>
      cross_join(dum_shift[[1]]) |> cross_join(dum_shift[[2]])

    ard3_shift <- ard3 |> select(-matches("^grp\\d_totalfl2$")) |>
      dplyr::full_join(dum_shift3) |>
      mutate(val=if_else(is.na(valc),0,val),
             valc=if_else(is.na(valc),"0",valc)) |>
      left_join(grp_totalfl)

    ard3 <- ard3_shift
  } else if (str_detect(method,'\\bmax#')) {
    max_grp_var3 <- c(max_grp_var2,paste0(max_grp_var,c('_var','_totalfl')))
    max_grp_levels <- si$indata_factor[[max_grp_var3[2]]] |> levels()
    if (!is.null(max_grp_levels)) {
      dum_max_grp <- tibble(
        !!sym(max_grp_var3[1]):=c(-Inf,1:length(max_grp_levels)),
        !!sym(max_grp_var3[2]):=c('#Total',max_grp_levels),
        !!sym(max_grp_var3[3]):=ard3[[max_grp_var3[3]]][1],
        !!sym(max_grp_var3[4]):=c('#Total',rep('#Not_total',length(max_grp_levels))))
    } else {
      dum_max_grp <- ard3 |> distinct(pick(any_of(max_grp_var3)))
    }

    dum_max_data <- ard3 |> select(-any_of(matches("^subj_|_totalfl2"))) |>
      filter(!!sym(max_grp_var)!="#Total") |>
      select(-any_of(c(max_grp_var3,'val','valc'))) |>
      distinct(pick(everything())) |>
      cross_join(select(si$dum_subj_data,-operation,-operationn)) |>
      cross_join(dum_max_grp)

    ard3_max <- ard3 |> select(-matches("^grp\\d_totalfl2$")) |>
      dplyr::full_join(dum_max_data) |>
      mutate(val=if_else(is.na(valc),0,val),
             valc=if_else(is.na(valc),"0",valc)) |>
      left_join(grp_totalfl)

    ard3 <- ard3_max
  }

  # assign grpXn as per val
  grp_missn_id <- setdiff(str_subset(names(si$indata_sel),'^grp\\d$'),
                          str_subset(names(si$indata_sel),'^grp\\dn$') |> str_replace('n$','')) |>
    str_replace("^grp","")

  if (length(grp_missn_id)>0) {
    ard3 <- ard3 |>
      mutate(!!!setNames(map(str_glue("if_else(grp{grp_missn_id}_totalfl2,-val,NA)"),rlang::parse_expr),
                         str_glue("grp{grp_missn_id}n"))) |>
      group_by(pick(c(matches("subj_grp\\d")))) |>
      arrange(floor(operationn),pick(matches("^grp\\d$")),operationn) |>
      tidyr::fill(any_of(sprintf("grp%sn",grp_missn_id)),.direction="down") |>
      ungroup() |> select(any_of(c(si$grp_var_all,'operationn','operation','val','valc')))
  }

  ard4 <- ard3 |> rowwise() |> mutate(display1:={
    grp_vec <- c(!!!syms(grp_all))
    grp_vec[is.na(grp_vec)] <- ""
    if (!any(grp_vec!="#Total")) {
      return_val <- "#Total"
    } else {
      max_grp <- max(which(grp_vec!="#Total"))
      return_val <- str_c(c(rep("\uA0",(max_grp-1)*4),grp_vec[max_grp]),collapse="")
    }
    return_val
  },
  display2:={
    grp_vec <- c(!!!syms(grp_all[-length(grp_all)]))
    grp_vec[is.na(grp_vec)] <- ""
    if (!any(grp_vec!="#Total")) {
      return_val <- "#Total"
    } else {
      max_grp <- max(which(grp_vec!="#Total"))
      return_val <- str_c(c(rep("\uA0",(max_grp-1)*4),grp_vec[max_grp]),collapse="")
    }
    return_val
  }) |> ungroup() |> relocate(display1,display2,.before=subj_grp1_id) |>
    select(!contains("totalfl"))

  ard5 <- bind_rows(ard4,si$subj_N2)

  expr1 <- sprintf("ard6 <- ard5[order(%s,method='radix',na.last=FALSE),]",str_c("ard5$",names(ard5),collapse=','))
  eval(parse(text=expr1))

  method <- method |> str_replace('default','') |> str_trim()
  ard <- ard6 |> mutate(method=str_trim(paste('frequency',method)),.before=operationn)

  return_ard(ard,si$subj_N,idkey,trakdata)
}


#' Format ARD data to fit mix table
#'
#' @param indata a list of one or multiple objects returned from create_ard_*
#' @param main_grp_var the main group variable to display before each group
#' @param idkey column `idkey` in PDT
#' @param trakdata trakdata file name
#'
#' @return Each function returns a list containing
#' -   `ard` ARD data frame. See the data structure in util/documentation/ard_specification.xlsx.
#' -   `subj_N` a data frame storing big N for table column head in RTF
#' -   `shell` the base shell for RTF creation
#' -   `title` title for RTF creation
#' -   `footnote` footnotes for RTF creation
#' @export
#'
#' @examples see example programs in pgm/saf/example_t_mix.R
format_ard_mix <- function(indata,
                           main_grp_var='grp1',
                           idkey="",
                           trakdata='TrakData.csv') {

  main_grp_var <- str_trim(main_grp_var)
  if (str_detect(main_grp_var,' '))
    stop(sprintf("Only one variable is acceptable for main_grp_var, but main_grp_var=%s",main_grp_var))
  if (!str_detect(main_grp_var,'^grp\\d$')) {
    stop(sprintf("main_grp_var should be like \"grpX\", but main_grp_var=%s",main_grp_var))
  } else {
    main_grp_var_n <- str_replace(main_grp_var,'grp','') |> as.numeric()
    main_grp_var_a1 <- paste0("grp",main_grp_var_n+1)
  }

  for (seq_num in 1:length(indata)) {
    if (seq_num==1) {
      ard1 <- indata[[seq_num]]$ard |> filter(operation=="bigN")
      subj_N <- indata[[seq_num]]$subj_N
    }
    if (!main_grp_var %in% names(indata[[seq_num]]$ard))
      stop(sprintf("main_grp_var=%s does not exist in indata[[%s]]$ard.",main_grp_var,seq_num))

    ard_loop1 <- indata[[seq_num]]$ard |>
      filter(if_all(matches("^grp\\d$"),~ !is.na(.)), operation!="bigN") |>
      mutate(ndistinct_val=if (main_grp_var_a1 %in% names(indata[[seq_num]]$ard)) !!sym(main_grp_var_a1) else "1",
             ndistinct_val=if_else(method=='descriptive statistics',operation,ndistinct_val)) |>
      group_by(pick(any_of(matches(sprintf("sub_grp|^grp[1-%s]$",main_grp_var_n))))) |>
      mutate(ndistinct_main_grp=n_distinct(ndistinct_val, na.rm=TRUE)) |>
      ungroup()
    ard_loop2 <- ard_loop1 |> filter(if_all(matches(sprintf("^grp[1-%s]$",main_grp_var_n+1)),~ (.!="#Total" | ndistinct_main_grp==1) ))

    ard1 <- bind_rows(ard1,ard_loop2)
  }

  grp_var_vec <- names(ard1) |> str_subset("^grp\\d$") |> sort()
  var_all1 <- as.vector(cbind(rbind(paste0("grp",1:9,"_var"),paste0("grp",1:9,"n"),paste0("grp",1:9))))
  var_all2 <- c(var_all1,"display1",paste0("subj_",var_all1),
                "apcsnum","analysis_varn","analysis_var","method","operationn","operation","val","valc")

  ard4 <- ard1 |> filter(operation!="bigN", ndistinct_main_grp>1) |>
    select(-any_of(c('operation','operationn','val','valc','display1','display2','ndistinct_val'))) |>
    select(-matches(sprintf("^grp[%s-9]",main_grp_var_n+1))) |>
    distinct() |>
    mutate(operation='blank',operationn=2,valc="",
           display1=!!sym(main_grp_var)) |>
    bind_rows(ard1) |>
    mutate(display1=case_when(ndistinct_main_grp==1~!!sym(main_grp_var),
                              !is.na(display1)~display1,
                              TRUE~paste0("\uA0\uA0",operation))) |>
    select(any_of(var_all2))

  expr1 <- sprintf("ard <- ard4[order(%s,method='radix',na.last=FALSE),]",str_c("ard4$",names(ard4),collapse=','))
  eval(parse(text=expr1))

  return_ard(ard,subj_N,idkey,trakdata)
}





#' Format ARD data to standard format.
#'
#' @param path a character vector of the full path names of the input ARD data.
#' @param main_grp_var an optional regular expression. Only ARD data names which match the regular expression will be formatted.
#' @param trakdata trakdata file name
#'
#' @return a standardized ARD data frame
#'
#' @export
#'
#' @examples format_ard_standard("/view/pangcha_view/vob/CVAY736F1/CVAY736F12301/csr_1/reports/saf", "a.*ex.rds")


format_ard_standard <- function(path,
                                pattern = NULL,
                                trakdata='TrakData.csv') {
  ard_files <- list.files(path,pattern,full.names=TRUE)
  ard1 <- ard_files |> Vectorize(read_rds)() |> reduce(bind_rows)

  subj_grp_len <- names(ard1) |> str_subset("^subj_grp\\d$") |> length()
  grp_len <- names(ard1) |> str_subset("^grp\\d$") |> length()

  std_grp_vec <- as.vector(rbind(paste0("groupingId",1:9),paste0("groupingDataset",1:9),paste0("groupingVariable",1:9),
                                 paste0("groupId",1:9),paste0("group_name",1:9),paste0("group_order",1:9)))

  grp_vec1 <- c(sprintf("subj_grp%s",1:subj_grp_len),sprintf("grp%s",(subj_grp_len+1):9))
  names(std_grp_vec) <- c("_id","_data","_var","_grpid","","n") |> map(~paste0(grp_vec1,.)) |>
    as_tibble(.name_repair="universal_quiet") |> t() |> as.vector()

  rename_vars <- c('idkey'='outputId','Title2'='Title2','display1'='listItem_name','listItem_order'='listItem_order',
                   'analysisId'='analysisId','analysis_dataset'='analysis_dataset','analysis_var'='analysis_variable',
                   'method'='methodId','operation'='operationId','operationn'='operation_order',
                   'resultPattern'='resultPattern','val'='rawValue','valc'='formattedValue',std_grp_vec)

  TrakData <- read_delim(paste0(st$rautil, trakdata), col_names = TRUE,
                         delim = "<[]>",show_col_types = FALSE, name_repair="universal_quiet",na="") |>
    select(idkey=Unique.ID.reference.number,Title2=Report.title)

  ard2 <- ard1 |> inner_join(TrakData) |>
    mutate(apcsnum=if_else(is.na(apcsnum),0,apcsnum),
           resultPattern=case_when(operation=='blank'~'',
                                   operation %in% c('n','nsubj','denom','bigN')~'xx',
                                   operation %in% c('percent','EAIR','Exposure')~'xx.x',
                                   operation=='n_percent'~'xx (xx.x)',
                                   operation=='n_denom_percent'~'xx/xx (xx.x)',
                                   operation %in% c('Mean','Median','Q1','Q3')~paste0('xx.',str_dup('x',apcsnum+1)),
                                   operation=='SD'~paste0('xx.',str_dup('x',apcsnum+2)),
                                   operation %in% c('Min','Max')~paste0('xx.',str_dup('x',apcsnum)),
                                   operation=='Mean (SD)'~sprintf("xx.%s (xx.%s)",str_dup('x',apcsnum+1),str_dup('x',apcsnum+2)),
                                   operation=='Q1 - Q3'~sprintf("xx.%s - xx.%s",str_dup('x',apcsnum+1),str_dup('x',apcsnum+1)),
                                   operation=='Min - Max'~sprintf("xx.%s - xx.%s",str_dup('x',apcsnum),str_dup('x',apcsnum)),
                                   TRUE~NA
           ),
           resultPattern=str_replace_all(resultPattern,"\\.(?!x)","")) |>
    select(any_of(names(rename_vars)))

  names(ard2) <- rename_vars[names(ard2)]
  ard2[setdiff(rename_vars,names(ard2))] <- NA

  names(rename_vars) <- NULL
  ard3 <- ard2 |>
    mutate(!!!setNames(map(str_glue("if_else(is.na(groupingVariable{i}),NA,paste(outputId,groupingVariable{i},sep='.'))",
                                    i=(subj_grp_len+1):9),rlang::parse_expr),
                       str_glue("groupingId{i}",i=(subj_grp_len+1):9))) |>
    mutate(!!!setNames(map(str_glue("case_when(!is.na(groupingId{i})~paste('subjectLevelId',groupingId{i},sep='.'),
                                               !is.na(groupingVariable{i})~paste('subjectLevelVar',groupingVariable{i},sep='.'),
                                               TRUE~NA)",
                                    i=1:subj_grp_len),rlang::parse_expr),
                       str_glue("groupingId{i}",i=1:subj_grp_len))) |>
    mutate(analysisId=outputId) |>
    select(all_of(rename_vars))

  ard3
}



Glossary	Description
CDISC ARS	CDISC analysis result standard which defines analysis results and TFL metadata should be stored.
ARS instance	A data that stores analysis results and TFL metadata. It can be JSON, YAML, excel or any other format.
TFL metadata	"A data that stores TFL relative metadata, like title, footnote, dataset & variable used, group variables, data filter, analysis method, reference document.
It could contains any information relative to TFL, except analysis results."
TFL metadata specification	The document about TFL metadata standard
Analysis results	Analysis result numbers, such as the numbers presented in RTF TFL outputs.
ARD	Analysis result data in tabular format. It must contain analysis results. It could contain more variables for output ID, group values.
	
	
	TFL metadata specification structure
	Different domains must be presented as different tabs of ARS in MDR.
	Domain name, Variable name store the value of column header when exporting/importing metadata as an excel file.
	Domain Label, Variable Label store the value to display as column header in webpage.

Category	Sub-category	Checklist	Allocation	Response
Schema		Is the MOSAIC schema equivalent to the CDISC schema?	Conor	Very minor changes made to  MOSAIC from CDISC SChema
Schema		If the MOSAIC schema is derived from CDISC standards, can you provide details on the updates or differences?	Conor	Conor to confirm and share the list of changes from CDISC Schema
Schema		MOSAIC Schema considers all type of TFLs, e.g. figure, listing?	Conor	Tables, listing and Figure in scope. Currently release is focused on Tables. Currently for figures and listing no official date planned
ARS instance		Does MOSAIC ARS instance include the same elements as CDISC ARS?	Conor	Same response as #2
ARS instance		If a study applies additional filtering conditions or minor customizations, will MOSAIC ARS instance automatically reflect these updates?	Conor	Additional condition changes gets automatically taken care. ARD keeps getting refreshed using the inputs and regenrates the data.
TFL metadata		When multiple users work on the same study across different data domains in MOSAIC, how is TFL metadata refreshed to incorporate customized changes?	Conor	"One user can work on data domain, but different user can work on different domains. User will have to trigger to refresh.
ARD is Per Domain is different database file. R script required."
TFL metadata		Can a single table be regenerated to reflect updated data? If so, does it impact all ARD tables or only the specific table?	Conor	When ADaM is refreshed, regenerate ARD. But for single table currently needs further discussin and work with users.
TFL metadata		Is it currently possible to select specific table metadata from ARD for write‑back to MDR?	Conor/TCS	Currently not possible to be worked upon.
TFL metadata	write back	Can user update metadata by outputId, or any other ID via API?	TCS	
TFL metadata	write back	Can user update metadata by outputId, or any other ID during file uploading?	TCS	
TFL metadata		How are versions of ARD managed at study level for data refresh/changes currently in MOSAIC?	Conor	GPS/Quanta currently manages the version.
TFL metadata		How should updates to subgroup variables at the study level be handled?	Conor	Added as part of grouping elements in ARS, not added separately. Takes ordered format
TFL metadata		How should updates to analysis sections (additions or deletions) at the study level be handled?	Conor/TCS	Changes will have to be handled via ADaM. Need to be discussed further.
TFL metadata		Please share the process how admin can update MDR structure as per metadata specification	TCS	
TFL metadata	write back	Could MDR support input & output via API?	TCS	
TFL metadata	write back	Which file format could the API support, JSON, YAML, EXCEL, binary data like parquet?	TCS	
TFL metadata	write back	How the modification history be traced?	TCS	
TFL metadata		For WhereClause, please store it as a string in YAML format. Display in webpage in more readable format, e.g. ((SEX eq "Male" and (ANL01FL eq "Y" or CHG is not missing)).	TCS	
TFL metadata		Some columns must be unedited by user, and retained from other domains via ID. Please see details in variables sheet.	TCS	
TFL metadata		Will MOSAIC support to change variable name, such as for data filter or grouping variable?	Conor	Changes will have to exist in ADaM.
TFL metadata		Will MOSAIC provide a guidance for variable usage, especially for the variable not having standard derivation in CDISC, e.g. ANL01FL?	Conor	Not there yet, possibly in the future.
TFL metadata		MDR should be able to import and export TFL metadata, and present it.	TCS	
TFL metadata		eRAP and go/tflglobal will not be retired as the tool to create study TFL shell and export relative information, such as title, footnotes	Standard	
TFL metadata	Additional tool	Additional tool is needed to flow information from eRAP to MDR.	TCS/Novartis	
TFL metadata	Additional tool	"Additional efforts to capture annotation from global TFL shell to store in global TFL metadata.
- AI can be used to create the initial version, and additional efforts to review them"	TCS/Novartis	
TFL metadata	Additional tool	Additional tool is helpful to generate non-standard TFL metadata, at least for partial information.	Carlos/Novartis	
global shell	Additional tool	Additiona tool is needed to combine go/globaltfl with global TFL shell metadata for filter & review	Standard	
global shell		To build a process to update global TFL shell metadata when the shell in go/globaltfl is updated	Standard	
eRAP		Can eRAP add global shell ID?	Standard	
TFL metadata		What version management features are available ?		
TFL metadata		Can the MDR system ingest or map to CDISC schemas?	TCS	
TFL metadata		Can it auto populate metadata from MOSAIC?	TCS/Standard	
TFL metadata		For bulk upload, is manual intervention required, or can it automatically compare to existing records and retire superseded ones?	TCS/Standard	
TFL metadata		Can users copy an existing table from global or study and then update it?	TCS/Standard	
TFL metadata		Can MDR support ARS metadata generation when the study TFL is updated?	Standard	
TFL metadata/ARD		For the value of groupingId in MOSAIC, it's some random characters (hash value?) for now. Is it possible to make it more meaningful, such as "ADAE.AEDECOD._110ca2b06c8e2de4c53574385246ed51"? In this case, user can understand it in analysis domain and ARD without jumping to the grouping domain.	Conor	Based on engineering requirements. Currently not in scope, intent is different.
mainListOfContents	List of Outputs	The main list of the analyses and outputs defined for the reporting event.	One record per outputId	mainListOfContents Class, outputs Class	Called as LOA in the current demo, but we still prefer to display it as "List of Outputs".
analysis	Analysis	The analyses defined for the reporting event.	One or multiple records per outputId. Linked to mainListOfContents by outputId.	analysis Class	
analysisGroupings	Analysis Grouping	Characteristics used to subdivide the subject population (e.g., treatment, sex, age group) or data records in analysis datasets (e.g., visit, system organ class).	One or multiple records per groupingId. Linked to analysis by groupingId.	groups Class	
AnalysisSet	Analysis Set	The analysis sets (subject populations) defined for the reporting event.	One record per id. Linked to mainListOfContents by mainListOfContents.analysisSetId = AnalysisSet.id.	AnalysisSet Class	
AnalysisMethod	Analysis Method	Statistical Analysis Method	"One record per id. Linked to mainListOfContents by mainListOfContents.methodId = AnalysisSet.id.
It's usually helpful for inference analysis outputs."	AnalysisMethod Class	
ReferenceDocument	Reference Document	An external document containing supporting documentation or programming code.	One record per id	referenceDocuments Class	
ARD	Analysis Result Dataset	Analysis Result Dataset	One record per outputId, methodId, operationId, groupingIdX, (groupIdX or group_nameX)	"OperationResult Class, and Analysis Result Dataset (ARD) in 
https://wiki.cdisc.org/spaces/ARSP/pages/222298985/Analysis+Result+Dataset+ARD"	out of scope of MDR
Domain name	Variable Name	Variable Label	Description	"Required
in global"	"Required
in study"	Source of non-standard outputs	Source of standard outputs	Type	Details for MDR UI	Referred ARS Component
mainListOfContents	status	Status	The status to description the current domain, e.g. new, retired#1, retired#2	Y	Y	MDR	MDR	string	"Freeze at left.
Derived by MDR. Allowed value: new, updated, retired#1, retired#2, retired#3, ..."	N/A
mainListOfContents	outputId	Output ID	Unique ID in the domain	Y	Y	SP input	SP input	string	"Freeze at left.
Free text, required.
When user clicks it, jump to analysis tab filtered the records of the same outputId."	outputId
mainListOfContents	Title1	Title Line 1	The first line of output title, e.g. Table 14.1-1.1	Y	Y	eRAP automatically	eRAP automatically	string	Free text, required	displaySections.subSection.text where displaySections.sectionType=Title
mainListOfContents	Title2	Title Line 2	The second line of output title, e.g. Disposition at screening	Y	Y	eRAP automatically	eRAP automatically	string	Free text, required	displaySections.subSection.text where displaySections.sectionType=Title
mainListOfContents	analysisSetId	Analysis Set ID	The identifier of the referenced analysis set specified in AnalysisSet.id.	N	N	SP update from global TFL metadata	SP update from global TFL metadata	string	"Drop down list or free text. Drop down list values are from AnalysisSet.id.
If free text values do not exist in AnalysisSet.id, jump to AnalysisSet for user to create it.
When user clicks it, jump to AnalysisSet tab of the id in view/edit mode."	analysisSetId
mainListOfContents	analysisSet_name	Analysis Set Name	Name corresponding to analysisSetId	N	N	SP update from global TFL metadata	MDR retains from analysisSet.name as per analysisSetId	string	Uneditable for user. Retained from AnalysisSet.name by mainListOfContents.analysisSetId=AnalysisSet.id	analysisSet.name
mainListOfContents	category_label1	Category 1	A category of outputs	N	N	SP input	SP input	string	Free text	AnalysisOutputCategory.label
mainListOfContents	category_label2	Category 2	A category of outputs	N	N	SP input	SP input	string	Free text	AnalysisOutputCategory.label
mainListOfContents	Footnote	Footnote	Output footnote	N	N	eRAP automatically	eRAP automatically	string	Free text	displaySections.subSection.text where displaySections.sectionType=Footnote
mainListOfContents	output_description	Output Description	A textual description about the output, e.g. programming note	N	N	eRAP automatically	eRAP automatically	string	Free text	output.description
mainListOfContents	programmingCode_name	Program File	Program file name, e.g. t_ds_scr.R	N	N	SP input	SP input	string	Free text	ReferenceDocument.name where ReferenceDocument.id=programmingCode.documentRef.referenceDocumentId
mainListOfContents	programmingCode_context	Program Context	The name and version of the computer language used for the program.	N	N	SP input	SP input	string	Free text	programmingCode.context
mainListOfContents	programmingCode_parameter_value	Program Code Parameter	Parameters used as input of program	N	N	SP input	SP input	string	Free text	programmingCode.parameters.value
mainListOfContents	programmingCode_code	Program Code	Programming key code, e.g. code used to filter input datasets	N	N	SP input	SP input	string	Free text, monospaced font	programmingCode.code
mainListOfContents	outputFile_name	Output File Name	Output file name, e.g. t141_1_01.rtf	N	N	created according to Title1	created according to Title1	string	Free text	OutputFile.name
mainListOfContents	outputFile_location	Output File Location	A relative path indicating the output file location, e.g. reports/saf	N	N	SP input	SP input	string	Free text	OutputFile.location
mainListOfContents	order	Output Order	The order of output for publishing	N	N	SP input	SP input	integer	Integer input	order
mainListOfContents	ReferenceDocumentId1	Reference Document ID 1	The identifier of the referenced document, linked from ReferenceDocument.id, e.g. GSHELL for global TFL shell.	N	N	eRAP automatically	eRAP automatically	string	"Drop down list. Optional values from ReferenceDocument.id.
When user clicks it, jump to ReferenceDocument tab of the id in view/edit mode."	ReferenceDocumentId
mainListOfContents	pageRefs1	Reference Page 1	A list of references to specific parts of a document, which may be referenced as a list of one or more page numbers, a range of page numbers, or a list of named destinations in the document (e.g. bookmarks), e.g. global shell ID	N	N	eRAP automatically	eRAP automatically	string	Free text	pageRefs
mainListOfContents	ReferenceDocumentId2	Reference Document ID 2	The identifier of the referenced document, linked from ReferenceDocument.id, e.g. RASHELL for study RA TFL shell.	N	N	eRAP automatically	eRAP automatically	string	See ReferenceDocumentId1	ReferenceDocumentId
mainListOfContents	pageRefs2	Reference Page 2	A list of references to specific parts of a document, which may be referenced as a list of one or more page numbers, a range of page numbers, or a list of named destinations in the document (e.g. bookmarks), e.g. study shell ID	N	N	eRAP automatically	eRAP automatically	string	Free text	pageRefs
mainListOfContents	ReferenceDocumentId3	Reference Document ID 3	The identifier of the referenced document, linked from ReferenceDocument.id	N	N	SP input	SP input	string	See ReferenceDocumentId1	ReferenceDocumentId
mainListOfContents	pageRefs3	Reference Page 3	A list of references to specific parts of a document, which may be referenced as a list of one or more page numbers, a range of page numbers, or a list of named destinations in the document (e.g. bookmarks).	N	N	SP input	SP input	string	Free text	pageRefs
mainListOfContents	ReferenceDocumentId4	Reference Document ID 3	The identifier of the referenced document, linked from ReferenceDocument.id	N	N	SP input	SP input	string	See ReferenceDocumentId1	ReferenceDocumentId
mainListOfContents	pageRefs4	Reference Page 4	A list of references to specific parts of a document, which may be referenced as a list of one or more page numbers, a range of page numbers, or a list of named destinations in the document (e.g. bookmarks).	N	N	SP input	SP input	string	Free text	pageRefs
mainListOfContents	ReferenceDocumentId5	Reference Document ID 3	The identifier of the referenced document, linked from ReferenceDocument.id	N	N	SP input	SP input	string	See ReferenceDocumentId1	ReferenceDocumentId
mainListOfContents	pageRefs5	Reference Page 5	A list of references to specific parts of a document, which may be referenced as a list of one or more page numbers, a range of page numbers, or a list of named destinations in the document (e.g. bookmarks).	N	N	SP input	SP input	string	Free text	pageRefs
analysis	id	Analysis ID	The identifier of the analysis.	N	N	SP update from global TFL metadata	MOSAIC	string	"Freeze at left.
Free text"	Analysis.id
analysis	description	Description	A textual description	N	N	SP update from global TFL metadata	MOSAIC	string	Free text	description
analysis	outputId	Output ID	Unique output ID retained from mainListOfContents.outputId	N	N	SP select from mainListOfContents.outputId	MOSAIC	string	Drop down list. Optional values from mainListOfContents.outputId	outputId
analysis	Title2	Title Line 2	The second line of output title. It's linked from mainListOfContents.title2, and is uneditable in this domain.	N	N	MDR retains from mainListOfContents.Title2	MDR retains from mainListOfContents.Title2	string	Uneditable for user. Retained from mainListOfContents.Title2 by mainListOfContents.outputId=analysis.outputId	displaySections.subSection.text where displaySections.sectionType=Footnote
analysis	dataset	Input Dataset	The name of the analysis dataset	N	N	SP update from global TFL metadata	MOSAIC	string	Free text	dataset
analysis	variable	Analysis Variable	The name of the variable from analysis.dataset	N	N	SP update from global TFL metadata	MOSAIC	string	Free text	variable
analysis	dataSubset_WhereClause	Data Subset Where Clause	data subset selection criteria for analysis, stored in YAML format	N	N	SP update from global TFL metadata	MOSAIC	string	"Nested yaml following CDISC ARS. If user wants to edit it, jump to a new interface for user to edit.
In the overall tab of the current domain, it's presented as programming language, e.g. ((ADSL.SEX eq ""Male"" and (ADLB.ANL01FL eq ""Y"" or ADLB.CHG is not missing)), for review."	WhereClause Class
analysis	methodId	Method ID	Unique method ID retained from analysisMethod.id	N	N	SP update from global TFL metadata	MOSAIC	string	"Free text.
When user clicks it, jump to AnalysisMethod tab of the id in view/edit mode."	methodId
analysis	groupingId1	Grouping ID 1	The identifier of the referenced subject or data grouping factor.	N	N	SP update from global TFL metadata	MOSAIC	string	"Drop down list or free text. Drop down list values are from analysisGroupings.id.
If free text values do not exist in analysisGroupings.id, jump to analysisGroupings for user to create it.
When user clicks it, jump to analysisGroupings tab of the id in view/edit mode."	groupingId
analysis	groupingId2	Grouping ID 2	The identifier of the referenced subject or data grouping factor.	N	N	SP update from global TFL metadata	MOSAIC	string	See groupingId1	groupingId
analysis	groupingId3	Grouping ID 3	The identifier of the referenced subject or data grouping factor.	N	N	SP update from global TFL metadata	MOSAIC	string	See groupingId1	groupingId
analysis	groupingId4	Grouping ID 4	The identifier of the referenced subject or data grouping factor.	N	N	SP update from global TFL metadata	MOSAIC	string	See groupingId1	groupingId
analysis	groupingId5	Grouping ID 5	The identifier of the referenced subject or data grouping factor.	N	N	SP update from global TFL metadata	MOSAIC	string	See groupingId1	groupingId
analysis	groupingId6	Grouping ID 6	The identifier of the referenced subject or data grouping factor.	N	N	SP update from global TFL metadata	MOSAIC	string	See groupingId1	groupingId
analysis	groupingId7	Grouping ID 7	The identifier of the referenced subject or data grouping factor.	N	N	SP update from global TFL metadata	MOSAIC	string	See groupingId1	groupingId
analysis	groupingId8	Grouping ID 8	The identifier of the referenced subject or data grouping factor.	N	N	SP update from global TFL metadata	MOSAIC	string	See groupingId1	groupingId
analysis	groupingId9	Grouping ID 9	The identifier of the referenced subject or data grouping factor.	N	N	SP update from global TFL metadata	MOSAIC	string	See groupingId1	groupingId
analysisGroupings	id	Grouping ID	The identifier of the referenced subject or data grouping factor.	N	N	SP update from global TFL metadata	MOSAIC	string	"Freeze at left.
Free text"	GroupingFactor.id
analysisGroupings	description	Grouping Description	A detailed description of the grouping factor, e.g. subject level, data level.	N	N	SP update from global TFL metadata	MOSAIC	string	Free text	GroupingFactor.description
analysisGroupings	groupingDataset	Grouping Dataset	A reference to the dataset containing the variable upon which grouping is based	N	N	SP update from global TFL metadata	MOSAIC	string	Free text	GroupingFactor.groupingDataset
analysisGroupings	groupingVariable	Grouping Variable	A reference to the dataset variable upon which grouping is based.	N	N	SP update from global TFL metadata	MOSAIC	string	Free text	GroupingFactor.groupingVariable
analysisGroupings	dataDriven	Data Driven	Indicates whether the groups defined by the grouping are prespecified (false) or obtained from distinct data values of the groupingVariable (true).	N	N	SP update from global TFL metadata	MOSAIC	boolean	True, False	GroupingFactor.dataDriven
analysisGroupings	groupId	Group ID	The identifier of a referenced predefined group within a grouping.	N	N	SP update from global TFL metadata	MOSAIC	string	Free text	groupId
analysisGroupings	group_name	Group Name	The name for the group	N	N	SP update from global TFL metadata	MOSAIC	string	Free text	Group.name
analysisGroupings	WhereClause	Where Clause	data subset selection criteria for group, stored in YAML format	N	N	SP update from global TFL metadata	MOSAIC	string	See analysis.dataSubset_WhereClause	WhereClause Class
AnalysisSet	id	ID	Unique ID in the domain	N	N	SP update from global TFL metadata	MOSAIC	string	"Freeze at left.
Free text"	id
AnalysisSet	name	Name	Name corresponding to ID	N	N	SP update from global TFL metadata	MOSAIC	string	Free text	name
AnalysisSet	description	Description	A textual description	N	N	SP update from global TFL metadata	MOSAIC	string	Free text	description
AnalysisSet	WhereClause	Where Clause	data subset selection criteria for analysis set, stored in YAML format	N	N	SP update from global TFL metadata	MOSAIC	string	See analysis.dataSubset_WhereClause	WhereClause Class
AnalysisMethod	status	Status	The status to description the current domain, e.g. new, retired#1, retired#2	Y	Y	MDR	MDR	string	Derived by MDR. Allowed value: new, updated, retired#1, retired#2, retired#3, ...	N/A
AnalysisMethod	id	ID	Unique ID in the domain	N	N	Stat input	MOSAIC	string	"Freeze at left.
Free text"	id
AnalysisMethod	name	Name	Name corresponding to ID	N	N	Stat input	MOSAIC	string	Free text	name
AnalysisMethod	description	Description	A textual description	N	N	Stat input	MOSAIC	string	Free text	description
AnalysisMethod	ReferenceDocumentId	Reference Document ID	The identifier of the referenced document, linked from ReferenceDocument.id	N	N	Stat input	Stat input	string	See mainListOfContents.ReferenceDocumentId1	ReferenceDocumentId
AnalysisMethod	pageRefs	Reference Page	A list of references to specific parts of a document, which may be referenced as a list of one or more page numbers, a range of page numbers, or a list of named destinations in the document (e.g. bookmarks).	N	N	Stat input	Stat input	string	Free text	pageRefs
AnalysisMethod	codeTemplate	Code Template	Template programming statements used to perform the statistical analysis method	N	N	Stat input	Stat input	string	Free text, monospaced font	codeTemplate
AnalysisMethod	context	Computer Language Context	The name and version of the computer language used for the Code Template.	N	N	Stat input	Stat input	string	Free text	context
ReferenceDocument	id	ID	Unique ID in the domain, e.g. GSHELL for global TFL shell.	Y	Y	SP input	SP input	string	"Freeze at left.
Free text, required"	id
ReferenceDocument	name	Name	Name corresponding to ID	N	N	SP input	SP input	string	Free text	name
ReferenceDocument	location	Location	A path or web link indicating the location of the file.	Y	N	SP input	SP input	string	Free text	location
ReferenceDocument	description	Description	A textual description	N	N	SP input	SP input	string	Free text	description



Variable Name	Variable Label	Description	Required	Available for MOSAIC?	Type	Range	Linked Domain.Variable	Referred ARS Component	Comment
outputId	Output ID	Unique ID in the domain	Y	Y	string	<mainListOfContents.outputId>	mainListOfContents.outputId	outputId	
Title2	Title Line 2	The second line of output title, e.g. Disposition at screening	Y	N	string	<mainListOfContents.Title2>		displaySections.subSection.text where displaySections.sectionType=Title	This could be used in the future by Mosaic, at the moment we use the PDT to get this information
listItem_name	List Item Name	Items in the list. Each item may include a reference to an analysis, a reference to an output, or a sub-list.	N	N	string			OrderedListItem.name	
listItem_order	List Item Order	The ordinal of the List Item.	N	N	integer			OrderedListItem.order	
analysisId	Analysis ID	The identifier of the referenced analysis.	N	Y	string	<analysis.id>	analysis.id	analysisId	
analysis_dataset	Analysis Dataset	The name of the analysis dataset	N	Y	string	<analysis.dataset>		dataset	
analysis_variable	Analysis Variable	The name of the variable from analysis.dataset	N	Y	string	<analysis.variable>		variable	
methodId	Method ID	Unique method ID retained from analysisMethod.id	Y	Y	string	<analysis.methodId>	analysis.methodId	methodId	
operationId	Operation ID	The identifier of the referenced operation.	Y	Y	string			OperationResult.operationId	
operation_order	Operation Order	The ordinal of the operation.		Y	float			Operation.order	
resultPattern	Result Display Pattern	"The default pattern or format to apply to the result for display.
May be a textual representation of a generic result to be displayed in a table shell (e.g. XX.X) or a machine readable formatting instruction."	N	Y	string			resultPattern	
rawValue	Raw Result Value	The raw result value (e.g., with no rounding applied).	N	Y	float			rawValue	
formattedValue	Display Result Value	The result value formatted for display according to the resultPattern.	N	Y	string			formattedValue	
groupingIdX	Grouping ID X	The identifier of the referenced subject or data grouping factor.	N	Y	string	<analysisGroupings.id>	analysisGroupings.id	groupingId	X=1, 2, 3, 4, 5, 6, 7, 8, 9
groupingDatasetX	Grouping Dataset X	A reference to the dataset containing the variable upon which grouping is based	N	Y	string			GroupingFactor.groupingDataset	X=1, 2, 3, 4, 5, 6, 7, 8, 9
groupingVariableX	Grouping Variable X	A reference to the dataset variable upon which grouping is based.	N	Y	string			GroupingFactor.groupingVariable	X=1, 2, 3, 4, 5, 6, 7, 8, 9
groupIdX	Group ID X	The identifier of the group.	N	Y	string	<analysisGroupings.groupid>	analysisGroupings.groupid	groupId	X=1, 2, 3, 4, 5, 6, 7, 8, 9
group_nameX	Group Name X	The name for the group.	N	Y	string			Group.name	X=1, 2, 3, 4, 5, 6, 7, 8, 9
group_orderX	Group order X	The ordinal of the group.	N	Y	float			OrderedGroupingFactor.order	X=1, 2, 3, 4, 5, 6, 7, 8, 9
