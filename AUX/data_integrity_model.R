#' #' @title data integrity model (UNDER DEVELOPMENT)
#' #' @description data_integrity_model
#' #' @param data data frame or a single vector
#' #' @param model_name model name
#' #' @param MAX_UNIQUE max unique
#' #' @return metrics
#' data_integrity_model <- function(data, model_name, MAX_UNIQUE=35)
#' {
#'   model_selected=metadata_models %>% dplyr::filter(name==model_name)
#'
#'   if(nrow(model_selected)==0) {
#'     stop("Configuration not available")
#'   }
#'
#'   l_err_msgs=list()
#'
#'   # Run the data_integrity with custom max unique due to random forest
#'   if(model_selected$max_unique != "Inf") {
#'     MAX_UNIQUE = model_selected$max_unique
#'   }
#'
#'   san=data_integrity(data, MAX_UNIQUE = MAX_UNIQUE)
#'
#'   # Check NA in both: numerical or categorical variables
#'   if(!model_selected$allow_NA & (nrow(san$vars_num_with_NA)>0 | nrow(san$vars_cat_with_NA)>0))
#'   {
#'     vars_NA=c(san$vars_num_with_NA$variable, san$vars_cat_with_NA$variable)
#'     vars_NA=paste(vars_NA, collapse = ", ")
#'     err_msg_NA=sprintf("{NA detected} %s", vars_NA)
#'     l_err_msgs=c(l_err_msgs, msg=err_msg_NA)
#'   }
#'
#'   # Check if only numeric (categorical and other are not allow)
#'   if(model_selected$only_numeric & (length(san$vars_cat)>0 | length(san$vars_other)>0))
#'   {
#'     vars_non_num=c(san$vars_cat, san$vars_other)
#'     vars_non_num=paste(vars_non_num, collapse = ", ")
#'     err_msg_non_num=sprintf("{Non-numeric detected} %s", vars_non_num)
#'     l_err_msgs=c(l_err_msgs, msg=err_msg_non_num)
#'   }
#'
#'   # Check if character are not allowed
#'   if(!model_selected$allow_character & length(san$vars_char))
#'   {
#'     vars_char=paste(san$vars_char, collapse = ", ")
#'     err_msg_char=sprintf("{Character detected} %s", vars_char)
#'     l_err_msgs=c(l_err_msgs, msg=err_msg_char)
#'   }
#'
#'   # Check categorical variables with high cardinality
#'   if(nrow(san$vars_cat_high_card)>0)
#'   {
#'     vars_high_card=paste(san$vars_cat_high_card$variable, collapse = ", ")
#'     err_msg_high_card=sprintf("{High cardinality detected (MAX_UNIQUE > %s)} %s", MAX_UNIQUE, vars_high_card)
#'     l_err_msgs=c(l_err_msgs, msg=err_msg_high_card)
#'   }
#'
#'
#'   final_msg=""
#'   if(length(l_err_msgs)>0)
#'   {
#'     for(i in 1:length(l_err_msgs))
#'     {
#'
#'       if(str_detect(l_err_msgs[[i]], "High cardinality")) {
#'         emoji_mark=ifelse(model_selected$max_unique != Inf, emo::ji('x'), emo::ji('warning'))
#'       } else {
#'         emoji_mark=emo::ji('x')
#'       }
#'
#'       final_msg=str_c(final_msg, str_c(emoji_mark, l_err_msgs[[i]], sep=" "), sep = "\n")
#'       data_ok=F
#'     }
#'
#'   } else {
#'     final_msg=str_c(str_c(emo::ji('white_check_mark') , "Data integrity ok!", sep=" "), sep = "\n")
#'     data_ok=T
#'   }
#'
#'   # Creating S3 object
#'   obj_chk=list(final_msg=final_msg, data_ok=data_ok, model_selected=model_selected, checked=san)
#'   class(obj_chk)="integritymodel"
#'
#'   return(obj_chk)
#'
#' }
#'
#' #' @export
#' print.integritymodel <- function(object) {
#'   cat(object$final_msg)
#' }
#'
#' #' @export
#' summary.integritymodel <- function(object) {
#'   return(object$data_ok)
#' }
#'
