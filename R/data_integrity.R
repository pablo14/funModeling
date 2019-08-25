#' @title Get a summary for the given data frame (o vector).
#' @description For each variable it returns: Quantity and percentage of zeros (q_zeros and p_zeros respectevly). Same metrics for NA values (q_NA/p_na), and infinite values (q_inf/p_inf). Last two columns indicates data type and quantity of unique values.
#' @param data data frame, tibble or a single vector
#' @examples
#' status(heart_disease)
#' @return Tibble with metrics
#' @export
status <- function (data)
{
  if (mode(data) %in% c("logical", "numeric", "complex", "character")) {
    data = data.frame(var = data)
    input = "var"
  }

  status_res = tibble(
    q_zeros = sapply(data, function(x) sum(x == 0, na.rm = T)),
    p_zeros = sapply(data, function(x) sum(x == 0, na.rm = T))/nrow(data),
    q_na = sapply(data, function(x) sum(is.na(x))),
    p_na = sapply(data, function(x) sum(is.na(x)))/nrow(data),
    q_inf = sapply(data, function(x) sum(is.infinite(x))),
    p_inf = sapply(data, function(x) sum(is.infinite(x)))/nrow(data),
    type = sapply(data, get_type_v),
    unique = sapply(data, function(x) sum(!is.na(unique(x))))
    )


  status_res=status_res %>% mutate(unique=ifelse(type=="logical", 2, unique))

  status_res$variable = colnames(data)
  status_res = status_res %>% select(variable, everything())

  status_res
}

#' @title Data integrity
#' @description A handy function to return different vectors of variable names aimed to quickly filter NA, categorical (factor / character), numerical and other types (boolean, date, posix).
#' It also returns a vector of variables which have high cardinality.
#' It returns an 'integrity' object, which has: 'status_now' (comes from status function), and 'resutls' list, following elements can be found:
#' vars_cat: Vector containing the categorical variables names (factor or character)
#' vars_num: Vector containing the numerical variables names
#' vars_char: Vector containing the character variables names
#' vars_factor: Vector containing the factor variables names
#' vars_other: Vector containing the other variables names (date time, posix and boolean)
#' vars_num_with_NA: Summary table for numerical variables with NA
#' vars_cat_with_NA: Summary table for categorical variables with NA
#' vars_cat_high_card: Summary table for  high cardinality variables (where thershold = MAX_UNIQUE parameter)
#'
#' @param data data frame or a single vector
#' @param MAX_UNIQUE max unique threshold to flag a categorical variable as a high cardinality one. Normally above 35 values it is needed to reduce the number of different values.
#' @examples
#' # Example 1:
#' data_integrity(heart_disease)
#' # Example 2:
#' # changing the default minimum threshold to flag a variable as high cardiniality
#' data_integrity(data=data_country, MAX_UNIQUE=50)
#' @return An 'integrity' object.
#' @export
data_integrity <- function(data, MAX_UNIQUE=35)
{
  stat=status(data)

  vars_with_na=stat %>% dplyr::filter(q_na>0) %>%
    select(variable, q_na, p_na)

  num_data_type=c('numeric', 'integer')
  cat_data_type=c('factor', 'character')

  vars_cat = stat %>% dplyr::filter(type %in% cat_data_type) %>%
    pull(variable)

  vars_num = stat %>% dplyr::filter(type %in% num_data_type) %>%
    pull(variable)

  vars_char = stat %>% dplyr::filter(type == "character") %>%
    pull(variable)

  vars_factor = stat %>% filter(type == "factor") %>%
    pull(variable)

  vars_other = stat %>% dplyr::filter(!(type %in% c(num_data_type, cat_data_type))) %>%
    pull(variable)

  vars_num_with_NA=stat %>% dplyr::filter(q_na>0, variable %in% vars_num) %>%
    select(variable, q_na, p_na)

  vars_cat_with_NA=stat %>%
  	dplyr::filter(q_na>0, variable %in% vars_cat) %>%
    select(variable, q_na, p_na) %>%
    arrange(-q_na)

  vars_cat_high_card=stat %>%
  	dplyr::filter(variable %in% vars_cat, unique>MAX_UNIQUE) %>%
    select(variable, unique) %>%
    arrange(-unique)

  # chequear target, cant valores unicos y tipo
  integrity_results=list(
  				 vars_num_with_NA=vars_num_with_NA,
           vars_cat_with_NA=vars_cat_with_NA,
           vars_cat_high_card=vars_cat_high_card,
           MAX_UNIQUE=MAX_UNIQUE,
           vars_cat=vars_cat,
           vars_num=vars_num,
           vars_char=vars_char,
           vars_factor=vars_factor,
           vars_other=vars_other
           )

  obj_integ=list(results=integrity_results, status_now=stat)

  class(obj_integ)="integrity"
  return(obj_integ)
}

#' @export
print.integrity <- function(obj_integ) {
	print(obj_integ$results)
}

#' @export
summary.integrity <- function(obj_integ) {

	l_msgs=list()

	res=obj_integ$results
	if(nrow(res$vars_num_with_NA)>0)
	{
		vars_num_na=paste(res$vars_num_with_NA$variable, collapse = ", ")

		err_msg_vars_num=str_c(emo::ji("straight_ruler"), emo::ji("hole"), sprintf("{Numerical with NA} %s",  vars_num_na), sep=" ")

		l_msgs=c(l_msgs, msg=err_msg_vars_num)
	}

	if(nrow(res$vars_cat_with_NA)>0)
	{
		vars_cat_na=paste(res$vars_cat_with_NA$variable, collapse = ", ")
		err_msg_vars_cat=str_c(emo::ji("bar_chart"), emo::ji("hole"), sprintf("{Categorical with NA} %s",  vars_cat_na), sep=" ")

		l_msgs=c(l_msgs, msg=err_msg_vars_cat)
	}

	if(nrow(res$vars_cat_high_card)>0)
	{
		vars_high_card=paste(res$vars_cat_high_card$variable, collapse = ", ")

		err_msg_high_card=str_c(emo::ji("up_arrow"), emo::ji("office"), sprintf("{High cardinality (MAX_UNIQUE > %s)} %s", res$MAX_UNIQUE, vars_high_card), sep=" ")

		l_msgs=c(l_msgs, msg=err_msg_high_card)
	}

	final_msg=""
	if(length(l_msgs)>0)
	{
		for(i in 1:length(l_msgs))
		{
			final_msg=str_c(final_msg, l_msgs[[i]], sep = "\n")
		}

	} else {
		final_msg=str_c(str_c(emo::ji('white_check_mark') , "No NA nor high cardinality!", sep=" "), sep = "\n")
	}

	cat(final_msg)
}


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
