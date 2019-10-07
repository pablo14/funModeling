#' @title Get a summary for the given data frame (o vector).
#' @description For each variable it returns: Quantity and percentage of zeros (q_zeros and p_zeros respectevly). Same metrics for NA values (q_NA/p_na), and infinite values (q_inf/p_inf). Last two columns indicates data type and quantity of unique values.
#' 'status' function is the evolution of 'df_status'. Main change is to have the decimal points as it is, except in percentage. For example now p_na=0.04 means 4% in df_status.
#' This time it's easier to embbed in a data process flow and to take actions based on this number.
#' @param data data frame, tibble or a single vector
#' @examples
#' status(heart_disease)
#' @return Tibble with metrics
#' @export
status <- function (data)
{
  if (mode(data) %in% c("logical", "numeric", "complex", "character")) {
    data = data.frame(var = data)
  }

  status_res = data.frame(
    q_zeros = sapply(data, function(x) sum(x == 0, na.rm = T)),
    p_zeros = sapply(data, function(x) sum(x == 0, na.rm = T))/nrow(data),
    q_na = sapply(data, function(x) sum(is.na(x))),
    p_na = sapply(data, function(x) sum(is.na(x)))/nrow(data),
    q_inf = sapply(data, function(x) sum(is.infinite(x))),
    p_inf = sapply(data, function(x) sum(is.infinite(x)))/nrow(data),
    type = sapply(data, get_type_v),
    unique = sapply(data, function(x) sum(!is.na(unique(x))))
    )


  status_res=status_res %>% dplyr::mutate(unique=ifelse(type=="logical", 2, unique))

  status_res$variable = colnames(data)
  status_res = status_res %>% dplyr::select(variable, everything())

  return(status_res)
}




#' @title Data integrity
#' @description A handy function to return different vectors of variable names aimed to quickly filter NA, categorical (factor / character), numerical and other types (boolean, date, posix).
#' It also returns a vector of variables which have high cardinality.
#' It returns an 'integrity' object, which has: 'status_now' (comes from status function), and 'results' list, following elements can be found:
#'
#' vars_cat: Vector containing the categorical variables names (factor or character)
#'
#' vars_num: Vector containing the numerical variables names
#'
#' vars_char: Vector containing the character variables names
#'
#' vars_factor: Vector containing the factor variables names
#'
#' vars_other: Vector containing the other variables names (date time, posix and boolean)
#'
#' vars_num_with_NA: Summary table for numerical variables with NA
#'
#' vars_cat_with_NA: Summary table for categorical variables with NA
#'
#' vars_cat_high_card: Summary table for  high cardinality variables (where thershold = MAX_UNIQUE parameter)
#'
#' vars_one_value: Vector containing the variables names with 1 unique different value
#'
#' Explore the NA and high cardinality variables by doing summary(integrity_object), or a full summary by doing print(integrity_object)
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
  	dplyr::select(variable, q_na, p_na)

  num_data_type=c('numeric', 'integer')
  cat_data_type=c('factor', 'character')

  vars_cat = stat %>% dplyr::filter(type %in% cat_data_type) %>%
    pull(variable)

  vars_num = stat %>% dplyr::filter(type %in% num_data_type) %>%
    pull(variable)

  vars_char = stat %>% dplyr::filter(type == "character") %>%
    pull(variable)

  vars_factor = stat %>% dplyr::filter(type == "factor") %>%
    pull(variable)

  vars_other = stat %>% dplyr::filter(!(type %in% c(num_data_type, cat_data_type))) %>%
    pull(variable)

  vars_num_with_NA=stat %>% dplyr::filter(q_na>0, variable %in% vars_num) %>%
  	dplyr::select(variable, q_na, p_na)

  vars_cat_with_NA=stat %>%
  	dplyr::filter(q_na>0, variable %in% vars_cat) %>%
  	dplyr::select(variable, q_na, p_na) %>%
    arrange(-q_na)

  vars_cat_high_card=stat %>%
  	dplyr::filter(variable %in% vars_cat, unique>MAX_UNIQUE) %>%
  	dplyr::select(variable, unique) %>%
    arrange(-unique)

  vars_one_value=stat %>%
  	dplyr::filter(unique==1) %>% pull(variable)


  # chequear target, cant valores unicos y tipo
  integrity_results=list(
  				 vars_num_with_NA=vars_num_with_NA,
           vars_cat_with_NA=vars_cat_with_NA,
           vars_cat_high_card=vars_cat_high_card,
           MAX_UNIQUE=MAX_UNIQUE,
  				 vars_one_value=vars_one_value,
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
print.integrity <- function(x, ...) {
	print(x$results)
}

#' @export
summary.integrity <- function(object, ...) {

	l_msgs=list()

	res=object$results
	if(nrow(res$vars_num_with_NA)>0)
	{
		vars_num_na=paste(res$vars_num_with_NA$variable, collapse = ", ")

		err_msg_vars_num=str_c(cli::symbol$circle_dotted, sprintf("{Numerical with NA} %s",  vars_num_na), sep=" ")

		l_msgs=c(l_msgs, msg=err_msg_vars_num)
	}

	if(nrow(res$vars_cat_with_NA)>0)
	{
		vars_cat_na=paste(res$vars_cat_with_NA$variable, collapse = ", ")
		err_msg_vars_cat=str_c(cli::symbol$circle_dotted, sprintf("{Categorical with NA} %s",  vars_cat_na), sep=" ")

		l_msgs=c(l_msgs, msg=err_msg_vars_cat)
	}

	if(length(res$vars_one_value)>0)
	{
		vars_one_value=paste(res$vars_one_value, collapse = ", ")
		err_msg_one_value=str_c(cli::symbol$bullet, sprintf("{One unique value} %s",  vars_one_value), sep=" ")

		l_msgs=c(l_msgs, msg=err_msg_one_value)
	}

	if(nrow(res$vars_cat_high_card)>0)
	{
		vars_high_card=paste(res$vars_cat_high_card$variable, collapse = ", ")

		err_msg_high_card=str_c(cli::symbol$circle_circle, sprintf("{High cardinality (MAX_UNIQUE > %s)} %s", res$MAX_UNIQUE, vars_high_card), sep=" ")

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
		final_msg=str_c(str_c(cli::symbol$tick , "No num/cat with NA. No high card. No one-value var.", sep=" "), sep = "\n")
	}

	cat(final_msg)
}


