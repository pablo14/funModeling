#' @export
contr.ltfr <- caret::contr.ltfr

#' @title xxx
#' @description xxx
#'
#' @param df xxx

#' @return xxx
#' @export
one_hot_encoding <- function(df, fullRank=F)
{
	dmy = caret::dummyVars(formula=" ~ .", data = df, fullRank=fullRank)
	dummy_data = data.frame(predict(dmy, newdata = df))
	dummy_data[is.na(dummy_data)] = 0

	return(dummy_data)
}

#' @export
data_to_num <- function(data, target=NULL, fullRank=F)
{
	#data=heart_disease
	#target="has_heart_disease"
	#library(tidyverse)

	## removing target from one hot
	if(!missing(target))
	{
		df_to_prep=select(data, -target)
	} else {
		df_to_prep=data
	}

	status=df_status(df_to_prep, print_results = F)

	## NA treatment
	var_num_NA=filter(status, q_na>0, type %in% c("numeric", "integer")) %>% pull(variable)

	if(length(var_num_NA)>0)
	{
		NUM_BINS=5
		# df_to_prep[1,'oldpeak']=NA
		d_bins=discretize_get_bins(data = df_to_prep, n_bins = NUM_BINS, input = var_num_NA)

		if(length(d_bins)>0)
		{
			df_disc=suppressMessages(discretize_df(df_to_prep, d_bins, stringsAsFactors = F))
		}
	}

	## removing categorical variables with more than MAX_CATEGORIES
	MAX_CATEGORIES=50

	status2=status  %>%
		mutate(cat_to_prep=ifelse(unique > MAX_CATEGORIES & type %in% c('factor', 'character'), 1, 0))

	## reporting the others
	vars_high_card=filter(status2, cat_to_prep==1) %>% pull(variable)
	if(length(vars_high_card)>0)
	{
		message(sprintf("Skipping high cardinallity variables (> MAX_CATEGORIES): %s",	paste(vars_high_card, collapse = ', ')))
	}

	## keeping valid categorical variables
	df_cat_onehot=select(df_to_prep, filter(status2, cat_to_prep==0) %>% pull(variable))

	## One hot
	if(nrow(df_cat_onehot)>0)
	{
		df_onehot=funModeling::one_hot_encoding(df_cat_onehot)
	}

	## Adding discretized numeric variable
	if(exists("df_disc") & exists("df_onehot"))
	{
		d1=cbind(df_disc, df_onehot)
	} else if(!exists("df_onehot")) {
		d1=df_disc
	} else {
		d1=df_onehot
	}

	if(nrow(d1)==0) stop("No data.")

	## Adding the target if exists
	if(!missing(target))
	{
		d1=cbind(d1, select(data, target))
	}

	return(d1)
}


# prep_categorical(

# que pasa si es categ > 50 y no es tgt binario



