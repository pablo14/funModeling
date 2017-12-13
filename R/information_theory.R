#' @title Convert every column in a data frame to character
#' @description It converts all the variables present in 'data' to character. Criteria conversion is based on
#' two functions, \code{\link{discretize_get_bins}} plus \code{\link{discretize_df}}, which will discretize
#' all the numerical variables based on equal frequency criteria, with the number of bins equal to 'n_bins'.
#' This only applies for numerical variables which unique valuesare more than 'n_bins' parameter.
#' After this step, it may happen that variables remain non-character, so these variables will be converting
#' directly into character.
#'
#' @param
#' @examples
#' \dontrun{
#'
#' }
#' @return
#' @export
convert_df_to_char <- function(data, n_bins)
{
	# Discretizing numerical variables
	d_cuts=discretize_get_bins(data = data, n_bins = n_bins)
	data_cat=discretize_df(data = data, data_bins = d_cuts, stringsAsFactors = F)

	# Converting remaining variables
	data_cat_2=data_cat %>% mutate_all(as.character)

	return(data_cat_2)
}

#' @title Concatenate 'N' variables
#' @description Concatenate 'N' variables using the char pipe: <|>.
#' This function is used when there is the need of measuring the mutual information and/or the information
#' gain between 'N' input variables an against a target variable. This function makes sense when it is used based on categorical data.
#' @param data data frame containing the two variables to concatenate
#' @param vars character vector containing all variables to concatenate
#' @examples
#' \dontrun{
#' new_variable=concatenate_n_vars(mtcars, c("cyl", "disp"))
#' # Checking new variable
#' head(new_variable)
#' }
#' @return
#' @export
concatenate_n_vars <- function(data, vars)
{
	df=data %>% select(one_of(vars))

	new_col=apply(df, 1, function(x) paste(x, collapse = " | ") )

	return(new_col)
}

#' @title Calculate entropy between two variables
#' @description It calculates the entropy between two categorical variables using log2.
#' This log2 is mentioned in most of the Claude Shannon bibliography.
#' Input vectors, 'x' and 'y' can be numeric or character. REVISAR QUE CALCULO HAGO ACA
#' @param x input numeric vector
#' @param y input numeric vector
#' @examples
#' \dontrun{
#' entropy_2(heart_disease$gender, heart_disease$chest_pain)
#' }
#' @return
#' @export
entropy_2 <- function(x, y)
{
	# converting x input into frequency table
	tbl_x=table(x)

	# cell percentages (distribution)
	probs_x=prop.table(tbl_x)

	tbl=table(x, y)

	df_tbl=as.data.frame.matrix(tbl)
	res_entropy=data.frame(t(df_tbl)) %>% mutate_all(funs(entropy(., unit = "log2"))) %>% head(.,1)

	# computing total entropy
	total_en=sum(probs_x*res_entropy)

	return(total_en)
}

#' @title
#' @description
#'
#' @param
#' @examples
#' \dontrun{
#'
#' }
#' @return
#' @export
information_gain <- function(x, y)
{
	tbl=table(y)
	en_y=entropy(tbl, unit = "log2")
	en=entropy_2(x, y)
	info_gain=en_y-en

	return(info_gain)
}

#' @title
#' @description
#'
#' @param
#' @examples
#' \dontrun{
#'
#' }
#' @return
#' @export
gain_ratio <- function(input, target)
{
	ig=information_gain(input, target)
	split=information_gain(input, input)

	gain_r=ig/split

	return(gain_r)

}

#' @title
#' @description
#'
#' @param
#' @examples
#' \dontrun{
#'
#' }
#' @return
#' @export
var_rank_info <- function(data, target)
{
	nam=colnames(data)
	nam=nam[nam!=target]

	df_res=data.frame(var=NULL, en=NULL, mi=NULL, ig=NULL, stringsAsFactors = F)

	for(var in nam)
	{
		r=infor_magic(data[[var]], data[[target]])
		df_res=rbind(df_res, data.frame(var=var, en=r[1],mi=r[2],ig=r[3], gr=r[4]))
	}

	df_res$var=as.character(df_res$var)
	return(df_res %>% arrange(-gr))
}

#' @title
#' @description
#'
#' @param
#' @examples
#' \dontrun{
#'
#' }
#' @return
#' @export
infor_magic=function(input, target)
{
	tbl_2v=table(input, target)

	# computing maximum total entropy
	en=round(entropy::entropy(tbl_2v, unit = "log2") ,3)

	# other way of computing max chaos...
	# log(nrow(tbl_2v)*ncol(tbl_2v))

	# mutual entropy based on 'entropy' package
	mi=round(entropy::mi.empirical(tbl_2v, unit = "log2"),3)

	# mutual information or entroy based on 'infotheo' package
	# mi=mutinformation(data[[col1]],data[[col2]])

	# Computing information gain between input and target variable
	ig=information_gain(input, target)

	# Computing information gain between input and target variable.
	gr=gain_ratio(input, target)

	return(c(en, mi, ig , gr))
}

#' @title
#' @description
#'
#' @param
#' @examples
#' \dontrun{
#'
#' }
#' @return
#' @export
var_rank_variation_coef <- function(data, target, sample_size=0.3, repeats=5)
{
	df_gr=data.frame(i=NULL, var=NULL,  gr=NULL)

	for(i in 1:repeats)
	{
		ix=get_sample(data, percentage_tr_rows = sample_size, seed = i)
		d_samp=data[ix, ]
		df_info=var_rank_info(d_samp, target) %>% select(var, gr)

		df_gr=rbind(df_gr, data.frame(df_info, i=i))
	}

	stats=group_by(df_gr, var) %>% summarise(variation_coef=sd(gr)/mean(gr)) %>% arrange(-variation_coef)
}

