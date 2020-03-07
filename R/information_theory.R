#' @title Computes the entropy between two variables
#' @description It calculates the entropy between two categorical variables using log2.
#' This log2 is mentioned in most of the Claude Shannon bibliography.
#' Input/target can be numeric or character.
#' @param input numeric/character vector
#' @param target numeric/character vector
#' @examples
#' \dontrun{
#' # Measuring entropy between input and target variable
#' entropy_2(input=data_golf$outlook, target=data_golf$play_golf)
#' }
#' @return Entropy measured in bits
#' @export
entropy_2 <- function(input, target)
{
	# converting x input into frequency table
	tbl_input=table(input)

	# cell percentages (distribution)
	probs_input=prop.table(tbl_input)

	tbl=table(input, target)

	# get partial entropy
	df_tbl=as.data.frame.matrix(tbl)
	res_entropy=data.frame(t(df_tbl)) %>% mutate_all(funs(entropy(., unit = "log2"))) %>% head(.,1)

	# computing total entropy
	total_en=sum(probs_input*res_entropy)

	return(total_en)
}

#' @title Information gain
#' @description Computes the information gain between an 'input' and 'target' variable (using log2). In general terms, the higher the more predictable the input is.
#' @param input numeric/character vector
#' @param target numeric/character vector
#' @examples
#' \dontrun{
#' information_gain(input=data_golf$outlook, target=data_golf$play_golf)
#' }
#' @return information gain
#' @export
information_gain <- function(input, target)
{
	tbl=table(target)
	en_y=entropy::entropy(tbl, unit = "log2")
	en=entropy_2(input, target)
	info_gain=en_y-en

	return(info_gain)
}

#' @title Gain ratio
#' @description Computes the information gain between an 'input' and 'target' variable (using log2). Similar to information gain but less sensitive to high cardinality variables.
#' @param input numeric/character vector
#' @param target numeric/character vector
#' @examples
#' \dontrun{
#' gain_ratio(input=data_golf$outlook, target=data_golf$play_golf)
#' }
#' @return gain ratio
#' @export
gain_ratio <- function(input, target)
{
	ig=information_gain(input, target)
	split=information_gain(input, input)

	gain_r=ig/split

	return(gain_r)
}

#' @title Importance variable ranking based on information theory
#' @description Retrieves a data frame containing several metrics related to information theory.
#' Metrics are: entropy (en), mutual information (mi), information gain (ig) and gain ratio (gr).
#'
#' @param data input data frame, all the variables will be evaluated against the variable defined in 'target' parameter
#' @param target string variable name containing the output variable.
#' @examples
#' \dontrun{
#' var_rank_info(data_golf, "play_golf")
#' }
#' @return data frame ordered by gain ratio metric
#' @export
var_rank_info <- function(data, target)
{
	nam=colnames(data)
	nam=nam[nam!=target]

	df_res=data.frame(var=NULL, en=NULL, mi=NULL, ig=NULL, gr=NULL, stringsAsFactors = F)

	for(var in nam)
	{
		r=infor_magic(data[[var]], data[[target]])
		df_res=rbind(df_res, data.frame(var=var, en=r[1], mi=r[2], ig=r[3], gr=r[4]))
	}

	df_res$var=as.character(df_res$var)

	df_res=df_res %>% arrange(-gr)

	return(df_res)
}

#' @title Computes several information theory metrics between two vectors
#' @description It retrieves the same as \code{\link{var_rank_info}} but receiving two vectors.
#' Metrics are: entropy (en), mutual information (mi), information gain (ig) and gain ratio (gr).
#'
#' @param input vector to be evaluated against the variable defined in 'target' parameter
#' @param target vector containing the output variable.
#' @examples
#' \dontrun{
#' infor_magic(data_golf$outlook, data_golf$play_golf)
#' }
#' @return Matrix of 1 row and 4 columns, where each column represent the mentioned metrics
#' @export
infor_magic <- function(input, target)
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

	res=c(en=en, mi=mi, ig=ig , gr=gr)

	return(res)
}

