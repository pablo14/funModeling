#' @title Reduce cardinality in categorical variable
#' @description Reduce the cardinality of an input variable based on a target -binary for now- variable based on attribitues of
#' accuracy and representativity, for both input and target variable. It uses a cluster model to create the new groups
#' @param data data frame source
#' @param str_input categorical variable indicating
#' @param str_target string of the variable to optimize the re-grouping
#' @param n_groups number of groups for the new category based on str_input, normally between ~3 and ~10.
#' @param seed optional, random number used internally for the k-means, changing this value will change the model
#' @examples
#' # Reducing quantity of countries based on has_flu variable
#' reduce_cardinallity(data=data_country, str_input='country', str_target="has_flu")
#' @return A list containing 3 elements: recateg_results which contains the description of the target variable with the new groups;
#' df_equivalence is a data frame containing the str_input category and the new category; fit_cluster which is the cluster model used to do the re-grouping
#' @export
reduce_cardinality <- function(data, str_input, str_target, n_groups, seed=999)
{
	# str_target="has_flu";str_input='country';data=data_country
	data[,str_input]=as.character(data[,str_input])

	df_categ=categ_analysis(data, str_input , str_target)

	d=select_(df_categ, paste("sum_", str_target, sep=""), paste("mean_", str_target, sep=""), "perc_rows")

	set.seed(seed)
	fit_cluster=kmeans(scale(data.frame(d)), n_groups)
	fit_cluster$centers;fit_cluster$size

	## Equivalence table
	var_rec=paste(str_input, "rec", sep="_")
	df_categ[, var_rec]=paste("group_", fit_cluster$cluster, sep = "")

	## See new profiling based on new groups
	data_rec=merge(select_(data, str_input, str_target), select_(df_categ, str_input, var_rec), by=str_input)
	recateg_results=categ_analysis(data_rec, var_rec, str_target)

	l_res=list()
	l_res$recateg_results=recateg_results
	l_res$df_equivalence=arrange_(unique(select_(data_rec, str_input, var_rec)), var_rec)
	l_res$fit_cluster=fit_cluster

	return(l_res)
}
