#' @title Reduce cardinality in categorical variable by automatic grouping
#' @description Reduce the cardinality of an input variable based on a target -binary by now- variable based on attribitues of accuracy and representativity,
#'  for both input and target variable. It uses a cluster model to create the new groups.
#'   Full documentation can be found at:
#'   \url{https://livebook.datascienceheroes.com/data-preparation.html#high_cardinality_predictive_modeling}
#' @param data data frame source
#' @param input categorical variable indicating
#' @param target string of the variable to optimize the re-grouping
#' @param n_groups number of groups for the new category based on input, normally between 3 and 10.
#' @param model is the clustering model used to create the grouping, supported models: "kmeans" (default) or "hclust" (hierarchical clustering).
#' @param seed optional, random number used internally for the k-means, changing this value will change the model
#' @examples
#' \dontrun{
#' # Reducing quantity of countries based on has_flu variable
#' auto_grouping(data=data_country, input='country', target="has_flu", n_groups=8)
#' }
#' @return A list containing 3 elements: recateg_results which contains the description of the target variable with the new groups;
#' df_equivalence is a data frame containing the input category and the new category; fit_cluster which is the cluster model used to do the re-grouping
#' @export
auto_grouping <- function(data, input, target, n_groups, model="kmeans", seed=999)
{
	data[[input]]=as.character(data[[input]])

	df_categ=categ_analysis(data, input , target)

	d=select_(df_categ, "perc_target",  "perc_rows")

	set.seed(seed)
	if(model=="kmeans") {
		fit_cluster=kmeans(scale(data.frame(d)), n_groups)
		# Checking results: it_cluster$centers;fit_cluster$size
		cluster_vec=fit_cluster$cluster
		} else if(model=="hclust"){
			# hclust
		fit_cluster=hclust(dist(scale(data.frame(d))))
		cluster_vec=cutree(fit_cluster, k=n_groups)
	} else {
		stop("Parameter 'model' can be 'kmeans' or 'hclust'")
	}


	## Equivalence table
	var_rec=paste(input, "rec", sep="_")
	df_categ[, var_rec]=paste("group_", cluster_vec, sep = "")

	## See new profiling based on new groups
	data_rec=merge(select_(data, input, target), select_(df_categ, input, var_rec), by=input)
	recateg_results=categ_analysis(data_rec, var_rec, target)

	l_res=list()
	l_res$recateg_results=recateg_results
	l_res$df_equivalence=arrange_(unique(select_(data_rec, input, var_rec)), var_rec)
	l_res$fit_cluster=fit_cluster

	return(l_res)
}

#' @title Transform a variable into the [0-1] range
#' @description Range a variable into [0-1], assigning 0 to the min and 1 to the max of the input variable. All NA values will be removed.
#' @param var numeric input vector
#' @examples
#' range01(mtcars$cyl)
#' @return vector with the values scaled into the 0 to 1 range
#' @export
range01 <- function(var)
{
	return((var-min(var, na.rm=T))/(max(var, na.rm=T)-min(var, na.rm=T)))
}

