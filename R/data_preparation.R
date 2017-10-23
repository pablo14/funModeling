#' @title Reduce cardinality in categorical variable by automatic grouping
#' @description Reduce the cardinality of an input variable based on a target -binary by now- variable based on attribitues of accuracy and representativity, for both input and target variable. It uses a cluster model to create the new groups. Full documentation can be found at: <http://http://livebook.datascienceheroes.com/data_preparation/high_cardinality_predictive_modeling.html/>
#' @param data data frame source
#' @param input categorical variable indicating
#' @param target string of the variable to optimize the re-grouping
#' @param n_groups number of groups for the new category based on str_input, normally between 3 and 10.
#' @param model is the clustering model used to create the grouping, supported models: "kmeans" (default) or hclust.
#' @param seed optional, random number used internally for the k-means, changing this value will change the model
#' @examples
#' \dontrun{
#' # Reducing quantity of countries based on has_flu variable
#' auto_grouping(data=data_country, input='country', target="has_flu", n_groups=8)
#' }
#' @return A list containing 3 elements: recateg_results which contains the description of the target variable with the new groups;
#' df_equivalence is a data frame containing the str_input category and the new category; fit_cluster which is the cluster model used to do the re-grouping
#' @export
auto_grouping <- function(data, input, target, n_groups, model="kmeans", seed=999)
{
	data[, input]=as.character(data[, input])

	df_categ=categ_analysis(data, input , target)

	d=select_(df_categ, "perc_target",  "perc_rows")

	set.seed(seed)
	if(model=="kmeans") {
		fit_cluster=kmeans(scale(data.frame(d)), n_groups)
		# Checking results: it_cluster$centers;fit_cluster$size
		cluster_vec=fit_cluster$cluster
		} else {
			# hclust
		fit_cluster=hclust(dist(scale(data.frame(d))))
		cluster_vec=cutree(fit_cluster, k=n_groups)
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


dis=function(x, n_bins)
{
	#res=discretize(x, "frequency", categories = n_bins)
	cuts=get_cuts_processed(x, n_bins)
	res=discretize(x, "fixed", categories = cuts)
	#res=gsub(" ", "", res, fixed = TRUE)

	return(res)
}



get_cuts_processed <- function(x, n_bins)
{
	cuts=discretize(x, "frequency", onlycuts = T, categories = n_bins)

	cuts[1]=-Inf
	cuts[length(cuts)]=Inf

	return(cuts)
}


dis_bins=function(x, n_bins=5, save_cuts=F)
{
	cuts=get_cuts_processed(x, n_bins)
	res=paste(cuts, collapse = "|")

	names(x)
	colnames(x)

	return(res)
}

df_save_cuts <- function(data, n_bins)
{
	# Parte 2a: solo cuts
	vars_num=df_status(data,print_results = F) %>% filter(type %in% c("integer","numeric")) %>% .$variable

	data_3=data %>% select(one_of(vars_num)) %>% mutate_all(funs(dis_bins(., n_bins))) %>% head(.,1)

	write.table(data_3, file = "cuts.txt", col.names = T, row.names=FALSE, fileEncoding="UTF-8", quote=F, sep="\t")
}


dis_recover <- function(x, cuts, stringsAsFactors)
{
	cuts_v=as.numeric(unlist(strsplit(cuts, '[|]')))
	x[x<min(cuts_v)]=min(cuts_v)
	x[x>max(cuts_v)]=max(cuts_v)

	res=discretize(x, "fixed", categories = cuts_v)

	if(!stringsAsFactors)
	{
		res=as.character(res)
	}

	return(res)
}


df_recover_cuts <- function(data, file_name, stringsAsFactors=F)
{
	## Parte 2b: recover cuts
	data_cuts=read.delim(file=file_name, stringsAsFactors = F)
	vars_num=data_cuts$variable

	for(i in cols_to_conv)
	{
		v_orig=data[[i]]
		v_cuts=filter(data_cuts, variable==i)  %>% .$cuts
		v_res=dis_recover(v_orig, v_cuts, stringsAsFactors)

		data[[i]]=v_res
	}

	if(stringsAsFactors)
	{
		data_2b=data %>% mutate_at(vars(vars_num), conv_factor)
		data_3=data_2b %>% mutate_at(vars(vars_num), funs(factor(replace(., is.na(.), "NA."))))
	} else {
		data_3=data %>% mutate_at(vars(vars_num), funs(ifelse(is.na(.), "NA.", .)))
	}

	print(sprintf("Variables processed: %s", paste(vars_num, collapse = ", ")))

	return(data_3)
}


df_categorical <- function(data, input=NULL, n_bins=5, save_cuts=F, save_as=NULL, stringsAsFactors=F)
{
	vars_num=df_status(data, print_results = F) %>% filter(type %in% c("integer","numeric"), unique>n_bins) %>% .$variable

	## If str_input then runs for all variables
	if(!missing(input))
	{
		vars_num=vars_num[vars_num %in% input]
	}

	data_2=data %>% mutate_at(.vars=vars(vars_num), .funs = funs(dis(., n_bins)))

	if(save_cuts)
	{
		d_cuts=sapply(select(data, one_of(vars_num)), function(x) dis_bins(x, n_bins)) %>% as.data.frame(.)
		d_cuts$variable=rownames(d_cuts)
		d_cuts=rename(d_cuts, cuts='.') %>% select(variable, cuts)
		write.table(d_cuts, file = save_as, col.names = T, row.names=FALSE, fileEncoding="UTF-8", quote=F, sep="\t")
	}

	if(stringsAsFactors)
	{
		data_2b=data_2 %>% mutate_at(vars(vars_num), conv_factor)
		data_3=data_2b %>% mutate_at(vars(vars_num), funs(factor(replace(., is.na(.), "NA."))))
	} else {
		data_2b=data_2 %>% mutate_at(vars(vars_num), as.character)
		data_3=data_2b %>% mutate_at(vars(vars_num), funs(ifelse(is.na(.), "NA.", .)))
	}

	print(sprintf("Variables processed: %s", paste(vars_num, collapse = ", ")))
	return(data_3)
}

conv_factor <- function(x)
{
	levels(x)=c(levels(x), "NA.")
	new_x=factor(x, levels = levels(x))

	return(new_x)
}
