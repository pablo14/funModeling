#' @title xx
#' @description xxx
#' @param data data frame source
#' @param data_cuts xx
#' @param stringsAsFactors xx
#' @examples
#' \dontrun{
#' # Reducing quantity of countries based on has_flu variable
#' xxx
#' }
#' @return xxx
#' @export
discretize_df <- function(data, data_bins, stringsAsFactors=F)
{
	## Parte 2b: recover cuts
	vars_num=data_bins$variable

	for(i in vars_num)
	{
		v_orig=data[[i]]
		v_bins=filter(data_bins, variable==i)  %>% .$cuts
		v_res=dis_recover(v_orig, v_bins, stringsAsFactors)

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

#' @title xx
#' @description xxx
#' @param data data frame source
#' @param input xx
#' @param n_bins XX
#' @param stringsAsFactors xx
#' @examples
#' \dontrun{
#' # Reducing quantity of countries based on has_flu variable
#' xxx
#' }
#' @return xxx
#' @export
discretize_get_bins <- function(data, input=NULL, n_bins=5, stringsAsFactors=F)
{
	vars_num=df_status(data, print_results = F) %>% filter(type %in% c("integer","numeric"), unique>n_bins) %>% .$variable

	## If str_input then runs for all variables
	if(!missing(input))
	{
		vars_num=vars_num[vars_num %in% input]
	}

	d_bins=sapply(select(data, one_of(vars_num)), function(x) dis_bins(x, n_bins)) %>% as.data.frame(.)
	d_bins$variable=as.character(rownames(d_bins))
	d_bins=rename(d_bins, cuts='.') %>% select(variable, cuts)
	d_bins$cuts=as.character(d_bins$cuts)
	rownames(d_bins)=NULL

	print(sprintf("Variables processed: %s", paste(vars_num, collapse = ", ")))

	return(d_bins)
}

dis=function(x, n_bins)
{
	bins=discretize_bins(x, n_bins)
	res=discretize(x, "fixed", categories = bins)

	return(res)
}


get_bins_processed <- function(x, n_bins)
{
	cuts=discretize(x, "frequency", onlycuts = T, categories = n_bins)

	cuts[1]=-Inf
	cuts[length(cuts)]=Inf

	return(cuts)
}


dis_bins=function(x, n_bins=5, save_cuts=F)
{
	cuts=get_bins_processed(x, n_bins)
	res=paste(cuts, collapse = "|")

	names(x)
	colnames(x)

	return(res)
}


dis_recover <- function(x, cuts, stringsAsFactors)
{
	cuts_v=as.numeric(unlist(strsplit(cuts, '[|]')))
	x[x<min(cuts_v)]=min(cuts_v)
	x[x>max(cuts_v)]=max(cuts_v)

	res=arules::discretize(x, "fixed", categories = cuts_v)

	# Correction on "-Inf" label: [-Inf, min_value)
	if("-Inf" %in% levels(res))
	{
		cuts_v_aux=cuts_v[cuts_v!=-Inf]
		min_value=min(cuts_v_aux)
		levels(res)[levels(res)=="-Inf"]=sprintf("[-Inf, %s)", min_value)
	}
	########################


	if(!stringsAsFactors)
	{
		res=as.character(res)
	}

	return(res)
}

conv_factor <- function(x)
{
	levels(x)=c(levels(x), "NA.")
	new_x=factor(x, levels = levels(x))

	return(new_x)
}

