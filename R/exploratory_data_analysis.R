#' @title Plotting numerical data
#' @description
#' Retrieves one plot containing all the histograms for numerical variables. NA values will not be displayed.
#' @param data data frame
#' @param bins number of bars (bins) to plot each histogram, 10 by default
#' @param path_out path directory to export the output, if it has a value the plot is saved,
#' if the directory doesn't existis it will try to create it. To save in current directory path must be dot: "."
#' @examples
#' \dontrun{
#' plot_num(mtcars)
#' # changing the bins parameter and exporting the plot
#' plot_num(data=mtcars, bins=5, path_out="my_folder")
#' }
#' @return plot containing all numerical variables
#' @export
plot_num <- function(data, bins=10, path_out=NA)
{
	## The concept of 'wide' and 'long' is crucial to understand how to pass the correct data to ggplot.
	# The official documentation is quite clear about it: http://seananderson.ca/2013/10/19/reshape.html
	wide_data=suppressMessages(melt(data))
	p=ggplot(data = wide_data, mapping = aes(x = value)) +
		geom_histogram(bins = bins, na.rm=T) + facet_wrap(~variable, scales = 'free_x') +  aes(fill = variable) + guides(fill=FALSE)


	## Save plot
	if(!is.na(path_out))
	{
		export_plot(p, path_out, "histograms")
	}

	plot(p) # plot

}


#' @title Profiling numerical data
#' @description
#' Get a metric table with many indicators for all numerical variables, automatically skipping the non-numerical variables. Current metrics are:
#' mean, std_dev: standard deviation, all the p_XX: percentile at XX number, skewness, kurtosis, iqr: inter quartile range, variation_coef: the ratio of sd/mean, range_98 is the limit for which the 98% of fall, range_80 similar to range_98 but with 80%. All NA values will be skipped from calculations.
#' @param data data frame
#' @param print_results prints the result, TRUE by default.
#' @param digits the number of digits to show the result, 2 by default.
#' @examples
#' profiling_num(mtcars)
#' @return metrics table
#' @export
profiling_num <- function(data, print_results=T, digits=2)
{
	options(digits=digits)

	if(mode(data) %in% c("logical","numeric","complex","character"))
	{
		# creates a ficticious variable called 'var'
		data=data.frame(var=data)
		input="var"
	}

	## Keeping all non factor nor char variables
	status=df_status(data, print_results = F)
	vars_num=filter(status, type %in% c("numeric", "integer", "logical")) %>% .$variable

	if(length(vars_num)==0)
		stop("None of the input variables are numeric, integer nor logical")

	data_num=select(data, one_of(vars_num))

	if(missing(print_results))
		print_results=T


	df_res=data.frame(
		mean=sapply(data_num, function(x) mean(x, na.rm=T)),
		std_dev=sapply(data_num, function(x) sd(x, na.rm=T)),
		p_01=sapply(data_num, function(x) quantile(x, probs = 0.01, na.rm=T)),
		p_05=sapply(data_num, function(x) quantile(x, probs = 0.05, na.rm=T)),
		p_10=sapply(data_num, function(x) quantile(x, probs = 0.10, na.rm=T)),
		p_25=sapply(data_num, function(x) quantile(x, probs = 0.25, na.rm=T)),
		p_50=sapply(data_num, function(x) quantile(x, probs = 0.50, na.rm=T)),
		p_75=sapply(data_num, function(x) quantile(x, probs = 0.75, na.rm=T)),
		p_90=sapply(data_num, function(x) quantile(x, probs = 0.90, na.rm=T)),
		p_95=sapply(data_num, function(x) quantile(x, probs = 0.95, na.rm=T)),
		p_99=sapply(data_num, function(x) quantile(x, probs = 0.99, na.rm=T)),

		skewness=sapply(data_num, function(x) skewness(x, na.rm=T)),
		kurtosis=sapply(data_num, function(x) kurtosis(x, na.rm=T)),
		iqr=sapply(data_num, function(x) IQR(x, na.rm=T))
	)

	df_res$variation_coef=df_res$std_dev/df_res$mean
	df_res$range_98=sprintf("[%s, %s]", round(df_res$p_01, digits), round(df_res$p_99, digits))
	df_res$range_80=sprintf("[%s, %s]", round(df_res$p_10, digits), round(df_res$p_90, digits))

	df_res=select(df_res, -p_10, -p_90)

	## Create new variable for column name
	df_res$variable=rownames(df_res)
	rownames(df_res)=NULL

	# reordering columns
	df_res=select(df_res, variable, mean, std_dev, variation_coef, everything(), iqr, skewness, kurtosis)

	## Print or return results
	if(print_results) print(df_res) else return(df_res)
}




#' @title Compare two vectors
#' @description Obtaing coincident and not coincident elements between two vectors.
#' @param vector_x 1st vector to compare
#' @param vector_y 2nd vector to compare
#' @examples
#' v1=c("height","weight","age")
#' v2=c("height","weight","location","q_visits")
#' res=v_compare(vector_x=v1, vector_y=v2)
#' # Print the keys that didn't match
#' res
#' # Accessing the keys not present in
#' @return Correlation index for all data input variable
#' @export
v_compare <- function(vector_x, vector_y)
{
	# vector_x=v1;vector_y=v2
  df_x=data.frame(vector_x=vector_x, flag_x=1)
  df_y=data.frame(vector_y=vector_y, flag_y=1)

  df_x$vector_x=as.character(df_x$vector_x)
	df_y$vector_y=as.character(df_y$vector_y)

  merge_all=merge(df_x, df_y, by.x='vector_x', by.y='vector_y', all=T)

  names(merge_all)[1]="key"

  merge_all_nona=merge_all[!is.na(merge_all$flag_x) & !is.na(merge_all$flag_y),]

  not_in_x=merge_all[is.na(merge_all$flag_x),]
  not_in_y=merge_all[is.na(merge_all$flag_y),]

  print(sprintf("Coincident in both: %s", nrow(merge_all_nona)))
  print(sprintf("Rows not present in X: %s", nrow(not_in_x)))
  print(sprintf("Rows not present in Y: %s", nrow(not_in_y)))


  list_diff=list()

  res=list(
    present_in_both=merge_all_nona$key,
    rows_not_in_X=not_in_x$key,
    rows_not_in_Y=not_in_y$key
  	)

  return(res)
}

#' @title Get correlation against target variable
#' @description Obtain correlation table for all variables against target variable. Only numeric variables are analyzed (factor/character are skippted automatically).
#' @param data data frame
#' @param target string variable to predict
#' @param str_target THIS PARAMETER WILL BE DEPRECATED. Please use 'target' insted. Only name changes, not functionality.
#' @examples
#' correlation_table(data=heart_disease, target="has_heart_disease")
#' @return Correlation index for all data input variable
#' @export
correlation_table <- function(data, target, str_target)
{
	if(!missing(str_target))
	{
		target=str_target
		.Deprecated(msg="Parameter 'str_target' will be deprecated, please use 'target' insted (only name changed, not its functionality)")
	}

	data=as.data.frame(data)

	data[[target]]=as.numeric(data[[target]])

	data=data %>% select(one_of(c(give_me_num_vars(data), target)))

  df_cor=as.data.frame(round(cor(data, use="complete.obs"	),2))
  df_cor$Variable = rownames(df_cor)
  df_cor=df_cor %>% select(one_of(c(target, "Variable")))

  df_cor=df_cor[, c(2,1)]

  df_cor_final=df_cor[order(-df_cor[,2]) , ]
	row.names(df_cor_final) = NULL

	return(df_cor_final)
}



#' @title Get a summary for the given data frame (o vector).
#' @description For each variable it returns: Quantity and percentage of zeros (q_zeros and p_zeros respectevly). Same metrics for NA values (q_NA/p_na), and infinite values (q_inf/p_inf). Last two columns indicates data type and quantity of unique values.
#' This function print and return the results.
#' @param data data frame or a single vector
#' @param print_results if FALSE then there is not a print in the console, TRUE by default.
#' @examples
#' df_status(heart_disease)
#' @return Metrics data frame
#' @export
df_status <- function(data, print_results)
{
	## If input is NA then ask for a single vector. True if it is a single vector
	if(mode(data) %in% c("logical","numeric","complex","character"))
	{
		# creates a ficticious variable called 'var'
		data=data.frame(var=data)
		input="var"
	}

	if(missing(print_results))
		print_results=T

	df_status_res=data.frame(
		q_zeros=sapply(data, function(x) sum(x==0,na.rm=T)),
		p_zeros=round(100*sapply(data, function(x) sum(x==0,na.rm=T))/nrow(data),2),
		q_na=sapply(data, function(x) sum(is.na(x))),
		p_na=round(100*sapply(data, function(x) sum(is.na(x)))/nrow(data),2),
		q_inf=sapply(data, function(x) sum(is.infinite(x))),
		p_inf=round(100*sapply(data, function(x) sum(is.infinite(x)))/nrow(data),2),
		type=sapply(data, get_type_v),
		unique=sapply(data, function(x) sum(!is.na(unique(x))))
	)

	## Create new variable for column name
	df_status_res$variable=rownames(df_status_res)
	rownames(df_status_res)=NULL

	## Reordering columns
	df_status_res=df_status_res[, c(9,1,2,3,4,5,6,7,8)]

	## Print or return results
	if(print_results) print(df_status_res) else return(df_status_res)
}

is.POSIXct <- function(x) inherits(x, "POSIXct")
is.POSIXlt <- function(x) inherits(x, "POSIXlt")
is.POSIXt <- function(x) inherits(x, "POSIXt")

get_type_v <- function(x)
{
	## handler for posix object, because class function returns a list in this case
	posix=ifelse(is.POSIXct(x), "POSIXct", "")
	posix=ifelse(is.POSIXlt(x), paste(posix, "POSIXlt", sep="/"), posix)
	posix=ifelse(is.POSIXt(x), paste(posix, "POSIXt", sep="/"), posix)

	# ifnot posix..then something else
	if(posix=="")
	{
		cl=class(x)
		return(ifelse(length(cl)>1, paste(cl, collapse = "-"), cl))
	} else {
		return(posix)
	}
}


#' @title Frequency table for categorical variables
#' @description Retrieves the frequency and percentage for input
#' @param data input data containing the variable to describe
#' @param input string input variable (if empty, it runs for all numeric variable), it can take a single character value or a character vector.
#' @param str_input THIS PARAMETER WILL BE DEPRECATED. Please use 'input' insted. Only name changes, not functionality. String input variable (if empty, it runs for all numeric variable), it can take a single character value or a character vector.
#' @param plot flag indicating if the plot is desired, TRUE by default
#' @param na.rm flag indicating if NA values must be included in the analysis, FALSE by default
#' @param path_out path directory, if it has a value the plot is saved
#' @examples
#' freq(data=heart_disease$thal)
#' freq(data=heart_disease, input = c('thal','chest_pain'))
#' @return vector with the values scaled into the 0 to 1 range
#' @export
freq <- function(data, input=NA, str_input=NA, plot=TRUE, na.rm=FALSE, path_out)
{
	if(!missing(str_input))
	{
		input=str_input
		.Deprecated(msg="Parameter 'str_input' will be deprecated, please use 'input' insted (only name changed, not its functionality)")
	}

	if(missing(path_out)) path_out=NA

  ## If input is NA then it runs for all variables in case it is not a single vector
	if(sum(is.na(input)>0))
	{
  	# True if it is a single vector
  	if(mode(data) %in% c("logical","numeric","complex","character"))
  	{
  		data=data.frame(var=data)
  		input="var"
  	} else {
			## Keeping all categorical variables
  		data=data.frame(data)
			status=df_status(data, print_results = F)
			input=status[status$type %in% c("factor", "character"), 'variable']
			if(length(input)==0)
				stop("None of the input variables are factor nor character")

  	}
	}

	## Iterator
	tot_vars=length(input)
	if(tot_vars==1)
	{
		res=freq_logic(data = data, input=input, plot, na.rm, path_out = path_out)
		return(res)
	} else {
		for(i in 1:tot_vars)
		{
			res=freq_logic(data = data, input=input[i], plot, na.rm, path_out = path_out)
			print(res)
			cat("", sep="\n")
		}

		return(sprintf("Variables processed: %s", paste(input, collapse = ", ")))

	}

}

freq_logic <- function(data, input, plot, na.rm, path_out)
{
	if(!na.rm) {
		# if exclude = NULL then it adds the NA cases
		tbl=data.frame(table(factor(data[[input]], exclude = NULL)))
	} else {
		tbl=data.frame(table(data[[input]]))
	}

	tbl=rename(tbl, category=Var1, frequency=Freq) %>% arrange(-frequency)
	tbl$percentage=round(100*tbl$frequency/sum(tbl$frequency),2)
	tbl$cumulative_perc=cumsum(tbl$percentage)
	tbl$cumulative_perc[length(tbl$cumulative_perc)]=100.00

	## calculating best font size
	uq=nrow(tbl)
	if(uq<=10)
	{
		letter_size=3
		axis_size=12
	} else if(uq<=20){
		letter_size=2.5
		axis_size=10
	} else {
		letter_size=2
		axis_size=8
	}

	if(plot)
	{
		# Plot
		tbl_plot=tbl
		tbl_plot$label=sprintf('%s (%s%%)', tbl_plot$frequency, tbl_plot$percentage)

		tbl_plot$category=factor(tbl_plot$category, levels =  tbl_plot$category[order(tbl_plot$percentage)])


		if(nrow(tbl_plot)<200)
		{
			p=ggplot(tbl_plot,aes(x=tbl_plot$category,y=tbl_plot$frequency,fill=tbl_plot$category, label=label)) +
				geom_bar(stat='identity') + coord_flip() +	theme_bw() +
				theme(
					panel.grid.minor=element_blank(),
						panel.grid.major =element_blank(),
					legend.title=element_blank(),
					plot.title = element_text(vjust=2),
					axis.ticks.y=element_blank(),
					axis.ticks.x=element_blank(),
					axis.text.x=element_blank(),
					axis.text.y=element_text(size=axis_size),
					axis.title.x=element_text(size=12, margin=margin(10,0,0,0)),
					axis.title.y=element_text(size=14, margin=margin(0,10,0,0))
				) + ylab("Frequency / (Percentage %)") + xlab(input) +
				geom_text( color="#151515", size=letter_size, hjust=-.06) +
				guides(fill=F) +
				scale_y_continuous(expand = c(0,0),limits = c(0, max(tbl_plot$frequency)*1.2))

			## Save plot
			if(!is.na(path_out))
			{
				dir.create(path_out, showWarnings = F)

				if(dir.exists(path_out))
				{
					jpeg(sprintf("%s/%s.jpeg", path_out, input), width= 12.25, height= 6.25, units="in",res=200, quality = 90)

					plot(p)
					dev.off()
				} else {
					warning(sprintf("The directory '%s' doesn't exists.", path_out))
				}
			} else {
				plot(p)
			}

		} else {
			message_high_card=sprintf("Skipping plot for variable '%s' (more than 200 categories)", input)
		}

	}

	colnames(tbl)[1]=input
	tbl[[input]]=as.character(tbl[[input]])

	if(exists("message_high_card")) {warning(message_high_card)}

	return(tbl)
}


#' @title Compare two data frames by keys
#' @description Obtain differences between two data frames
#' @param dfcomp_x first data frame to compare
#' @param dfcomp_y second data frame to compare
#' @param keys_x keys of the first dataframe
#' @param keys_y (optional) keys of the second dataframe, if missing both data frames will be compared with the keys_x
#' @param compare_values (optional) if TRUE it will not only compare keys, but also will check if the values of non-key matching columns have the same values
#' @examples
#' data(heart_disease)
#' a=heart_disease
#' b=heart_disease
#' a=subset(a, age >45)
#' b=subset(b, age <50)
#' b$gender='male'
#' b$chest_pain=ifelse(b$chest_pain ==3, 4, b$chest_pain)
#' res=compare_df(a, b, c('age', 'gender'))
#' # Print the keys that didn't match
#' res
#' # Accessing the keys not present in the first data frame
#' res[[1]]$rows_not_in_X
#' # Accessing the keys not present in the second data frame
#' res[[1]]$rows_not_in_Y
#' # Accessing the keys which coincide completely
#' res[[1]]$coincident
#' # Accessing the rows whose values did not coincide
#' res[[1]]$different_values
#' @return Differences and coincident values
#' @export
compare_df <- function(dfcomp_x, dfcomp_y, keys_x, keys_y=NA, compare_values=FALSE)
{
	#Setup internal flags for merging data
	internal_flags = c('comparedf_flag_x', 'comparedf_flag_y', 'comparedf_flag_equal')
  dfcomp_x$comparedf_flag_x=1
  dfcomp_y$comparedf_flag_y=1

  #If keys_y is missing, it is equal to keys_x
  if (missing(keys_y)){
    keys_y = keys_x
    all_keys = keys_x
  }else{
    all_keys = unique(c(keys_x, keys_y))
  }

  #Merge the input data frames
  merge_all=merge(dfcomp_x, dfcomp_y, by.x=keys_x, by.y=keys_y, all=T)

  #These are only the coincident keys
  merge_all_nona=subset(merge_all, !is.na(merge_all$comparedf_flag_x) & !is.na(merge_all$comparedf_flag_y))

  #Get non intersecting keys
  not_in_x=merge_all[is.na(merge_all$comparedf_flag_x), keys_y]
  not_in_y=merge_all[is.na(merge_all$comparedf_flag_y), keys_x]

  #If there are coincident keys
  if(nrow(merge_all_nona) > 0){

  	#Search for different values in non-key columns
    merge_all_nona$comparedf_flag_equal = TRUE
    if(compare_values){
	    for(varx in names(merge_all_nona)){
	      if(grepl(".*\\.x", varx)){
	        vary = gsub("\\.x", ".y", varx)
	        merge_all_nona$comparedf_flag_equal = ifelse(as.character(merge_all_nona[[varx]]) != as.character(merge_all_nona[[vary]]), FALSE, merge_all_nona$comparedf_flag_equal)
	      }
	    }
    }

    #Print results
    print(sprintf("Coincident keys: %s", nrow(merge_all_nona)))
    if(compare_values){
    	print(sprintf("Coincident entire rows: %s", nrow(merge_all_nona[merge_all_nona$comparedf_flag_equal == TRUE,])))
   		print(sprintf("Coincident keys with different values: %s", nrow(merge_all_nona[merge_all_nona$comparedf_flag_equal == FALSE,])))
    }

    #Save results into output list
    if(compare_values){
    	list_diff=list(
	      coincident=subset(merge_all_nona[,all_keys], merge_all_nona$comparedf_flag_equal == TRUE),
	      different_values=subset(merge_all_nona[,!names(merge_all_nona) %in% internal_flags], merge_all_nona$comparedf_flag_equal == FALSE),
	      rows_not_in_X=not_in_x,
	      rows_not_in_Y=not_in_y
	    )
    }else{
    	list_diff=list(
	      coincident=merge_all_nona[,all_keys],
	      rows_not_in_X=not_in_x,
	      rows_not_in_Y=not_in_y
	    )
    }


  #If no key coincides
  }else{
    print("No coincident keys")

    list_diff=list(
      rows_not_in_X=not_in_x,
      rows_not_in_Y=not_in_y
    )

  }

  #Print non-coincident keys
  print(sprintf("Keys not present in X: %s", nrow(not_in_x)))
  print(sprintf("Keys not present in Y: %s", nrow(not_in_y)))

  return(list_diff)
}
