


run_graphics <- function(data, str_target, path_out)
{
	if(missing(path_out)) path_out=NA

	## Filtering only numeric variables
	status=df_status(data)
	colnam_num=status[!(status$type %in% "factor" | status$type %in% "factor"), 'variable']

	## Excluding target variable (if it's detected as numeric)
	colnam_num=colnam_num[colnam_num!=str_target]

  for(i in 1:length(colnam_num))
  {
    str_input=colnam_num[i]
    print(str_input)

    hist_targ=histdens_target(data, str_input, str_target)
  	boxp_targ=boxplot_target(data, str_input, str_target)

  	## Save plot
	  if(!is.na(path_out))
	  {
	  	dir.create(path_out, showWarnings = F)

	    if(dir.exists(path_out))
	    {
		     png(sprintf("%s/%s_hist.png", path_out, col_param), width= 12.25, height= 6.25, units="in",res=200, quality = 90)
				 plot(hist_targ)
	    dev.off()

	    	png(sprintf("%s/%s_box.png", path_out, col_param), width= 12.25, height= 6.25, units="in",res=200, quality = 90)
				plot(boxp_targ)
	    	dev.off()
	    } else {
	      warning(sprintf("The directory '%s' doesn't exists.", path_out))
	    }
	  }


  }

}

#' @title Get a summary for the given data frame.
#' @description For each variable it returns: Quantity and percentage of zeros (q_zeros and p_zeros respectevly). Same metrics for NA values (q_NA and p_na). Last two columns indicates data type and quantity of unique values.
#' This function print and return the results.
#' @param data data frame
#' @examples
#' df_status(heart_disease)
#' @return Metrics data frame
#' @export
histdens_target <- function(data, str_input, str_target)
{
  cdf=ddply(data, str_target, .fun = function(d)
    c(
      "var.mean" = round(mean(d[,str_input],na.rm=TRUE),2)
    ))

  return(ggplot(data, aes_string(x=str_input, colour=str_target)) + geom_density() + geom_vline(data=cdf, aes_string(xintercept="var.mean",  colour=str_target), linetype="dashed", size=0.5))

}

#' @title Plot
#' @description For each variable it returns: Quantity and percentage of zeros (q_zeros and p_zeros respectevly). Same metrics for NA values (q_NA and p_na). Last two columns indicates data type and quantity of unique values.
#' This function print and return the results.
#' @param data data frame
#' @examples
#' df_status(heart_disease)
#' @return Metrics data frame
#' @export
boxplot_target <- function(data, str_input, str_target)
{

  return(ggplot(data, aes_string(x=str_target, y=str_input, fill=str_target)) + geom_boxplot() +
         guides(fill=FALSE)+stat_summary(fun.y=mean, geom="point", shape=5, size=4))


}

# get_cor_matrix <- function(data, target)
# {
#   data[,target]=as.numeric(data[,target])
#   df_cor=as.data.frame(round(cor(data),2))
#   df_cor$Variable = rownames(df_cor)
#   df_cor=df_cor[,names(df_cor) %in% c(target,"Variable")]
#   df_cor=subset(df_cor, df_cor$Variable!= target)
#   row.names(df_cor) <- NULL
#   df_cor=df_cor[,c(2,1)]
#
#   pander(df_cor)
# }
