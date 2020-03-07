#' @title Outliers Data Preparation
#' @description
#' Deal with outliers by setting an 'NA value' or by 'stopping' them at a certain.
#' There are three supported methods to flag the values as outliers: "bottom_top", "tukey" and "hampel".
#' The parameters: 'top_percent' and/or 'bottom_percent' are used only when method="bottom_top".
#'
#' For a full reference please check the official documentation at: \url{https://livebook.datascienceheroes.com/data-preparation.html#treatment_outliers}>
#' Setting NA is recommended when doing statistical analysis, parameter: type='set_na'.
#' Stopping is recommended when creating a predictive model without biasing the result due to outliers, parameter: type='stop'.
#'
#' The function can take a data frame, and returns the same data plus the transformations specified in the input parameter. Or it can take a single vector (in the same 'data' parameter), and it returns a vector.
#'
#' @param data a data frame or a single vector. If it's a data frame, the function returns a data frame, otherwise it returns a vector.
#' @param input string input variable (if empty, it runs for all numeric variable).
#' @param type can be 'stop' or 'set_na', in the first case all falling out of the threshold will be converted to the threshold, on the other case all of these values will be set as NA.
#' @param method indicates the method used to flag the outliers, it can be: "bottom_top", "tukey" or "hampel".
#' @param top_percent value from 0 to 1, represents the highest X percentage of values to treat. Valid only when method="bottom_top".
#' @param bottom_percent value from 0 to 1, represents the lowest X percentage of values to treat. Valid only when method="bottom_top".
#' @param k_mad_value only used when method='hampel', 3 by default, might seem quite restrictive. Set a higher number to spot less outliers.
#' @examples
#' \dontrun{
#' # Creating data frame with outliers
#' set.seed(10)
#' df=data.frame(var1=rchisq(1000,df = 1), var2=rnorm(1000))
#' df=rbind(df, 1135, 2432) # forcing outliers
#' df$id=as.character(seq(1:1002))
#'
#' # for var1: mean is ~ 4.56, and max 2432
#' summary(df)
#'
#' ########################################################
#' ### PREPARING OUTLIERS FOR DESCRIPTIVE STATISTICS
#' ########################################################
#'
#' #### EXAMPLE 1: Removing top 1%% for a single variable
#' # checking the value for the top 1% of highest values (percentile 0.99), which is ~ 7.05
#' quantile(df$var1, 0.99)
#'
#' # Setting type='set_na' sets NA to the highest value specified by top_percent.
#' # In this case 'data' parameter is single vector, thus it returns a single vector as well.
#' var1_treated=prep_outliers(data = df$var1, type='set_na', top_percent  = 0.01,method = "bottom_top")
#'
#' # now the mean (~ 1) is more accurate, and note that: 1st, median and 3rd
#' #  quartiles remaining very similar to the original variable.
#' summary(var1_treated)
#'
#' #### EXAMPLE 2: Removing top and bottom 1% for the specified input variables.
#' vars_to_process=c('var1', 'var2')
#' df_treated3=prep_outliers(data = df, input = vars_to_process, type='set_na',
#'  bottom_percent = 0.01, top_percent  = 0.01, method = "bottom_top")
#' summary(df_treated3)
#'
#' ########################################################
#' ### PREPARING OUTLIERS FOR PREDICTIVE MODELING
#' ########################################################
#'
#' data_prep_h=funModeling::prep_outliers(data = heart_disease,
#' input = c('age','resting_blood_pressure'),
#'  method = "hampel",  type='stop')
#'
#' # Using Hampel method to flag outliers:
#' summary(heart_disease$age);summary(data_prep_h$age)
#' # it changed from 29 to 29.31, and the max remains the same at 77
#' hampel_outlier(heart_disease$age) # checking the thresholds
#'
#' data_prep_a=funModeling::prep_outliers(data = heart_disease,
#' input = c('age','resting_blood_pressure'),
#'  method = "tukey",  type='stop')
#'
#' max(heart_disease$age);max(data_prep_a$age)
#' # remains the same (77) because the max thers for age is 100
#' tukey_outlier(heart_disease$age)
#'
#' }
#' @return A data frame with the desired outlier transformation
#' @export
prep_outliers <- function(data, input=NA, type=NA, method=NA, bottom_percent=NA, top_percent=NA, k_mad_value=NA)
{
	if(!(type %in% c('stop', 'set_na')))
		stop("Parameter 'type' must be one of the following 'stop' or 'set_na'")

	if(!(method %in% c("bottom_top", "tukey", "hampel")))
		stop("Parameter 'method' must be one of the following 'bottom_top', 'tukey' or 'hampel'")

	if(method !="bottom_top" & (!missing(top_percent) | !missing(bottom_percent)))
		warning("Parameters 'bottom_percent' and/or 'top_percent' will be ignored. Only valid when method='bottom_top'")


	## If input is NA then ask for a single vector. True if it is a single vector
	if(sum(is.na(input)>0) & mode(data) %in% c("logical","numeric","complex","character"))
	{
		# creates a ficticious variable called 'var'
		data=data.frame(var=data)
		input="var"
		input_one_var=TRUE
	} else {
		input_one_var=FALSE
	}


	#########################################################
	### Stopping and Setting NA processing
	#########################################################
	if(missing(top_percent) & missing(bottom_percent) & method=="bottom_top")
		stop("Parameters 'top_percent' and 'bottom_percent' cannot be missing at the same time if method is 'bottom_top'.")


	## logic to compute bott and/or top:
	flag_bott_top=NA
	if(method!="bottom_top")
	{
		flag_bott_top="run_both" # it's tuley or hampel
	} else {
		if(!missing(top_percent) & !missing(bottom_percent)) {
			# both with values? if yes the it run for both, otherwise, we must know which way
			flag_bott_top="run_both"
		} else if(!missing(top_percent)) {
			flag_bott_top="only_top"
		} else {
			flag_bott_top="only_bottom"
		}
	}



	if(flag_bott_top %in% c("only_top", "run_both"))
	{
		## Logic for top value ##################################################
		warn_mess_top=NA
		for(i in 1:length(input))
		{
			cur_var=input[i]
			x=data[[cur_var]]

			if(method=="bottom_top" & !missing(top_percent))
			{
				top_value=quantile(x, probs=(1-top_percent), names=F, na.rm=T)
			} else if(method=="tukey")
			{
				top_value=tukey_outlier(x)[2]
			} else if(method=="hampel")
			{
				top_value=hampel_outlier(x, k_mad_value)[2]
			}


			bkp_var=x
			x[x>=top_value]=ifelse(type=='stop', top_value, NA)

			if(length(na.omit(unique(x)))==1)
			{
				## if there is only 1 unique value, then recover the original value (thus no transformation) and warn it
				x=bkp_var
				if(is.na(warn_mess_top))
				{
					warn_mess_top=cur_var
				} else {
					warn_mess_top=paste(warn_mess_top, cur_var, sep=", ")
				}
			}

			## assign the final value to the input data frame
			data[[cur_var]]=x

		}

		if(!is.na(warn_mess_top))
		{
			warning(sprintf("Skip the transformation (top value) for some variables because the threshold would have left them with 1 unique value. Variable list printed in the console."))
			message(sprintf("Variables to adjust top threshold: %s", warn_mess_top))
		}
	}


	if(flag_bott_top %in% c("only_bottom", "run_both"))
	{
		## Logic for bottom value ######################################################
		warn_mess_bott=NA

		for(i in 1:length(input))
		{
			cur_var=input[i]
			x=data[[cur_var]]

			if(method=="bottom_top" & !missing(bottom_percent))
			{
				bottom_value=quantile(x, probs=bottom_percent, names=F, na.rm=T)
			} else if(method=="tukey")
			{
				bottom_value=tukey_outlier(x)[1]
			} else if(method=="hampel")
			{
				bottom_value=hampel_outlier(x, k_mad_value)[1]
			} else {
				stop(sprintf("Method '%s' is not implemented.", method))
			}

			bkp_var=x
			x[x<=bottom_value]=ifelse(type=='stop', bottom_value, NA)

			if(length(na.omit(unique(x)))==1)
			{
				## if there is only 1 unique value, then recover the original value (thus no transformation) and warn it
				x=bkp_var
				if(is.na(warn_mess_bott))
				{
					warn_mess_bott=cur_var
				} else {
					warn_mess_bott=paste(warn_mess_bott, cur_var, sep=", ")
				}
			}

			## assign the final value to the input data frame
			data[[cur_var]]=x

			if(!is.na(warn_mess_bott))
			{
				warning(sprintf("Skip the transformation (bottom value) for some variables because the threshold would have left them with 1 unique value. Variable list printed in the console."))
				message(sprintf("Variables to adjust bottom threshold: %s", warn_mess_bott))
			}
		}
	}

	ifelse(input_one_var,  return(data$var), return(data))

}


#' @title Tukey Outlier Threshold
#' @description
#' Retrieves the bottom and top boundaries to flag outliers or extreme values, according to the Tukey's test. More info at \url{https://en.wikipedia.org/wiki/Outlier#Tukey.27s_test}
#' This function is used in 'prep_outliers' function. All `NA`s values are automatically excluded. More information at: \url{https://livebook.datascienceheroes.com/data-preparation.html#how_to_deal_with_outliers_in_r}.
#' @param input Numeric variable vector
#' @examples
#' \dontrun{
#' tukey_outlier(heart_disease$age)
#' }
#' @return A two-item vector, the first value represents the bottom threshold, while the second one is the top threshold
#' @export
tukey_outlier <- function(input)
{
	input_cleaned=na.omit(input)

	q_var=quantile(input_cleaned, na.rm=T, names = F)
	q_25=q_var[2]
	q_75=q_var[4]
	iqr=IQR(input, na.rm = T)

	bottom_threshold=q_25-(iqr * 3)
	top_threshold=(iqr * 3) + q_75

	v_res=c(bottom_threshold, top_threshold)
	names(v_res)=c("bottom_threshold", "top_threshold")

	return(v_res)
}

#' @title Hampel Outlier Threshold
#' @description
#' Retrieves the bottom and top boundaries to flag outliers or extreme values, according to the Hampel method. This technique takes into account the median and MAD value, which is a is a robust measure of the variability of a univariate sample of quantitative data (Wikipedia). Similar to standard deviation but less sensitve to outliers.
#' This function is used in 'prep_outliers' function. All `NA`s values are automatically excluded. More information at: \url{https://livebook.datascienceheroes.com/data-preparation.html#how_to_deal_with_outliers_in_r}.
#' @param input Numeric variable vector
#' @param k_mad_value 'K' multiplier for the median absolute deviation. The higher the value, the more outliers will be detected. Default value=3 (it's an standad)
#' @examples
#' \dontrun{
#' hampel_outlier(heart_disease$age)
#' }
#' @return A two-item vector, the first value represents the bottom threshold, while the second one is the top threshold
#' @export
hampel_outlier <- function(input, k_mad_value=3)
{
	input_cleaned=na.omit(input)

	mad_value=mad(input_cleaned)
	median_value=median(input_cleaned)

	bottom_threshold=median_value-k_mad_value*mad_value
	top_threshold=median_value+k_mad_value*mad_value

	v_res=c(bottom_threshold, top_threshold)
	names(v_res)=c("bottom_threshold", "top_threshold")

	return(v_res)
}
