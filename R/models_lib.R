#' @title Sampling training and test data
#' @description Split input data into training and test set, retrieving always same sample by setting the seed.
#' @param data input data source
#' @param percentage_tr_rows percentage of training rows, range value from 0.1 to 0.99, default value=0.8 (80 percent of training data)
#' @param seed to generate the sample randomly, default value=987
#' @examples
#' # Training and test data. Percentage of training cases default value=80%.
#' index_sample=get_sample(data=heart_disease, percentage_tr_rows=0.8)
#' # Generating the samples
#' data_tr=heart_disease[index_sample,]
#' data_ts=heart_disease[-index_sample,]
#' @return TRUE/FALSE vector same length as 'data' param. TRUE represents that row position is for training data
#' @export
get_sample <- function(data, percentage_tr_rows=0.8, seed=987)
{
	set.seed(seed)
	tr=sample(nrow(data), round(percentage_tr_rows*nrow(data)))
	return(tr)
}


#' @title Generates lift and cumulative gain performance table and plot
#' @description It retrieves the cumulative positive rate -gain curve- and the lift chart & plot when score is divided
#' in 5, 10 or 20 segments. Both metrics give a quality measure about how well the model predicts.
#' Higher values at the beginning of the population implies a better model. More info at:
#'  \url{https://livebook.datascienceheroes.com/model-performance.html#scoring_data}
#' @param data input data source
#' @param score the variable which contains the score number, or likelihood of being positive class
#' @param target target binary variable indicating class label
#' @param str_score THIS PARAMETER WILL BE DEPRECATED. Please use 'score' insted. Only name changes, not functionality.
#' @param str_target THIS PARAMETER WILL BE DEPRECATED. Please use 'target' insted. Only name changes, not functionality.
#' @param q_segments quantity of segments to split str_score, valid values: 5, 10 or 20
#' @examples
#' fit_glm=glm(has_heart_disease ~ age + oldpeak, data=heart_disease, family = binomial)
#' heart_disease$score=predict(fit_glm, newdata=heart_disease, type='response')
#' gain_lift(data=heart_disease, score='score', target='has_heart_disease')
#'
#' @return lift/gain table, column: gain implies how much positive cases are catched if the cut point to define the
#' positive class is set to the column "Score Point"
#' @export
gain_lift <- function(data, score, target, str_score, str_target, q_segments=10)
{
	if(!missing(str_score))
	{
		score=str_score
		.Deprecated(msg="Parameter 'str_score' will be deprecated, please use 'score' insted (only name changed, not its functionality)")
	}

	if(!missing(str_target))
	{
		target=str_target
		.Deprecated(msg = "Parameter 'str_target' will be deprecated, please use 'target' insted (only name changed, not its functionality)")
	}

	options(scipen = 999)
	data=data.frame(data)
	# The negative score produces that the highest score are at the top
	data$neg_score=-data[[score]]

	# Valid values for q_segments
	if(q_segments<1 | missing(q_segments)){
		q_segments = 10
	}

	seq_v=c(seq(from=1/q_segments, to=1-1/q_segments, by=1/q_segments), 1) # Add all cutpoints from 1/q_segments to 1, by 1/q_segments increments.

	quantile_cuts=quantile(data$neg_score, probs=seq_v)

	data[[target]]=as.character(data[[target]])

	grp=group_by(data, data[,target]) %>% summarise(q=n()) %>% arrange(q)

	less_representative_class=as.character(grp[1,1])

	lift_table=round(100*sapply(quantile_cuts, function(x) sum(data[data$neg_score<=x, target]==less_representative_class))/sum(data[, target]==less_representative_class),2)

	lift_res=rbind(lift_table,-quantile_cuts)
	rownames(lift_res)=c("Gain", "Score.Point")

	# likelihood of being less representative class (lrc)
	likelihood_lrc=grp[1,2]/(grp[2,2]+grp[1,2])

	## Create table
	lift_res_t=data.frame(t(lift_res))
	lift_res_t$Population=as.numeric(100*seq_v)
	row.names(lift_res_t)=NULL
	lift_res_t=select(lift_res_t, Population, Gain, Score.Point)

	## Generate lift variable
	lift_res_t$Lift=round(lift_res_t$Gain/100/seq_v,2)
	lift_res_t_gain=rbind(c(0, 0, NA, NA), lift_res_t)

	p_gain=
		ggplot(lift_res_t_gain, aes(Population, Gain, label=round(Gain,1), group=1)) + geom_line(stat="identity") +   geom_point(aes(colour=Gain) ) +
		theme_bw()  + ylab("Cumulative Gain (%)") + xlab("Population (%)")+
		theme(
			panel.grid.minor=element_blank(),
			legend.title=element_blank(),
			plot.title = element_text(vjust=2),
			axis.ticks.y=element_blank(),
			axis.text.y=element_blank(),
			panel.background = element_blank(),
			axis.title.x=element_text(margin=margin(15,0,0,0)),
			axis.title.y=element_text(margin=margin(0,15,0,0))
		)+geom_label(aes(fill = factor(Gain)), colour = "white", fontface = "bold",vjust = -.5, label.padding = unit(.2, "lines")) + ylim(0, 110)  +
			guides(fill=F) +  scale_colour_continuous(guide = FALSE)  +
		geom_segment(x = 0, y = 0, xend = 100, yend = 100,linetype="dotted") + scale_x_continuous(breaks = c(0, seq(10, 100, by=10)))


	p_lift=
		ggplot(lift_res_t, aes(Population, Lift, label=Lift, group=1)) + geom_line(stat="identity") + geom_point(aes(colour=Lift)) +
		theme_bw()  + ylab("Lift") + xlab("Population (%)")+
		theme(
			panel.grid.minor=element_blank(),
			legend.title=element_blank(),
			plot.title = element_text(vjust=2),
			axis.ticks.y=element_blank(),
			axis.text.y=element_blank(),
			axis.title.x=element_text(margin=margin(15,0,0,0)),
			axis.title.y=element_text(margin=margin(0,15,0,0))
		)+geom_label(aes(fill=-Lift), size=3.5, colour="white", vjust = -.5, label.padding = unit(.2, "lines")) + ylim(min(lift_res_t$Lift), max(lift_res_t$Lift*1.1)) +
			guides(fill=F) + scale_colour_continuous(guide = FALSE) + scale_x_continuous(breaks = c(0, seq(10, 100, by=10)))



	grid.arrange(p_gain, p_lift, ncol=2)

	lift_res_t=select(lift_res_t, Population, Gain, Lift, Score.Point)

	print(lift_res_t)
}

#' @title Profiling categorical variable
#' @description Calculate the means (or other function) per group to analyze how each segment behave. It scales each variable mean inti the 0 to 1 range to easily profile the groups according to its mean. It also calculate the mean regardless the grouping. This function is also useful when you want to profile cluster results in terms of its means. It automatically adds a row representing the sumarization of the column regardless the group_var categories, this is useful to compare each segement with the whole population. It will exclude all factor/character variables.
#' @param data input data source
#' @param group_var variable to make the group by
#' @param group_func the data type of this parameter is a function, not an string, this is the function to be used in the group by, the default value is: mean
#' @param add_all_data_row flag indicating if final data contains the row: 'All_Data', which is the function applied regardless the grouping. Useful to compare with the rest of the values.
#' @examples
#' # default grouping function: mean
#' desc_groups(data=mtcars, group_var="cyl")
#'
#' # using the median as the grouping function
#' desc_groups(data=mtcars, group_var="cyl", group_func=median)
#'
#' # using the max as the grouping function
#' desc_groups(data=mtcars, group_var="gear", group_func=max)
#' @return grouped data frame
#' @export
desc_groups <- function(data, group_var, group_func=mean, add_all_data_row=T)
{
	## calculate only for numeric variables
	status=df_status(data, print_results = F)
	vars_to_keep=status[status$type %in% c("integer", "numeric") & status$variable != group_var, "variable"]

	grp_mean=data %>% group_by_(group_var) %>% summarise_each_(funs(group_func), vars_to_keep) %>% mutate_each_(funs(round(.,2)), vars_to_keep)
	grp_mean=data.frame(grp_mean)

	grp_mean[,group_var]=as.character(grp_mean[,group_var])

	# select all except the group var
	a=select(grp_mean, -one_of(group_var))

	# vars_to_keep have all num variables (excluding group_var and factor/char). Calculate 'All_Data' means per column
	data_num=select(data, one_of(vars_to_keep))
	b=as.data.frame(data_num) %>% summarise_each(funs(group_func))

	## putting all together: the sumarization per group plus the total per column
	all_results=rbind(a, b)
	all_results_report=all_results

	all_results_report[,group_var]=c(grp_mean[,group_var], "All_Data")

	# rearrange columns
	nc=ncol(all_results_report)
	all_results_report=all_results_report[,c(nc, 1:(nc-1))]

	# excluding all_data row if needed
	if(!add_all_data_row) {
		all_results_report=all_results_report[-nrow(all_results_report),]
	}

	return(all_results_report)
}

#' @title Profiling categorical variable (rank)
#' @description Similar to 'desc_groups' function, this one computes the rank of each value in order to quickly know what is the value in each segment that has the highest value (rank=1). 1 represent the highest number. It will exclude all factor/character variables.
#' @param data input data source
#' @param group_var variable to make the group by
#' @param group_func the data type of this parameter is a function, not an string, this is the function to be used in the group by, the default value is: mean
#' @examples
#' # default grouping function: mean
#' desc_groups_rank(data=mtcars, group_var="gear")
#'
#' # using the median as the grouping function
#' desc_groups(data=mtcars, group_var="cyl", group_func=median)
#'
#' # using the max as the grouping function
#' desc_groups_rank(data=mtcars, group_var="gear", group_func=max)
#' @return grouped data frame, showing the rank instead of the absolute values/
#' @export
desc_groups_rank <- function(data, group_var, group_func=mean)
{
	d_group=desc_groups(data, group_var, group_func, add_all_data_row = F)

	# excluding grouping variable, from the grouping
	all_col=colnames(d_group)
	vars_to_group=all_col[all_col!=group_var]

	# mutate each does the group by only for variables defined in vars_to_group
	d_group_rank=d_group %>% mutate_each_(funs(dense_rank(desc(.))), vars_to_group)

	return(d_group_rank)
}

#' @title Coordinate plot
#' @description Calculate the means (or other function defined in 'group_func' parameter) per group to analyze how each segment behave. It scales each variable mean inti the 0 to 1 range to easily profile the groups according to its mean. It also calculate the mean regardless the grouping. This function is also useful when you want to profile cluster results in terms of its means.
#' @param data input data source
#' @param group_var variable to make the group by
#' @param group_func the data type of this parameter is a function, not an string, this is the function to be used in the group by, the default value is: mean
#' @param print_table False by default, if true it retrieves the mean table used to generate the plot.
#' @examples
#' \dontrun{
#' # calculating the differences based on function 'mean'
#' coord_plot(data=mtcars, group_var="cyl")
#' # printing the table used to generate the coord_plot
#' coord_plot(data=mtcars, group_var="cyl", print_table=TRUE)
#' # printing the table used to generate the coord_plot
#' coord_plot(data=mtcars, group_var="cyl", group_func=median, print_table=TRUE)
#' }
#' @return coordinate plot, if print_table=T it also prints a table with the average per column plus the average of the whole column
#' @export
coord_plot <- function(data, group_var, group_func=mean, print_table=FALSE)
{
	all_results_report=desc_groups(data = data, group_var = group_var, group_func = group_func)

	# excluding group_var column
	all_results=all_results_report[,2:ncol(all_results_report)]
	######################################################################
	##  Group profiling. Extracting main characteristics from each one.
	######################################################################

	## Scale data to plot all in only one graph
	maxs=apply(all_results, 2, max)
	mins=apply(all_results, 2, min)
	cl_scaled=as.data.frame(scale(all_results, center = mins, scale = maxs - mins))

	## Assign group number (label)
	cl_scaled[,group_var]=all_results_report[, group_var]

	## This transform the data according to needed input of ggplot. The best way to understand this is to take a look at the data.
	melted_data=melt(cl_scaled, id.vars = group_var)

	colourCount = length(unique(cl_scaled[,group_var]))
	getPalette = suppressWarnings(colorRampPalette(brewer.pal(9, "Set2")))

	## Coordinate plot
	co_plot=ggplot(melted_data, aes_string(x="variable", y="value",  group=group_var, color=group_var),  environment = environment()) +
		geom_path(alpha = 0.9) +
		geom_point() +
		xlab("Variables") +
		ylab("Scaled value") +
		ggtitle("Coordinate Plot") +
		theme_bw() +
		theme(axis.text.x=element_text(angle = 90, vjust = 0.5), plot.title=element_text(size=14,face="bold")) +
		scale_fill_manual(values = getPalette(colourCount))

	plot(co_plot)

	if(print_table)
	{
		return(all_results_report)
	}
}


