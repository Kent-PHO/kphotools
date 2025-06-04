#' @title kpho_trend
#'
#' @description This function creates a trend graph of a specific health indicator using ggplot and patchwork.
#' 
#' @param data Dataframe containing a long-form dataset of a health indicator.
#' @param values A unique variable found in 'data' containing numeric values to be plotted.
#' @param timeperiods A unique variable found in 'data' which list the different timeperiods which the values are calculated for.
#' @param group A unique variable found in 'data' to group the value outputs on the graph.
#' @param description A string describing what the values show.
#' @param stat_type A string describing the units of the values presented.
#' @param timerange A concatenated vector of years or timeperiods to plot on the trend graph.
#' @param palette A concatenated string of group names and color values to be applied to the graph. Default is set to null. Use `kpho_colours()`.
#' @param ci A logical variable for the inclusion of confidence intervals which defaults to FALSE. If set to TRUE, the columns 'lowercl' and 'uppercl' in 'data' are needed.
#' @return A patchwork object.
#' 
#' @examples 
#' smoking_trend <- kpho_trend(data = smoking_prevalence,
#'                             values = "numbers",
#'                             timeperiods = "years",
#'                             group = "area",
#'                             description = "Smoking prevalence by district in Kent"
#'                             stat_type = "Prevalence (%)"
#'                             timerange = c("2012", "2013", "2014", "2015"),
#'                             ci = FALSE)
#' 
#' @export
kpho_trend <- function(data,
    values,
    timeperiods,
    group,
    description,
    stat_type,
    timerange,
    palette=NULL,
    ci=FALSE){

    #Checks all the columns in data that are required exist
    if(values %in% names(data)){
        if(timeperiods %in% names(data)){
            print("All variables needed are found in the data passed to the function.")
        } else{
            stop("The specified 'timeperiods' must exist as a column in 'data'.")
        }
    } else{
        stop("The specified 'values' must exist as a column in 'data'.")
    }

    #Check the value variable is numeric so it is passed as a continuous variable
    if(is.numeric(as.numeric(data[[values]]))){
        print("The 'values' variable is numeric and will be used as a continuous measure.")
    } else{
        stop("The 'values' variable is not numeric and therefore cannot be plotted. Please coerce the class of the variable 'value' to be numeric.")
    }

    #Check that lowercl and uppercl exist in data if ci is set to TRUE
    if(ci==TRUE){
        lowercl <- "lowercl"
        uppercl <- "uppercl"
        
        if(ci==TRUE){
            if(lowercl %in% names(data)){
                print("ci set to TRUE. lowercl found in 'data'")
            } else {
                stop("If ci is set to TRUE, the columns 'lowercl' need to exist in 'data'.")
            }
        }

        if(ci==TRUE){
            if(uppercl %in% names(data)){
                print("ci set to TRUE. uppercl found in 'data'")
            } else {
                stop("If ci is set to TRUE, the columns 'uppercl' need to exist in 'data'.")
            }
        } else{
            print("ci is set to FALSE (by default). Confidence intervals will not be plotted.")
        }
    }

    #Filter the data for the timerange selected
    data <- data %>% filter(timeperiod %in% timerange)

    #Check a valid combination is selected
    if(nrow(data)==0){
        stop("The 'timerange' chosen is not valid and has resulted in 0 rows. Please check the timerange you've selected is available in the data you're using.")
    }

    #Plotting
    if(ci==TRUE){a <- ggplot()+
    geom_line(data=data, aes(x=.data[[timeperiods]], y=.data[[values]], group=.data[[group]], color=.data[[group]]), linewidth=1.5)+
    geom_ribbon(data=data, aes(x=.data[[timeperiods]], ymin=.data[[lowercl]], ymax=.data[[uppercl]], group=.data[[group]], fill=.data[[group]]), alpha=0.2, color="transparent")+
    geom_point(size=3)+
    scale_y_continuous(expand=c(0,0))+
    scale_x_discrete(expand=c(0,0))+
    labs(y=paste0(stat_type), x="Time", color=paste0(group))+
    theme(
        legend.key.size = unit(2, "lines"),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm"),
        legend.text = element_text(size = rel(1.1), margin = margin(l = 8)), 
        legend.title = element_text(size = rel(1.1)), 
        legend.spacing = unit(0.5, "cm"), 
        plot.caption = element_text(size = 12.5, hjust = 1),
        plot.title= element_text(size=rel(1.4)),
        axis.text=element_text(size=rel(1.2)),
        axis.title=element_text(size=rel(1.3)),
        legend.position="bottom",
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm")
    )+
    guides(color=guide_legend(nrow=2))

    if(!is.null(palette)){
        a <- a + scale_color_manual(values=palette)+
        scale_fill_manual(values=palette)
    }

    title <- paste0(description, " between ", timerange[1], " and ", timerange[length(timerange)])

    a <- a + plot_annotation(title=title, theme=theme(plot.title=element_text(size=rel(1.8), face="bold", hjust=0.5)))

    print(a)

    } else{ a <- ggplot()+
    geom_line(data=data, aes(x=.data[[timeperiods]], y=.data[[values]], group=.data[[group]], color=.data[[group]]), linewidth=1.5)+
    geom_point(size=3)+
    scale_y_continuous(expand=c(0,0))+
    scale_x_discrete(expand=c(0,0))+
    labs(y=paste0(stat_type), x="Time", color=paste0(group))+
    theme(
        legend.key.size = unit(2, "lines"),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm"),
        legend.text = element_text(size = rel(1.1), margin = margin(l = 8)), 
        legend.title = element_text(size = rel(1.1)), 
        legend.spacing = unit(0.5, "cm"),
        plot.caption = element_text(size = 12.5, hjust = 1),
        plot.title= element_text(size=rel(1.4)),
        axis.text=element_text(size=rel(1.2)),
        axis.title=element_text(size=rel(1.3)),
        legend.position="bottom",
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm")
    )+
    guides(color=guide_legend(nrow=2))

    if(!is.null(palette)){
        a <- a + scale_color_manual(values=palette)
    }

    title <- paste0(description, " between ", timerange[1], " and ", timerange[length(timerange)])
    
    a <- a + plot_annotation(title=title, theme=theme(plot.title=element_text(size=rel(1.8), face="bold", hjust=0.5)))

    print(a)
    }
}