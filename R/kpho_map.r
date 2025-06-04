#' @title kpho_map
#'
#' @description This function creates a map using data and boundaries passed to the function. The parameters passed to the function have minimum column name requirements. Please check these below before using the function.
#' 
#' @param data A dataframe to be plotted on the map.
#' @param joining_code A unique variable found in 'data' and 'boundaries'.
#' @param values A unique variable found in 'data' containing numeric values to be plotted.
#' @param timeperiods A unique variable found in 'data' which list the different timeperiods which the values are calculated for.
#' @param description A string describing what the values show.
#' @param stat_type A string describing the units of the values presented.
#' @param boundaries Spatial boundaries for the data to be plotted on. These should be spatial objects that correspond to the same geography type that the data is calculated for (e.g. lower-super output area boundaries) and should be an sf object.
#' @param year A timeperiod or year to filter the data shown on the map.
#' @return A patchwork object.
#' 
#' @examples 
#' smoking_map <- kpho_map(data = smoking_prevalence,
#'                         joining_code = "area_code",
#'                         values = "value",
#'                         timeperiods = "years",
#'                         description = "Number of people smoking in Kent",
#'                         stat_type = "Rate per 100,000 people",
#'                         boundaries = msoa, 
#'                         year = "2013/14-2014/15")
#' 
#' @export
kpho_map <- function(data, 
    joining_code,
    values,
    timeperiods,
    description,
    stat_type,
    boundaries, 
    year){

    #Checks all the columns in data that are required exist
    if(joining_code %in% names(data)){
        if(values %in% names(data)){
            if(timeperiods %in% names(data)){
                print("All variables needed are found in the data passed to the function.")
            } else{
                stop("The specified 'timeperiods' must exist as a column in 'data'.")
            }
        } else{
            stop("The specified 'values' must exist as a column in 'data'.")
        }
    } else{
        stop("The specified 'joining_code' must exist as a column in 'data'.")
    }

    #Check the value variable is numeric so it is passed as a continuous variable
    if(is.numeric(as.numeric(data[[values]]))){
        print("The 'values' variable is numeric and will be used as a continuous measure.")
    } else{
        stop("The 'values' variable is not numeric and therefore cannot be plotted. Please coerce the class of the variable 'value' to be numeric.")
    }

    #Check area_code exists in boundaries so joining works
    if(joining_code %in% names(boundaries)){
        print("Joining variable exists")
    } else{
        stop("The variable 'joining_code' does not exist in the boundaries being passed to the function. Please check the spatial object you are using.")
    }

    #Check that boundaries is a spatial object
    if("geometry" %in% names(boundaries)){
        print("Boundaries passed to the function contain spatial geometry.")
    } else{
        stop("The boundaries passed to the function might not be a spatial object. Please check again.")
    }

    #Filter to the timeperiod selected
    data <- data %>% filter(year %in% data[[timeperiods]])

    #Check there is a valid combination
    if(nrow(data)==0){
        stop("The year/timeperiod you have selected does not appear in the 'timeperiods' range you have selected")
    }

    #Check there are the same number of spatial rows as data rows
    if(nrow(data)!=nrow(boundaries)){
        warning("There are a different number of rows in the data and boundaries objects being passed to the function. Not all areas/data points may be plotted. Please check.")
    }

    #Join the data to the boundaries so it can be plotted
    plotting_data <- inner_join(boundaries, data, by=c(joining_code)) #join data to msoa and keep as sf object

    if(nrow(plotting_data)==0){
        stop("The 'joining_code' specified returned 0 rows. Please ensure this variable exists in both 'data' and 'boundaries'")
    }

    #Kent and Medway
    a <- ggplot()+ #create plot a
    geom_sf(data=plotting_data, aes(fill=.data[[values]]), color="black")+
    scale_fill_viridis(option="G", direction=-1)+
    labs(fill=paste0(stat_type),
        caption="")+
    theme_void() +  
    theme(
        plot.background = element_rect(fill="#E0E0E0", color=NA),
        legend.key.size = unit(2, "lines"),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm"),
        legend.text = element_text(size = rel(1.1), margin = margin(l = 8)), 
        legend.title = element_text(size = rel(1.1)), 
        legend.spacing = unit(0.5, "cm"),
        plot.margin = margin(t = 0, r = 5, b = 0, l = 5), 
        plot.caption = element_text(size = 12.5, hjust = 1),
        plot.title= element_text(size=rel(1.4)),
        legend.position="bottom" 
    )

    title <- paste0(description, " during ", plotting_data[1, timeperiods])

    a <- a + plot_annotation(
        title=title,
        caption=paste0("Digitial boundary source:\nOffice for National Statistics licensed under the Open Government License v.3.0\nContains OS data © Crown Copyright and database", " ", format(Sys.Date(), "%Y"), "\n", title, "\nProduced by KPHO")) &
        theme(plot.title=element_text(size=rel(1.8), face="bold", hjust=0.5)) #add title and timeperiod, change title format

    print(a)
}