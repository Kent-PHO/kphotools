#' @title kpho_colours
#'
#' @description This function prints standard colour schemes for different geography types plotted on KPHO charts. 
#' 
#' @param geography String of the type of geography to get the colour scheme for. Options include: `"district"`, `"hcp"`, `"nhs_trust"`, `"pcn"`, `"utla_national"` and `"district_medway"`
#' @return A patchwork object.
#' 
#' @examples 
#' kpho_colours("district")
#' 
#' @export
kpho_colours <- function(geography){
    
    district <- c("Ashford"="", "Canterbury"="", "Dartford"="", "Dover"="", "Folkestone & Hythe"="", "Gravesham"="", "Maidstone"="", "Sevenoaks"="", "Swale"="", "Thanet"="",
        "Tonbridge & Malling"="", "Tunbridge Wells"="")

    hcp <- c("East Kent HCP"="", "West Kent HCP"="", "Medway & Swale HCP"="", "Dartford, Gravesham & Swanley HCP"="")

    nhs_trust <- c()

    utla_national <- c("Kent"="#2D7C82", "Medway"="#D04F30", "Kent & Medway"="#7D3F64", "South east region"="#338543", "England"="#0245A1")
    
    district_medway <- c("Ashford"="", "Canterbury"="", "Dartford"="", "Dover"="", "Folkestone & Hythe"="", "Gravesham"="", "Maidstone"="", "Sevenoaks"="", "Swale"="", "Thanet"="",
        "Tonbridge & Malling"="", "Tunbridge Wells"="", "Medway"="")
    
    if(geography=="district"){
        paste0(district)
    } else if(geography=="hcp"){
        paste0(hcp)
    } else if(geography=="nhs_trust"){
        paste0(nhs_trust)
    } else if (geography=="utla_national"){
        paste0(utla_national)
    } else if(geography=="district_medway"){
        paste0(district_medway)
    } else{
        stop("Please select a valid geography. You can choose from: district, hcp, nhs_trust or pcn.")
    }
}