# This is a small R packages that plots sectoral financial balances of countries in the AMECO data set.

#' #' Sectoral Balances
#' #'
#' #' This function receives AMECO sectoral balances data and outputs data and plots
#' #'
#' @param list_of_countries List of countries
#' @param t_start start year
#' @param t_end end year
#' @param getData output data table
#' @param getPlot output sectoral balance plot
#' @return data and/or plots of sectoral balances
#' @export
sfb <- function(list_of_countries, t_start = 1950, t_end=9999, getData=F, getPlot=T){

  # change first letter to uppercase
  list_of_countries <- stringr::str_to_title(list_of_countries)

  # create AMECO ID-Codes using create_id() function
  idcodes <- create_id(list_of_countries)

  # load AMECO data for requested countries
  df <- rdbnomics::rdb(ids = idcodes);

  # clean up data using clean_data() function
  df_wide <- clean_data(data = df, t_start = t_start, t_end = t_end)

  # calculate sectoral balances to GDP using calculate_sfb() function
  df_wide <- calculate_sfb(data = df_wide)

  # select columns
  df_wide <- df_wide[,c("period", "Country", "Households", "Corporations", "Government", "Foreign")]

  # return data if argument "getData" was set TRUE
  if(isFALSE(getPlot) & isTRUE(getData)){
    return(df_wide)
  }

  # plot sectoral balances using plot_SFB() function
  plots <- lapply(list_of_countries, plot_SFB, data = df_wide)

  # return plot and data
  if(isFALSE(getData)){
    return(plots)
  } else {
    return(list(plots, df_wide))
  }
}


# Function: create_id()
# Create AMECO id-codes for sectoral net savings
create_id <- function(list_of_countries){
  # converts county names into standardized country codes
  country <- countrycode::countrycode(list_of_countries,
                                      "country.name", "wb")

  #  ids for sectoral net savings and gdp
  sectorcodes <- c("UBLH", "UBLC", "UBLA", "UBLGE", "UVGD")

  # create ids in the rdbnomics format (e. g. "AMECO/UBLH/DEU.1.0.0.0.UBLH)
  idcodes <- vector()
  if(length(list_of_countries) == 1){
    idcodes <- paste0("AMECO/", sectorcodes, "/", country, ".1.0.0.0.", sectorcodes)
  } else{
    idcodes <- rep(paste0("AMECO/", sectorcodes), each = length(country))
    idcodes <- paste0(idcodes, "/", country)
    idcodes <- paste0(idcodes, ".1.0.0.0.", rep(sectorcodes, each = length(country)))
  }

  return(idcodes)
}

# Function: clean_data()
# Clean up data
clean_data <- function(data, t_start, t_end){
  df <- data

  # reshape data to wide format and delete missing values
  df_wide <- data.table::dcast(df, period+Country+geo~dataset_code, value.var="value")
  df_wide <- na.omit(df_wide)

  # shorten data to time frame
  t_start <- paste0(t_start, "-01-01")
  t_end <- paste0(t_end, "-01-01")
  df_wide <- df_wide[df_wide$period >= t_start & df_wide$period <= t_end,]

  return(df_wide)
}


# Function: calculate_sfb()
# Calculate sectoral balances of GDP
calculate_sfb <- function(data){
  df_wide <- data
  df_wide$Foreign <- -df_wide$UBLA/df_wide$UVGD*100
  df_wide$Corporations<- df_wide$UBLC/df_wide$UVGD*100
  df_wide$Households <- df_wide$UBLH/df_wide$UVGD*100
  df_wide$Government <- df_wide$UBLGE/df_wide$UVGD*100

  return(df_wide)
}


# Function: plot_SFB()
# Function: input dataset and country name, output sectoral balances plot
plot_SFB <- function(data, country) {
  df_long <- data.table::melt(data,
                              id.vars=c("period", "Country"),
                              variable.name = "Sector",
                              value.name = "balance")

  ggplot2::ggplot(
    data = df_long[df_long$Country == country,],
    mapping = ggplot2::aes(x=period, y=balance)
  ) +
    ggplot2::geom_line(mapping = ggplot2::aes(color=Sector), size = 2) +
    ggplot2::geom_hline(yintercept = 0)+
    ggplot2::ggtitle(country)+
    ggplot2::theme_bw()+
    ggplot2::theme(axis.title=ggplot2::element_blank())
}


