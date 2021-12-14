# This is a small R packages that plots sectoral financial balances of countries in the AMECO data set.
#
#


# Function: input dataset and country name, output sectoral balances plot
plot_SFB <- function(data, country) {
  ggplot2::ggplot(
    data = data[data$Country == country,],
    mapping = ggplot2::aes(x=period, y=balance)
  ) +
    ggplot2::geom_line(mapping = ggplot2::aes(color=Sector)) +
    ggplot2::ggtitle(country)+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))+
    ggplot2::labs(caption="Source: AMECO")+
    ggplot2::ylab("Financial Balance in % of GDP")+
    ggplot2::xlab("Year")

}


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
  #list_of_countries= "germany"
  # converts county names into standardized country codes
  country <- countrycode::countrycode(list_of_countries,
                                      "country.name", "wb")

  #  ids for sectoral net savings and gdp
  sectorcodes <- c("UBLH", "UBLC", "UBLA", "UBLGE", "UVGD")

  # create ids in the rdbnomics format (e. g. "AMECO/UBLH/DEU.1.0.0.0.UBLH)
  idcodes <- vector()
  idcodes <- rep(paste0("AMECO/", sectorcodes), each = length(country))
  idcodes <- paste0(idcodes, "/", country)
  idcodes <- paste0(idcodes, ".1.0.0.0.", rep(sectorcodes, each = length(country)))

  # load AMECO data for requested countries
  df <- rdbnomics::rdb(ids = idcodes);

  # reshape data to wide format and delete missing values
  df_wide <- data.table::dcast(df, period+Country+geo~dataset_code, value.var="value")
  df_wide <- na.omit(df_wide)

  # shorten data to time frame
  t_start <- paste0(t_start, "-01-01")
  t_end <- paste0(t_end, "-01-01")
  df_wide <- df_wide[df_wide$period >= t_start & df_wide$period <= t_end,]

  # calculate sectoral balances to GDP
  df_wide$Foreign <- -df_wide$UBLA/df_wide$UVGD
  df_wide$Corporations<- df_wide$UBLC/df_wide$UVGD
  df_wide$Households <- df_wide$UBLH/df_wide$UVGD
  df_wide$Government <- df_wide$UBLGE/df_wide$UVGD

  # select columns
  df_wide <- df_wide[,c("period", "Country", "Households", "Corporations", "Government", "Foreign")]

  # return data if argument "getData" was set TRUE
  if(isFALSE(getPlot) & isTRUE(getData)){
    return(df_wide)
  }

  # reshape data to long format and rename
  df_long <- data.table::melt(df_wide, id.vars=c("period", "Country"), variable.name = "Sector", value.name = "balance")
  # colnames(df_long) <- c("Year", "Country", "Sector", "Financial Balance in % of GDP")
  # names(df_long)[names(df_long) == 'variable'] <- 'sector'
  # names(df_long)[names(df_long) == 'value'] <- 'balance'

  # plot sectoral balances using plot_sectoral_balances function
  plots <- lapply(list_of_countries, plot_SFB, data = df_long)

  # return plot and data
  if(isFALSE(getData)){
    return(plots)
  } else {
    return(list(plots, df_wide))
  }

}


