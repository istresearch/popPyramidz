library(ggplot2)
library(RColorBrewer)
library(plotly)

#' A plotting Function for Population Pyramids
#'
#' This function pulls down census population data and puts it into a nice dataframe.
#' @param country.frame Enter dataframe generated from get_data()
#' @param plot.title (optional) title of plot
#' @keywords census population dataframe age gender
#' @export
#' @import ggplot2 RColorBrewer plotly
#' @examples
#' plot_pyramid(iraq.frame, 'Iraq Population Pyramid')

plot_pyramid <- function(country.frame,plot.title='Population Pyramid',plotly=F) {
  
  if (!is.element('Population',colnames(country.frame)) |
      !is.element('Age',colnames(country.frame)) | 
      !is.element('Gender',colnames(country.frame)) ) {
    return('This is not a valid df. Use get_data() to download your data.')
  }
  
  else {
    
    country.frame <- within(country.frame, 
                       Age <- factor(Age, 
                          levels=c('0-4','5-9','10-14','15-19','20-24','25-29',
                                   '30-34','35-39','40-44','45-49','50-54','55-59',
                                   '60-64','65-69','70-74','75-79','80-84','85-89',
                                   '90-94','95-99','100+')))
    
    country.frame <- within(country.frame,
                            Gender <- factor(Gender,
                                             levels=c('Male','Female')))
    
    if (max(country.frame$Population) > 100) {
      
      percents.male <- country.frame$Population[country.frame$Gender == 'Male'] / 
        sum(country.frame$Population[country.frame$Gender == 'Male']) * 100
      country.frame$Population[country.frame$Gender == 'Male'] <- percents.male
      percents.female <- country.frame$Population[country.frame$Gender == 'Female'] / 
        sum(country.frame$Population[country.frame$Gender == 'Female']) * 100
      country.frame$Population[country.frame$Gender == 'Female'] <- percents.female * (-1)
      
    }
    
    plot_breaks <- seq(
      -as.integer(max(abs(country.frame$Population))/5)*5,
      as.integer(max(abs(country.frame$Population))/5)*5,by=5)
    
    p <- ggplot(country.frame, aes(x = Age, y = Population, fill = Gender)) + 
      geom_bar(subset = .(Gender == "Male"), stat = "identity") + 
      geom_bar(subset = .(Gender == "Female"), stat = "identity") + 
      coord_flip() + 
      scale_x_discrete(drop=FALSE) +
      scale_fill_brewer(palette = "Set1") + 
      scale_y_continuous(name='Percent', breaks=plot_breaks, 
                         labels=abs(plot_breaks), 
        limits=c(min(country.frame$Population) - 2,
                 max(country.frame$Population) + 2)) +
      theme_bw() +
      ggtitle(plot.title)
    
    if (plotly) {
      return(ggplotly(p))
    }
    else {
      return(p)
    }
    
  }
  
}
