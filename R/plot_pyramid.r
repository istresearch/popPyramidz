library(ggplot2)
library(RColorBrewer)

#' A plotting Function for Population Pyramids
#'
#' This function pulls down census population data and puts it into a nice dataframe.
#' @param country.frame Enter dataframe generated from get_data()
#' @param plot.title (optional) title of plot
#' @keywords census population dataframe age gender
#' @export
#' @import ggplot2 RColorBrewer
#' @examples
#' plot_pyramid(iraq.frame, 'Iraq Population Pyramid')

plot_pyramid <- function(country.frame,plot.title='Population Pyramid') {
  
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
    
    p <- ggplot(country.frame, aes(x = Age, y = Population, fill = Gender)) + 
      geom_bar(subset = .(Gender == "Male"), stat = "identity") + 
      geom_bar(subset = .(Gender == "Female"), stat = "identity") + 
      coord_flip() + 
      scale_size_area() +
      scale_fill_brewer(palette = "Set1") + 
      theme_bw() +
      ggtitle(plot.title)
    
    plot(p)
    
  }
  
}
