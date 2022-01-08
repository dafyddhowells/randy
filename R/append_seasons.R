#' append_seasons
#'
#' Using the date of the fixture, the season is appended to a data frame of fixtures.
#' Typically seasons run from August to May however Covid rescheduling increased the length of the season in 2021.
#'
#' @param x A \code{data.frame} of fixtures with a \code{date} column
#' @import dplyr
#' @import lubridate
#' @import magrittr
#' @export
#' @examples
#' \dontrun{
#' df <- append_seasons()
#' }
#' @return This function returns a \code{data.frame} with the following column appended:
#' \describe{
#'  \item{season}{Character values representing the season that each match was played in.}
#' }
#' @author Daf Howells <dafhowells@yahoo.co.uk>

append_seasons <- function(x) {

  df_seasons_appended <-
    x %>%
    mutate(season =
             case_when(
               date >= dmy('15/08/1992') & date <= dmy('11/05/1993') ~ '1992/1993',
               date >= dmy('14/08/1993') & date <= dmy('08/05/1994') ~ '1993/1994',
               date >= dmy('20/08/1994') & date <= dmy('14/05/1995') ~ '1994/1995',
               date >= dmy('19/08/1995') & date <= dmy('05/05/1996') ~ '1995/1996',
               date >= dmy('17/08/1996') & date <= dmy('11/05/1997') ~ '1996/1997',
               date >= dmy('09/08/1997') & date <= dmy('10/05/1998') ~ '1997/1998',
               date >= dmy('15/08/1998') & date <= dmy('16/05/1999') ~ '1998/1999',
               date >= dmy('07/08/1999') & date <= dmy('14/05/2000') ~ '1999/2000',
               date >= dmy('19/08/2000') & date <= dmy('19/05/2001') ~ '2000/2001',
               date >= dmy('18/08/2001') & date <= dmy('11/05/2002') ~ '2001/2002',
               date >= dmy('17/08/2002') & date <= dmy('11/05/2003') ~ '2002/2003',
               date >= dmy('16/08/2003') & date <= dmy('15/05/2004') ~ '2003/2004',
               date >= dmy('14/08/2004') & date <= dmy('15/05/2005') ~ '2004/2005',
               date >= dmy('13/08/2005') & date <= dmy('07/05/2006') ~ '2005/2006',
               date >= dmy('19/08/2006') & date <= dmy('13/05/2007') ~ '2006/2007',
               date >= dmy('11/08/2007') & date <= dmy('11/05/2008') ~ '2007/2008',
               date >= dmy('16/08/2008') & date <= dmy('24/05/2009') ~ '2008/2009',
               date >= dmy('15/08/2009') & date <= dmy('09/05/2010') ~ '2009/2010',
               date >= dmy('14/08/2010') & date <= dmy('22/05/2001') ~ '2010/2011',
               date >= dmy('13/08/2011') & date <= dmy('13/05/2012') ~ '2011/2012',
               date >= dmy('18/08/2012') & date <= dmy('19/05/2013') ~ '2012/2013',
               date >= dmy('17/08/2013') & date <= dmy('11/05/2014') ~ '2013/2014',
               date >= dmy('16/08/2014') & date <= dmy('24/05/2015') ~ '2014/2015',
               date >= dmy('08/08/2015') & date <= dmy('17/05/2016') ~ '2015/2016',
               date >= dmy('13/08/2016') & date <= dmy('21/05/2017') ~ '2016/2017',
               date >= dmy('11/08/2017') & date <= dmy('13/05/2018') ~ '2017/2018',
               date >= dmy('10/08/2018') & date <= dmy('12/05/2019') ~ '2018/2019',
               date >= dmy('09/08/2019') & date <= dmy('26/07/2020') ~ '2019/2020',
               date >= dmy('12/08/2020') & date <= dmy('23/05/2021') ~ '2020/2021',
               date >= dmy('13/08/2021') & date <= dmy('22/05/2022') ~ '2021/2022'))

  return(df_seasons_appended)

}
