\name{xplot}
\alias{xplot}
\title{
  Graph \code{xplot}
}
\description{
 Graph \code{xplot}, this graph compares the appearance of entities or relations during one period
}
\usage{
  xplot(e1 = "", e2 = "", t = "")
}
\arguments{
  \item{e1}{O or 1 entity1 value }
  \item{e2}{a vector of entity2 value}
  \item{t}{a time value, format (mm.yyyy) or inteval of time value, for example: t=c("02.2010","02.2012")}
}
\details{
  Result after calling the function \code{xparse} has the following format:
  \enumerate{
              \item file_name:entity:$:list_value_found
              \item ...
              \item file_name:entity1:entity2:...:$$:value_e1:value_e2:....:negation
            }
  Function \code{xplot} will convert the data format above to a data frame. The \code{xplot} uses this data frame to display graphs.
  The data frame format: 
  \enumerate{
              \item column file : name of file
              \item column date : (format mm.yyyy)
              \item column value_date, this value is used for creating graph
              \item column visible: if visible = 1 then this record will be used in graph  
              \item column value of entite \code{e1} or \code{e2} or \code{e1} combined with \code{e2}
            }
}
\value{
  This function returns a data frame so that users can check or use it to create new graphs.
  \item{dataframe}{return a data frame}
}
\examples{
  xplot(e1="colza")
  xplot(e1="colza",e2=c("altice","rouille"))
  xplot(e1="colza",e2=c("altice","rouille"),t="09.2010")
  xplot(e1="colza",e2=c("altice","rouille"),t=c("09.2010","02.2011"))
}
\seealso{
  \code{\link{xhist}} type graphique histogram\cr
  \code{\link{xshow}} displays results of extracted data\cr
  \code{\link{xconfig}} system configuration\cr
}
\keyword{plot}
\keyword{graph}
