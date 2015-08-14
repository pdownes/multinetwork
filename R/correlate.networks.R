correlate.networks <-
function(networks)
{
  if (is.null(names(networks[[1]])))
  {
    networks <- lapply(networks,function(relation)
    {
      names(relation) <- 1:length(relation);
      relation;
    }) 
  }
  cors<-data.frame(team=names(networks[[1]]))
  headers<-'team'
  
  for (relation1 in names(networks))
  {
    for (relation2 in names(networks))
    {
      if (which(names(networks) == relation1) < which(names(networks) == relation2))
      {
        headers<-c(headers,paste(relation1,relation2,sep="."))
        cors<-cbind(cors,as.data.frame(sapply(names(networks[[relation1]]),function(team) 
          {
            round(cor(as.vector(networks[[relation1]][[team]]),as.vector(networks[[relation2]][[team]]),use='pairwise.complete.obs'),digits=4);
          })))
      }
    }
  }
  colnames(cors)<-headers
  return(cors)
}
