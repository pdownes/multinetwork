multiplex.two <-
function(networks,relation1,relation2)
{
  new.networks <- list(relation1=networks[[relation1]],relation2=networks[[relation2]])
  new.networks[[2]] <- lapply(new.networks[[2]],function(x) x*2)
  
  new.networks <- rep(
    list(
      lapply(1:length(new.networks[[1]]),function(team.num)
      {
        temp <- matrix(mapply(sum,new.networks[[1]][[team.num]],new.networks[[2]][[team.num]]),nrow=nrow(new.networks[[1]][[team.num]]),ncol=ncol(new.networks[[1]][[team.num]]));
        rownames(temp) <- rownames(new.networks[[1]][[team.num]]);
        colnames(temp) <- colnames(new.networks[[1]][[team.num]]);
        temp;
      })
    ),3)
  
  new.networks <- lapply(1:length(new.networks),function(rel.num) 
    {
      lapply(new.networks[[rel.num]],function(team)
        {
          temp <- matrix(sapply(team,function(y) 
            ifelse(is.na(y),NA,ifelse(y==rel.num,1,0))),
          nrow=nrow(team),ncol=ncol(team));
          rownames(temp) <- rownames(team);
          colnames(temp) <- colnames(team);
          temp;
      })
    })
  names(new.networks) <- c(paste(relation1,".only",sep=''),paste(relation2,".only",sep=''),'both');
  if (is.null(names(new.networks[[1]])))
  {
    new.networks <- lapply(new.networks,function(relation)
    {
      names(relation) <- names(networks[[1]]);
      relation;
    }) 
  }  
  return(new.networks);
}
