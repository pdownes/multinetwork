symmetrize.networks <-
function(networks,method=c('max','min','mean','sum'),na.to.zero=FALSE) # EEAN'S FEATURE REQUEST: CHANGE SYMMETRIZATION TO MAX MIN AND MEAN. 
{
  if (na.to.zero)
  {
    lapply(networks,function(relation) lapply(relation,function(team)
    {
      team[is.na(team)] <- 0;
      diag(team) <- NA;
      team;
    }))
  }
  new.nets <- lapply(networks,function(relation) lapply(relation,function(team) 
    { 
        if (method =='max')
        {
          temp <- mapply(team,t(team),FUN=max);
        }else if (method =='min'){
          temp <- mapply(team,t(team),FUN=min);
        }else if (method=="mean"){
          temp <- mapply(team,t(team),FUN=sum)/2;
        }else if (method=="sum"){
          temp <- mapply(team,t(team),FUN=sum); 
        }
        temp <- matrix(temp,nrow(team),ncol(team));
        rownames(temp) <- rownames(team);
        colnames(temp) <- colnames(team);
        temp;
    })
  )
  return(new.nets)
}
