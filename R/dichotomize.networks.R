dichotomize.networks <-
function(networks,method=c('grand.mean','relation.mean','team.mean','value','specific.list'),value=NULL,specific.list=NULL)
{
  #set comparator
  if (method == 'relation.mean' || is.null(method))#default is relation.mean
  {
    numer <- lapply(networks,function(tie.type) 
      sapply(tie.type,sum,na.rm=T)
    )
    denom <- lapply(
      lapply(networks,function(tie.type) 
        sapply(tie.type,length)#THIS NEEDS TO BE REVISED TO HANDLE NA VALUES
      ),
      function(x) x-sqrt(x)
    )
    tmean <- lapply(names(networks),function(tie.type) 
      sum(numer[[tie.type]])/sum(denom[[tie.type]])
    )
    print(paste(names(networks),"mean:",tmean,sep=" "))
    names(tmean) <- names(networks)
    comparator <- lapply(names(networks),function(tie) rep(tmean[[tie]],length(networks[[tie]])))
    names(comparator) <- names(networks)
  }else if (method == 'grand.mean'){
    numer <- lapply(networks,function(tie.type) 
      sapply(tie.type,sum,na.rm=T)
    )
    denom <- lapply(
      lapply(networks,function(tie.type) 
        sapply(tie.type,length)
      ),
      function(x) x-sqrt(x)
    )
    gmean <- sum(sapply(numer,sum))/sum(sapply(denom,sum))
    print(paste("Grand mean: ",gmean,sep=''))
    comparator <- lapply(networks,function(tie) rep(gmean,length(tie)))
  }else if (method == 'team.mean'){
    comparator <- lapply(networks,function(tie.type) 
      sapply(tie.type,mean,na.rm=T)
    )
    print("Team Means:")
    print(comparator)
  }else if (method == 'value'){
    if (is.null(value)){
      stop("Cannot dichotomize...enter a 'value' argument to use value method\n")
    }
    comparator <- lapply(networks,function(tie) rep(value,length(tie)))
  }else if (method == 'specific.list'){
    comparator <- specific.list
  }else{
    stop("A method input is required")
  }
  
  #use the comparator object to dichotomize
  new.networks <- lapply(names(networks),function(relation) 
    lapply(1:length(networks[[relation]]),function(team)
    {
      temp <- matrix(
        sapply(networks[[relation]][[team]],function(tie) 
          ifelse(tie > comparator[[relation]][[team]],1,0)
        ),
        nrow(networks[[relation]][[team]]),
        ncol(networks[[relation]][[team]])
      );
      rownames(temp) <- rownames(networks[[relation]][[team]])
      colnames(temp) <- colnames(networks[[relation]][[team]])
      temp;
    })
  )
  names(new.networks) <- names(networks)
  if (is.null(names(new.networks[[1]])))
  {
    new.networks <- lapply(new.networks,function(relation)
    {
      names(relation) <- names(networks[[1]]);
      relation;
    }) 
  }
  return(new.networks)
}
