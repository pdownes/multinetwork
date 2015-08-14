build.networks <-
function(data,tielist,groupcol,indcol)
{
  networks <- lapply(names(tielist),function(tie) 
    {
      sapply(split(data,data[,groupcol]),function(team) 
      {
        #manage teams smaller than the largest team
        if (nrow(team) < length(tielist[[tie]])+1)
          {
            ties <- tielist[[tie]][1:nrow(team)-1];
          }else{
            ties <- tielist[[tie]];
          }
        
        temp <- as.matrix(
          do.call(
            rbind,lapply(1:nrow(team),function(x) 
              {
                temp <- as.numeric(team[x,append(ties,indcol,x-1)]); # append is a function that appends argument 2 to the position specified by argument 3 in the vector specified in argument 1 
                #(1:nrow(team)); # Eean's question for the future: This means that the node IDs for every team become 1:nrow (1, 2, 3, 4...). What if we want to preserve the original node IDs for future printing or identification? 
                
                temp;
              })
              )
              );
        rownames(temp) <- team[,indcol]
        colnames(temp) <- team[,indcol]
        temp;
      })
  })
  
  #cannot replace diagonals in the above process
  networks  <- lapply(networks,function(tie.type) 
    sapply(tie.type,function(team) 
      {
        diag(team) <- NA;
        team;
      })
    )
  
  names(networks) <- names(tielist)
  #Set all the team names to numbers 1 : number of teams
  if (is.null(names(networks[[1]])))
  {
    networks <- lapply(networks,function(relation)
    {
      names(relation) <- data[,groupcol];
      relation;
    }) 
  }
  return(networks) #Eean's Feature Request: Define the network list elements by the original Team IDs, and define the rows and columns within each list element (team) as the original Node IDs.
  
}
