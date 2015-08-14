
multiplex.three<-function(networks,relation.list)
{
  if (length(relation.list) != 3)
  {
    stop('Must have exactly 3 relations');
  }
  nets<-networks[relation.list]
  output<- list(
    one = lapply(1:length(nets[[1]]),function(team) nets[[relation.list[1]]][[team]]+nets[[relation.list[1]]][[team]]*nets[[relation.list[2]]][[team]]+nets[[relation.list[1]]][[team]]*nets[[relation.list[3]]][[team]]+nets[[relation.list[1]]][[team]]*nets[[relation.list[2]]][[team]]*nets[[relation.list[3]]][[team]]),
    two = lapply(1:length(nets[[1]]),function(team) nets[[relation.list[2]]][[team]]+nets[[relation.list[1]]][[team]]*nets[[relation.list[2]]][[team]]+nets[[relation.list[2]]][[team]]*nets[[relation.list[3]]][[team]]+nets[[relation.list[1]]][[team]]*nets[[relation.list[2]]][[team]]*nets[[relation.list[3]]][[team]]),
    three = lapply(1:length(nets[[1]]),function(team) nets[[relation.list[3]]][[team]]+nets[[relation.list[3]]][[team]]*nets[[relation.list[2]]][[team]]+nets[[relation.list[1]]][[team]]*nets[[relation.list[3]]][[team]]+nets[[relation.list[1]]][[team]]*nets[[relation.list[2]]][[team]]*nets[[relation.list[3]]][[team]]),
    four = lapply(1:length(nets[[1]]),function(team) nets[[relation.list[1]]][[team]]*nets[[relation.list[2]]][[team]]+nets[[relation.list[1]]][[team]]*nets[[relation.list[2]]][[team]]*nets[[relation.list[3]]][[team]]),
    five = lapply(1:length(nets[[1]]),function(team) nets[[relation.list[2]]][[team]]*nets[[relation.list[3]]][[team]]+nets[[relation.list[1]]][[team]]*nets[[relation.list[2]]][[team]]*nets[[relation.list[3]]][[team]]),
    six = lapply(1:length(nets[[1]]),function(team) nets[[relation.list[1]]][[team]]*nets[[relation.list[3]]][[team]]+nets[[relation.list[1]]][[team]]*nets[[relation.list[2]]][[team]]*nets[[relation.list[3]]][[team]]),
    seven = lapply(1:length(nets[[1]]),function(team) nets[[relation.list[1]]][[team]]*nets[[relation.list[2]]][[team]]*nets[[relation.list[3]]][[team]])
  )
  
  output<-lapply(1:length(output),function(rel.num) 
  {
    templist <- lapply(1:length(output[[rel.num]]), function(team.num) 
    { 
      team <- output[[rel.num]][[team.num]];
      team[team!=1]<-0;  
      rownames(team) <- rownames(networks[[1]][[team.num]]);
      colnames(team) <- colnames(networks[[1]][[team.num]]);
      team;
    }
    );
    names(templist) <- names(networks[[1]]);
    templist;
  })
  
  names(output) <- c(
    paste(relation.list[1],".only",sep=''),
    paste(relation.list[2],".only",sep=''),
    paste(relation.list[3],".only",sep=''),
    paste(relation.list[1],relation.list[2],sep='.and.'),
    paste(relation.list[2],relation.list[3],sep='.and.'),
    paste(relation.list[1],relation.list[3],sep='.and.'),
    'all.ties'
  )
  return(output)
}