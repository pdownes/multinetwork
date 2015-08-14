analyze.networks <-
function(networks,...) #Eean's FEATURE REQUEST - MAKE THE ... ARGUMENT REQUEST THE LIST OF IGRAPH MEASURES YOU WANT CALCULATED ON YOUR NETWORKS. IF NONE ARE SUPPLIED, IT WILL CALCULATE ALL OF THESE??? THAT MAY BE A STRANGE DEFAULT, BUT WE CAN REVISIT THAT.
{
  library('igraph')
  output <- do.call('cbind',lapply(networks,function(relation)
  {
    do.call('rbind',lapply(relation,function(team) 
    {
      # need to think about this - I need to add ifs for each argument to trigger each calculation, but I'm not sure how to compose the data frame once all the vectors are computed. There has to be a better way!
      graph<-graph.adjacency(team);
      density <- graph.density(graph);
      clusters <- clusters(graph)$no;
      all.dc<-centralization.degree(graph,mode=c('all'),loops=FALSE,normalized=TRUE);
      in.dc<-centralization.degree(graph,mode=c('in'),loops=FALSE,normalized=TRUE);
      out.dc<-centralization.degree(graph,mode=c('out'),loops=FALSE,normalized=TRUE);
      total.dc <- centralization.degree(graph,mode=c('total'),loops=FALSE,normalized=TRUE);
      bc<-centralization.betweenness(graph);
      cc<-centralization.closeness(graph);
      trans <- transitivity(graph);
      #data.frame(density=as.numeric(density),clusters=as.numeric(clusters),all.dc=as.numeric(all.dc$centralization),in.dc=as.numeric(in.dc$centralization),out.dc=as.numeric(out.dc$centralization),bc=as.numeric(bc$centralization),cc=as.numeric(cc$centralization),total.dc=as.numeric(total.dc$centralization));
      data.frame(density=(density),clusters=(clusters),all.dc=(all.dc$centralization),in.dc=(in.dc$centralization),out.dc=(out.dc$centralization),bc=(bc$centralization),cc=(cc$centralization),total.dc=(total.dc$centralization),trans=trans);
    }));
  }));
  output$TeamID  <- names(networks[[1]])
  return(output)
  #   output <- lapply(names(output),function(relation)
  #   {
  #     temp <- t(output[[relation]]);
  #     names(temp) <- paste(relation,rownames(output[[relation]]),sep='.');
  #     temp;
  #   })
  #   output <- do.call('cbind',output)
  #   return(as.data.frame(output) )
}
