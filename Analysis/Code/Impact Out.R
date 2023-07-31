
impout<-function(x){
  a<-data.frame(x$res$direct)
    names(a) <- "Direct"
    a$variable<-row.names(a)
    row.names(a)<-NULL
  b<-data.frame(x$res$indirect)
    names(b) <- "Indirect"
    b$variable<-row.names(b)
    row.names(b)<-NULL
  c<-data.frame(x$res$total)
    names(c) <- "Total"
    c$variable<-row.names(c)
    row.names(c)<-NULL
    
  temp <- a %>%
    full_join(., b, by="variable") %>%
    full_join(., c, by="variable") %>%
    select(., variable, Direct, Indirect, Total)
  rm(a,b,c)
  
  a<-data.frame(x$pzmat)
    names(a)<-c("Direct P-Val", "Indirect P-Val", "Total P-Val")
    a$variable<-row.names(a)
    row.names(a)<-NULL
  
  temp <- temp %>%
    full_join(., a, by="variable") %>%
    select(variable, Direct, 'Direct P-Val', Indirect, 'Indirect P-Val', Total, 'Total P-Val')
  
  return(temp)
}

