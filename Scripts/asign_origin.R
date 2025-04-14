# función para asignar el origen de los fragmentos
# recibe un vector y devuelve un vector

asign_origin<- function(code=NULL)
{
  origen<-NULL
  for (structure in seq_along(code)) 
  {
    if (code[structure]=="6"|code[structure]=="7"|code[structure]=="8"|
        code[structure]=="9"|code[structure]=="10"|code[structure]=="171"|
        code[structure]=="172"|code[structure]=="175")
    {
      origen[structure]<-"Matapalo"
    }
    else if (code[structure]=="11"|code[structure]=="12"|code[structure]=="13"|
             code[structure]=="14"|code[structure]=="15"|
             code[structure]=="16"|code[structure]=="17") 
    {
      origen[structure]<-"Jicaro"
    }
    else if (code[structure]=="18"|code[structure]=="19"|code[structure]=="20"|
             code[structure]=="173"|code[structure]=="174"|code[structure]=="169"|
             code[structure]=="170") 
    {
      origen[structure]<-"Marina"
    }
    else {origen[structure]<-NA}
    
  }
  return(origen)
}