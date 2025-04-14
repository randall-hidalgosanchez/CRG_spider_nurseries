# función para cambiar el número de cada estructura
# debido a que en ocasiones se cae la etiqueta y
# esta se cambia pero la estructura es la misma

# recibe el número de estructura en la base de datos
# devuelve el número que tuvo dicha estructura al inicio

fix_structure<- function(actual=NULL)
{
  original<-NULL
  for (structure in seq_along(actual)) 
  {
    if (actual[structure]=="285"|actual[structure]=="90")
    {
      original[structure]<-"15"
    }
    else if (actual[structure]=="283") 
    {
      original[structure]<-"96"
    }
    else if (actual[structure]=="78") 
    {
      original[structure]<-"7"
    }
    else if (actual[structure]=="160") 
    {
      original[structure]<-"10"
    }
    else if (actual[structure]=="84") 
    {
      original[structure]<-"16"
    }
    else if (actual[structure]=="101") 
    {
      original[structure]<-"12"
    }
    else if (actual[structure]=="103"|actual[structure]=="17 (103)") 
    {
      original[structure]<-"17"
    }
    else if (actual[structure]=="104"|actual[structure]=="19 (104)") 
    {
      original[structure]<-"19"
    }
    else if (actual[structure]=="107") 
    {
      original[structure]<-"13"
    }
    else if (actual[structure]=="109"|actual[structure]=="174 (109)") 
    {
      original[structure]<-"174"
    }
    else if (actual[structure]=="161") 
    {
      original[structure]<-"20"
    }
    else if (actual[structure]=="292") 
    {
      original[structure]<-"105"
    }
    else if (actual[structure]=="293") 
    {
      original[structure]<-"175"
    }
    else {original[structure]<-actual[structure]}
    
  }
  return(original)
}