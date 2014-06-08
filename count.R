count <- function(cause){
  
  homicides <- readLines("homicides.txt")
  c <- switch(cause,
            "asphyxiation" = length(grep("[Cc]ause: [Aa]sphyxiation",homicides)),
             "blunt force" = length(grep("[Cc]ause: [Bb]lunt [Ff]orce",homicides)),
            "other" = length(grep("[Cc]ause: [Oo]ther",homicides)),
            "shooting" = length(grep("[Cc]ause: [Ss]hooting",homicides)),
            "stabbing" = length(grep("[Cc]ause: [Ss]tabbing",homicides)),
              "unknown" = length(grep("[Cc]ause: [Uu]nknown",homicides)),
              stop( "invalid cause"))
   return(c)
}
