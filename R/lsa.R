lsa <- function() {
       obj_type <- function(x) { class(get(x)) }
       foo=data.frame(sapply(ls(envir=.GlobalEnv),obj_type))
       foo$object_name=rownames(foo)
       names(foo)[1]="class"
       names(foo)[2]="object"
       return(unrowname(foo))
}
