## init a stack object. not perfect.

new_stack <- function(){

    stack <- NULL

    stack_reset <- function() {
        stack <<- new.env(TRUE, emptyenv())
        assign("vals", list(), envir = stack)
    }

    stack_set <- function(vals){
        assign("vals", vals, envir = stack)
    }

    stack_vals <- function() {
        get("vals", envir = stack)
    }

    stack_push <- function(val){
        vals <- get("vals", envir = stack)
        assign("vals", c(val, vals), envir = stack)
    }
    
    stack_pop <- function(){
        vals <- get("vals", envir = stack)
        assign("vals", vals[1:(length(vals) - 1)], envir = stack)
        vals[length(vals)]
    }

    stack_reset()
    list(
        reset = stack_reset,
        set = stack_set,
        vals = stack_vals,
        push = stack_push,
        pop = stack_pop
        )
}
