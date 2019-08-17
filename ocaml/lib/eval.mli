open Abs

type model = {
  comp : comp;
  env : env;
  affenv : affenv;
  k : f list
}

val step : model -> model
val eval : t -> w
