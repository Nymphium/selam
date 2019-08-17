open Abs

val (!) : variable -> t
val (let* ) : t -> (t -> t) -> t
val (let*!) : t -> (variable -> t) -> t
val lam : (t -> t) -> t
val lama : (variable -> t) -> t
val ( @ ) : t -> t -> t
val ( @! ) : t -> t -> t
val ( |@> ) : t -> t -> t
val atom : atom -> t
