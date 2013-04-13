module type TypeOrdonne = 
sig
  type t
  val compare : t -> t -> int
  val succ : t -> t
  val pred : t -> t
end
  
module Entier : TypeOrdonne
  
module type Make_sig_ensemble =
  functor (Ord : TypeOrdonne) ->
sig
  type element
  type ensemble
  exception Ensemble_vide
  val est_vide : ensemble -> bool
  val appartient_a : element -> ensemble -> bool
  val ajoute : element -> ensemble -> ensemble
  val singleton : element -> ensemble
  val supprime : element -> ensemble -> ensemble
  val compare : ensemble -> ensemble -> int
  val succ : ensemble -> ensemble
  val pred : ensemble -> ensemble
  val union : ensemble -> ensemble -> ensemble
  val intersect : ensemble -> ensemble -> ensemble
  val difference : ensemble -> ensemble -> ensemble
  val diff_sym : ensemble -> ensemble -> ensemble
  val ensemble_vers_liste : ensemble -> element list
  val liste_vers_ensemble : element list -> ensemble
  val egal : ensemble -> ensemble -> bool
  val inclus : ensemble -> ensemble -> bool
  val fold : (element -> 'a -> 'a) -> ensemble -> 'a -> 'a
  val pour_tous : (element -> bool) -> ensemble -> bool
  val existe : (element -> bool) -> ensemble -> bool
  val filtre : (element -> bool) -> ensemble -> ensemble
  val partition : (element -> bool) -> ensemble -> ensemble * ensemble
  val cardinal : ensemble -> int
  val min_element : ensemble -> element
  val max_element : ensemble -> element
  val split : element -> ensemble -> ensemble * bool * ensemble
end
