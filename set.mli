(***********************************************************************)
(*                                                                     *)
(*                                Projet LSI                           *)
(*                                                                     *)
(* Fichier : set.mli                                                   *)
(* Auteur : Matthieu Fin                                               *)
(* Date : 13/04/13                                                     *)
(*                                                                     *)
(*                          Licence informatique 2eme année 2012/2013  *)
(*                                  UFR-Sciences et Techniques         *)
(*                                     Université de Rouen             *)
(*                                                                     *)
(***********************************************************************)

(** Ensembles sur des elements ordonné.

   Ce module implante la structure de donnée d'ensemble sur des elements
   ordonnés.
   Les ensemble sont géré par une implantation d'arbre bicolores,
   Permettant une insertion/suppression en un temps logarithmique
   du cardinal de l'ensemble.
*)

module type TypeOrdonne = 
sig
  type t
    (** Le type des elements de l'ensemble. *)
  val compare : t -> t -> int
    (** Fonction permettant d'instaurer un ordre total sur les elements.
	Prend deux elements a et b et retourne 
	0  si a = b,
	1  si a > b
	-1 si a < b *)
  val succ : t -> t
    (** Fonction qui renvoie l'element directement successeur de
        l'element pris en paramettre.*)
  val pred : t -> t
    (** Fonction qui renvoie l'element directement predecesseur de
        l'element pris en paramettre.*)
end
(** Signature du parametre du foncteur Make. *)

module Entier : TypeOrdonne
(** Module de type TypeOrdonne pouvant servir de paramettre au 
    foncteur Make pour obtenir un ensemble d'entier.
    Ce module implantant les entiers (int). *)
  
module type Make_sig_ensemble =
  functor (Ord : TypeOrdonne) ->
sig
  type element
    (** Le type des elements de l'ensemble. *)
  type ensemble
    (** Type des ensembles. *)
  exception Ensemble_vide
    (** Exception levee en cas d'evaluation sur un ensemble vide impossible *)
  val est_vide : ensemble -> bool
    (** Param -: (a : ensemble)
	@return 
	  Test si a est vide ou non *)
  val appartient_a : element -> ensemble -> bool
    (** Param -: (e : element) (ens : esemble)
	@return
	  Test si e est dans ens ou non *)
  val ajoute : element -> ensemble -> ensemble
    (** Param -: (v : element) (e : ensemble)
	@return
	  Ajoute l'element v a l'ensemble e *)
  val singleton : element -> ensemble
    (** Param -: (v : element)
	@return
	  Le singleton contenant v. *)
  val supprime : element -> ensemble -> ensemble
    (** Param -: (v : element) (e : ensemble)
	@return
	  Retire l'element v de l'ensemble e si il existe ne fait rien
	  sinon. *)
  val compare : ensemble -> ensemble -> int
    (** Param -: (e1 : ensemble) (e2 : ensemble)
	@return
	   0 si e1 = e2
	   1 si e1 contient e2
	  -1 si ei est inclus dans e2 *)
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
(** Signature du module retourné par le foncteur Make *)

module Make : Make_sig_ensemble
(** Foncteur prenant un module de type TypeOrdonne en parametre *)
