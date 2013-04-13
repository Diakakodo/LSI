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
   du cardinal de l'ensemble.  *)

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
    (** Param -: (a : ensemble)
	@return L'ensemble sucesseur de a grace a Ord.succ *)

  val pred : ensemble -> ensemble
    (** Param -: (a : ensemble)
	@return L'ensemble predecesseur de a grace a Ord.pred *)

  val union : ensemble -> ensemble -> ensemble
    (** Param -: (a : ensemble) (b : ensemble)
	@return L'union des ensembles a et b *)

  val intersect : ensemble -> ensemble -> ensemble
    (** Param -: (a : ensemble) (b : ensemble)
	@return L'intersection des ensembles a et b *)

  val difference : ensemble -> ensemble -> ensemble
    (** Param -: (a : ensemble) (b : ensemble)
	@return Ladiference des ensembles a et b *)

  val diff_sym : ensemble -> ensemble -> ensemble
    (** Param -: (a : ensemble) (b : ensemble)
	@return La difference symetrique des ensembles a et b *)

  val ensemble_vers_liste : ensemble -> element list
    (** Param -: (a : ensemble)
	@return La liste des elements de a par 
	parcourt infixe de l'arbre donc une liste triee *)

  val liste_vers_ensemble : element list -> ensemble
    (** Param -: (l : elt list)
	@return L'ensemble contenant tous les elements de la liste l *)

  val egal : ensemble -> ensemble -> bool
    (** Param -: (a : ensemble) (b : ensemble)
	@return Test de l'egalitee ensembiste de a et de b *)

  val inclus : ensemble -> ensemble -> bool
    (** Param -: (a : ensemble) (b : ensemble)
	@return Test de l'inclusion de a dans b *)

  val fold : (element -> 'a -> 'a) -> ensemble -> 'a -> 'a
    (** Permet d'appliquer une fonction a chaques elements
	de l'ensemble a la maniere de List.fold_left *)

  val pour_tous : (element -> bool) -> ensemble -> bool
    (** Param -: (p : element -> bool) (a : ensemble)
	@return true si tous les elements de a verifient p
	        false si non *)

  val existe : (element -> bool) -> ensemble -> bool
    (** Param -: (p : element -> bool) (a : ensemble)
	@return true au moins un element de a verifie p
                false si non *)

  val filtre : (element -> bool) -> ensemble -> ensemble
    (** Param -: (p : element -> bool) (a : ensemble)
	@retun L'ensemble des elements de a qui verifient p *)

  val partition : (element -> bool) -> ensemble -> ensemble * ensemble
    (** Param -: (p : element -> bool) (a : ensemble)
	@return Le couple (t,f) ou 
	        t : est l'ensemble des elements de a
	            qui verifient p
                f : est l'ensemble des elements de a
                    qui ne verifient pas p *)

  val cardinal : ensemble -> int
    (** Param -: (a : ensemble)
	@return Le cardinal de a. *)

  val min_element : ensemble -> element
    (** Param -: (a : ensemble)
	@return L'element minimal de a. *)

  val max_element : ensemble -> element
    (** Param -: (a : ensemble)
	@return L'element maximal de a. *)

  val split : element -> ensemble -> ensemble * bool * ensemble
    (** Param -: (v : element) (a : ensemble)
	@return Le triplet (inf,pres,sup) ou 
	        inf : est l'ensemble des elements de a
	              strictement inferieur a v.
	        pres : est le booleen indiquant la presence de v dans a.
	        sup : est l'ensemble des elements de a
	              strictement superieur a v. *)

end
(** Signature du module retourné par le foncteur Make *)

module Make : Make_sig_ensemble
(** Foncteur prenant un module de type TypeOrdonne en parametre *)

