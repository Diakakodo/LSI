(***********************************************************************)
(*                                                                     *)
(*                                Projet LSI                           *)
(*                                                                     *)
(* Fichier : jeuxExemples.ml                                           *)
(* Auteurs : Fin Matthieu                                              *)
(*           Poinsot Clement                                           *)
(* Date : 10/05/13                                                     *)
(*                                                                     *)
(*                          Licence informatique 2eme année 2012/2013  *)
(*                                  UFR-Sciences et Techniques         *)
(*                                     Université de Rouen             *)
(*                                                                     *)
(***********************************************************************)

#use "set_lsi.ml";;

(* Ensemble d'entiers *)
module SetInt = Make(Entier);;
open SetInt;;
ensemble_vers_liste (ajoute 1 ens_vide);;

(* Enemble de couples EntierEntier *)

module SetIntInt = Make(CoupleEntierEntier);;
open SetIntInt;;
ensemble_vers_liste (ajoute (1,1) ens_vide);;

(* Enemble de couples EntierChar *)

module SetIntChar = Make(CoupleEntierChar);;
open SetIntChar;;
ensemble_vers_liste (ajoute (1,'a') ens_vide);;
