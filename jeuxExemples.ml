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

(* Ensemble de couples EntierEntier *)

module SetIntInt = Make(CoupleEntierEntier);;
open SetIntInt;;
ensemble_vers_liste (ajoute (1,1) ens_vide);;

(* Ensemble de couples EntierChar *)

module SetIntChar = Make(CoupleEntierChar);;
open SetIntChar;;
ensemble_vers_liste (ajoute (1,'a') ens_vide);;




(* fonctions de remplissage d'ensemble *)


let mot_en_lettres string =
  let rec aux str i res = match str with
     "" -> List.rev res
    |_ -> aux (String.sub str 1 (String.length(str) - 1))
              (i + 1)
              ((i,str.[0])::res)
  in aux string 1 [];;

let ajoute_couples_ens liste ensemble =
  let rec aux list ens = match list with
     [] -> ens
    |e::l -> aux l (ajoute e ens)
  in aux liste ensemble;;

let supprime_couples_ens liste ensemble =
  let rec aux list ens = match list with
     [] -> ens
    |e::l -> aux l (supprime e ens)
  in aux liste ensemble;;

let ajoute_ensemble_mot mot ens =
  ajoute_couples_ens (mot_en_lettres mot) ens;;

let supprime_ensemble_mot mot ens =
  supprime_couples_ens (mot_en_lettres mot) ens;;

let affichage_split = function
  (e1,bool,e2)
    -> (ensemble_vers_liste e1, bool, ensemble_vers_liste e2);;



(* test sur les ensembles *)

let e1 = ens_vide;;
let e1 = ajoute_ensemble_mot "clement" e1;;
let e1 = ajoute_ensemble_mot "poinsot" e1;;
ensemble_vers_liste e1;;

let e2 = supprime_ensemble_mot "element" e1;;
let e2 = ajoute_ensemble_mot "matthieu fin" e2;;
ensemble_vers_liste e2;;

let min_e1 = min_element e1;;
let max_e2 = max_element e2;;

let cardinal_e1 = cardinal e1;;
let cardinal_e2 = cardinal e2;;

let union_e1_e2 = ensemble_vers_liste (union e1 e2);;
let cardinal_union = cardinal (union e1 e2);;

let difference_e1_e2 = ensemble_vers_liste (difference e1 e2);;
let cardinal_difference = cardinal (difference e1 e2);;

let egalite_e1_e2 = egal e1 e2;;
let compare_e1_e2 = compare e1 e2;;

ensemble_vers_liste (succ e2);;

let e3 = ajoute_ensemble_mot "poinsot" ens_vide;;
inclus e3 e1;;

let split_e2 = split (9, ' ') e2;;
affichage_split (split_e2);;

let ensemble = creer_ensemble (1, 'a') (26, 'z') 257;;
ensemble_vers_liste ensemble;;




open SetInt;;

let ensemble_int = creer_ensemble 0 10000 250;;
ensemble_vers_liste ensemble_int;;
