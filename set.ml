(***********************************************************************)
(*                                                                     *)
(*                                Projet LSI                           *)
(*                                                                     *)
(* Fichier : set.ml                                                    *)
(* Auteurs : Fin Matthieu                                              *)
(*           Poinsot Clement                                           *)
(* Date : 10/05/13                                                     *)
(*                                                                     *)
(*                          Licence informatique 2eme année 2012/2013  *)
(*                                  UFR-Sciences et Techniques         *)
(*                                     Université de Rouen             *)
(*                                                                     *)
(***********************************************************************)

open Set_lsi;;

module SetInt = Make(Entier);;

print_string 
"Bienvenue dans le gestionnaire d'ensembles magique !
Veuillez choisir un ensemble :
entier, coupleEntierEntier, coupleEntierChar\n";;

let rec set_entier (s : string) (l : int list) = 
  let string_of_listInt l =
    let rec aux l acc =
      match l with
	  []    -> acc ^ "]"
	| e::ll -> 
	  if acc = "[" then
	    aux ll (acc ^ (string_of_int e))
	  else
	    aux ll (acc ^ ";" ^ (string_of_int e))
    in
    aux l "["
  in
  let listInt_of_string s =
    let rec aux i acc =
      if i < ((String.length s) - 1)
      then
	if s.[i] != ';' && s.[i] != ' '
	then
	  (aux
	     (i + 1)
	     (SetInt.ensemble_vers_liste
		(SetInt.ajoute
		   (int_of_string (Char.escaped (s.[i])))
		   (SetInt.liste_vers_ensemble acc))))
	else
	  (aux (i + 1) acc)
      else
	acc
    in
    aux 1 []
  in
  match s with
      "return" -> print_newline ()
    | "ens" -> 
      print_newline (print_string (string_of_listInt (l)));
      set_entier (read_line (print_string "$ ")) l;
    | "ajout" | "add" ->
      let e = read_line (print_string "-> ") in
      let res = 
	(SetInt.ensemble_vers_liste 
	   (SetInt.ajoute
	      (int_of_string e)
	      (SetInt.liste_vers_ensemble l)))
      in
      print_newline (print_string (string_of_listInt (res)));
      set_entier (read_line (print_string "$ ")) res;
    | "del" | "suppr" ->
      let e = read_line (print_string "-> ") in
      let res =
	(SetInt.ensemble_vers_liste 
	   (SetInt.supprime
	      (int_of_string e)
	      (SetInt.liste_vers_ensemble l)))
      in
      print_newline (print_string (string_of_listInt (res)));
      set_entier (read_line (print_string "$ ")) res;
    | _ ->
      set_entier (read_line (print_string "# Commande inconnue !\n$ ")) l
;;

let rec selection_set (s : string) =
  match s with
      "exit" ->
	let () = print_string "# Bye !"
	in print_newline ()
    | "entier" ->
      set_entier (read_line (print_string "# Ensemble d'entier :\n$ ")) [];
      selection_set (read_line (print_string "# Choix d'ensemble !\n$ "))
    | _ ->
      selection_set (read_line (print_string "# Ensemble inconnu !\n$ "))
;;


selection_set (read_line (print_string "$ "));;
