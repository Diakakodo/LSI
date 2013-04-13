module type TypeOrdonne = 
sig
  type t
  val compare : t -> t -> int
  val succ : t -> t
  val pred : t -> t
end

module Entier : TypeOrdonne =
struct
  type t = int
  let compare a b = if a > b then 1 else if a = b then 0 else -1;;
  let succ a = a + 1;;
  let pred a = a - 1;;
end

(*
module CoupleEntier =
struct
  type t = int*int
  let compare = ...
  let succ (a,b) = (a,(b+1))
  let pred (a,b) = (a,(b-1))
end
  *)


module type Make_sig_ensemble = functor (Ord : TypeOrdonne) ->
sig
  type element
  type ensemble
  exception Ensemble_vide
  val est_vide : ensemble -> bool
  val appartient_a : element -> ensemble -> bool
  val ajoute : element -> ensemble -> ensemble
  val singleton : element -> ensemble
  val supprime : element -> ensemble -> ensemble

  (* Ajout de fonctions a Make_sig_ensemble *)
  val compare : ensemble -> ensemble -> int (* Comparaison selon l'ordre
					       lexicographique par longueur
					       sur la liste trier de l'ens *)
  val succ : ensemble -> ensemble (* Exemple pour {1,3,5,7,9} 
				     son succ est {1,3,5,7,10} *)
  val pred : ensemble -> ensemble (* Exemple pour {1,3,5,7,9}
				     son pred est {1,3,5,7,8} *)
  (* ** *)
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


  
module Make : Make_sig_ensemble = functor (Ord : TypeOrdonne) ->
struct
  (* Types *)
  type element = Ord.t
  type couleur = Noir | Rouge
  type arbre_bi = Vide | Noeud of couleur * element * arbre_bi * arbre_bi
  type ensemble = arbre_bi
  (* Exception *)
  exception Ensemble_vide
  (* Test ensemble vide *)
  let est_vide a = (*(a = Vide);;*) raise Ensemble_vide;;
  let rec recherche e a =
    match a with
	Vide -> false
      | Noeud (c,i,sag,sad) ->
	if ((Ord.compare i e) = 0)
	then
	  true
	else
	  if ((Ord.compare e i) < 0)
	  then
	    (recherche e sag)
	  else
	    (recherche e sad)
  ;;
  let appartient_a = recherche;;
  let insertion_equilibre = function
    Noeud (Noir,i,
	   (Noeud (Rouge,ig,(Noeud (Rouge,x,y,z)),sgD)),
	   (Noeud (Rouge,id,sdG,sdD))) -> 
      Noeud (Rouge,i,
	     (Noeud (Noir,ig,(Noeud (Rouge,x,y,z)),sgD)),
	     (Noeud (Noir,id,sdG,sdD)))
	
    | Noeud (Noir,i,
	     (Noeud (Rouge,ig,sgG,(Noeud (Rouge,x,y,z)))),
	     (Noeud (Rouge,id,sdG,sdD))) -> 
        Noeud (Rouge,i,
	       (Noeud (Noir,ig,sgG,(Noeud (Rouge,x,y,z)))),
	       (Noeud (Noir,id,sdG,sdD)))
	
    | Noeud (Noir,i,
	     (Noeud (Rouge,ig,sgG,sgD)),
	     (Noeud (Rouge,id,(Noeud (Rouge,x,y,z)),sdD))) -> 
        Noeud (Rouge,i,
	       (Noeud (Noir,ig,sgG,sgD)),
	       (Noeud (Noir,id,(Noeud (Rouge,x,y,z)),sdD)))
	
    | Noeud (Noir, i,
	     (Noeud (Rouge,ig,sgG,sgD)),
	     (Noeud (Rouge,id,sdG,(Noeud (Rouge,x,y,z))))) -> 
        Noeud (Rouge,i,
	       (Noeud (Noir,ig,sgG,sgD)),
	       (Noeud (Noir,id,sdG,(Noeud (Rouge,x,y,z)))))

    | Noeud (Noir, i,
	     (Noeud (Rouge,ii,(Noeud (Rouge,iii,s1,s2)),s3)),
	     (Noeud (Noir,iiii,s4,s5))) ->
        Noeud (Rouge, ii,
	       (Noeud (Noir, iii, s1, s2)),
	       (Noeud (Noir, i, s3, (Noeud (Noir, iiii, s4, s5)))))

    | Noeud (Noir, i,
	     (Noeud (Rouge, ii, s1, (Noeud (Rouge, iii, s2,s3)))),
	     (Noeud (Noir, iiii, s4, s5))) ->
	Noeud (Rouge, iii,
	       (Noeud (Noir, ii, s1, s2)),
	       (Noeud (Noir, i, s3, (Noeud (Noir, iiii, s4, s5)))))

    | Noeud (Noir, i, 
	     (Noeud (Noir, ii, s1, s2)),
	     (Noeud (Rouge, iii, s3, (Noeud (Rouge, iiii, s4, s5))))) -> 
        Noeud (Rouge, iii,
	       (Noeud (Noir, i, (Noeud (Noir, ii, s1, s2)), s3)),
	       (Noeud (Noir, iiii, s4, s5)))

    | Noeud (Noir, i,
	     (Noeud (Noir, ii, s1, s2)),
	     (Noeud (Rouge, iii, (Noeud (Rouge, iiii, s3, s4)), s5))) ->
        Noeud (Rouge, iiii,
	       (Noeud (Noir, i, (Noeud (Noir, ii, s1, s2)), s3)),
	       (Noeud (Noir, iii, s4, s5)))

    | Noeud (Noir, i,
	     (Noeud (Rouge, ii, (Noeud (Rouge, iii, s1, s2)), s3)), Vide) ->
        Noeud (Noir, ii,
	       (Noeud (Rouge, iii, s1, s2)), (Noeud (Rouge, i, s3, Vide)))

    | Noeud (Noir, i,
	     (Noeud (Rouge, ii, s1, (Noeud (Rouge, iii, s2, s3)))), Vide) ->
        Noeud (Noir, iii,
	       (Noeud (Rouge, ii, s1, s2)), (Noeud (Rouge, i, s3, Vide)))

    | Noeud (Noir, i,
	     Vide, (Noeud (Rouge, ii, s1, (Noeud (Rouge, iii, s2, s3))))) ->
        Noeud (Noir, ii,
	       (Noeud (Rouge, i, Vide, s1)), (Noeud (Rouge, iii, s2, s3)))

    | Noeud (Noir, i,
	     Vide, (Noeud (Rouge, ii, (Noeud (Rouge, iii, s1, s2)), s3))) ->
        Noeud (Noir, iii,
	       (Noeud (Rouge, i, Vide, s1)), (Noeud (Rouge, ii, s2, s3)))

    | a -> a
  ;;
  let rec insertion_rech e a = 
    match a with
	Vide -> Noeud (Rouge,e,Vide,Vide)
      | Noeud (c,i,sag,sad) when (Ord.compare e i) < 0 -> 
	(insertion_equilibre (Noeud (c,i,(insertion_rech e sag),sad)))
      | Noeud (c,i,sag,sad) when (Ord.compare e i) > 0 -> 
	(insertion_equilibre (Noeud (c,i,sag,(insertion_rech e sad))))
      | _ -> a
  ;;
  
  let insertion e a =
    let res = (insertion_rech e a)
    in
    match res with
	(Noeud (c,i,sag,sad)) -> Noeud (Noir, i, sag, sad)
      | _ -> raise Ensemble_vide;;

  let ajout = insertion;;
  let ajoute = ajout;;
  let singleton e = ajout e Vide;;
  let suppression_equilibreg =
  function 
      Noeud (Noir,i,Vide,Noeud (Noir,i2,Vide,Vide)) -> 
	(false,Noeud (Noir,i,Vide,Noeud (Rouge,i2,Vide,Vide)))
    | Noeud (Noir,i,Vide,Noeud (Noir,i2,(Noeud (Rouge,i3,Vide,Vide)),Vide)) ->
	(true,Noeud (Noir,i3,(Noeud (Noir,i,Vide,Vide)),
		       (Noeud (Noir,i2,Vide,Vide))))
    | Noeud (Noir,i,Vide,Noeud (Noir,i2,Vide,(Noeud (Rouge,i3,Vide,Vide)))) ->
	(true,Noeud (Noir,i2,(Noeud (Noir,i,Vide,Vide)),
		       (Noeud (Noir,i3,Vide,Vide))))
    | Noeud (Noir,i,Vide,Noeud (Noir,i2,(Noeud (Rouge,i3,Vide,Vide)),
				(Noeud (Rouge,i4,Vide,Vide)))) -> 
	(true,Noeud (Noir,i2,
		       (Noeud (Noir,i,Vide,(Noeud (Rouge,i3,Vide,Vide)))),
		       (Noeud (Noir,i4,Vide,Vide))))
    | Noeud (Noir,i,Vide,Noeud (Rouge,i2,(Noeud (Noir,i3,Vide,Vide)),
				(Noeud (Noir,i4,ag4,ad4)))) -> 
	(true,(Noeud (Noir,i2,
			(Noeud (Noir,i,
				Vide,
				(Noeud (Rouge,i3,Vide,Vide)))),
			(Noeud (Noir,i4,ag4,ad4)))))
    | Noeud (Noir,i,Vide,Noeud (Rouge,i2,
				(Noeud (Noir,i3,
					(Noeud (Rouge,i5,Vide,Vide)),Vide)),
				(Noeud (Noir,i4,ag4,ad4)))) -> 
	(true,(Noeud (Noir,i2,
			(Noeud (Noir,i5,
				(Noeud (Rouge,i,Vide,Vide)),
				(Noeud (Rouge,i3,Vide,Vide)))),
			(Noeud (Noir,i4,ag4,ad4)))))
    | Noeud (Noir,i,
	     Vide,
	     Noeud (Rouge,i2,
		    Noeud (Noir,i3,
			   ag3,
			   Noeud (Rouge,i5,Vide,Vide)),
		    Noeud (Noir,i4,ag4,ad4))) -> 
	(true,Noeud (Noir,i3,
		     Noeud (Noir,i,
			    Vide,
			    ag3),
		     Noeud (Rouge,i2,
			    Noeud (Noir,i5,Vide,Vide),
			    Noeud (Noir,i4,ag4,ad4))))
    | Noeud (Rouge,i,Vide,Noeud (Noir,i2,Vide,Vide)) ->
	(true,Noeud (Noir,i,Vide,Noeud (Rouge,i2,Vide,Vide)))
    | Noeud (Rouge,i,Vide,Noeud (Noir,i2,(Noeud (Rouge,i3,Vide,Vide)),Vide)) -> 
	(true,Noeud (Noir,i3,(Noeud (Rouge,i,Vide,Vide)),
		       (Noeud (Rouge,i2,Vide,Vide))))
    | Noeud (Rouge,i,Vide,Noeud (Noir,i2,Vide,(Noeud (Rouge,i3,Vide,Vide)))) ->
	(true,Noeud (Noir,i2,(Noeud (Rouge,i,Vide,Vide)),
		       (Noeud (Rouge,i3,Vide,Vide))))
    | Noeud (Rouge,i,Vide,Noeud (Noir,i2,(Noeud (Rouge,i3,Vide,Vide)),
				 (Noeud (Rouge,i4,Vide,Vide)))) -> 
	(true,Noeud (Rouge,i2,
		       (Noeud (Noir,i,Vide,(Noeud (Rouge,i3,Vide,Vide)))),
		       (Noeud (Noir,i4,Vide,Vide))))
    | Noeud (Noir,i,Noeud (Noir,i2,ag2,ad2),
	     Noeud (Noir,i3,Noeud (Noir,i4,ag4,ad4),Noeud (Noir,i5,ag5,ad5))) ->
	(false,Noeud (Noir,i,Noeud (Noir,i2,ag2,ad2),
	     Noeud (Rouge,i3,Noeud (Noir,i4,ag4,ad4),Noeud (Noir,i5,ag5,ad5))))
    | Noeud (Noir,i,Noeud (Noir,i2,ag2,ad2),
	     Noeud (Noir,i3,Noeud (Rouge,i4,ag4,ad4),
		    Noeud (Noir,i5,ag5,ad5))) ->
	(true,Noeud (Noir,i4,Noeud (Noir,i,Noeud (Noir,i2,ag2,ad2),ag4),
		      Noeud (Noir,i3,ad4,Noeud (Noir,i5,ag5,ad5))))
    | Noeud (Noir,i,Noeud (Noir,i2,ag2,ad2),
	     Noeud (Noir,i3,Noeud (Noir,i4,ag4,ad4),
		    Noeud (Rouge,i5,ag5,ad5))) -> 
	(true,Noeud (Noir,i3,
	       Noeud (Noir,i,
		      Noeud (Noir,i2,ag2,ad2),
		      Noeud (Noir,i4,ag4,ad4)),
	       Noeud (Noir,i5,ag5,ad5)))
    | Noeud (Noir,i,Noeud (Noir,i2,ag2,ad2),
	     Noeud (Noir,i3,Noeud (Rouge,i4,ag4,ad4),
		    Noeud (Rouge,i5,ag5,ad5))) -> 
	(true,Noeud (Noir,i4,Noeud (Noir,i,Noeud (Noir,i2,ag2,ad2),ag4),
		     Noeud (Noir,i3,ad4,Noeud (Rouge,i5,ag5,ad5))))
    | Noeud (Noir,i,Noeud (Noir,i2,ag2,ad2),
	     Noeud (Rouge,i3,
		    Noeud (Noir,i4,
			   Noeud (Noir,i6,ag6,ad6),
			   Noeud (Noir,i7,ag7,ad7)),
		    Noeud (Noir,i5,ag5,ad5))) -> 
	(true,Noeud (Noir,i3,
		     Noeud (Noir,i,
			    Noeud (Noir,i2,ag2,ad2),
			    Noeud (Rouge,i4,
				   Noeud (Noir,i6,ag6,ad6),
				   Noeud (Noir,i7,ag7,ad7))),
		     Noeud (Noir,i5,ag5,ad5)))
    | Noeud (Noir,i,Noeud (Noir,i2,ag2,ad2),
	     Noeud (Rouge,i3,
		    Noeud (Noir,i4,
			   Noeud (Rouge,i6,ag6,ad6),
			   Noeud (Noir,i7,ag7,ad7)),
		    Noeud (Noir,i5,ag5,ad5))) -> 
	(true,Noeud (Noir,i3,
		     Noeud (Rouge,i6,
			    Noeud (Noir,i,Noeud (Noir,i2,ag2,ad2),ag6),
			    Noeud (Noir,i4,ad6,
				   Noeud (Noir,i7,ag7,ad7))),
		     Noeud (Noir,i5,ag5,ad5)))
    | Noeud (Noir,i,Noeud (Noir,i2,ag2,ad2),
	     Noeud (Rouge,i3,
		    Noeud (Noir,i4,
			   Noeud (Noir,i6,ag6,ad6),
			   Noeud (Rouge,i7,ag7,ad7)),
		    Noeud (Noir,i5,ag5,ad5))) -> 
	(true,Noeud (Noir,i3,
		     Noeud (Rouge,i4,
			    Noeud (Noir,i,
				   Noeud (Noir,i2,ag2,ad2),
				   Noeud (Noir,i6,ag6,ad6)),
			    Noeud (Noir,i7,ag7,ad7)),
		     Noeud (Noir,i5,ag5,ad5)))
    | Noeud (Noir,i,Noeud (Noir,i2,ag2,ad2),
	     Noeud (Rouge,i3,
		    Noeud (Noir,i4,
			   Noeud (Rouge,i6,ag6,ad6),
			   Noeud (Rouge,i7,ag7,ad7)),
		    Noeud (Noir,i5,ag5,ad5))) -> 
	(true,Noeud (Noir,i3,
		     Noeud (Rouge,i4,
			    Noeud (Noir,i,
				   Noeud (Noir,i2,ag2,ad2),
				   Noeud (Rouge,i6,ag6,ad6)),
			    Noeud (Noir,i7,ag7,ad7)),
		     Noeud (Noir,i5,ag5,ad5)))
    | Noeud (Noir,i,Noeud (Rouge,i2,ag2,ad2),ad) -> 
	(true,Noeud (Noir,i,Noeud (Noir,i2,ag2,ad2),ad)) 
    | Noeud (Rouge,i,
	     Noeud (Noir,i2,ag2,ad2),
	     Noeud (Noir,i3,Noeud (Noir,i4,ag4,ad4),ad3)) ->
	(true,Noeud (Noir,i3,
		     Noeud (Rouge,i,
			    Noeud (Noir,i2,ag2,ad2),
			    Noeud (Noir,i4,ag4,ad4)),
		     ad3))
    | Noeud (Rouge,i,
	     Noeud (Noir,i2,ag2,ad2),
	     Noeud (Noir,i3,Noeud (Rouge,i4,ag4,ad4),ad3)) ->
	(true,Noeud (Rouge,i4,
		     Noeud (Noir,i,
			    Noeud (Noir,i2,ag2,ad2),
			    ag4),
		     Noeud (Noir,i3,ad4,ad3)))
    | a -> (true,a);;

let suppression_equilibred =
  function 
      Noeud (Noir,i,Noeud (Noir,i2,Vide,Vide),Vide) -> 
	(false,Noeud (Noir,i,Noeud (Rouge,i2,Vide,Vide),Vide))
    | Noeud (Noir,i,Noeud (Noir,i2,(Noeud (Rouge,i3,Vide,Vide)),Vide),Vide) ->  
	(true,Noeud (Noir,i2,(Noeud (Noir,i3,Vide,Vide)),
		       (Noeud (Noir,i,Vide,Vide))))
    | Noeud (Noir,i,Noeud (Noir,i2,Vide,(Noeud (Rouge,i3,Vide,Vide))),Vide) ->
	(true,Noeud (Noir,i3,(Noeud (Noir,i2,Vide,Vide)),
		       (Noeud (Noir,i,Vide,Vide))))
    | Noeud (Noir,i,Noeud (Noir,i2,(Noeud (Rouge,i3,Vide,Vide)),
				(Noeud (Rouge,i4,Vide,Vide))),Vide) -> 
	(true,Noeud (Noir,i2,
		     (Noeud (Noir,i3,Vide,Vide)),
		     (Noeud (Noir,i,(Noeud (Rouge,i4,Vide,Vide)),Vide))))
    | Noeud (Noir,i,
	     Noeud (Rouge,i2,
		    Noeud (Noir,i3,ag3,ad3),
		    Noeud (Noir,i4,
			   Noeud (Rouge,i5,Vide,Vide),
			   ad4)),
	     Vide) -> 
	(true,Noeud (Noir,i2,
		     Noeud (Noir,i3,ag3,ad3),
		     Noeud (Rouge,i4,
			    Noeud (Noir,i5,Vide,Vide),
			    Noeud (Noir,i,ad4,Vide))))
    | Noeud (Noir,i,
	     Noeud (Rouge,i2,
		    Noeud (Noir,i3,ag3,ad3),
		    Noeud (Noir,i4,Vide,Vide)),
	     Vide) ->
	(true,Noeud (Noir,i2,
		     Noeud (Noir,i3,ag3,ad3),
		     Noeud (Noir,i,
			    Noeud (Rouge,i4,Vide,Vide),
			    Vide)))
    | Noeud (Noir,i,
	     Noeud (Rouge,i2,
		    Noeud (Noir,i3,ag3,ad3),
		    Noeud (Noir,i4,
			   Vide,
			   Noeud (Rouge,i5,Vide,Vide))),
	     Vide) ->
	(true,Noeud (Noir,i2,
		     Noeud (Noir,i3,ag3,ad3),
		     Noeud (Noir,i5,
			    Noeud (Rouge,i4,Vide,Vide),
			    Noeud (Rouge,i,Vide,Vide))))
    | Noeud (Rouge,i,Noeud (Noir,i2,Vide,Vide),Vide) ->	
	(true,Noeud (Noir,i,Noeud (Rouge,i2,Vide,Vide),Vide))
    | Noeud (Rouge,i,Noeud (Noir,i2,(Noeud (Rouge,i3,Vide,Vide)),Vide),Vide) -> 
	(true,Noeud (Noir,i2,(Noeud (Rouge,i3,Vide,Vide)),
		       (Noeud (Rouge,i,Vide,Vide))))
    | Noeud (Rouge,i,Noeud (Noir,i2,Vide,(Noeud (Rouge,i3,Vide,Vide))),Vide) ->
	(true,Noeud (Noir,i3,
		     (Noeud (Rouge,i2,Vide,Vide)),
		     (Noeud (Rouge,i,Vide,Vide))))
    | Noeud (Rouge,i,Noeud (Noir,i2,(Noeud (Rouge,i3,Vide,Vide)),
			    (Noeud (Rouge,i4,Vide,Vide))),Vide) -> 
	(true,Noeud (Rouge,i2,
		     (Noeud (Noir,i3,Vide,Vide)),
		     (Noeud (Noir,i,(Noeud (Rouge,i4,Vide,Vide)),Vide))))
    | Noeud (Noir,i,
	     Noeud (Noir,i3,
		    Noeud (Noir,i4,ag4,ad4),
		    Noeud (Noir,i5,ag5,ad5)),
	     Noeud (Noir,i2,ag2,ad2)) -> 
	(false,Noeud (Noir,i3,
		      Noeud (Noir,i4,ag4,ad4),
		      Noeud (Rouge,i,
			     Noeud (Noir,i5,ag5,ad5),
			     Noeud (Noir,i2,ag2,ad2))))
    | Noeud (Noir,i,
	     Noeud (Noir,i3,
		    Noeud (Rouge,i4,ag4,ad4),
		    Noeud (Noir,i5,ag5,ad5)),
	     Noeud (Noir,i2,ag2,ad2)) ->
	(true,Noeud (Noir,i3,
		     Noeud (Noir,i4,ag4,ad4),
		     Noeud (Noir,i,
			    Noeud (Noir,i5,ag5,ad5),
			    Noeud (Noir,i2,ag2,ad2))))
    | Noeud (Noir,i,
	     Noeud (Noir,i3,
		    Noeud (Noir,i4,ag4,ad4),
		    Noeud (Rouge,i5,ag5,ad5)),
	     Noeud (Noir,i2,ag2,ad2)) -> 
	(true,Noeud (Noir,i5,
		     Noeud (Noir,i3,
			    Noeud (Noir,i4,ag4,ad4),
			    ag5),
		     Noeud (Noir,i,
			    ad5,
			    Noeud (Noir,i2,ag2,ad2)))) 
    | Noeud (Noir,i,
	     Noeud (Noir,i3,
		    Noeud (Rouge,i4,ag4,ad4),
		    Noeud (Rouge,i5,ag5,ad5)),
	     Noeud (Noir,i2,ag2,ad2)) -> 
	(true,Noeud (Noir,i5,
		     Noeud (Noir,i3,
			    Noeud (Rouge,i4,ag4,ad4),ag5),
		     Noeud (Noir,i,ad5,Noeud (Noir,i2,ag2,ad2))))	
    | Noeud (Noir,i,
	     Noeud (Rouge,i3,
		    Noeud (Noir,i4,
			   Noeud (Noir,i6,ag6,ad6),
			   Noeud (Noir,i7,ag7,ad7)),
		    Noeud (Noir,i5,ag5,ad5)),
	     Noeud (Noir,i2,ag2,ad2)) -> 
	(true,Noeud (Noir,i5,
		     Noeud (Noir,i3,
			    Noeud (Rouge,i4,
				   Noeud (Noir,i6,ag6,ad6),
				   Noeud (Noir,i7,ag7,ad7)),
			    ag5),
		     Noeud (Noir,i,ad5,Noeud (Noir,i2,ag2,ad2))))
    | Noeud (Noir,i,
	     Noeud (Rouge,i3,
		    Noeud (Noir,i4,
			   Noeud (Rouge,i6,ag6,ad6),
			   Noeud (Noir,i7,ag7,ad7)),
		    Noeud (Noir,i5,ag5,ad5)),
	     Noeud (Noir,i2,ag2,ad2)) -> 
	(true,Noeud (Noir,i5,
		     Noeud (Rouge,i4,
			    Noeud (Noir,i6,ag6,ad6),
			    Noeud (Noir,i3,
				   Noeud (Noir,i7,ag7,ad7),
				   ag5)),
		     Noeud (Noir,i,ad5,Noeud (Noir,i2,ag2,ad2))))
    | Noeud (Noir,i,
	     Noeud (Rouge,i3,
		    Noeud (Noir,i4,
			   Noeud (Noir,i6,ag6,ad6),
			   Noeud (Rouge,i7,ag7,ad7)),
		    Noeud (Noir,i5,ag5,ad5)),
	     Noeud (Noir,i2,ag2,ad2)) -> 
	(true,Noeud (Noir,i5,
		     Noeud (Rouge,i7,
			    Noeud (Noir,i4,
				   Noeud (Noir,i6,ag6,ad6),
				   ag7),
			    Noeud (Noir,i3,ad7,ag5)),
		     Noeud (Noir,i,ad5,Noeud (Noir,i2,ag2,ad2)))) 
    | Noeud (Noir,i,
	     Noeud (Rouge,i3,
		    Noeud (Noir,i4,
			   Noeud (Rouge,i6,ag6,ad6),
			   Noeud (Rouge,i7,ag7,ad7)),
		    Noeud (Noir,i5,ag5,ad5)),
	     Noeud (Noir,i2,ag2,ad2)) -> 
	(true,Noeud (Noir,i5,
		     Noeud (Rouge,i4,
			    Noeud (Noir,i6,ag6,ad6),
			    Noeud (Noir,i3,
				   Noeud (Rouge,i7,ag7,ad7),
				   ag5)),
		     Noeud (Noir,i,
			    ad5, 
			    Noeud (Noir,i2,ag2,ad2))))
    | Noeud (Noir,i,ag,Noeud (Rouge,i2,ag2,ad2)) ->
	(true,Noeud (Noir,i,ag,Noeud (Noir,i2,ag2,ad2)))
    | Noeud (Rouge,i,
	     Noeud (Noir,i2,ag2,
		    Noeud (Noir,i4,ag4,ad4)),
	     Noeud (Noir,i3,ag3,ad3)) -> 
	(true,Noeud (Noir,i2,ag2,
	       Noeud (Rouge,i,
		      Noeud (Noir,i4,ag4,ad4),
		      Noeud (Noir,i3,ag3,ad3))))
    | Noeud (Rouge,i,
	     Noeud (Noir,i2,ag2,
		    Noeud (Rouge,i4,ag4,ad4)),
	     Noeud (Noir,i3,ag3,ad3)) -> 
	(true,Noeud (Rouge,i4,
		     Noeud (Noir,i2,ag2,ag4),
		     Noeud (Noir,i,ad4,
			    Noeud (Noir,i3,ag3,ad3))))
    | a -> (true,a);;

    let rec succ_arbre = function
        Noeud(Rouge, i, Vide, Vide) -> (true, i, Vide)

        |Noeud(Noir, i, Vide, Vide) -> (false, i, Vide)

        |Noeud(Noir, i, Vide, Noeud(Rouge, ii, Vide, Vide)) ->
        (true, i, Noeud(Noir, ii, Vide, Vide))

        |Noeud(c, i, sag, sad) ->
        (let (b, x, a) = (succ_arbre sag) in
            (if b then
                (true, x, Noeud(c, i, a, sad))
            else
		(let (b2,a2) = (suppression_equilibreg(Noeud(c, i, a, sad))) in
		 (b2, x, a2))));;

    let rec suppression_rech e a =
      match a with
	  Vide->(true,Vide)
	|Noeud (c,i,ag,ad) when i>e ->
          (let (b,a2)=(suppression_rech e ag) in
           (if b
            then (true,Noeud (c,i,a2,ad))
	    else (suppression_equilibreg (Noeud (c,i,a2,ad)))))
	|Noeud (c,i,ag,ad) when i<e ->
          (let (b,a2)=(suppression_rech e ad) in
           (if b
            then (true,Noeud (c,i,ag,a2))
            else (suppression_equilibred (Noeud (c,i,ag,a2)))))
	|Noeud (c,i,ag,ad) ->
          (match ad with
              Vide ->
		(match ag with
                   Vide when c=Rouge         ->(true,Vide)
                  |Vide                      ->(false,Vide)
		  |Noeud (Rouge,i2,Vide,Vide)->(true,Noeud (Noir,i2,Vide,Vide)))
            |Noeud (c2,i2,ag2,ad2) ->
              (let (b,x,a2)=(succ_arbre (Noeud (c2,i2,ag2,ad2))) in
               (if b 
		then (true,Noeud (c,x,ag,a2))
		else (suppression_equilibred (Noeud (c,x,ag,a2))))));;
    let suppression e a = (snd (suppression_rech e a));;
    let supprime = suppression;;

    let union a1 a2 =
      let rec aux a b acc = 
	match (a,b) with
	    Vide,Vide -> acc
	  | Vide,(Noeud (c2,v2,ag2,ad2)) -> 
	    aux Vide (suppression v2 b) (insertion v2 acc)
	  | (Noeud (c1,v1,ag1,ad1)),Vide -> 
	    aux (suppression v1 a) Vide (insertion v1 acc)
	  | (Noeud (c1,v1,ag1,ad1)),b -> 
	    aux (suppression v1 a) b (insertion v1 acc)
      in
      aux a1 a2 Vide
    ;;

    (* Intersection ensembliste des deux ensemble ens1 et ens2 *)
    let intersect ens1 ens2 =
      let rec aux a1 a2 acc =
	match (a1,a2) with
	    Vide,Vide -> acc
	  | Vide,a2 -> acc
	  | a1,Vide -> acc
	  | (Noeud (c,e,ag,ad)),a2 when (appartient_a e a2) -> 
	    aux (supprime e (Noeud (c,e,ag,ad))) (supprime e a2) (ajoute e acc)
	  | (Noeud (c,e,ag,ad)),a2 -> 
	    aux (supprime e (Noeud (c,e,ag,ad))) (a2) (acc)
      in
      aux ens1 ens2 Vide;;
    
    let rec difference_rec a2 acc = 
      match a2 with
	  Vide -> acc
	| Noeud (c,i,ag,ad) ->
	  (difference_rec ag (difference_rec ad (suppression i acc)));;

    (* Calcul a privé de b *)
    let difference a b = (difference_rec b a);;
    
    (* Calcul la difference symetrique de a et b
       C'est a dire l'ensemble des éléments appartenant 
       à A ou à B exclusivement! ((aUb)-(anb)) *)
    let diff_sym a b = difference (union a b) (intersect a b);;
    
    (* Retourne la liste croissante des elements de l'ensemble.
       issue du parcourt infixe de l'arbre représentant l'enemble. *)
    let rec ensemble_vers_liste = function
    Vide -> []
      | Noeud (c,v,Vide,Vide) -> [v]
      | Noeud (c,v,ag,ad) -> 
	(ensemble_vers_liste ag) @ [v] @ (ensemble_vers_liste ad)
    ;;
    let liste_vers_ensemble li =
      let rec aux acc = function
      [] -> acc
	| e::l -> aux (insertion e acc) l
      in
      aux Vide li;;
    
    (* Verifie si a est inclus dans b *)
    let rec inclus a b = 
      match a with
	  Vide -> true
	| (Noeud (c,v,ag,ad)) when (appartient_a v b) -> 
	  inclus (suppression v a) b
	| _ -> false;;
    
    let rec fold f ens acc =
      match ens with
          Vide -> acc
	| Noeud (c, v, ag, ad) -> fold f ad (f v (fold f ag acc))
    ;;
    
    (* Egalitée ensembliste *)
    (*let egal a b = (inclus a b) && (inclus b a);;*)
    let rec egal a b =
      match a with
	  Vide -> if (est_vide b) then true else false
	| Noeud (c,v,ag,ad) when (appartient_a v b) -> 
	  egal (supprime v a) (supprime v b)
	| _ -> false;;
    
    (* Test si la propriétée p (sur un element) est verifiée 
       pour tous les elements de l'ensemble *)
    let rec pour_tous p = function
    Vide -> true
      | Noeud (c,e, ag, ad) -> p e && pour_tous p ag && pour_tous p ad;;

    (* Test si la propriétée p (sur un element) est verifiée 
       pour au moin un element de l'ensemble *)
    let rec existe p = function
    Vide -> false
      | Noeud (c,e, ag, ad) -> p e || existe p ag || existe p ad;;
    
    (* Sous ensemble des elt respectant p *)
    let rec filtre p = function
        Vide -> Vide
      | Noeud (c,v,ag,ad) -> 
	let agg = filtre p ag
	and add = filtre p ad
	and pv = p v in
	if pv then
	  (ajoute v (union agg add))
	else
	  (union agg add)
    ;;
    
    (* Retourne le couple 
       (ens des elts respectant p * ens des elt ne respectant pas p) *)
    let rec partition p = function
        Vide -> (Vide, Vide)
      | Noeud (c,v,ag,ad) ->
	let (agv,agf) = partition p ag
	and (adv,adf) = partition p ad
	and pv = p v in
	if pv then
	  ((ajoute v (union agv adv)), (union agf adf))
	else
	  ((union agv adv), (ajoute v (union agf adf)))
    ;;
    
    (* Calcul le cardinal d'un ensemble *)
    let rec cardinal = function
	Vide -> 0
      | Noeud (c,v,ag,ad) -> 1 + (cardinal ag) + (cardinal ad);;
    
    (* Retourne l'element minimum de l'ensemble si il est non vide *)
    let rec min_element = function
        Vide -> raise Ensemble_vide
      | Noeud (c, e, Vide, _) -> e
      | Noeud (c, e, ag, ad) -> min_element ag;;
    (* Retourne l'element maximum de l'ensemble si il est non vide *)
    let rec max_element = function
        Vide -> raise Ensemble_vide
      | Noeud (c, e, _, Vide) -> e
      | Noeud (c, e, ag, ad) -> min_element ad;;
    
    (* Retourne le sous ensemble de ens contenant 
       tous les elements de ens inferieur stricrement a elem *)
    let rec sous_ens_inf_a elem ens = 
      match ens with
	  Vide -> Vide
	| Noeud (c,e,ag,ad) -> 
	  let comp = Ord.compare elem e in
	  if comp = 0 then ag else
	    if comp > 0 then
	      (union (insertion e ag) (sous_ens_inf_a elem ad))
	    else 
	      (sous_ens_inf_a elem ag);;
    (* Retourne le sous ensemble de ens contenant 
       tous les elements de ens superieur stricrement a elem *)
    let rec sous_ens_sup_a elem ens = 
      match ens with
	  Vide -> Vide
	| Noeud (c,e,ag,ad) -> 
	  let comp = Ord.compare elem e in
	  if comp = 0 then ad else
	    if comp > 0 then
	      (sous_ens_sup_a elem ad)
	    else 
	      (union (insertion e ad) (sous_ens_sup_a elem ag));;
    
    (* Retourne un tirplet (inf,present,sup) où 
       inf est l'ensemble des elements de ens strictement inferieur a elem
       sup est l'ensemble des elements de ens strictement superieur a elem
       present vaut true si e appartient a ens false sinon *)
    let split elem ens =
      let inf = (sous_ens_inf_a elem ens) in
      let present = (recherche elem ens) in
      let sup = (sous_ens_sup_a elem ens)
      in (inf,present,sup);;

    let compare ens1 ens2 =
      let rec compare_aux l1 l2 = 
	match l1,l2 with
	    [],[] -> 0
	  | l,[] -> 1
	  | [],l -> -1
	  | (v1::ll1,v2::ll2) -> 
	    let c = Ord.compare v1 v2 in
	    if c != 0
	    then c
	    else compare_aux ll1 ll2
      in
      compare_aux (ensemble_vers_liste ens1) (ensemble_vers_liste ens2)
    ;;
    
    let succ ens =
      let rec succ_aux acc li = 
	match li with
            [] -> raise Ensemble_vide
	  | v::[] -> acc @ [(Ord.succ v)]
	  | v::l -> succ_aux (acc @ [v]) l
      in liste_vers_ensemble (succ_aux [] (ensemble_vers_liste ens))
    ;;
    
    let pred ens =
      let rec pred_aux acc li = 
	match li with
            [] -> raise Ensemble_vide
	  | v::[] -> acc @ [(Ord.pred v)]
	  | v::l -> pred_aux (acc @ [v]) l
      in liste_vers_ensemble (pred_aux [] (ensemble_vers_liste ens))
    ;;
      
    
end
