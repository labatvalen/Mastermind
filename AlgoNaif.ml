#use "Code.ml";;
module Algonaif =
  struct
    

    let algorandom liste =
      let rec algonaifacc tt acc =
        match tt with
        | _::q when acc > 0 -> algonaifacc q (acc-1)
        | t::_ -> t
        | [] -> []
      in algonaifacc liste (Random.int (List.length(tous)));;



    let rec evaluation a s e =
      match s with
      | [] -> 0
      | c::q -> (if reponse c e = Some(a) then 0 else 1) + (evaluation a q e);;

    
    (** Attribut un score a un code
     *@author Mathieu Surman
     *@param la liste de tous les arbitrages
     *@param tous les codes possibles
     *@param le code essayé
     *@return un entier
     *)
    let rec score t_arb s e =
      match t_arb with
      | [] -> max_int
      | a::q -> min (evaluation a s e) (score q s e);;
    
    (** Retourne la liste des codes qui ont le meilleur scores
     *@author Mathieu Surman
     *@param la liste de tous les ocdes possibles
     *@param la liste de tous les arbitrages
     *@param la liste des codes filtrer
     *@return le meilleur code qui a le meilleur score
     *)
    let meilleur_score tous_codes tous_arb s =
      let rec aux = function
        | [] -> (0, [])
        | e::q -> let (s_q, e_q) = aux q
                  in let s_e = score tous_arb s e
                     in if s_e > s_q
                        then (s_e, e)
                        else (s_q, e_q)
      in snd (aux tous_codes);;



  end;;
open Algonaif;;
