
#use "AlgoNaif.ml";;

(** Algorithmes de recherche de code *)
module IA :
sig
  
(** Nombre d'algorithmes developpes*)
val nombre_methodes : int

    
(** Choisit un code a proposer
         *@param methode 0 pour l'algorithme naif,
         *               1 pour l'algorithme de KNUTH
         *               ... et ainsi de suite
         *@param essais la liste des codes deja proposes
         *@param possibles la liste des codes possibles
         *@return le prochain code a essayer
*)
val choix  :int -> Code.t list -> Code.t list -> Code.t


    
(** Filtre les codes possibles
 *@param methode 0 pour l'algorithme naif,
 *               1 pour l'algorithme de KNUTH
 *               ... et ainsi de suite
 *@param (code, rep) le code essaye et la reponse correspondante
 *@param la liste de courante de codes possibles
 *@return la nouvelle liste de codes possibles
*) 
val filtre : int -> (Code.t * (int * int) option) -> Code.t list -> Code.t list
    
end =

struct
  let nombre_methodes = 2;;

  (** Filtre les codes possible
   *@author Mathieu Surman
   *@param (code, rep) le code essaye et la reponse correspondante
   *@param la liste de courante de codes possibles
   *@return la nouvelle liste de codes possibles
   *)  
  let rec filtrerec (code,rep) tous_les_codes =
    match tous_les_codes with
    | [] -> []
    | c::q -> if reponse c code = rep
              then c::(filtrerec (code,rep) q)
              else filtrerec (code,rep) q;;

  
(** Choisit un code a proposer
 *@author Mathieu Surman
 *@param methode 0 pour l'algorithme Naif,
 *               1 pour l'algorithme random
 *@param essais la liste des codes deja proposes
 *@param possibles la liste des codes possibles
 *@return le prochain code a essayer
*)
  let choix nombre l_c_propose l_c_possible =
    match nombre with
    |nb when nombre = 1 -> algorandom l_c_possible
    |nb when nombre = 0 -> meilleur_score tous toutes_reponses l_c_possible
                         (*
      |nb when nb = 4 -> reskmp (string_of_code(code)) (string_of_code(proposition))*)
      |_ -> [];;


  
(** Filtre les codes possibles
 *@author Mathieu Surman
 *@param methode 0 pour l'algorithme naif,
 *               1 pour l'algorithme random        
 *@param (code, rep) le code essaye et la reponse correspondante
 *@param la liste de courante de codes possibles
 *@return la nouvelle liste de codes possibles
 *)
let filtre nb (code,rep) s =
  match nb with
  | nombre when nb = 0 -> filtrerec (code,rep) s
  | nombre when nb = 1 -> filtrerec (code,rep) s
  | _ -> [];;

end;;
open IA;;
