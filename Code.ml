
(**Module de definition d'un code dans le jeu Mastermind*)
module Code :
sig
  
  (** Le type d'un pion*)
  type pion = int ;;


  (** Le type d'un code*)
  type t = pion list

         
  (** Nombre de pions par code*)
  val nombre_pions : int

    
  (** Liste des couleurs possibles*)
  val couleurs_possibles : pion list 

    
  (** Compare deux codes
   *@param code1 premier code a comparer
   *@param code2 second  code a comparer
   *@return 0 si les deux codes sont identiques,
 un entier positif si [code1] est strictement plus grand que [code2]
 un entier negatif si [code1] est strictement plus petit que [code2]
   *)
  val compare : t -> t -> int

  (** La liste de tous les codes permis*)
  val tous : t list 

    
  (** Conversion code vers chaine de caracteres (pour affichage)
   *@param code code a convertir
   *@return la representation en chaine de caracteres de [code]
   *)
  val string_of_code : t -> string

    
  (** Conversion chaine de caracteres vers code (pour saisie)
   *@param string chaine de caractere saisie
   *@return le code correspondant a la saisie si la conversion est possible
 [None] si la conversion n'est pas possible
   *)
  val code_of_string : string -> t option

    
  (** La liste de toutes les reponses possibles*)
  val toutes_reponses : (int * int) list;;


  (** Calcule la reponse d'un code par rapport au code cache
   *@param code le code propose
   *@param vrai_code le code cache
   *@return un couple (nombre de pions bien places, nombre de pions mal places)
 [None] si la reponse ne peut etre calculee
   *)
  val reponse : t -> t -> (int*int) option 

end =

  
  struct
    (** Le type d'un pion*)
    type pion = int ;;

    (** Le type d'un code*)
    type t = pion list ;;

    let couleurs_possibles = [0;1;2];;

    let nombre_pions = 4;;

    
    (** Compare deux codes
     *@author Valentin Labat
     *@param code1 premier code a comparer
     *@param code2 second  code a comparer
     *@return 0 si les deux codes sont identiques,
 un entier positif si [code1] est strictement plus grand que [code2]
 un entier negatif si [code1] est strictement plus petit que [code2]
     *)
    let compare code1 code2 =
      match (code1,code2) with
      | (c1,c2) when c1 = c2 -> 0
      | (c1,c2) when c1 > c2 -> 1
      | (c1,c2)              -> -1;;


    (** genere une liste de pion en sachant le premier
     *@author Mathieu Surman
     *@param nombre de couleur possible
     *@param nombre de pion possible
     *@param premier pion
     *@return retourne une liste de pion en sachant le premier
     *)
    let rec  ecriture_base_b c_p nb_p x =
      if nb_p = 0
      then []
      else (x mod c_p)::(ecriture_base_b c_p (nb_p-1) (x/c_p)) ;;


    (** La liste de tous les codes permis
     *@author Mathieu Surman*)
    let tous =
      let tous_les_codes c_p nb_p =
        let rec aux = function
          | -1 -> []
          | x -> (ecriture_base_b c_p nb_p x)::(aux (x-1))
        in aux (int_of_float((float_of_int c_p) ** (float_of_int nb_p)) - 1)
      in tous_les_codes (List.length couleurs_possibles) (nombre_pions) ;;


    (** Conversion code vers chaine de caracteres (pour affichage)
     *@author Valentin Labat
     *@param code code a convertir
     *@return la representation en chaine de caracteres de [code]
     *)
    let rec string_of_code code = 
      match code with
      | [] -> String.make 1 (' ')
      | h :: [] -> string_of_int(h)
      | h :: t -> (string_of_int(h)) ^ " " ^ string_of_code t;;

    
    (** a partir d'un char renvoie son numero ASCII
     *@author Valentin Labat
     *@param un char
     *@return un entier
     *)
    let match_ASCII_int char = int_of_char(char) - 48;;

    
    (** renvoie le code a partir d'une chaine de caractères
     *@author Valentin Labat
     *@param chaine de caractère
     *)
    let code_of_string1 chaine =
      let rec cosrec chn lg acc puissance =
        match lg with
        | a when a >= 0 -> if chn.[a] = ' ' then (cosrec chn (a-1) 0 0.) @ [acc]
                           else cosrec chn (a-1) (acc + int_of_float(float_of_int(match_ASCII_int(chn.[a])) *. (10. ** puissance))) (puissance+.1.)
        | _ -> [acc]
      in cosrec chaine (String.length(chaine) -1) 0 0.;;


    (** Verifie si le code convertie appartient a la liste de tous les codes possibles
     *@author Mathieu Surman
     *@param tous les codes possible
     *@param le code converti
     *@return un bolean
     *)
    let rec verification codes_possibles code =
      match codes_possibles with
      | [] -> false
      | h :: [] -> h = code
      | h :: t -> if h = code then true else verification t code;;


    (** Conversion chaine de caracteres vers code (pour saisie)
     *@author Valentin Labat
     *@param string chaine de caractere saisie
     *@return le code correspondant a la saisie si la conversion est possible
     [None] si la conversion n'est pas possible
     *)
    let code_of_string chaine =
      if verification tous (code_of_string1 chaine) then Some(code_of_string1 chaine)
      else None;;


    (**gener les scores possible mal placés en sachant les bien placés 
     *@author Mathieu Surman
     *@param le nombre de pions bien placés
     *@return une liste avec toutes les reponses possibles sachant le nombre de bien placés
     *)
    let rec arbitrages_sachant_bp b_p = function
      | 0 -> [(b_p,0)]
      | m_p -> (b_p , m_p)::(arbitrages_sachant_bp b_p (m_p -1));;


    (** La liste de toutes les reponses possibles
     *@author Mathieu Surman*)
    let toutes_reponses =
      let tous_les_arbitrages nb_p =
        let rec aux = function
          | -1 -> []
          | b_p -> (arbitrages_sachant_bp b_p (if b_p = nb_p then 0 else (nb_p -b_p))) @ (aux (b_p-1))
        in aux nb_p
      in tous_les_arbitrages nombre_pions;;


    (** Calcule les pions bien placé
     *@author Mathieu Surman
     *@param le code que l'on test
     *@param le code a trouver
     *@return le nombre de pion bien placés
     *)
    let rec bienplaces code vrai_code =
      match (code,vrai_code) with
      | ([],_) -> 0
      | (h1 :: t1,h2 :: t2) -> if h1 <> h2 then bienplaces t1 t2
                               else 1 + bienplaces t1 t2
      | _ -> 0;;

    
    (** Construit un triplet
     *@author Mathieu Surman
     *@param cs : le code secret
     *@param e : essai du joueur
     *@return un trilpet (nombre de pion bien placé, code secret moins les biens placé, l'essais moins les biens placé
     *)                
    let rec compter_et_retirer_bp cs e =
      match (cs,e) with
      | ([], _) | (_, []) -> (0, [] , [])
      | (tcs::qcs, te :: qe) -> let (bp,qcs' ,qe') = compter_et_retirer_bp qcs qe
                                in if tcs = te
                                   then (bp + 1, qcs', qe')
                                   else (bp, tcs::qcs', te::qe');;

    let rec occ x = function
      | [] -> 0
      |t::q -> (if t=x then 1 else 0) + (occ x q);;

    
    (** Compte les pions qui sont dans les deux listes ( ceux qui sont mal placé)
     *@author Mathieu Surman
     *@param cs : le code secret
     *@param e : essai du joueur
     *@return un entier
     *)
    let compter_mp mb_p cs' e' =
      let rec aux = function
        | -1 -> 0
        | x -> (min(occ x cs') (occ x e')) + (aux (x-1))
      in aux (mb_p-1) ;;

    
    (** Calcule la reponse d'un code par rapport au code cache
     *@author Mathieu Surman
     *@param cs : le code secret
     *@param e : essai du joueur
     *@return un couple (nombre de pions bien places, nombre de pions mal places)
 [None] si la reponse ne peut etre calculee
     *)        
    let reponse cs e = if List.length cs <> List.length e then None
                       else Some(let (bp, cs', e') = compter_et_retirer_bp cs e
                                 in (bp, compter_mp (List.length couleurs_possibles)  cs' e'));;


  end;;
open Code;;
