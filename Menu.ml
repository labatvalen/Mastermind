#use "Code.ml";;
#use "IA.ml";;

module Menu =

struct

(** Renvoie ce qu'il y a "à l'intérieur" d'un 'a list option
   *@param   a   'a list option
   *@return  [] si a = None
             une 'a list sinon
   *)
let option_to_thing a =
   match a with
      | Some(b) -> b
      | None -> [];;


(** Renvoie ce qu'il y a "à l'intérieur" d'un (int*int) option
   *@param   un (int*int) option
   *@return  [] si a = None
             une (int*int) sinon
   *)
let option_to_couple a =
   match a with
      | Some(b) -> b
      | None -> (-1,-1);;


(** Renvoie un code au hasard, parmi la liste de tous les codes possibles
   *@param   ()   unit
   *@return  une 'a list appartenant à "tous"
   *)
let rec code_aleat () =
 let place = Random.int(List.length(tous)) in List.nth tous place;;


(** Renvoie le code que le joueur veut faire deviner
   *@param   ()   unit
   *@return  une 'a list qui correspond au code à deviner
   *)
let rec code_entre () =
 let _ = print_string "C'est donc l'IA qui va tenter de deviner le code que vous avez choisi. Entrez le code que vous voulez que l'IA devine." in
  let _ = print_newline () in
   let i = read_line () in
    option_to_thing(code_of_string(i));;


(** Permet de savoir si le code entré est correct
   *@param   ()   unit
   *@return  une affichage
   *)
let rec choisir_code  () =
 let _ = print_string "Entrez le code que vous voulez faire deviner." in
  let _ = print_newline () in
   let i = read_line () in 
      if (option_to_thing (code_of_string(i))) <> [] then 
         let _ = print_string "Le code entré est correct." in
          let _ = print_newline () in
           print_newline ()
      else
         let _ = print_string "Le code entré est incorrect. Veuillez réessayer." in
          let _ = print_newline () in
           print_newline ();;


(** Renvoie le code choisi par l'IA
   *@param   code_secret   le code que l'IA doit trouver
   *@param   code_propose  le code proposé par l'IA
   *@param   codes_possibles  la liste des codes possibles
   *@param   lstcp          la liste des codes déjà proposés
   *@return  le nouveau code à proposer
   *)
let rec choix_code_IA code_secret code_propose codes_possibles lstcp =
 let bp_mp = reponse code_secret code_propose in
  let _ = print_string "Entrez la méthode de votre choix :" in
   let _ = print_newline () in
    let _ = print_string "0 : Algorithme random" in
     let _ = print_newline () in
      let _ = print_string "1 : Algorithme naïf" in
       let _ = print_newline () in
        let a = read_line () in
         if a = "0" then
            let ncp = filtre 0 (code_propose,bp_mp) codes_possibles in
             choix 0 (lstcp @ [code_propose]) ncp
         else
            if a = "1" then
               let ncp = filtre 1 (code_propose,bp_mp) codes_possibles in
               choix 1 (lstcp @ [code_propose]) ncp
            else
               let _ = print_string "Le choix entré est incorrect. Réessayez." in
                let _ = print_newline () in
                 choix_code_IA code_secret code_propose codes_possibles lstcp;;


(** Menu du déroulement de la partie lorsque le joueur doit deviner le code
   *@param   tentatives   le nombre total de tentatives lors d'une partie
   *@param   tentatives_restantes le nombre de tentatives restantes
   *@param   parties  le nombre de parties restantes
   *@param   code  le code à deviner
   *@param   pts1  le nombre de points de l'utilisateur
   *@param   pts2  le nombre de points de l'IA
   *@param   booleen  la variable permettant d'alterner entre les 2 modes de jeu
   *@return  le couple comprenant le couple (pts1,pts2) et le booleen
   *)
let rec joueur_devine tentatives tentatives_restantes parties code pts1 pts2 booleen =
 let i = read_line () in
  if tentatives_restantes <= 0 then
     let _ = print_string "Vous avez perdu, dommage" in
      let _ = print_newline ()
       in ((pts1, pts2 +1), not booleen)
  else
     if List.mem (option_to_thing(code_of_string(i))) tous then
        if option_to_thing(code_of_string(i)) = code then
         let _ = print_string "Bravo, vous avez trouvé le code" in
            let _ = print_newline ()
             in ((pts1 +1, pts2), not booleen)
        else
          let _ = print_string "Non, ce n'est pas le code à trouver. Il y a " in
           let z = reponse code (option_to_thing(code_of_string(i))) in
              let _ = print_string(string_of_int(fst(option_to_couple(z)))) in
              let _ = print_string " pions bien placés et " in
              let _ = print_string(string_of_int(snd(option_to_couple(z)))) in
              let _ = print_string " pions mal placés. Il vous reste donc " in
            let _ = print_string(string_of_int(tentatives_restantes)) in
             let _ = print_string " tentatives pour trouver le code. Veuillez réessayer." in 
              let _ = print_newline () in
               joueur_devine tentatives (tentatives_restantes -1) parties code pts1 pts2 booleen
     else
        let _ = print_string "Le code n'a pas été rentré correctement. Réessayez." in
         let _ = print_newline ()
          in joueur_devine tentatives tentatives_restantes parties code pts1 pts2 booleen;;


(** S'occupe de la première occurence du mode dans lequel le joueur doit deviner le code
   *@param   tentatives   le nombre total de tentatives lors d'une partie
   *@param   tentatives_restantes le nombre de tentatives restantes
   *@param   parties  le nombre de parties restantes
   *@param   code  le code à deviner
   *@param   pts1  le nombre de points de l'utilisateur
   *@param   pts2  le nombre de points de l'IA
   *@param   booleen  la variable permettant d'alterner entre les 2 modes de jeu
   *@return  le couple comprenant le couple (pts1,pts2) et le booleen
   *)
let rec joueur_devine_annonce tentatives tentatives_restantes parties code pts1 pts2 booleen =
 let _ = print_string "C'est donc à vous de deviner le code défini par l'ordinateur. Entrez votre code d'essai." in
  let _ = print_newline () in
   joueur_devine tentatives (tentatives_restantes-1) parties code pts1 pts2 booleen;;


(** Renvoie le booléen correspondant à l'entrée de l'utilisateur
   *@param   code   le code
   *@param   calcul le mode d'"arbitrage" du jeu
   *@param   parties  le nombre de parties restantes
   *@return  true si l'entrée est "o"
             false si elle est "f"
   *)
let rec verif_main tentatives code calcul =
 let i = read_line () in
    if i = "o" then
       true
    else
       if i = "n" then
          false
       else
          let _ = print_string "Vous n'avez pas rentré une valeur correcte. Recommencez." in
           let _ = print_newline () in
            verif_main tentatives code calcul;;

(** Menu du déroulement de la partie lorsque l'IA doit deviner le code
   *@param   tentatives   le nombre total de tentatives lors d'une partie
   *@param   tentatives_restantes le nombre de tentatives restantes
   *@param   parties  le nombre de parties restantes
   *@param   code  le code à deviner
   *@param   code_propose  le code proposé par l'IA
   *@param   lst_codes_proposes  la liste de tous les codes proposés
   *@param   pts1  le nombre de points de l'utilisateur
   *@param   pts2  le nombre de points de l'IA
   *@param   booleen  la variable permettant d'alterner entre les 2 modes de jeu
   *@return  le couple comprenant le couple (pts1,pts2) et le booleen
   *)
let rec ia_devine tentatives tentatives_restantes parties code code_propose lst_codes_proposes calcul pts1 pts2 booleen =
    let _ = print_string "Il reste " in let _ = print_string (string_of_int(tentatives_restantes)) in let _ = print_string " tentatives restantes à l'IA pour trouver votre code." in
    if tentatives_restantes <= 0 then
         let _ = print_string "L'IA n'est pas parvenue à trouver le code." in let _ = print_newline in ((pts1 +1, pts2), not booleen)
 else
    if calcul then
       if code = code_propose then
               let _ = print_string "L'IA a trouvé votre code" in let _ = print_newline () in ((pts1, pts2+1), not booleen)
       else
       let _ = print_string "Entrez la méthode de votre choix :" in
        let _ = print_newline () in
         let _ = print_string "0 : Algorithme random" in
         let _ = print_newline () in
         let _ = print_string "1 : Algorithme naïf" in
         let _ = print_newline () in
         let a = read_line () in
           if a = "0" then
             let nvll = lst_codes_proposes @ [code_propose] in
             let bpmp = reponse code code_propose in
              let reponses_possibles = filtre 0 (code_propose,bpmp) tous in
               let nouveau_code_propose = choix_code_IA code code_propose reponses_possibles nvll in
                ia_devine tentatives (tentatives_restantes -1) parties code nouveau_code_propose nvll calcul pts1 pts2 booleen
           else
               if a = "1" then
                let bpmp = reponse code code_propose in
                 let reponses_possibles = filtre 1 (code_propose,bpmp) tous in
                  let nouveau_code_propose = choix_code_IA code code_propose reponses_possibles (lst_codes_proposes @ [code_propose]) in
                   ia_devine tentatives (tentatives_restantes -1) parties code nouveau_code_propose (lst_codes_proposes @ [code_propose]) calcul pts1 pts2 booleen
               else
                let _ = print_string "Le choix entré est incorrect. Réesseayez." in
                 let _ = print_newline () in
                  ia_devine tentatives tentatives_restantes parties code code_propose lst_codes_proposes calcul pts1 pts2 booleen
    else
               let _ = print_string "Voici le code proposé par l'IA." in
                let _ = print_newline () in
                 let _ = print_string(string_of_code(code_propose)) in
                  let _ = print_newline () in
                   let _ = print_string "Et voici le code que vous vouliez faire deviner" in
                    let _ = print_newline () in
                     let _ = print_string(string_of_code(code)) in
                      let _ = print_newline () in
                       let _ = print_string "Sont-ils identiques ? Si oui, tapez o, sinon n" in
                        let _ = print_newline () in
                         if (verif_main tentatives code calcul) = (code = code_propose) then
                            if code = code_propose then
                               ((pts1, pts2 +1), not booleen)
                            else
let _ = print_string "Entrez la méthode de votre choix :" in
        let _ = print_newline () in
         let _ = print_string "0 : Algorithme random" in
         let _ = print_newline () in
         let _ = print_string "1 : Algorithme naïf" in
         let _ = print_newline () in
         let a = read_line () in
           if a = "0" then
             let nvll = lst_codes_proposes @ [code_propose] in
             let bpmp = reponse code code_propose in
              let reponses_possibles = filtre 0 (code_propose,bpmp) tous in
               let nouveau_code_propose = choix_code_IA code code_propose reponses_possibles nvll in
                ia_devine tentatives (tentatives_restantes -1) parties code nouveau_code_propose nvll calcul pts1 pts2 booleen
           else
               if a = "1" then
                let bpmp = reponse code code_propose in
                 let reponses_possibles = filtre 1 (code_propose,bpmp) tous in
                  let nouveau_code_propose = choix_code_IA code code_propose reponses_possibles (lst_codes_proposes @ [code_propose]) in
                   ia_devine tentatives (tentatives_restantes -1) parties code nouveau_code_propose (lst_codes_proposes @ [code_propose]) calcul pts1 pts2 booleen
               else
                let _ = print_string "Le choix entré est incorrect. Réessayez." in
                 let _ = print_newline () in
                  ia_devine tentatives tentatives_restantes parties code code_propose lst_codes_proposes calcul pts1 pts2 booleen

                         else
                            ((-1,-1),booleen);;


(** Première occurence de la partie lorsque l'IA doit deviner le code
   *@param   tentatives   le nombre total de tentatives lors d'une partie
   *@param   tentatives_restantes le nombre de tentatives restantes
   *@param   parties  le nombre de parties restantes
   *@param   calcul  le choix d'"arbitrage"
   *@param   pts1  le nombre de points de l'utilisateur
   *@param   pts2  le nombre de points de l'IA
   *@param   booleen  la variable permettant d'alterner entre les 2 modes de jeu
   *@return  le couple comprenant le couple (pts1,pts2) et le booleen
   *)
let rec ia_devine_annonce tentatives tentatives_restantes parties calcul pts1 pts2 booleen = 
 let _ = print_string "C'est donc l'IA qui va tenter de deviner le code que vous avez choisi. Entrez le code que vous voulez que l'IA devine." in
  let _ = print_newline () in
   let i = read_line () in
    let cd = option_to_thing(code_of_string(i)) in
     if List.mem cd tous then
      let _ = print_string "Le code est entré correctement." in
       let a = choix_code_IA cd [] tous tous in
        ia_devine tentatives (tentatives_restantes -1) parties cd a [a] calcul pts1 pts2 booleen
     else
      let _ = print_string "Le code n'a pas été rentré correctement. Réessayez." in
       let _ = print_newline () in
        ia_devine_annonce tentatives (tentatives_restantes-1) parties calcul pts1 pts2 booleen;;


(** Détermine qui commence
   *@param   tentatives   le nombre total de tentatives lors d'une partie
   *@param   tentatives_restantes le nombre de tentatives restantes
   *@param   parties  le nombre de parties restantes
   *@param   calcul  le type d'arbitrage
   *@param   pts1  le nombre de points de l'utilisateur
   *@param   pts2  le nombre de points de l'IA
   *@param   booleen  la variable permettant d'alterner entre les 2 modes de jeu
   *@return  le couple (pts1,pts2)
   *)
let rec qui_commence tentatives tentatives_restantes parties calcul pts1 pts2 booleen =
 if parties <= 0 then (pts1,pts2)
 else
    if booleen then
       let code = code_aleat () in
        let a = joueur_devine_annonce tentatives tentatives_restantes parties code pts1 pts2 booleen in
         let b = fst a in
          qui_commence tentatives tentatives (parties-1) calcul (fst b) (snd b) (snd a)
    else
       let a = ia_devine_annonce tentatives tentatives_restantes parties calcul pts1 pts2 booleen in
        qui_commence tentatives tentatives (parties-1) calcul (fst(fst a)) (snd(fst a)) (snd a);;


(** Permet de choisir le mode
   *@param   tentatives   le nombre total de tentatives lors d'une partie
   *@param   tentatives_restantes le nombre de tentatives restantes
   *@param   parties  le nombre de parties restantes
   *@param   calcul  le type d'arbitrage
   *@param   pts1  le nombre de points de l'utilisateur
   *@param   pts2  le nombre de points de l'IA
   *@return  
   *)
let rec choisir_mode tentatives tentatives_restantes parties calcul pts1 pts2 =
 let _ = print_string "Choisissez le mode de jeu souhaité :" in
  let _ = print_newline () in
   let _ = print_string "1 : IA vs IA" in
    let _ = print_newline () in
     let _ = print_string "2 : Humain vs IA" in
      let _ = print_newline () in
       let i = read_line () in 
        if i = string_of_int(1) then 
           let _ = print_string "Mode de jeu IA vs IA." in
            let _ = print_newline () in
             let _ = print_string "Cette partie n'a peu être traitée car il y avait un problème dans l'enchaînement des fonctions mais avait un système semblable à celui Humain vs IA" in let _ = print_newline () in (-1,-1)
        else
           if i = string_of_int(2) then
              let _ = print_string "Mode de jeu Humain vs IA." in
               let _ = print_newline () in
                let booleen = Random.bool () in qui_commence tentatives tentatives_restantes parties calcul pts1 pts2 booleen
           else
              let _ = print_string "L'option entrée est incorrecte. Remarque : le nombre entré doit être 1 ou 2." in
               let _ = print_newline () in
                choisir_mode tentatives tentatives_restantes parties calcul pts1 pts2;;


(** Affiche le départ du jeu
   *@param   tentatives   le nombre total de tentatives lors d'une partie
   *@param   tentatives_restantes le nombre de tentatives restantes
   *@param   parties  le nombre de parties restantes
   *@param   calcul  le type d'arbitrage
   *@param   booleen  la variable permettant d'alterner entre les 2 modes de jeu
   *@return  
   *)
let rec jeu nom tentatives tentatives_restantes parties calcul =
 let _ = print_string "Bonjour " in 
  let _ = print_string nom in 
   let _ = print_string " et bienvenue dans le Mastermind." in 
    let _ = print_newline () in 
     choisir_mode tentatives tentatives_restantes parties calcul 0 0;;;;


(** Affiche le nombre de parties
   *@param   parties  le nombre de parties
   *@return  
   *)
let rec intro_parties parties =
 let _ = print_string "Vous avez donc décidé de jouer " in
  let _ = print_string(string_of_int(parties)) in
   let _ = print_string " parties. Chaque joueur va deviner le code de l'autre chacun son tour. A la fin, celui qui aura deviné le plus de code aura gagné" in
    let _ = print_newline () in
     print_newline ();;


(** Détermine qui commence
    *@param   couple le couple de points
    *@return  affiche le gagnant
   *)
let rec affichage_vainqueur couple =
   if fst(couple) > snd(couple) then
      let _ = print_string "Le joueur a gagné. Il a totalisé " in let _ = print_string(string_of_int(fst(couple))) in let _ = print_string " points contre " in let _ = print_string(string_of_int(snd(couple))) in let _ = print_string " pour l'IA." in
       let _ = print_newline () in
        print_newline ()
   else
     if fst(couple) = snd(couple) then
       let _ = print_string "Match nul. " in let _ = print_string(string_of_int(snd(couple))) in let _ = print_string " partout."in let _ = print_newline () in
print_newline ()
     else
      let _ = print_string "L'IA a gagné. Elle a totalisé " in let _ = print_string(string_of_int(snd(couple))) in let _ = print_string " points contre " in let _ = print_string(string_of_int(fst(couple))) in let _ = print_string " pour le joueur." in
       let _ = print_newline () in
        print_newline ();;

(** Commande du lancement du jeu
   *@param   nom   le nom du candidat
   *@param   tentatives   le nombre total de tentatives lors d'une partie
   *@param   tentatives_restantes le nombre de tentatives restantes
   *@param   parties  le nombre de parties
   *@param   calcul  le type d'arbitrage
   *@return  le couple (pts1,pts2)
   *)
let rec mastermind nom tentatives parties calcul =
   if (parties mod 2 = 1) then mastermind nom tentatives (parties+1) calcul
   else
      let tentatives_restantes = tentatives in let couple = jeu nom tentatives tentatives_restantes parties calcul in affichage_vainqueur couple;;

end;;

open Menu;;
