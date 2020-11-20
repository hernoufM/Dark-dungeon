module Modele where

import System.Random

import qualified Data.Map.Strict as M 
import qualified Data.Set as S 
import Data.List (foldl', find)
import SDL 
import Carte
import Environnement
import Entite
import Keyboard
import Events
import PowerUp

{- ***************** Ordre ********************* -}

-- Ordres dont les mobs sont capables gérer.
data Ordre = Aller_Nord 
           | Aller_Sud 
           | Aller_Est 
           | Aller_Ouest 
           | Rien_faire 
           deriving (Show, Eq)

{- *************** Modele ***************** -}

-- Modele du jeu. Reuni la carte, environnement, generateur standart (pour générer mouvements des mobs),
-- l'etat du clavier (pour pouvoir deplacer joueur dans bonne direction),
-- derniere touche tappé pour se deplacer (pour pouvoir graphiquement dessiner dans quelle direction regarde joueur).
-- et la liste des evenements permettant de gerer de facons efficace le cooldown d'attack des entites
data Modele = Cont {
                     carte :: Carte , -- carte 
                     envi :: Environnement , -- environnement
                     gene :: StdGen , -- generateur standart
                     keyboard :: Keyboard, -- l'etat du clavier
                     lastKeyCode :: Keycode, -- derniere touche tappé pour se deplacer
                     events :: [GameEvent]  -- liste des evenements du jeu
                    } deriving (Show)

-- propriete 1
-- L'environnement n'est pas en contradiction avec la carte:
-- Tous les cases où se trouvent les entites sont traversables (les coordonnees existantes, pas d'entites dans mures, dans portes femée). 
env_fit_carte_prop :: Modele -> Bool
env_fit_carte_prop modele@(Cont carte (Envi content) _ _ _ _) =
    all (\ coord -> isTraversable carte coord) (M.keys content)

-- propriete 2
-- Chaque evenement de la liste des evenements du modele doit repecter son invariant, et pour chaque entite ne contenir qu'une seul evenement d'attaque.
-- Le temps de debut du chaque evenement ne peut pas etre negative.
evenements_valides_prop :: Modele -> Bool
evenements_valides_prop modele@(Cont _ (Envi content) _ _ _ evenements) =
    all game_event_inv evenements
    && all (\evt -> begin_time evt >= 0) evenements
    && length (filter isJoueurAttack evenements) <= 1 
    && all (\evt1 -> length (filter (\evt2 -> evt1 == evt2) evenements) == 1) evenements

-- invariant du modele
-- La carte, environnement du modele doivent respecter ses invariants.
-- En plus modele doit respecter les proprietes en dessus.
modele_inv :: Modele -> Bool
modele_inv modele@(Cont carte env _ _ _ _) = 
    (carte_inv carte)
    && (environnement_inv env)
    && (evenements_valides_prop modele)
    && (env_fit_carte_prop modele)

-- pre-condition du bouge.
-- modele specifié doit respecter son invariant et les coordonnées specifié doivent etre traversables sur la carte.
bouge_pre :: Modele -> Entite -> Coord -> Bool
bouge_pre modele _ _ = (modele_inv modele)

-- fonction qui bouge une entite dans modele.
bouge :: Modele -> Entite -> Coord -> Modele
bouge modele@(Cont carte env gen kbd lkc _) entite coord =
    let new_env = if isTraversable carte coord -- si les coordonnees specifiés sont traversables
                  then bouge_id (iden entite) coord env  -- alors on appelle au bouge_id qui bouge un entite dans environnement
                  else env -- sinon ancien environnement est retourné
    in modele {envi = new_env}

-- post condition du bouge.
bouge_post :: Modele -> Entite -> Coord -> Bool
bouge_post modele@(Cont carte env@(Envi content) _ _ _ _) ent coord =
    let new_modele@(Cont _ new_env@(Envi new_content) _ _ _ _) = bouge modele ent coord
    in (modele_inv new_modele)      -- nouvelle modele respecte son invariant
       && (if content /= new_content    -- si la modification a été fait
           then case trouve_id (iden ent) env of
                    Nothing -> False
                    Just (_,ent) -> head (new_content M.! coord) == ent  -- alors entite specifié se trouve à la tete du liste des entites du coordonnes specifié dans nouvelle environnement
           else case trouve_id (iden ent) env of    -- si la modification n'a pas été fait
                    Nothing -> True     -- alors soit entite pareil n'existe pas
                    Just (coord_old,ent) -> 
                        coord_old == coord  -- soit la coordonnées source et coordonnées destination du deplacements sont egaux 
                        || (not (franchissable_env coord env) && not (isJoueur ent))) -- soit la coordonnées destination n'est pas franchissable et l'entite n'est pas un joueur (un NPC)
                        || (not (isTraversable carte coord))   -- soit les coordoonnees ne sont pas traversables

-- pre-condition du decide.
decide_pre :: [(Int,Ordre)] -> Modele -> Entite -> Bool
decide_pre ordres modele@(Cont _ env _ _ _ _) entite =
    (modele_inv modele) -- modele doit respecter son invariant
    && (trouve_id (iden entite) env /= Nothing) -- entite doit exister dans environnement 
    && (not (isJoueur entite)) -- entite doit etre un NPC (pas joueur)
    && (length ordres > 0) -- la liste pondérée doit pas etre vide
    && (all (\ (p,_) -> p > 0) ordres) -- les poids des ordres doivent etre strictement positive
    && (any (\ (_,ord) -> ord == Rien_faire) ordres)  -- l'ordre Rien_faire doit etre present dans la liste

-- fonction qui permet de choisir quelle ordre on vas appliquer sur un mob, quand il ne s'agresse pas au joueur (ou quand le mob est "peaceful").
-- fonction prend la liste pondérée des ordres (poids doivent etre > 0).
-- tire une ordre au hazard du cette liste et applique cette ordre sur mob specifié.
-- les ordres ont étais calculer dans fonction prevoit, donc ils contient les ordres valides (apres avoir appliquer l'ordre, modele vas respecter son invariant).
-- pour l'instant, mobs ne sont pas agressive, donc leur comportement possible c'est de se deplacer dans 4 direction et rester sur place.
decide :: [(Int,Ordre)] -> Modele -> Entite -> Modele
decide ordres modele@(Cont _ env gen _ _ _) entite =
    let (ordre, new_gen) = random_ordre ordres gen  -- on tire une ordre au hazard
        Just (C x y, _) = trouve_id (iden entite) env -- on recupere les coordonnees courants d'entite
    in
        let new_modele = case ordre of 
                Aller_Nord -> bouge modele entite (C x (y-1))  -- se deplacer au nord
                Aller_Sud -> bouge modele entite (C x (y+1)) -- se deplacer au sud
                Aller_Est -> bouge modele entite (C (x+1) y) -- se deplacer à l'est
                Aller_Ouest -> bouge modele entite (C (x-1) y) -- se deplacer à l'ouest
                Rien_faire -> modele  -- rester sur place
        in new_modele {gene = new_gen}  -- on passe nouvelle generateur pour prochaines appelles
    where
        -- fonction auxilliere, qui tire un ordre au hazard du liste pondérée des ordres.
        -- chaque element de la liste est un couple (poids, ordre)
        -- L'algorithme du choix d'ordre est suivant:
        --      1) on calcule la somme de tous les poids (poids_total)
        --      2) on genere un nombre entre 1 et poids_total au hazard (nombre)
        --      3) on parcours la liste, en sauvegardant la derniere ordre passé et la somme des poids des ordres deja passé
        --      4) quand la somme des poids atteint le nombre généré, le derniere ordre passé est retourné
        -- Algorithme permet de garder la probabilités des ordres (qui est calculer en fonction de poids) au moment du choix.
        random_ordre :: [(Int,Ordre)] -> StdGen -> (Ordre, StdGen)
        random_ordre ordres gen =
            let poids_total = foldr (\(p,_) acc -> p + acc) 0 ordres
                (nombre, new_gen) = randomR (1, poids_total) gen 
            in
                let res = foldr (\ (p,ord) (pacc,ordacc) -> 
                                        if (p+pacc >= nombre) && (pacc < nombre) 
                                        then (p+pacc, ord)
                                        else (p+pacc, ordacc))
                                    (0, Rien_faire) ordres
                in (snd res, new_gen)

-- post-condition du decide.
decide_post :: [(Int,Ordre)] -> Modele -> Entite -> Bool
decide_post ordres modele@(Cont _ env _ _ _ _) entite =
    let new_modele@(Cont _ new_env _ _ _ _) = decide ordres modele entite     -- on recupere la modele resultat avec nouvelle environnement
    in 
        (modele_inv new_modele) -- modele resultat respecte invariant
        && (let Just (coord_old@(C x y),_) = trouve_id (iden entite) env    -- on recupere l'ancien coordonnees d'entite 
                Just (coord_new,_) = trouve_id (iden entite) new_env        -- on recupere nouvelle coordonnees d'entite
                -- on construit un liste des coordonnees a partir d'ancien coordonnes et la liste des ordres. 
                -- Cette liste represente tous les positions possibles d'entite dans la nouvelle modele  
                possible_pos = fmap (\ (_,ordre) -> case ordre of        
                                                        Rien_faire -> coord_old
                                                        Aller_Nord -> C x (y-1)
                                                        Aller_Sud -> C x (y+1)
                                                        Aller_Ouest -> C (x-1) y
                                                        Aller_Est -> C (x+1) y)
                                    ordres
            in 
                any (== coord_new) possible_pos)  -- nouvelle coordonnees d'entite doit etre present dans la liste des positions possibles.

-- pre-condition du prevoit.
prevoit_pre :: Modele -> Entite -> Bool
prevoit_pre modele@(Cont _ env _ _ _ _) entite =
    (modele_inv modele) -- modele doit respecter son invariant
    && (trouve_id (iden entite) env /= Nothing) -- entite doit exister dans environnement 
    && (not (isJoueur entite)) -- entite doit etre un NPC (pas joueur)

-- fonction qui genere la liste pondérée des ordres possibles pour une entite dans la modele.
-- La liste retoruné peut etre traité comme les ordres possibles à appliquer avec leur probabilité (poids) correspondante.
-- D'apres le choix d'implementation:
-- si l'entite est coincé (n'as pas de possibilités de se deplacer) alors la liste des regles vas contenir une seul regle "Rien faire" avec probabilité 100%
-- si l'entite peut se deplacer dans 1 direction (par example sud) alors : "Rien faire" avec probabilité 50%, "Aller_Sud" avec probabilité 50%
-- si l'entite peut se deplacer dans 2 directions (par example sud, nord) alors : "Rien faire" avec probabilité 50%, "Aller_Sud" avec probabilité 25%, "Aller_Nord" avec probabilité 25%
-- si l'entite peut se deplacer dans 3 directions (par example sud, nord, est) alors : "Rien faire" avec probabilité 50%, "Aller_Sud" avec probabilité 16.66%, "Aller_Nord" avec probabilité 16.66%,, "Aller_Est" avec probabilité 16.66%
-- si l'entite peut se deplacer dans tous les 4 directions alors:  "Rien faire" avec probabilité 50% et aller vers les autres directions avec probabilité 12.5%.
prevoit :: Modele -> Entite -> [(Int,Ordre)]
prevoit modele@(Cont carte env _ _ _ _) entite =
    let Just (C x y, _) = trouve_id (iden entite) env  -- d'abbord la fonction recupere coordonnees d'entite
    in 
        -- apres, construit une liste des couples '(ord, coord)' dont 'coord' devrais etre nouvelle position apres avoir appliquer l'ordre 'ord'.
        -- tous les coords du liste doivent etre accessible (traversable et franchissable)
        -- Donc la liste construit represente les ordres de deplacements qu'on peut appliquer à l'entite.
        let aller_liste = do
              (ordre,coord) <- [(Aller_Nord, C x (y-1)), (Aller_Sud, C x (y+1)), (Aller_Ouest, C (x-1) y), (Aller_Est, C (x+1) y)]
              if accessible modele coord
              then [(1,ordre)]  -- chaque ordre de deplacement a une poids 1
              else []
        in let poids_rf = (max 1 (length aller_liste))
           in (poids_rf, Rien_faire):aller_liste
    where
        accessible :: Modele -> Coord -> Bool
        accessible (Cont carte env _ _ _ _) coord = 
            (isTraversable carte coord) && (franchissable_env coord env)

-- post-condition du prevoit.
prevoit_post :: Modele -> Entite -> Bool
prevoit_post modele@(Cont carte env _ _ _ _) entite =
    let liste_ordre = prevoit modele entite   -- on recupere la liste resultat
        Just (coord@(C x y),_) = trouve_id (iden entite) env  -- on recupere la coordonnees du entite
    in 
        -- premiere element du liste doit etre une ordre "Rien faire" avec un poids egale au nombre de deplacements possibles (ou 1 si aucun deplacement n'est possible) 
        (head liste_ordre) == (max 1 (length (tail liste_ordre) ), Rien_faire)   
         -- pour chaque element de la liste a partir du deuxieme element :
        && all (\ (p,ord) ->        
                            let coord = case ord of 
                                            Aller_Nord -> C x (y-1)
                                            Aller_Sud -> C x (y+1)
                                            Aller_Ouest -> C (x-1) y
                                            Aller_Est -> C (x+1) y
                            in  
                                (p == 1)         -- le poids du deplacement egale à 1
                                && (isTraversable carte coord) && (franchissable_env coord env)) -- le coordonnees du deplacement doivent etre traversable et franchissable 
                (tail liste_ordre) 

-- pre-condition du tour.
tour_pre :: Double -> Modele -> Entite -> Bool
tour_pre time modele@(Cont _ env _ _ _ _) entite =
    time >= 0.0
    && (modele_inv modele) -- modele doit respecter son invariant
    && if isJoueur entite 
       then (trouve_id (iden entite) env /= Nothing) -- si l'entite est un jouer alors il doit exister dans environnement
       else True  

-- fonction qui prends une modele et entite et applique une action sur cette entite, en produissant nouvelle modele.
-- Si l'entite est un joueur alors l'action à appliquer est choisi selon la touche du clavier pressé (champ 'keyboard' du modele permet de savoir ça).
-- L'actions possible pour joueur sont : se deplacer, frapper une case (potentiellement avec un monstre) à coté, utiliser (ouvrir/fermer la porte) et rien faire.
-- Si l'entite est un NPC (non jouer) alors plusieurs regles d'applition sont possibles.
-- 
-- Si le mob est "peacefull" alors il se deplace aleatoirement dans la carte. Cela est géré grace au fonctions 'prevoit' et 'decide'.
-- 
-- Si le mob est agressive il y a trois comportement differents possible:
-- 1) si joueur est dans la case à coté ou dans la meme case, alors mob commence attaquer le joueur (en respectant son coolDown d'attaque). Donction frapper gere la mise à jour
--    des points de vie d'un joueur.
--    Dans interface graphique cela ressemble à 'saut' rapide sur joueur. Le mob ne se deplace plus, avant qu'il tue le jouer (ou bien avant que joueur ne s'échappe pas)
-- 2) si joueur est dans le champs de vision d'un mob, alors le mob commence s'approcher tres rapidement. Quel que soit ça vitesse normal 
--    (cela depend du monstre; par example pour un 'rat' la vitesse ordinaire c'est 0.7, pour un 'slime' 1.0, etc.), quand joueur est dans le champs de vision d'un mob
--    ça vitesse devient 0.5 (coolDown de mouvement), ce que est plus rapidement qu'habituellement. Quand il attrape joueur, il commence l'attaquer (cf. point (1)).
--    Fonction 'way_to_player' permet au monstre de savoir sur laquelle direction il doit partir pour ratrapper le joueur. Grace à cela, 
--    monstre choisi plus court chemin vers le joueur. Si le joueur disparait du champs de vision d'un monstre il retourne sur la comportement (3)
-- 3) si joueur est dans le champs de vision d'un mob agressive, alors il se comporte comme 'peacefull' mob
-- 
-- Si le mob est "neutral" alor il se comporte comme un mob 'peacefull' avant que joueur lui frappe. Apres avoir frapper le mob "neutral" il commence à se comporter
-- comme un mob agressive.
--
-- Si le mob est mort (il etait tué dans la meme cycle) alors 'tour' ne fait rien.
tour :: Double -> Modele -> Entite -> Modele
tour time modele@(Cont carte env _ kbd _ evenements) entite =
    if isJoueur entite
    then let Just (C x y,_) = trouve_id (iden entite) env
         in if keypressed KeycodeZ kbd -- si la touche Z pressé alors joueur se deplace au nord (s'il peut).
            then (bouge modele entite (C x (y-1))) {lastKeyCode = KeycodeZ} -- on fais m-a-j de lastKeyCode (ca permet choisir une bon fichier .png pour afficher le joueur) 
            else if keypressed KeycodeS kbd -- si la touche S pressé alors joueur se deplace au sud (s'il peut).
            then (bouge modele entite (C x (y+1))) {lastKeyCode  = KeycodeS}
            else if keypressed KeycodeD kbd -- si la touche D pressé alors joueur se deplace à l'est (s'il peut).
            then (bouge modele entite (C (x+1) y)) {lastKeyCode = KeycodeD}
            else if keypressed KeycodeQ kbd  -- si la touche Q pressé alors joueur se deplace à l'ouest (s'il peut).
            then (bouge modele entite (C (x-1) y)) {lastKeyCode = KeycodeQ}
            else if keypressed KeycodeSpace kbd  -- si la touche ESPACE pressé alors joueur ouvre/ferme tous les portes à cotés.
            then utiliser modele (C x y)
            else if keypressed KeycodeLeft kbd 
            then frapper_joueur time modele entite KeycodeLeft (C (x-1) y)
            else if keypressed KeycodeRight kbd 
            then frapper_joueur time modele entite KeycodeRight (C (x+1) y)
            else if keypressed KeycodeUp kbd 
            then frapper_joueur time modele entite KeycodeUp (C x (y-1))
            else if keypressed KeycodeDown kbd 
            then frapper_joueur time modele entite KeycodeDown (C x (y+1))
            else modele      -- sinon l'ancien modele est retourné
    else 
        if (cour_mouvement_coolDown entite) + (last_mouvement entite) >= time   -- si le coolDown de mouvement n'est pas ecoulé
        then modele --alors 'tour' pour ce mob ne fait rien
        else 
            case trouve_id (iden entite) env of 
                Nothing -> modele   -- entite est mort
                Just (coord,_)  -> 
                    if isPeacefull entite || (isNeutral entite && ((pvieTotal entite) == (pvie entite)))  -- si le mob est peacefull ou neutral avant que on le frappe
                    then let new_entite = entite {last_mouvement = time}
                         in let new_env = remplace entite new_entite env
                            in decide (prevoit (modele {envi=new_env}) new_entite) (modele {envi=new_env}) new_entite   -- alors il se deplace dans une case aleatoire
                    else if isAgressive entite || (isNeutral entite && ((pvieTotal entite) > (pvie entite))) -- si le mob est agressive ou neutral apres que on le frappe
                         then if player_is_near coord entite env -- si le joueur est à coté (ou sur la meme case)
                              then let new_entite = entite {cour_mouvement_coolDown=0.5}
                                   in 
                                       let new_env = remplace entite new_entite env
                                           Just coord_player = get_pos_joueur new_env
                                       in
                                           if all (not.isMonsterAttack new_entite) evenements -- si le cooldawn d'attaque est ecoulé
                                           then let new_modele = frapper time new_entite coord_player modele -- alors monstre attaque le joueur
                                                   -- evenement correpondant est ajouté dans la liste des evenements
                                                in new_modele { events = (mkMonsterAttackEvent (iden new_entite) (dir_from_coord coord coord_player) time ):evenements}
                                           else modele {envi = new_env} -- sinon 'tour' pour ce mob ne fait rien
                              else if player_is_arround coord entite env -- le joueur dans le champ de vision d'un enitite
                                   then let new_entite = entite {cour_mouvement_coolDown=0.5, last_mouvement = time} -- l'entite s'accelere
                                        in let new_env = remplace entite new_entite env
                                           in let next_coord = way_to_player coord carte new_entite new_env
                                              in bouge (modele {envi=new_env}) new_entite next_coord -- il se deplace vers la case plus proche au joueur
                                   else -- le mob se comporte comme mob 'peacefull' quand il n ya pas de joueur au tour
                                       let new_entite = entite {cour_mouvement_coolDown = init_mouvement_coolDown entite, last_mouvement = time}
                                       in let new_env = remplace entite new_entite env
                                          in decide (prevoit (modele {envi=new_env}) new_entite) (modele {envi=new_env}) new_entite
                         else error "should not occur"
    where
        -- fonction auxilliere qui prends une modele et une coordonnee et change l'etat de tous les portes à coté de cette coordonnee.
        utiliser :: Modele -> Coord -> Modele
        utiliser modele@(Cont carte _ _ _ _ _) (C x y) =
            let portes = portesAround carte (C x y) -- on recupere la liste des coordonnees contenant les portes
            in let new_carte = foldl' actWithDoor carte portes  -- on recupere la carte apres avoir changer l'etat de tous les portes
               in modele {carte = new_carte}
        -- fonction auxilliere pemettant de comparer les deux coordonnes et retourner la direction d'attaque
        dir_from_coord :: Coord -> Coord -> DirectionAttack 
        dir_from_coord (C x y) coord =
            if C x (y-1) == coord
            then At_Nord
            else if C x (y+1) == coord
            then At_Sud 
            else if C (x-1) y  == coord
            then At_Ouest 
            else if C (x+1) y == coord
            then At_Est
            else At_Central 
        -- fonction auxilliere qui gere une attaque d'un jouer. Cette fonction principalement verifie si le cooldown depuis la derniere attaque n'est pas encore écoulé.
        -- Si c'est le cas, alors elle fait rien. Sinon elle produit un evenement correspondante et delegue la suite de travail au fonction 'frapper' qui gere la mise a jour
        -- des points de vie d'un monstre (et si on le tue supprime le d'environnement) et une potentielle generation d'un powerUp si le monstre a été tué.
        frapper_joueur :: Double -> Modele -> Entite -> Keycode -> Coord -> Modele
        frapper_joueur time modele@(Cont carte env _ kbd _ evenements) joueur kc coord =
            let dir = case kc of
                        KeycodeLeft -> At_Ouest
                        KeycodeRight -> At_Est
                        KeycodeUp -> At_Nord
                        KeycodeDown -> At_Sud
            in 
                if isTraversable carte coord && (all (not.isJoueurAttack) evenements)
                then let new_modele@(Cont _ _ _ _ _ new_evenements) = frapper time joueur coord modele
                     in new_modele {lastKeyCode  = kc, events = (mkJoueurAttackEvent dir time ):new_evenements}
                else modele {lastKeyCode  = kc}
        -- fonction permettant au mob de savoir si le joueur est sur la meme case au sur la case vosin.
        player_is_near :: Coord -> Entite -> Environnement -> Bool
        player_is_near (C x y) entite env =
            let vis = (vision (stat entite))
            in
                let casesToVerifie = [C (x+1) y, C (x-1) y, C x (y+1), C x (y-1), C x y]
                    Just joueur_coord = get_pos_joueur env
                in
                    find (== joueur_coord) casesToVerifie /= Nothing
        -- fonction permettant au mob de savoir si le joueur est dans le champs de vision d'un monstre (selon son caracteristique 'vision').
        player_is_arround :: Coord -> Entite -> Environnement -> Bool
        player_is_arround (C x' y') entite env =
            let vis = (vision (stat entite))
            in
                let casesToVerifie = [C x y | x<-[(x'-vis) .. (x'+vis)], y<-[(y'-vis) .. (y'+vis)]]
                    Just joueur_coord = get_pos_joueur env
                in
                    find (== joueur_coord) casesToVerifie /= Nothing
        -- fonction permettant au mob agressive (ou neutral apres qu'il a été frappé) de savoir la prochaine case sur laquelle il doit partir
        way_to_player :: Coord -> Carte -> Entite -> Environnement -> Coord
        way_to_player cord_courrant@(C x y) carte entite env =
            let Just joueur_coord@(C xj yj) = get_pos_joueur env
            in
                let (dX, dY) = (x-xj, y-yj)
                    coordNord = C x (y-1)
                    coordSud = C x (y+1)
                    coordEst = C (x+1) y
                    coordOuest = C (x-1) y
                in
                    if dX > 0 && dY > 0 --joueur est dans la partie haut gauche
                    then next_coord_diagonal_cas dX dY cord_courrant coordNord coordOuest carte env
                    else if dX > 0 && dY < 0 --joueur est dans la partie bas gauche
                    then next_coord_diagonal_cas dX dY cord_courrant coordSud coordOuest carte env
                    else if dX < 0 && dY > 0 --joueur est dans la partie haut droite
                    then next_coord_diagonal_cas dX dY cord_courrant coordNord coordEst carte env
                    else if dX < 0 && dY < 0 --joueur est dans la partie bas droite
                    then next_coord_diagonal_cas dX dY cord_courrant coordSud coordEst carte env
                    else -- joueur est soit tout haut soit tout bas soit tout droite soit tout gauche
                        if dX == 0 && dY >0 -- joueur est en haut 
                        then next_coord_normal_cas cord_courrant coordNord carte env
                        else if dX == 0 && dY < 0 -- joueur est en bas
                        then next_coord_normal_cas cord_courrant coordSud carte env
                        else if dY == 0 && dX > 0 -- joueur est à gauche 
                        then next_coord_normal_cas cord_courrant coordOuest carte env
                        else -- joueur est à droite 
                             next_coord_normal_cas cord_courrant coordEst carte env
            where
                next_coord_diagonal_cas :: Int -> Int -> Coord -> Coord -> Coord -> Carte -> Environnement -> Coord
                next_coord_diagonal_cas dX dY cord_courrant coordVert coordHoriz carte env =
                    if (abs dX) >= (abs dY) -- la distance en largeur est plus grand que distance en hauteur
                    then if isTraversable carte coordHoriz && franchissable_env coordHoriz env
                         then coordHoriz
                         else if isTraversable carte coordVert && franchissable_env coordVert env
                         then coordVert
                         else cord_courrant
                    else if isTraversable carte coordVert && franchissable_env coordVert env
                         then coordVert
                         else if isTraversable carte coordHoriz && franchissable_env coordHoriz env
                         then coordHoriz
                         else cord_courrant
                next_coord_normal_cas :: Coord -> Coord -> Carte -> Environnement -> Coord
                next_coord_normal_cas cord_courrant coord_next carte env =
                    if isTraversable carte coord_next && franchissable_env coord_next env
                    then coord_next
                    else cord_courrant

-- post-condition du tour.
tour_post :: Double -> Modele -> Entite -> Bool
tour_post time modele@(Cont carte env _ kbd lkc evenements) entite 
    | isJoueur entite =
        let new_modele@(Cont new_carte new_env _ _ new_lkc new_evenements) = tour time modele entite  -- on recupere le modele resultat
        in
            let Just coord@(C x y) = get_pos_joueur env  -- on recupere l'ancien coord de joueur
                Just new_coord = get_pos_joueur new_env -- on recupere nouvelle coord de joueur
                in
                    if (keypressed KeycodeZ kbd) && (isTraversable carte (C x (y-1))) -- si la touche Z a été presser et les anciens coordonnees nord ont été accessibles
                    -- alors nouvelle coord est egale au coordonnees au nord d'ancien et la maj du derniere touche tappé a été effectuer
                    then (new_coord == C x (y-1)) && new_lkc == KeycodeZ 
                    else if (keypressed KeycodeS kbd) && (isTraversable carte (C x (y+1)))  -- idem pour touche S et sud
                    then (new_coord == C x (y+1)) && new_lkc == KeycodeS    
                    else if (keypressed KeycodeD kbd) && (isTraversable carte (C (x+1) y)) -- idem pour touche D et est
                    then (new_coord == C (x+1) y) && new_lkc == KeycodeD
                    else if (keypressed KeycodeQ kbd) && (isTraversable carte (C (x-1) y))     -- idem pour touche Q et ouest
                    then (new_coord == C (x-1) y) && new_lkc == KeycodeQ
                    else if keypressed KeycodeSpace kbd     -- si la touche Espace a été presser
                    then (lkc == new_lkc)   -- alors lastKeyCode reste tel que il est
                    -- tous les portes à coté ont changé leur etat (ouvert->fermee ou fermee -> ouvert)
                         && (all (\ coord -> case (getCase carte coord,getCase new_carte coord) of 
                                                (Just (Porte dir1 Ouverte), Just (Porte dir2 Fermee))-> dir1==dir2
                                                (Just (Porte dir1 Fermee), Just (Porte dir2 Ouverte))-> dir1==dir2
                                                (_,_) -> False) 
                                (portesAround carte coord))
                    else if (keypressed KeycodeLeft kbd) -- si la touche Z a été pressé
                            && (isTraversable carte (C (x-1) y))  -- et les anciens coordonnees ouest ont été accessibles
                            && (all (not.isJoueurAttack) evenements) -- et son cooldown d'attack est ecoulé
                    -- alors l'entite qui s'est trouvé à gauche a recu le degat qui est egale au damage du jouer et
                   
                    then (new_lkc == KeycodeLeft) -- alors la maj du derniere touche tappé a été effectuer
                         && find isJoueurAttack new_evenements /=Nothing --et le nouvelle evenement d'atack a été produisse
                         && case (contenu_envi new_env) M.!? (C (x-1) y) of
                             Nothing -> True -- si il n'ya aucune monstre à gauche  
                             Just [monstre_apres] -> case (contenu_envi env) M.!? (C (x-1) y) of
                                                        Nothing -> True
                                                        Just [monstre_avant] -> fromInteger (pvie monstre_avant) == (fromInteger (pvie monstre_apres)) + (damage (stat entite))
                    else if (keypressed KeycodeRight kbd) -- idem pour la touche Right
                            && (isTraversable carte (C (x+1) y))  
                            && (all (not.isJoueurAttack) evenements)
                    then (new_lkc == KeycodeRight) 
                         && find isJoueurAttack new_evenements /=Nothing 
                         && case (contenu_envi new_env) M.!? (C (x+1) y) of
                             Nothing -> True
                             Just [monstre_apres] -> case (contenu_envi env) M.!? (C (x+1) y) of
                                                        Nothing -> True
                                                        Just [monstre_avant] -> fromInteger (pvie monstre_avant) == (fromInteger (pvie monstre_apres)) + (damage (stat entite))
                    else if (keypressed KeycodeUp kbd) -- idem pour la touche Up
                            && (isTraversable carte (C x (y-1)))  
                            && (all (not.isJoueurAttack) evenements)
                    then (new_lkc == KeycodeUp) 
                         && find isJoueurAttack new_evenements /=Nothing 
                         && case (contenu_envi new_env) M.!? (C x (y-1)) of
                             Nothing -> True
                             Just [monstre_apres] -> case (contenu_envi env) M.!? (C x (y-1)) of
                                                        Nothing -> True
                                                        Just [monstre_avant] -> fromInteger (pvie monstre_avant) == (fromInteger (pvie monstre_apres)) + (damage (stat entite))
                    else if (keypressed KeycodeDown kbd) -- idem pour la touche Down
                            && (isTraversable carte (C x (y+1)))  
                            && (all (not.isJoueurAttack) evenements)
                    then (new_lkc == KeycodeDown) 
                         && find isJoueurAttack new_evenements /=Nothing 
                         && case (contenu_envi new_env) M.!? (C x (y+1)) of
                             Nothing -> True
                             Just [monstre_apres] -> case (contenu_envi env) M.!? (C x (y+1)) of
                                                        Nothing -> True
                                                        Just [monstre_avant] -> fromInteger (pvie monstre_avant) == (fromInteger (pvie monstre_apres)) + (damage (stat entite))
                    else -- sinon que la maj de touches a été effectué
                        if keypressed KeycodeZ kbd
                        then new_lkc == KeycodeZ
                        else if keypressed KeycodeS kbd
                        then new_lkc == KeycodeS
                        else if keypressed KeycodeQ kbd
                        then new_lkc == KeycodeQ
                        else if keypressed KeycodeD kbd
                        then new_lkc == KeycodeD
                        else if keypressed KeycodeLeft kbd
                        then new_lkc == KeycodeLeft
                        else if keypressed KeycodeRight kbd
                        then new_lkc == KeycodeRight
                        else if keypressed KeycodeUp kbd
                        then new_lkc == KeycodeUp
                        else if keypressed KeycodeDown kbd
                        then new_lkc == KeycodeDown
                        else new_lkc == lkc
                             && coord == new_coord                           -- sinon le aucune touche n as ete tappé
        | isAgressive entite || (isNeutral entite && (pvie entite) < (pvieTotal entite)) = -- si le mob est en collere 
            let new_modele@(Cont new_carte new_env _ _ new_lkc new_evenements) = tour time modele entite  -- on recupere le modele resultat
            in 
            (lkc == new_lkc) &&
            case trouve_id (iden entite) env of
                Nothing -> True -- entite est mort
                Just (coord@(C x y),_) -> if player_is_near coord entite env && (all (not.isMonsterAttack entite) evenements)  -- si le joueur est à coté et cooldown d attauque est écoulé 
                                          then if (time > (last_mouvement entite)+(cour_mouvement_coolDown entite)) -- si le cooldown de mouvement est ecoulé
                                               then find (isMonsterAttack entite) new_evenements /= Nothing -- nouvelle evenement d attaque est produit
                                                    && case (trouve_id 0 env,trouve_id 0 new_env) of
                                                        (Just(_,joueur_avant), Just(_,joueur_apres)) -> fromInteger (pvie joueur_avant) == (fromInteger (pvie joueur_apres)) + (damage (stat entite)) -- et joueur a recu le degat
                                                        _ -> False -- Joueur ne peut pas disparaitre 
                                               else Just (coord,entite) == (trouve_id (iden entite) new_env) -- le cooldown du mouvement n est pas encore ecoulé
                                          else case trouve_id (iden entite) new_env of  -- sinon mob se deplace ou reste sur la meme place se deplace
                                                    Nothing -> False  -- monstre ne peut pas disparaitre ici
                                                    Just (new_coord,_) -> (any (== new_coord) [coord, C x (y-1), C x (y+1), C (x+1) y, C (x-1) y])                               
        | otherwise = -- sinon les entites se deplacer aleatoirement
            let new_modele@(Cont new_carte new_env _ _ new_lkc new_evenements) = tour time modele entite  -- on recupere le modele resultat
            in 
            (lkc == new_lkc) &&
            case (trouve_id (iden entite) env, trouve_id (iden entite) new_env) of
                (Nothing,Nothing) -> True -- entite est mort
                (Just (coord@(C x y),_), Just (new_coord,_)) ->  (any (== new_coord) [coord, C x (y-1), C x (y+1), C (x+1) y, C (x-1) y]) 
                _ -> False
    where
        accessible :: Modele -> Coord -> Bool
        accessible (Cont carte env _ _ _ _) coord = 
            (isTraversable carte coord) && (franchissable_env coord env)
        player_is_near :: Coord -> Entite -> Environnement -> Bool
        player_is_near (C x y) entite env =
            let vis = (vision (stat entite))
            in
                let casesToVerifie = [C (x+1) y, C (x-1) y, C x (y+1), C x (y-1), C x y]
                    Just joueur_coord = get_pos_joueur env
                in
                    find (== joueur_coord) casesToVerifie /= Nothing
    

-- pre-condition du level_finished.
-- modele specifié doit respecter son invariant
level_finished_pre :: Modele -> Bool
level_finished_pre modele = modele_inv modele

-- dit si le niveau est terminé.
-- niveau est terminer quand joueur est sur la case du sortie.
level_finished :: Modele -> Bool
level_finished (Cont carte env _ _ _ _) =
    case get_pos_joueur env of
        Nothing -> False
        Just coord -> getCase carte coord == Just Sortie 

-- post-condition du level_finished.
level_finished_post :: Modele -> Bool
level_finished_post modele@(Cont carte env _ _ _ _) =
    (level_finished modele) == (case get_pos_joueur env of
                                    Nothing -> False 
                                    Just coord -> getCase carte coord == Just Sortie)

-- pre-condition du perte.
-- modele specifié doit respecter son invariant 
perte_pre :: Modele -> Bool
perte_pre modele = modele_inv modele

-- dit si la modele est en situation perdante.
-- pour l'instant on n'as pas une situation perdante.
perte :: Modele -> Bool
perte (Cont _ env _ _ _ _) = 
    let Just (_,joueur) = trouve_id 0 env
    in
        (pvie joueur) <= 0

-- post condition du perte
perte_post :: Modele -> Bool
perte_post modele =
    (perte modele) == (case trouve_id 0 (envi modele) of 
                        Nothing -> False
                        Just (_,joueur) -> (pvie joueur) <= 0)

-- pre-condition du frapper.
-- L'entite specifié ne doit pas etre un 'peacefull' monstre
-- Modele et entite doit respecter son invariant
-- La case à frapper doit etre traversable et doit etre à coté de l'entite specifié.
frapper_pre :: Double -> Entite -> Coord -> Modele -> Bool
frapper_pre time entite coord@(C x y) modele =
    time >= 0.0
    && entite_inv entite
    && not (isPeacefull entite)
    && modele_inv modele
    && isTraversable (carte modele) coord 
    && case trouve_id (iden entite) (envi modele) of
            Nothing -> False -- l'entite doit etre vivant
            Just (coord_voisin,_) -> if isJoueur entite                                                             
                                    then any (== coord_voisin) [C (x+1) y, C (x-1) y, C x (y+1), C x (y-1)] -- joueur peut frapper la case à coté
                                    else any (== coord_voisin) [C (x+1) y, C (x-1) y, C x (y+1), C x (y-1), coord] -- joueur peut frapper la case à coté et la case sur laquelle il se trouve

-- fonction permetant au l'entite de frapper la case à coter (ou la meme case dans le cas du monstre)          
frapper :: Double -> Entite -> Coord -> Modele -> Modele
frapper time joueur@(Joueur _ _ _ (Stat dmg _ _)) coord modele@(Cont _ env@(Envi contenu) gen _ _ evenemetns) = -- atack du joueur
    case contenu M.!? coord of
        Nothing -> modele       -- joueur frappe le vide
        Just [monstre] -> if (fromInteger (pvie monstre)) - dmg <=0     -- on a tué un monstre 
                          then
                            let (ent, new_gen) = apply_powerUp joueur gen   -- generation du powerUp
                                new_env = rm_env_id (iden monstre) env
                            in 
                                case ent of
                                    Nothing -> modele {envi = new_env}  -- powerUp n'etais pas généré
                                    Just new_joueur -> modele {envi = remplace joueur new_joueur new_env, events = (mkPowerUpEvent time):evenemetns}    -- joueur a obtenu un powerUp
                          else modele {envi =Envi (M.insert coord [monstre {pvie = (pvie monstre) - (fromIntegral dmg)}] contenu)}  -- sinon les points de vie des monstres sont maj 

frapper _ monstre coord modele@(Cont _ env@(Envi contenu) _ _ _ _) = -- atack du monstre
    case contenu M.!? coord of
        Nothing -> modele
        Just entites -> 
            case find isJoueur entites of 
                Nothing -> modele
                Just joueur -> let new_joueur = joueur {pvie = (pvie joueur) - (fromIntegral (damage (stat monstre)))} -- les points de vie du joueur sont maj 
                               in
                                   modele {envi = remplace joueur new_joueur env}

-- post-condition du frapper.
frapper_post :: Double -> Entite -> Coord -> Modele -> Bool
frapper_post time entite coord@(C x y) modele@(Cont _ env@(Envi contenu) _ _ _ _)
    | isJoueur entite = -- atack du joueur
        let new_modele@(Cont _ new_env _ _ _ _) = frapper time entite coord modele
        in
            case (contenu M.!? coord, (contenu_envi new_env) M.!? coord) of
                (Just [monstre_avant], Just [monstre_apres]) -> fromInteger (pvie monstre_avant) == (fromInteger (pvie monstre_apres)) + (damage (stat entite)) -- les points de vis du monstre a bien été maj
                (Just [monstre_avant], Nothing) -> fromInteger (pvie monstre_avant) - (damage (stat entite)) <= 0   -- on a tué un monstre
                (Nothing, Nothing) -> True  -- joueur a frappé le vide
    | otherwise = -- atack du monstre
        let new_modele@(Cont _ new_env _ _ _ _) = frapper time entite coord modele
        in
            case (contenu M.!? coord,(contenu_envi new_env) M.!? coord) of
                (Just entites_avant, Just entites_apres) -> 
                    case (find isJoueur entites_avant, find isJoueur entites_apres) of
                        (Just joueur_avant, Just joueur_apres) -> fromInteger (pvie joueur_avant) == (fromInteger (pvie joueur_apres)) + (damage (stat entite)) -- les points de vis du joueur a bien été maj
                        (_,_) -> False  -- monstre ne peut pas attacker les autres monstres
                (_,_) -> False -- monstre ne peut pas attacker le vide
