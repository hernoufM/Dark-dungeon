module Environnement where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Carte
import Entite 

{- ************** Environnement ***************** -}

-- Environnement est une correspondance entre le case (coordonnees) et les entites qui se trouvent sur cette case
data Environnement = Envi {contenu_envi :: M.Map Coord [Entite]} deriving (Show)

-- propriete 1
-- Dans le contenu il n'existe pas une correspandance avec la liste des entites vide
without_empty_lists_prop :: Environnement -> Bool
without_empty_lists_prop (Envi content) =
    M.null (M.filter (\ l -> l == []) content)

-- propriete 2
-- Tous les entites d'environnement sont differents (il n'existe pas 2 entites avec le meme identifiant). 
without_doubles_prop :: Environnement -> Bool
without_doubles_prop (Envi content) =
    without_doubles (M.toList content) S.empty
    where
        -- fonction auxilliere, qui prend contenu d'environnement sous forme du liste (ou tout court - environnement) et 
        -- l'ensemble des entites qu'il deja rencontré.
        without_doubles :: [(Coord,[Entite])] -> S.Set Entite -> Bool
        without_doubles [] _ = True -- fin du environnement
        without_doubles ((_,[]):xs) deja_present = without_doubles xs deja_present -- tous les entites pour cette case sont verifié
        without_doubles ((coord,ent:entites):xs) deja_present =
                    if S.member ent deja_present    -- si on rencontré deja une entite pareil 
                    then False
                    else without_doubles ((coord,entites):xs) (S.insert ent deja_present)

-- propriete 3
-- Tous les entites d'environnement respectent ses invariants 
tous_entites_valides :: Environnement -> Bool
tous_entites_valides (Envi content) =
    all (all entite_inv) (M.elems content)

-- propriete 4
-- Tous les coordonnees d'environnement sont positives. 
tous_coords_valides :: Environnement -> Bool
tous_coords_valides (Envi content) =
    all (\ (C x y) -> x>=0 && y>=0) (M.keys content)

-- invariant du environnement
-- combine les 4 proprietes
environnement_inv :: Environnement -> Bool
environnement_inv env =
    without_empty_lists_prop env
    && without_doubles_prop env
    && tous_entites_valides env
    && tous_coords_valides env

-- pre-condition du franchissable_env.
-- environnement doit respecter son invariant.
franchissable_env_pre :: Coord -> Environnement -> Bool
franchissable_env_pre _ env = environnement_inv env

-- fonction qui dit si la case est franchissable (il n'existe pas les autres entites en dessus)
franchissable_env :: Coord -> Environnement -> Bool
franchissable_env coord (Envi content) = M.lookup coord content == Nothing

-- post condition du franchissable_env.
franchissable_env_post :: Coord -> Environnement -> Bool
franchissable_env_post coord env@(Envi content) = 
    franchissable_env coord env == case M.lookup coord content of
                                      Nothing -> True
                                      Just _ -> False

-- pre-condition du trouve_id.
-- environnement doit respecter son invariant.
trouve_id_pre :: Int -> Environnement -> Bool
trouve_id_pre _ env = environnement_inv env

-- fonction qui prends un identifiant et environnement, 
-- et renvoie entite avec cette identifiant et coordonnes de ca case s'il existe.
-- sinon renvoie Nothing
trouve_id :: Int -> Environnement -> Maybe (Coord, Entite)
trouve_id id (Envi content) =
    trouve_aux  (M.toList content)
    where
        trouve_aux :: [(Coord, [Entite])]  -> Maybe (Coord, Entite)
        trouve_aux [] = Nothing
        trouve_aux ((_,[]):xs) = trouve_aux xs
        trouve_aux ((coord,ent:entites):xs) = 
            if (iden ent) == id
            then Just (coord, ent)
            else trouve_aux ((coord, entites):xs)

-- post condition du trouve_id.
trouve_id_post :: Int -> Environnement -> Bool
trouve_id_post id env@(Envi content) =
    case trouve_id id env of
        -- si le resultat est Nothing, alors dans content du environnement il n'existent pas des coordonnes qui contient entites avec meme identifiant.
        Nothing -> (M.filter (\ entites -> 
            case find (\ ent -> (iden ent) == id) entites of
                Nothing -> False
                Just _ -> True) content) == M.empty
        -- si le resultat est coordonnees du case et entite, alors dans la liste des entites de cette case il existe un entite avec meme identifiant
        Just (coord, ent) -> let entites = content M.! coord
                             in any (== ent) entites

-- pre-condition du rm_env_id.
-- environnement doit respecter son invariant.
rm_env_id_pre :: Int -> Environnement -> Bool
rm_env_id_pre _ env = environnement_inv env

-- fonction qui prends un identifiant et environnement, 
-- et retire entite avec cette identifiant d'environnement, s'il existe
-- sinon renvoie environnement tel que il est.
rm_env_id :: Int -> Environnement -> Environnement
rm_env_id id env@(Envi content) =
    case trouve_id id env of
        Nothing -> env -- si l'entite n'existe pas on renvoi l'environnement ancien 
        Just (coord, ent) -> -- sinon
            env {contenu_envi = M.update (\ entites ->   
                                            case filter (\ ent -> (iden ent) /= id) entites of -- on retire l'entite de la liste des entites de sa case
                                                [] -> Nothing -- si la liste des entites devient vide, on supprime coordonnes de la case dans l'environnement
                                                l -> Just l) 
                                          coord content}

-- post condition du rm_env_id.
rm_env_id_post :: Int -> Environnement -> Bool
rm_env_id_post id env@(Envi content) =
    let new_env@(Envi new_content) = rm_env_id id env 
    in (environnement_inv new_env) -- la nouvelle environnement respecte ces invariants
       && (if content /= new_content -- si la modification a été fait
           then trouve_id id new_env == Nothing -- alors dans nouvelle environnement il n'y a plus d'entite specifié
           else trouve_id id env == Nothing) -- si la modification n'a pas été fait alors il n'existe pas une entite pareil dans environnement

-- pre-condition du bouge_id.
-- environnement doit respecter son invariant et coordonnées specifié ne doit pas etre negative.
bouge_id_pre :: Int -> Coord -> Environnement -> Bool
bouge_id_pre id (C x y) env =
    (environnement_inv env) && (x >= 0) && (y >= 0)

-- fonction qui deplace une entite dans environnement.
-- si l'entite specifié n'existe pas dans environnement alors on renvoie l'environnement tel que il est
-- sinon si le coordonnees specifié n'est pas franchissable et l'entite n'est pas un joueur (un NPC) alors on renvoie l'environnement tel que il est
-- sinon on retire entite de son ancien coordonnees et l'ajoute dans la liste des entites du coords specifié. 
bouge_id :: Int -> Coord -> Environnement -> Environnement
bouge_id id coord env@(Envi content) =
    case trouve_id id env of
        Nothing -> env 
        Just (coord_old, ent) -> if coord_old == coord || (not (franchissable_env coord env) && not (isJoueur ent)) 
                                 then env -- on renvoie ancien environnement aussi quand l'ancien et nouvelle coords sont egaux
                                 else let env_tmp@(Envi content_tmp) =  rm_env_id id env -- environnement temporaire (l'entite specifié est supprimé)
                                      in env_tmp {contenu_envi = 
                                          if M.member coord content_tmp -- si la nouvelle coordonnees contient deja les entites.
                                          then M.update (\ entites -> Just (ent:entites)) coord content_tmp -- alors on ajoute l'entite à deplacer à la tete de la liste
                                          else M.insert coord [ent] content_tmp} -- sinon on insert cette coordonnes dans contenu du environnement 

-- post condition du bouge_id.
bouge_id_post :: Int -> Coord -> Environnement -> Bool
bouge_id_post id coord env@(Envi content) =
    let new_env@(Envi new_content) = bouge_id id coord env
    in (environnement_inv new_env)  -- la nouvelle environnement respecte ces invariants
       && (if content /= new_content    -- si la modification a été fait
           then case trouve_id id env of
                    Nothing -> False
                    Just (_,ent) -> head (new_content M.! coord) == ent  -- alors entite specifié se trouve à la tete du liste des entites du coordonnes specifié dans nouvelle environnement
           else case trouve_id id env of    -- si la modification n'a pas été fait
                    Nothing -> True     -- alors soit entite pareil n'existe pas
                    Just (coord_old,ent) -> 
                        coord_old == coord  -- soit la coordonnées source et coordonnées destination du deplacements sont egaux 
                        || (not (franchissable_env coord env) && not (isJoueur ent))) -- soit la coordonnées destination n'est pas franchissable et l'entite n'est pas un joueur (un NPC)

-- pre-condition du get_pos_joueur.
-- environnement doit respecter son invariant.
get_pos_joueur_pre :: Environnement -> Bool
get_pos_joueur_pre env = environnement_inv env


-- fonction qui renvoie la position (coordonnees) du joueur.
-- si plusuiers joueurs existent, les coordonnees du premier rencontré est renvoyé
-- si aucun joueur existe dans environnement error est produit (ne doit jamais se passé)
get_pos_joueur :: Environnement -> Maybe Coord
get_pos_joueur env@(Envi content) =
    get_aux (M.toList content)
    where
        get_aux :: [(Coord, [Entite])]  -> Maybe Coord
        get_aux [] = Nothing
        get_aux ((_,[]):xs) = get_aux xs
        get_aux ((coord,ent:entites):xs) = 
            if isJoueur ent
            then Just coord
            else get_aux ((coord, entites):xs)

-- post condition du get_pos_joueur.
get_pos_joueur_post :: Environnement -> Bool
get_pos_joueur_post env@(Envi content) =
    case get_pos_joueur env of  
        Nothing -> all (all (not.isJoueur)) (M.elems content) -- si resultat est Nothing alors tous les entites dans environnement sont des mobs (pas de joueur)
        Just coord -> any isJoueur (content M.! coord) -- sinon resultat est un coordonnes sur laquelle il existe un joueur.

-- pre-condition du info_env.
-- environnement doit respecter son invariant.
-- Il doit exister au moins un joueur.
info_env_pre :: Environnement -> Bool
info_env_pre env@(Envi contenu) =
    environnement_inv env
    && M.filter (\entites -> find isJoueur entites /= Nothing) contenu /= M.empty

-- fonction permettant de parcourir environnement et separer le joueur avec tous les monstres.
-- Cette fonction sert pour faire à jour l'environnement avec l'etat, contenant tous les entites de jeu (par example quand un monstre est mort ou joueur a obtenu powerUp, etc.).
info_env :: Environnement -> (Entite, S.Set Entite)
info_env (Envi contenu) =
    M.foldr (\entites acc-> foldr (\entite (joueur, monstres)-> 
                                                    if isJoueur entite
                                                    then (entite,monstres)
                                                    else (joueur, S.insert entite monstres)) 
                                                acc entites) (joueur_init, S.empty) contenu

-- post-condition du info_env.
-- l'information produite est à jour avec environnement et vice-versa.
info_env_post :: Environnement -> Bool
info_env_post env@(Envi contenu) =
    let (joueur, monstres) = info_env env
    in
        trouve_id (iden joueur) env /= Nothing
        && all (\ent -> trouve_id (iden ent) env /= Nothing) (S.toList monstres)  
        && all (\(_,entites) -> all (\ent -> S.member ent monstres || ent == joueur) entites) (M.toList contenu)

-- pre-condition du info_env.
-- environnement et les entites doit respecter ses invariants.
-- environnement doit contenir entite que on veut remplacer.
remplace_pre :: Entite -> Entite -> Environnement-> Bool
remplace_pre entite1 entite2 env =
    entite1 == entite2
    && trouve_id (iden entite1) env /= Nothing
    && environnement_inv env

-- fonction permetant de remplacer un entite dans environnement par lui meme (sert pour faire a jour les caracterestiques) 
remplace :: Entite -> Entite -> Environnement-> Environnement 
remplace ent new_ent env@(Envi contenu) =
    let Just (coord, _) = trouve_id (iden ent) env
    in
        let new_env@(Envi new_contenu) = rm_env_id (iden ent) env
        in 
            Envi (  if new_contenu M.!? coord == Nothing 
                    then M.insert coord [new_ent] new_contenu
                    else M.update (\ entites -> Just (new_ent:entites)) coord new_contenu)

-- post-condition du info_env.
-- nouvelle environnement doit respecter son invariant.
-- Nouvelle entite doit y etre present
-- Si l'ancien entite est present alors on lui a remplacé par lui-meme
remplace_post :: Entite -> Entite -> Environnement-> Bool
remplace_post entite1 entite2 env@(Envi contenu) =
    let new_env = remplace entite1 entite2 env
    in
        environnement_inv new_env
        &&  trouve_id (iden entite2) new_env /= Nothing