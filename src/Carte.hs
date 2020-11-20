module Carte where 

import qualified Data.Map.Strict as M
import qualified Data.Set as S



{- ****************** Coordonnees *********************** -}

data Coord = C {cx :: Int , cy :: Int} deriving (Show,Eq)

-- l'ordre des coordonnes selon les lignes
instance Ord Coord where
    (C x1 y1) <= (C x2 y2) = 
        if y1 <= y2
        then if y1 == y2 then x1 <= x2 else True
        else False

{- ********************* Case *********************** -}

-- direction d’une porte
data PDirection = NS | EO deriving Eq 

-- statut d’une porte
data StatutP = Ouverte | Fermee deriving Eq 

data Case = Normal -- une case vide
    | Porte PDirection StatutP -- une porte ouverte ou fermee
    | Mur -- infranchissable
    | Entree -- debut du niveau
    | Sortie -- fin du niveau
    deriving Eq

-- pour chaque type du case il corresponds une caractere 
instance Show Case where
    show Normal = " "
    show Mur = "X"
    show Entree = "E"
    show Sortie = "S"
    show (Porte dir stat) | dir == NS && stat == Fermee = "-"  
    show (Porte dir stat) | dir == NS && stat == Ouverte = "\\"
    show (Porte dir stat) | dir == EO && stat == Fermee = "|"
    show (Porte dir stat) | dir == EO && stat == Ouverte = "/"

-- lecture du case (fonction auxilliere pour instancier Read)
readCase :: String -> [(Case, String)]
readCase [] = []
readCase (c:xs) | c ==' ' = [(Normal,xs)]
                | c =='X' = [(Mur,xs)]
                | c =='E' = [(Entree,xs)]
                | c =='S' = [(Sortie,xs)]
                | c =='-' = [(Porte NS Fermee,xs)]
                | c =='\\' = [(Porte NS Ouverte,xs)]
                | c == '|' = [(Porte EO Fermee,xs)]
                | c == '/' = [(Porte EO Ouverte,xs)]
                | otherwise = []

-- lecture du case à partir d'un chaine
instance Read Case where
    readsPrec _ = readCase

-- predicat qui dit si la case est une porte 
isDoor :: Case -> Bool
isDoor (Porte _ _) = True
isDoor _ = False

-- predicat qui dit si la case est une porte fermée 
isClosedDoor :: Case -> Bool
isClosedDoor (Porte _ Fermee) = True
isClosedDoor _ = False

-- predicat qui dit si la case est une porte ouverte
isOpenedDoor :: Case -> Bool
isOpenedDoor (Porte _ Ouverte) = True
isOpenedDoor _ = False



{- ******************** Carte *********************** -}

data Carte = Carte { 
                     largeur :: Int ,                   -- largeur du carte (en nombre de colognes)         
                     hauteur :: Int ,                   -- largeur du carte (en nombre de lignes) 
                     contenu_carte :: (M.Map Coord Case)      -- correspandance entre le coordonnees et la case associe
                    } deriving Eq

-- convertion du carte vers string (fonction auxilliere). 
showCarte :: Carte -> String
showCarte (Carte largeur hauteur content) =
    let cases = M.toAscList content in
        aux cases 0 
    where
        -- fonction qui prend la liste des couples (coordonnees, case), le numero de la ligne actuelle.
        -- parcours la liste des couples en affichant case par case. 
        -- quand il tombe sur coord dont y est different au numero de la ligne courant, il affiche saut à la ligne  
        aux :: [(Coord, Case)] -> Int -> String 
        aux [] _ = "" 
        aux ((C x y, caase):xs) ligne = 
            (if ligne == y then "" else "\n") <> (show caase) <> (aux xs y) 

-- convertion du carte vers string
instance Show Carte where
    show = showCarte

-- lecture du carte (fonction auxilliere).
-- prend le chaine à analyzer, les numeros du ligne et cologne courrantes et la liste des couples (coordonnee, case) deja reconnu.
-- renvoie la carte reconnu et la suite du chaine non-reconnu (eventuellement vide)
readAux :: String -> Int -> Int -> [(Coord, Case)] -> (Carte, String)
-- fin du chaine à analizer
-- on construit la carte à partir de la liste des couples deja reconnu.
-- l'hauteur du carte est la ligne courant (+1). 
-- la largeur est le coordonnees 'x' (+1) du derniere case lu.
readAux [] haut larg cases =
    (Carte  ((cx (fst (head cases))) + 1) (haut+1) (M.fromDescList cases), []) 
readAux str line column acc = 
    let result = (reads :: ReadS Case) str in -- lecture du case
        if result == [] -- l'echec du lecture
        then if (head str) == '\n' -- l'echec est lié au rencontre du '\n'
             then readAux (tail str) (line+1) 0 acc     -- saut à la ligne
             -- le caractere lu invalide
             -- on construit la carte à partir de la liste des couples deja reconnu.
             -- la reste du chaine est consideré comme non reconnu.
             else (Carte ((cx (fst (head acc))) + 1) (line +1) (M.fromDescList acc), str)   
        else let [(caase , reste)] = result in
            readAux reste line (column+1) ((C column line, caase):acc)   -- on construit une couple à partir du ligne et cologne courrant et la case lu. 

-- lecture du carte.
instance Read Carte where
    readsPrec _  = (\ str -> [readAux str 0 0 []])

-- propriete 1
-- Tous les coordonnees du carte sont dans le rectangle :
--  (0,0)-----------------(larg-1,0)
--  |                              |
--  (0,haut-1)-------(larg-1,haut-1)
coords_in_rect_prop :: Carte -> Bool
coords_in_rect_prop (Carte larg haut cases) =
    null ( M.filterWithKey (\ (C x y) _ -> (x<0) || (x>=larg) || (y<0) || (y>=haut)) cases)

-- proptiete 2
-- Tous les coordoonnees possibles du rectangle au-dessus existent dans la carte
all_coords_exists_prop :: Carte -> Bool
all_coords_exists_prop (Carte larg haut cases) =
    let coords = [C x y| x <- [0 .. (larg-1)], y <- [0 .. (haut-1)]]
    in all (\ coord -> M.member coord cases) coords

-- propriete 3
-- Il existe une unique entree et une unique sortie
one_entry_one_exit_prop :: Carte -> Bool
one_entry_one_exit_prop (Carte _ _ cases) =
    (M.size (M.filter (\ caase -> caase == Entree ) cases) == 1) &&
    (M.size (M.filter (\ caase -> caase == Sortie ) cases) == 1)

-- propriete 4
-- Les coordonneés d'extremités contiennent des mures.
wall_surrounded_prop :: Carte -> Bool
wall_surrounded_prop (Carte larg haut cases) = 
    let list_coords = leftWall <> topWall <> rightWall <> downWall in
        all (\ coord -> (M.lookup coord cases) == Just (Mur)) list_coords
    where
        leftWall = [C 0 y | y <-[0..(haut-1)]]
        topWall = [C x 0 | x <-[1..(larg-2)]]
        rightWall = [C (larg-1) y | y <-[0..(haut-1)]]
        downWall = [C x (haut-1) | x <-[1..(larg-2)]]

-- propriete 5
-- Les portes sont entouré par des murs de deux cotés
doors_in_wall_prop :: Carte -> Bool
doors_in_wall_prop carte@(Carte _ _ cases) =
    M.null (M.filterWithKey (\ (C x y) caase -> 
                case caase of
                    Porte EO _ -> (getCase carte (C x (y+1)) /= Just Mur) 
                                    || (getCase carte (C x (y-1)) /= Just Mur)
                    Porte NS _ -> (getCase carte (C (x+1) y) /= Just Mur) 
                                    || (getCase carte (C (x-1) y) /= Just Mur)
                    _ -> False) 
            cases)

-- propriete 6
-- La sortie est accessible à partir d'entree
exit_acces_prop :: Carte -> Bool
exit_acces_prop carte@(Carte _ _ cases) =
    let coord = getEntreeCoord carte in
        case coord of 
            Nothing -> False
            Just coordEntry -> exists_path carte [coordEntry] (S.singleton coordEntry) -- on lance recherche à partir du coordonnees contenant l'entree.
    where
        -- fonction auxilliere, qui dit si la sortie est accessible.
        -- fonction prends carte, la liste des coordonnées à verifier et l'ensemble des coordonnes deja verifié
        exists_path :: Carte -> [Coord] -> S.Set Coord -> Bool
        exists_path _ [] _ = False -- il reste plus de coordonnees à verifier
        exists_path carte ((C x y):coords) deja_passe 
            | getCase carte (C x y) == Just Sortie = True -- on a tombé sur la sortie
            | otherwise =
                -- a partir du coordonnees courant on regarde les coords dans 4 directions, si ils sont ajoutables on les ajoutes dans la liste des coords à traiter
                let coordsToVerify = filter (\ coord -> ajoutable carte coord deja_passe) [C (x-1) y, C x (y+1), C (x+1) y, C x (y-1)] 
                in exists_path carte (coords <> coordsToVerify) (S.union deja_passe (S.fromList coordsToVerify))
                where
                    -- fonction auxilliere qui dit si coordonnées sont ajoutables dans la liste des coordonnees à verifier
                    ajoutable :: Carte -> Coord -> S.Set Coord -> Bool
                    ajoutable carte@(Carte _ _ cases) coord deja_passe
                        -- si les coordonnees ne sont pas traversables (coordonnees non existent, coordonnees avec une mur, coordonnees avec une porte fermée) 
                        | not (isTraversable carte coord) = case cases M.!? coord of
                                                                Nothing -> False
                                                                Just caase -> isDoor caase -- on neglige les portes 
                        -- si les coordonnees ont été deja traités
                        | S.member coord deja_passe = False
                        -- sinon
                        | otherwise = True

-- invariant du carte.
-- combine les 6 proprietes. 
carte_inv :: Carte -> Bool
carte_inv c = (coords_in_rect_prop c) 
              && (all_coords_exists_prop c) 
              && (one_entry_one_exit_prop c) 
              && (wall_surrounded_prop c)
              && (doors_in_wall_prop c)
              && (exit_acces_prop c)

-- pre-condition du getCase.
-- carte doit respecter son invariant 
getCase_pre :: Carte -> Coord -> Bool
getCase_pre carte _ = (carte_inv carte) 

-- renvoi la case assosié aux coordonnees specifié. 
getCase :: Carte -> Coord -> Maybe Case
getCase (Carte _ _ cases) coord = M.lookup coord cases

-- post-condition du getCase.
getCase_post :: Carte -> Coord -> Bool
getCase_post carte@(Carte _ _ cases) coord =
    getCase carte coord == cases M.!? coord

-- pre-condition du isTraversable.
-- carte doit respecter son invariant 
isTraversable_pre :: Carte -> Coord -> Bool
isTraversable_pre carte _= carte_inv carte

-- dit si les coordonneés sont traversables (c'est pas une mur ou porte fermeé)
isTraversable :: Carte -> Coord -> Bool
isTraversable (Carte _ _ cases) coord = 
    case M.lookup coord cases of
        Nothing -> False
        Just x -> x /= Mur && (not (isClosedDoor x))

-- post-condition du isTraversable.
isTraversable_post :: Carte -> Coord -> Bool
isTraversable_post carte@(Carte _ _ cases) coord =
    isTraversable carte coord == case cases M.!? coord of
                                        Nothing -> False
                                        Just Mur -> False
                                        Just x -> not (isClosedDoor x)

-- pre-condition du editCase.
-- carte doit respecter son invariant, on peut pas y mettre la sortie et l'entree
editCase_pre :: Carte -> Coord -> Case -> Bool
editCase_pre carte _ caase = carte_inv carte && (caase/=Entree) && (caase/=Sortie)

-- fonction qui permet de modifier une case dans la carte.
-- si apres modification du case la carte ne respecte pas son invariant, la modification est annulé et l'ancien carte est retourné.
-- sinon la nouvelle carte est retourné
editCase :: Carte -> Coord -> Case -> Carte
editCase carte@(Carte _ _ cases) coord caase =
    let new_carte = carte {contenu_carte = M.update (\ _ -> Just caase) coord cases}
    in if carte_inv new_carte
       then new_carte
       else carte

-- post-condition du editCase.
editCase_post :: Carte -> Coord -> Case -> Bool
editCase_post carte@(Carte _ _ cases) coord caase =
    let new_carte@(Carte _ _ new_cases) = (editCase carte coord caase)
    in (carte_inv new_carte) -- la carte resultat respecte son invariant
       && (if cases /= new_cases -- si la modification a été fait
           then (new_cases M.! coord) == caase -- alors nouvelle carte contient la nouvelle case (sous coords specifié)
           else 
               (cases M.!? coord == Nothing)    -- sinon soit le coordonnees n'existent pas
               || (Just caase == cases M.!? coord) -- soit le case est existé à cette position
               || not (carte_inv (carte {contenu_carte = M.update (\ _ -> Just caase) coord cases}))) -- soit la carte obtenu apres la modification ne respecte pas son invariant 

-- pre-condition du actWithDoor.
-- carte doit respecter son invariant 
actWithDoor_pre :: Carte -> Coord -> Bool
actWithDoor_pre carte _ = carte_inv carte

-- fonction qui ouvre/ferme la porte, qui se trouve aux coordonnees specifiés, et renvoie la nouvelle carte.
-- si aux coordonnees specifiés il n'y a pas du porte, la carte est retourné telle qu'elle est.
actWithDoor :: Carte -> Coord -> Carte
actWithDoor carte@(Carte _ _ cases) coord =
    carte {contenu_carte = M.update (\ caase -> 
        case caase of
            Porte dir Ouverte -> Just (Porte dir Fermee)
            Porte dir Fermee -> Just (Porte dir Ouverte)
            x -> Just x) coord cases}

-- post-condition du actWithDoor.
actWithDoor_post :: Carte -> Coord -> Bool
actWithDoor_post carte@(Carte _ _ cases) coord =
    let new_carte@(Carte _ _ new_cases) = (actWithDoor carte coord)
    in (carte_inv new_carte) -- la carte resultat respecte son invariant
       && (if cases /= new_cases -- si la modification a été fait
           then case (cases M.! coord, new_cases M.! coord) of
                    (Porte dir1 Ouverte, Porte dir2 Fermee) -> dir1 == dir2 -- si l'ancien carte contient la porte ouverte alors nouvelle carte contient la porte fermée
                    (Porte dir1 Fermee, Porte dir2 Ouverte) -> dir1 == dir2 -- si l'ancien carte contient la porte fermée alors nouvelle carte contient la porte ouverte
                    _ -> False 
           else     -- si modification n'ont pas été fait, alors
                (cases M.!? coord == Nothing)   -- soit les coordonnees n'existent pas
                || not (isDoor (cases M.! coord))) -- soit les coordonnées passés ne contiennent pas une porte

-- pre-condition du getEntreeCoord.
-- carte doit respecter son invariant.
getEntreeCoord_pre :: Carte -> Bool
getEntreeCoord_pre c = carte_inv c

-- fonction qui renvoie les coordonnees du entrée dans la carte. 
getEntreeCoord :: Carte -> Maybe Coord
getEntreeCoord (Carte _ _ cases) =
    let list_cases = M.toAscList cases in
        aux list_cases
    where
        aux [] = Nothing
        aux ((C x y, caase):xs) | caase == Entree = Just (C x y)
                                | otherwise = aux xs

-- post-condition du getEntreeCoord.
getEntreeCoord_post :: Carte -> Bool
getEntreeCoord_post carte@(Carte _ _ cases) =
    let Just coord = getEntreeCoord carte
    in getCase carte coord == Just Entree -- coordonnees retorunés contiennent bien l'entree. 

-- pre-condition du portesAround.
-- carte doit respecter son invariant.
portesAround_pre :: Carte -> Coord -> Bool
portesAround_pre carte _ = carte_inv carte

-- fonction qui prends une coordonnees et renvoi la liste des coordonnees voisins contenant des portes
portesAround :: Carte -> Coord -> [Coord]
portesAround carte (C x y) =
    let  list_cases = [ (C x (y-1),getCase carte (C x (y-1))), 
                        (C x (y+1),getCase carte (C x (y+1))), 
                        (C (x-1) y,getCase carte (C (x-1) y)), 
                        (C (x+1) y,getCase carte (C (x+1) y)) ]
    in do
            maybe_case <- list_cases
            case maybe_case of
                (_,Nothing) -> [] -- coordonnees n'existent pas
                (coord,Just caase) -> if isDoor caase  -- si coordonees contiennent une porte
                                      then return coord 
                                      else [] 

-- post-condition du portesAround.
portesAround_post :: Carte -> Coord -> Bool
portesAround_post carte coord@(C x y) = 
    let list_coords = portesAround carte coord
    in ((length list_coords) <= 4) -- le liste resultat est au plus de taille 4
       && all (\ coord -> case getCase carte coord of 
                            Nothing -> False
                            Just x -> isDoor x) list_coords  -- tout les coordonnees du liste contiennent portes.