module Generators where

import System.Random
import Carte
import Entite
import Environnement
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Test.QuickCheck

data Triplet a b c = Triplet a b c

create_bordure_pre :: Int -> Int -> Bool
create_bordure_pre larg haut =
    (larg > 2) && (haut > 2)

create_bordure :: Int -> Int -> M.Map Coord Case
create_bordure larg haut =
    let list_coords = leftWall <> topWall <> rightWall <> downWall in
        foldr (\ coord acc-> M.insert coord Mur acc) M.empty list_coords
    where
        leftWall = [C 0 y | y <-[0..(haut-1)]]
        topWall = [C x 0 | x <-[1..(larg-2)]]
        rightWall = [C (larg-1) y | y <-[0..(haut-1)]]
        downWall = [C x (haut-1) | x <-[1..(larg-2)]]

create_bordure_post :: Int -> Int -> Bool
create_bordure_post larg haut =
    let cases = create_bordure larg haut
        list_coords = leftWall <> topWall <> rightWall <> downWall
    in
        all (\coord -> (cases M.! coord) == Mur) list_coords
    where
        leftWall = [C 0 y | y <-[0..(haut-1)]]
        topWall = [C x 0 | x <-[1..(larg-2)]]
        rightWall = [C (larg-1) y | y <-[0..(haut-1)]]
        downWall = [C x (haut-1) | x <-[1..(larg-2)]]

fill_randomly_pre :: Int -> Int -> M.Map Coord Case -> StdGen -> Bool
fill_randomly_pre larg haut cases _ =
    (larg > 2) && (haut > 2)
    && (create_bordure larg haut) == cases


fill_randomly :: Int -> Int -> M.Map Coord Case -> StdGen-> (M.Map Coord Case, StdGen)
fill_randomly larg haut cases gen =
    foldr (\ coord acc -> 
                let (caase, new_gen) = random_case [(1,Mur), (1,Normal)] (snd acc)
                in (M.insert coord caase (fst acc), new_gen) ) (cases,gen) list_coords
    where
        list_coords = [C x y | x <-[1..(larg-2)], y<-[1..(haut-2)]]
        random_case :: [(Int,Case)] -> StdGen -> (Case, StdGen)
        random_case cases gen =
            let poids_total = foldr (\(p,_) acc -> p + acc) 0 cases
                (nombre, new_gen) = randomR (1, poids_total) gen 
            in
                let res = foldr (\ (p,caase) (pacc,caseacc) -> 
                                        if (p+pacc >= nombre) && (pacc < nombre) 
                                        then (p+pacc, caase)
                                        else (p+pacc, caseacc))
                                (0, Normal) cases
                in (snd res, new_gen)

fill_randomly_post :: Int -> Int -> M.Map Coord Case -> StdGen -> Bool
fill_randomly_post larg haut cases gen =
    let (new_cases, _) = fill_randomly larg haut cases gen 
    in
        all (\coord -> new_cases M.! coord == Mur) walls
        && all (\coord -> let caase = new_cases M.! coord 
                          in 
                              caase == Mur 
                              || caase == Normal) labirint
    where
        labirint = [C x y | x <-[1..(larg-2)], y<-[1..(haut-2)]]
        leftWall = [C 0 y | y <-[0..(haut-1)]]
        topWall = [C x 0 | x <-[1..(larg-2)]]
        rightWall = [C (larg-1) y | y <-[0..(haut-1)]]
        downWall = [C x (haut-1) | x <-[1..(larg-2)]]
        walls = leftWall <> topWall <> rightWall <> downWall

-- fonction auxillieure
coord_accesisble :: Case -> Coord -> Coord -> M.Map Coord Case -> Bool
coord_accesisble caase coord_src coord_dest cases =
    let res = cases M.!? coord_src in
        case res of 
            Nothing -> False
            Just _ -> exists_path caase [coord_src] coord_dest cases (S.singleton coord_src) 
    where
        exists_path :: Case -> [Coord] -> Coord -> M.Map Coord Case -> S.Set Coord -> Bool
        exists_path _ [] _ _ _ = False
        exists_path caase ((C x y):coords) coord_dest cases deja_passe 
            | (C x y) == coord_dest = True 
            | otherwise =
                let coordsToVerify = filter (\ coord -> ajoutable caase cases coord deja_passe) [C (x-1) y, C x (y+1), C (x+1) y, C x (y-1)] 
                in exists_path caase (coords <> coordsToVerify) coord_dest cases (S.union deja_passe (S.fromList coordsToVerify))
                where
                    ajoutable :: Case -> M.Map Coord Case -> Coord -> S.Set Coord -> Bool
                    ajoutable caase cases coord deja_passe
                        | cases M.!? coord == Just caase = not (S.member coord deja_passe)
                        | otherwise = False

all_groups_pre :: Case -> M.Map Coord Case -> Bool 
all_groups_pre caase cases = 
    (caase == Mur || caase == Normal)
    && M.filter (\caase -> caase /= Mur && caase /= Normal) cases == M.empty
 
all_groups :: Case -> M.Map Coord Case -> [[Coord]]
all_groups caase cases =
    let new_cases = M.filter (== caase) cases
    in 
        fst (M.foldrWithKey' (\ coord _ (groups,dejaDansGroup) ->
                                if S.member coord dejaDansGroup
                                then (groups, dejaDansGroup)
                                else 
                                    let group = visit_all_neighbours [coord] new_cases S.empty 
                                    in
                                        (group:groups, (foldr (\ coord acc -> S.insert coord acc) dejaDansGroup group))) 
                            ([], S.empty) new_cases)
    where
        visit_all_neighbours :: [Coord] -> M.Map Coord Case -> S.Set Coord -> [Coord]
        visit_all_neighbours [] _  dejaVisite = S.toList dejaVisite
        visit_all_neighbours ((C x y):coords) cases dejaVisite =
            let coordsToVerify = filter (\ coord -> 
                                            (M.member coord cases) && not (S.member coord dejaVisite)) 
                                        [C (x-1) y, C x (y+1), C (x+1) y, C x (y-1)] 
            in
                visit_all_neighbours (coords <> coordsToVerify) cases (S.insert (C x y) dejaVisite)

all_groups_post :: Case -> M.Map Coord Case -> Bool 
all_groups_post caase cases = 
    let groups = all_groups caase cases
    in
        all (\group -> length group > 0) groups 
        && all (\group -> all (\coord -> cases M.! coord == caase) group) groups
        && M.foldrWithKey (\coord casee acc -> if caase /=casee
                                               then True
                                               else any (any (== coord)) groups 
                                               && acc) True cases
        -- chaque case de chaque groupe est reuni avec les autres cases du meme group
        && all (\group -> 
                    all (\coord_src -> 
                            find (\cord_dest -> not (coord_accesisble caase coord_src cord_dest cases)) group == Nothing) 
                        group) groups
        -- une case du chaque groupe n'est pas reuni avec une case des autres  groupes
        && all (\group_src -> 
                    find (\group_dest -> 
                            if group_src == group_dest
                            then False
                            else coord_accesisble caase (group_src !! 0) (group_dest !! 0) cases) 
                          groups 
                    == Nothing) 
                groups

fix_walls_pre :: M.Map Coord Case -> StdGen -> Bool
fix_walls_pre cases _ =
    case M.lookupMax cases of
        Nothing -> False
        Just (C xMax yMax, _) -> 
            (xMax>0) && (yMax>0)
            &&
            let coords = [C x y | x<-[0..xMax], y<-[0..yMax]]
            in
                all (\coord -> cases M.!? coord /= Nothing) coords

fix_walls :: M.Map Coord Case -> StdGen -> (M.Map Coord Case, StdGen)
fix_walls cases gen =
    let groups = all_groups Mur cases
    in
        if any (\group -> length group < 4) groups
        then 
            let (new_cases, new_gen) = foldr (\group (cases,gen) -> 
                                                let (coord,new_gen) = random_coord_to_extend group cases gen
                                                in
                                                    (M.insert coord Mur cases, new_gen))
                                             (cases,gen) (filter (\group -> length group < 4) groups)
            in
                fix_walls new_cases new_gen
        else (cases,gen)
    where
        random_coord_to_extend ::  [Coord] -> M.Map Coord Case -> StdGen -> (Coord,StdGen)
        random_coord_to_extend group cases gen = 
            let coordsPossible = fst (foldr (\(C x y) (liste, dejaPresent)-> 
                                                let voisins = filter (\coord -> 
                                                                        (M.member coord cases) && (cases M.!? coord /= Just Mur) && (not (S.member coord dejaPresent))) 
                                                                        [C (x-1) y, C x (y+1), C (x+1) y, C x (y-1)]
                                                in
                                                    (voisins<>liste, S.union dejaPresent (S.fromList voisins))) 
                                        ([], S.empty) group)
                in
                    let (i,new_gen) = randomR (0, (length coordsPossible) - 1) gen
                    in
                        (coordsPossible !! i, new_gen) 

fix_walls_post :: M.Map Coord Case -> StdGen -> Bool
fix_walls_post cases gen =
    let (new_cases, _) = fix_walls cases gen
    in
        let groups = all_groups Mur new_cases
        in
            all (\group -> length group >= 4) groups

join_ways_pre :: Int -> Int -> M.Map Coord Case -> StdGen -> Bool
join_ways_pre larg haut cases _ =
    case M.lookupMax cases of
        Nothing -> False
        Just (C xMax yMax, _) -> 
            (larg == xMax+1) && (haut == yMax+1)
            &&
            let coords = [C x y | x<-[0..xMax], y<-[0..yMax]]
            in
                all (\coord -> cases M.!? coord /= Nothing) coords
                && any (\coord -> cases M.!? coord == Just Normal) coords 

join_ways :: Int -> Int -> M.Map Coord Case -> StdGen -> (M.Map Coord Case, StdGen)
join_ways larg haut cases gen =
    let groups = all_groups Normal cases
    in
        if (length groups) /= 1
        then 
            let (new_cases,new_gen) = foldr (\group (cases,gen) -> join_group larg haut group cases gen) (cases,gen) groups
            in 
                join_ways larg haut new_cases new_gen
        else (cases,gen)
    where 
        join_group :: Int -> Int -> [Coord] -> M.Map Coord Case -> StdGen -> (M.Map Coord Case, StdGen)
        join_group larg haut group cases gen =
            let coordsToChose = fst (foldr (\(C x y) (coords,dejaPrsente) -> 
                                            let liste = filter (\(C x y) -> (x /=0) && (x /= (larg-1) && (y/=0) && (y/=(haut-1)) && ((cases M.!? (C x y)) == Just Mur) && (not (S.member (C x y) dejaPrsente))))
                                                               [C (x-1) y, C x (y+1), C (x+1) y, C x (y-1)]
                                                in
                                                    (liste<>coords, S.union dejaPrsente (S.fromList liste))) 
                                      ([],S.empty) group)
                in
                    if coordsToChose == []
                    then (cases, gen)
                    else let (i, new_gen) = randomR (0,(length coordsToChose)-1) gen
                            in
                                (M.insert (coordsToChose !! i) Normal cases, new_gen)

join_ways_post :: Int -> Int -> M.Map Coord Case -> StdGen -> Bool
join_ways_post larg haut cases gen =
    let (new_cases, _) = join_ways larg haut cases gen
    in
        let groups = all_groups Normal new_cases
        in
            length groups == 1

generate_doors_pre :: M.Map Coord Case -> Bool
generate_doors_pre cases = 
    case M.lookupMax cases of
        Nothing -> False
        Just (C xMax yMax, _) -> 
            let coords = [C x y | x<-[0..xMax], y<-[0..yMax]]
            in
                all (\coord -> cases M.!? coord /= Nothing) coords
                && length (all_groups Normal cases) == 1 

generate_doors :: M.Map Coord Case -> M.Map Coord Case
generate_doors cases =
    let cases_for_doors = M.filterWithKey (\coord@(C x y) caase-> 
                                                (caase == Normal)
                                                && ((((cases M.!? (C (x-1) y)) == Just Mur) && ((cases M.!? (C (x+1) y)) == Just Mur) && ((cases M.!? (C x (y+1))) == Just Normal) && ((cases M.!? (C x (y-1))) == Just Normal))
                                                || (((cases M.!? (C (x-1) y)) == Just Normal) && ((cases M.!? (C (x+1) y)) == Just Normal) && ((cases M.!? (C x (y+1))) == Just Mur) && ((cases M.!? (C x (y-1))) == Just Mur))))
                                          cases
    in
        M.foldrWithKey (\coord@(C x y) _ acc -> 
                            if no_doors_arround coord acc
                            then 
                                if acc M.!? (C (x+1) y) == Just Normal
                                then M.insert coord (Porte EO Fermee) acc
                                else M.insert coord (Porte NS Fermee) acc
                            else acc) 
                       cases cases_for_doors
    where
        no_doors_arround :: Coord -> M.Map Coord Case -> Bool
        no_doors_arround (C x y) cases =
            let casesAVerifier = [C x' y' | x' <-[(x-3)..(x+3)], y' <- [(y-3)..(y+3)]]
            in
                all (\coord -> case cases M.!? coord of
                                Just (Porte _ _) -> False
                                _ -> True) casesAVerifier

generate_doors_post :: M.Map Coord Case -> Bool
generate_doors_post cases =
    let new_cases = generate_doors cases
    in
        let portes = M.difference new_cases cases
        in
            M.filter (not.isDoor) portes == M.empty
            && M.filterWithKey (\coord@(C x y) caase -> case caase of
                                Porte NS Fermee -> not (new_cases M.! (C x (y-1)) == Mur && new_cases M.! (C x (y+1)) == Mur)
                                Porte EO Fermee -> not (new_cases M.! (C (x-1) y) == Mur && new_cases M.! (C (x+1) y) == Mur)
                                _ -> True) portes
                == M.empty
            && all (\(C x y) -> let surrounding = [C x' y' | x' <-[(x-3)..(x+3)], y' <- [(y-3)..(y+3)]]
                                in
                                    all (\coord -> case new_cases M.!? coord of
                                                        Nothing -> True
                                                        Just caase -> not(isDoor caase)) surrounding) (M.keys portes)

generate_entry_pre :: Int -> Int -> M.Map Coord Case -> Bool
generate_entry_pre larg haut cases =
    (larg > 3) && (haut > 3)
    && M.filter (== Normal) cases /= M.empty

generate_entry :: Int -> Int -> M.Map Coord Case -> M.Map Coord Case
generate_entry larg haut cases =
    let coord = C (larg `div` 2) (haut `div` 2)
    in  
        if cases M.!? coord == Just Normal
        then M.insert coord Entree cases
        else place_entry 1 coord cases 
    where 
        place_entry :: Int -> Coord -> M.Map Coord Case -> M.Map Coord Case
        place_entry distance coord@(C x y) cases =
            let casesToVerify = rightWall<>downWall<>leftWall<>topWall
            in
                case find (\coord -> cases M.!? coord == Just Normal) casesToVerify of
                    Nothing -> place_entry (distance+1) coord cases
                    Just coord_entry -> M.insert coord_entry Entree cases 
            where
                leftWall = [C (x-distance) y' | y' <-[(y-distance)..(y+distance)]]
                topWall = [C x' (y-distance) | x' <-[(x-distance+1)..(x+distance-1)]]
                rightWall = [C (x+distance) y' | y' <-[(y-distance)..(y+distance)]]
                downWall = [C x' (y+distance) | x' <-[(x-distance+1)..(x+distance-1)]]

generate_entry_post :: Int -> Int -> M.Map Coord Case -> Bool
generate_entry_post larg haut cases =
    let new_cases = generate_entry larg haut cases
    in
        let entree = M.differenceWith (\case1 case2 -> if (case1/=case2) then Just case1 else Nothing) new_cases cases
        in
            M.size entree == 1 
            && snd (head (M.toList entree)) == Entree
  
data Direction = Nord | Sud | Ouest | Est
 
generate_exit_pre :: Int -> Int -> M.Map Coord Case -> Bool
generate_exit_pre larg haut cases =
    (larg > 4) && (haut > 4)
    && M.filter (== Normal) cases /= M.empty 

generate_exit :: Int -> Int -> M.Map Coord Case -> M.Map Coord Case
generate_exit larg haut cases =
    case find (\coord -> cases M.!? coord == Just Normal) [C 1 1, C 1 (haut-2), C (larg-2) 1, C (larg-2) (haut-2)] of
        Nothing -> place_exit 0 [  (coord_from_direction (C 1 1) Est,Est),
                                   (coord_from_direction (C 1 1) Sud,Sud),
                                   (coord_from_direction (C 1 (haut-2)) Est,Est),
                                   (coord_from_direction (C 1 (haut-2)) Nord,Nord),
                                   (coord_from_direction (C (larg-2) 1) Ouest,Ouest),
                                   (coord_from_direction (C (larg-2) 1) Sud,Sud),
                                   (coord_from_direction (C (larg-2) (haut-2)) Nord,Nord),
                                   (coord_from_direction (C (larg-2) (haut-2)) Ouest,Ouest) ]
                            cases
        Just coord -> M.insert coord Sortie cases
    where
        coord_from_direction :: Coord -> Direction -> Coord
        coord_from_direction (C x y) Nord = (C x (y-1))
        coord_from_direction (C x y) Sud = (C x (y+1))
        coord_from_direction (C x y) Ouest = (C (x-1) y)
        coord_from_direction (C x y) Est = (C (x+1) y)
        place_exit :: Int ->[(Coord,Direction)] -> M.Map Coord Case -> M.Map Coord Case
        place_exit _ [] _ = error "Should not occur"
        place_exit level ((coord,dir):coords) cases =
            case cases M.!? coord of 
                Nothing -> let coordLT = C (0+level +1) (0+level +1)
                               coordLB = C (0+level +1) (haut-level-2)
                               coordRT = C (larg-level-2) (0+level +1)
                               coordRB = C (larg-level-2) (haut-level-2)
                            in
                                place_exit (level + 1) [  (coord_from_direction coordLT Est,Est),
                                                          (coord_from_direction coordLT Sud,Sud),
                                                          (coord_from_direction coordLB Est,Est),
                                                          (coord_from_direction coordLB Nord,Nord),
                                                          (coord_from_direction coordRT Ouest,Ouest),
                                                          (coord_from_direction coordRT Sud,Sud),
                                                          (coord_from_direction coordRB Nord,Nord),
                                                          (coord_from_direction coordRB Ouest,Ouest)
                                                 ] cases
                Just caase -> if caase == Normal
                              then M.insert coord Sortie cases
                              else place_exit level (coords<>[(coord_from_direction coord dir,dir)]) cases 

generate_exit_post :: Int -> Int -> M.Map Coord Case -> Bool
generate_exit_post larg haut cases =
    let new_cases = generate_exit larg haut cases
    in
        let exit = M.differenceWith (\case1 case2 -> if (case1/=case2) then Just case1 else Nothing) new_cases cases
        in
            M.size exit == 1
            && snd (head (M.toList exit)) == Sortie

genCarte_pre :: Int -> Int -> StdGen -> Bool
genCarte_pre larg haut _ =
    (larg >= 10) && (haut >= 10) -- pour etre sûr que le generateur nous genere une map contenant ou moins 2 cases Normal

genCarte :: Int -> Int -> StdGen -> Carte
genCarte larg haut gen =
    let cases = create_bordure larg haut
    in
    let (new_cases,new_gen) = fill_randomly larg haut cases gen
    in
    let (new_cases2, new_gen2) = fix_walls new_cases new_gen
    in
    let (new_cases3, _) = join_ways larg haut new_cases2 new_gen2
    in
    let new_cases4 = generate_doors new_cases3 
    in
    let new_cases5 = generate_entry larg haut new_cases4 
    in
    let new_cases6 = generate_exit larg haut new_cases5
    in
        Carte larg haut new_cases6

genCarte_post :: Int -> Int -> StdGen -> Bool
genCarte_post larg haut gen =
    let carte = genCarte larg haut gen
    in
        carte_inv carte

genGameCarte :: IO Carte
genGameCarte = do
    seed <- randomRIO (0,100000) 
    let gen = mkStdGen seed
    return (genCarte 20 20 gen)

-- difuculté d'environnement. 
-- Chaque niveau du labirnt possede sa niveau du diffuclté (pour premiere niveau VeryEasy, pour deuxieme Easy, etc.)
data Difficulte = VeryEasy | Easy | Mediocre | Hard | VeryHard

add_player_pre :: Entite -> Carte -> Bool
add_player_pre ent carte =
    entite_inv ent
    && isJoueur ent
    && carte_inv carte

add_player :: Entite -> Carte -> M.Map Coord Entite
add_player ent carte@(Carte larg haut cases) =
    let (Just entry) = getEntreeCoord carte
    in
        M.singleton entry ent

add_player_post :: Entite -> Carte -> Bool
add_player_post ent carte =
    let contenu = add_player ent carte
    in
        M.size contenu == 1
        && let (Just entry) = getEntreeCoord carte
           in
               case contenu M.!? entry of
                   Nothing -> False
                   Just entite -> ent == entite

randomEntite_pre :: Double -> Difficulte -> Int -> StdGen -> Bool 
randomEntite_pre time _ id _ =
    time >= 0.0
    && id > 0

randomEntite :: Double -> Difficulte -> Int -> StdGen -> (Entite,StdGen)
randomEntite time diffuclte id gen = 
    let entites = case diffuclte of 
                    VeryEasy -> peaccefullMobs
                    Easy -> peaccefullMobs <> neutralMobs
                    Mediocre -> neutralMobs
                    Hard -> neutralMobs<>agressiveMobs
                    VeryHard -> agressiveMobs
    in randomMob entites gen
    where
        peaccefullMobs = [ PeacefullMonstre id "pink_slime" 20 1.0 time,
                           PeacefullMonstre id "worm" 20 1.5 time,
                           PeacefullMonstre id "blue_worm" 60 0.7 time ]
        neutralMobs = [ NeutralMonstre id "green_slime" 40 40 1.0 1.0 time (Stat 15 1.0 3),
                        NeutralMonstre id "rat" 30 30 0.7 0.7 time (Stat 15 0.7 5)]
        agressiveMobs = [ AgressiveMonstre id "red_slime" 60 1.0 1.0 time (Stat 40 1.0 3),
                          AgressiveMonstre id "spider" 50 0.7 0.7 time (Stat 30 0.7 4) ]
        randomMob :: [Entite] -> StdGen -> (Entite,StdGen)
        randomMob entites gen =
            let (i,new_gen) = randomR (0,(length entites) -1) gen
            in
                (entites !! i, new_gen)

randomEntite_post :: Double -> Difficulte -> Int -> StdGen -> Bool
randomEntite_post time diffuclte id gen =
    let (ent,_) = randomEntite time diffuclte id gen
    in
        iden ent == id
        && last_mouvement ent == time
        && case diffuclte of
            VeryEasy -> isPeacefull ent
            Easy -> isPeacefull ent || isNeutral ent
            Mediocre -> isNeutral ent
            Hard -> isNeutral ent || isAgressive ent
            VeryHard -> isAgressive ent 

generate_monsters_pre :: Double -> Difficulte -> Carte -> M.Map Coord Entite -> StdGen -> Bool
generate_monsters_pre time difficulte carte contenu _ =
    time >= 0.0
    && carte_inv carte
    && M.size contenu == 1
    && find (isJoueur.snd) (M.toList contenu) /= Nothing

generate_monsters :: Double -> Difficulte -> Carte -> M.Map Coord Entite -> StdGen -> (M.Map Coord Entite,StdGen)
generate_monsters time difficulte carte@(Carte _ _ cases) contenu gen  =
    let cases_for_monstres = M.filter (\caase-> caase == Normal) cases
    in
        let (Triplet new_contenu new_gen _) = M.foldrWithKey (\coord@(C x y) _ (Triplet contenu gen id)  -> 
                                                                    if no_entites_arround coord contenu
                                                                    then 
                                                                        let (ent,new_gen) = randomEntite time difficulte id gen
                                                                        in
                                                                            Triplet (M.insert coord ent contenu) new_gen (id+1)
                                                                    else (Triplet contenu gen id) ) 
                                                                (Triplet contenu gen 1) cases_for_monstres
        in
            (new_contenu,new_gen)
    where
        no_entites_arround :: Coord -> M.Map Coord Entite -> Bool
        no_entites_arround (C x y) contenu =
            let casesAVerifier = [C x' y' | x' <-[(x-5)..(x+5)], y' <- [(y-5)..(y+5)]]
            in
                all (\coord -> case contenu M.!? coord of
                                Just _ -> False
                                _ -> True) casesAVerifier

generate_monsters_post :: Double -> Difficulte -> Carte -> M.Map Coord Entite -> StdGen -> Bool
generate_monsters_post time difficulte carte contenu gen =
    let (new_contenu,_) = generate_monsters time difficulte carte contenu gen
    in
        all (\ (coord,_) -> no_entites_arround coord new_contenu) (M.toList new_contenu)   -- pas d'entites entour 
        && all (\ (coord,_) -> isTraversable carte coord) (M.toList new_contenu)     -- les monstres/joueur ne sont pas généré dans un mur
        && all (\ent -> M.size (M.filter (== ent) new_contenu) == 1)  new_contenu     -- id est unique pour chaque entite
        && let monstres = M.difference new_contenu contenu
           in
               all (\ent -> last_mouvement ent == time) monstres
               && M.size monstres == (M.size new_contenu - 1)
               && all (\ent -> case difficulte of
                                VeryEasy -> isPeacefull ent
                                Easy -> isPeacefull ent || isNeutral ent
                                Mediocre -> isNeutral ent
                                Hard -> isNeutral ent || isAgressive ent
                                VeryHard -> isAgressive ent ) monstres
    where
        no_entites_arround :: Coord -> M.Map Coord Entite -> Bool
        no_entites_arround (C x y) contenu =
            let casesAVerifier = [C x' y' | x' <-[(x-5)..(x+5)], y' <- [(y-5)..(y+5)]]
            in
                find (\coord -> case contenu M.!? coord of
                                Nothing -> False
                                _ -> True) (filter (/= (C x y)) casesAVerifier)
                    == Nothing

generateGameEnvironnement :: Double -> Difficulte -> Carte -> Entite -> IO Environnement
generateGameEnvironnement time difficulte carte joueur = do
    gen <- getStdGen
    let contenu = add_player joueur carte
    let (contenu2,_) = generate_monsters time difficulte carte contenu gen
    return (Envi (fmap (\ent -> [ent]) contenu2))
