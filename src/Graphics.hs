module Graphics where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.List
import Foreign.C.Types (CInt)
import SDL
import TextureMap 
import Etat 
import Modele
import Entite
import Carte
import Environnement
import qualified SDL.Image as IMG
import Events

-- creation de la zone rectangulaire à partir du 4 entier (x,y, width, height) 
mkArea :: CInt -> CInt -> CInt -> CInt -> Rectangle CInt
mkArea x y w h = Rectangle (P (V2 x y)) (V2 w h)

-- fonction auxilliere qui aide à structurer les données du modele pour l'affichage sur fenetre.
-- fonctions prend une carte, un environnement et un coordonnees (x).
-- et elle renvoie la matrice 9x9 dont :
--     1) une element du matrice est soit Nothing, soit Just (case, []), soit Just (case, entites)
--     2) la matrice correspond au entourrage du 'x' du 4 cases de chaque coté
--     3) la contentu d'element du matrice avec l'indice (4,4) correspond au contenu du 'x' sur la carte et dans environnement
--        donc par example si coord x vaut (C 10 20)
--        le premiere element du matrice (point plus haut gauche) vas contenir le contenu du coord (C 6 16),
--        le 10ieme element du matrice (point plus haut droite) vas contenir contenu du coord (C 14 16),
--        le 111ieme element du matrice (point plus bas gauche) vas contenir contenu du coord (C 6 24),
--        le 121ieme element du matrice (point plus bas droite) vas contenir contenu du coord (C 14 24)
--     4) si element du matrice vaut Nothing alors il n'existe pas une coordonnes correspondant sur la carte
--        si _______________________ Just case [] alors il existe une case sur la carte avec ce coord mais
--              il n'exitse aucun entite dans environnement assosié à cette case
--        sinon dans le cas du Just (case,entites), il existe 'entites' - sont la liste des entites 
--        associé à cette case dans environnement. 
cases_monsters_arround :: Entite -> Carte -> Environnement -> Coord -> [[Maybe (Case, [Entite])]]
cases_monsters_arround (Joueur _ _ _ (Stat _ _ vision))carte env (C x y) =
    let coords_to_check = [(C x' y') | y' <-[(y-4) .. (y+4)], x'<-[(x-4) .. (x+4)]] -- la liste de tous les coordonnees decris en dessus
    in 
       let vision_coords = [(C x' y') | y' <-[(y-vision) .. (y+vision)], x'<-[(x-vision) .. (x+vision)]]
       in let liste = fmap (\coord -> if elem coord vision_coords 
                                      then coord_info carte env coord
                                      else Nothing) coords_to_check  -- pour chaque coord on recupere toute l'info qui lui est assosié (type du case, entites sur cette case) 
       in groupListe liste -- on transforme notre liste de 81 elements en matrice 9x9 (liste de taille 9 qui contient les listes de taille 9)
    where
        -- fonction auxilliere pour tirer l'information qui est lié à une coordonnee
        coord_info :: Carte -> Environnement -> Coord -> Maybe (Case, [Entite])
        coord_info (Carte _ _ cases) (Envi content) coord =
            case cases M.!? coord of
                Nothing -> Nothing
                Just caase -> case content M.!? coord of
                                Nothing ->  Just (caase, [])
                                Just xs -> Just (caase, xs) 
        -- fonction auxilliere pour transformer la liste en matrice
        groupListe :: [a] -> [[a]]
        groupListe [] = []
        groupListe l = 
            let (l1,l2) = splitAt 9 l
            in (l1:(groupListe l2)) 

-- fonction qui initialise textureMap pour le jeu avec tous les textures requises.
initTexturesMap :: Renderer -> IO TextureMap
initTexturesMap rdr = do
    tmap1 <- loadTexture rdr "asserts/Void.png" (TextureId "void") createTextureMap
    tmap2 <- loadTexture rdr "asserts/Floor.png" (TextureId "floor") tmap1
    tmap3 <- loadTexture rdr "asserts/Wall.png" (TextureId "wall") tmap2
    tmap4 <- loadTexture rdr "asserts/Door_NS_Opened.png" (TextureId "door_ns_opened") tmap3
    tmap5 <- loadTexture rdr "asserts/Door_NS.png" (TextureId "door_ns_closed") tmap4
    tmap6 <- loadTexture rdr "asserts/Door_WE.png" (TextureId "door_we_closed") tmap5
    tmap7 <- loadTexture rdr "asserts/Door_WE_Opened.png" (TextureId "door_we_opened") tmap6
    tmap8 <- loadTexture rdr "asserts/Trapdoor.png" (TextureId "exit") tmap7
    tmap9 <- loadTexture rdr "asserts/Enter.png" (TextureId "enter") tmap8
    tmap10 <- loadTexture rdr "asserts/Hero_Front(1).png" (TextureId "player_front") tmap9
    tmap11 <- loadTexture rdr "asserts/Hero_Back(1).png" (TextureId "player_back") tmap10
    tmap12 <- loadTexture rdr "asserts/Hero_Left(1).png" (TextureId "player_left") tmap11
    tmap13 <- loadTexture rdr "asserts/Hero_Right(1).png" (TextureId "player_right") tmap12
    tmap14 <- loadTexture rdr "asserts/Pink_Slime(1).png" (TextureId "pink_slime") tmap13
    tmap15 <- loadTexture rdr "asserts/waiting_background.png" (TextureId "waiting") tmap14
    tmap16 <- loadTexture rdr "asserts/HP/0HP.png" (TextureId "barre_vie_0/10") tmap15
    tmap17 <- loadTexture rdr "asserts/HP/1HP.png" (TextureId "barre_vie_1/10") tmap16
    tmap18 <- loadTexture rdr "asserts/HP/2HP.png" (TextureId "barre_vie_2/10") tmap17
    tmap19 <- loadTexture rdr "asserts/HP/3HP.png" (TextureId "barre_vie_3/10") tmap18
    tmap20 <- loadTexture rdr "asserts/HP/4HP.png" (TextureId "barre_vie_4/10") tmap19
    tmap21 <- loadTexture rdr "asserts/HP/5HP.png" (TextureId "barre_vie_5/10") tmap20
    tmap22 <- loadTexture rdr "asserts/HP/6HP.png" (TextureId "barre_vie_6/10") tmap21
    tmap23 <- loadTexture rdr "asserts/HP/7HP.png" (TextureId "barre_vie_7/10") tmap22
    tmap24 <- loadTexture rdr "asserts/HP/8HP.png" (TextureId "barre_vie_8/10") tmap23
    tmap25 <- loadTexture rdr "asserts/HP/9HP.png" (TextureId "barre_vie_9/10") tmap24
    tmap26 <- loadTexture rdr "asserts/HP/FULL_HP.png" (TextureId "barre_vie_10/10") tmap25
    tmap27 <- loadTexture rdr "asserts/VIS/1VISION.png" (TextureId "barre_vision_1/4") tmap26
    tmap28 <- loadTexture rdr "asserts/VIS/2VISION.png" (TextureId "barre_vision_2/4") tmap27
    tmap29 <- loadTexture rdr "asserts/VIS/3VISION.png" (TextureId "barre_vision_3/4") tmap28
    tmap30 <- loadTexture rdr "asserts/VIS/FULL_VISION.png" (TextureId "barre_vision_4/4") tmap29
    tmap31 <- loadTexture rdr "asserts/DMG/0DMG.png" (TextureId "barre_damage_0/4") tmap30
    tmap32 <- loadTexture rdr "asserts/DMG/1DMG.png" (TextureId "barre_damage_1/4") tmap31
    tmap33 <- loadTexture rdr "asserts/DMG/2DMG.png" (TextureId "barre_damage_2/4") tmap32
    tmap34 <- loadTexture rdr "asserts/DMG/3DMG.png" (TextureId "barre_damage_3/4") tmap33
    tmap35 <- loadTexture rdr "asserts/DMG/FULL_DMG.png" (TextureId "barre_damage_4/4") tmap34
    tmap36 <- loadTexture rdr "asserts/ASPD/0ASPD.png" (TextureId "barre_attSpeed_0/4") tmap35
    tmap37 <- loadTexture rdr "asserts/ASPD/1ASPD.png" (TextureId "barre_attSpeed_1/4") tmap36
    tmap38 <- loadTexture rdr "asserts/ASPD/2ASPD.png" (TextureId "barre_attSpeed_2/4") tmap37
    tmap39 <- loadTexture rdr "asserts/ASPD/3ASPD.png" (TextureId "barre_attSpeed_3/4") tmap38
    tmap40 <- loadTexture rdr "asserts/ASPD/FULL_ASPD.png" (TextureId "barre_attSpeed_4/4") tmap39
    tmap41 <- loadTexture rdr "asserts/Attack/AttackBack.png" (TextureId "attack_up") tmap40
    tmap42 <- loadTexture rdr "asserts/Attack/AttackFront.png" (TextureId "attack_down") tmap41
    tmap43 <- loadTexture rdr "asserts/Attack/AttackLeft.png" (TextureId "attack_left") tmap42
    tmap44 <- loadTexture rdr "asserts/Attack/AttackRight.png" (TextureId "attack_right") tmap43
    tmap45 <- loadTexture rdr "asserts/POWERUP.png" (TextureId "power_up") tmap44
    tmap46 <- loadTexture rdr "asserts/Green_Slime(1).png" (TextureId "green_slime") tmap45
    tmap47 <- loadTexture rdr "asserts/Red_Slime(1).png" (TextureId "red_slime") tmap46
    tmap48 <- loadTexture rdr "asserts/RAT.png" (TextureId "rat") tmap47
    tmap49 <- loadTexture rdr "asserts/Worm.png" (TextureId "worm") tmap48
    tmap50 <- loadTexture rdr "asserts/BlueWorm.png" (TextureId "blue_worm") tmap49
    tmap51 <- loadTexture rdr "asserts/GAME OVER.png" (TextureId "game_over") tmap50
    tmap52 <- loadTexture rdr "asserts/CUP.png" (TextureId "cup") tmap51
    tmap53 <- loadTexture rdr "asserts/Victory.png" (TextureId "victory") tmap52
    tmap54 <- loadTexture rdr "asserts/Spider.png" (TextureId "spider") tmap53
    return tmap54

-- fonction central d'affichage sur fenetre.
-- elle recupere posiition du joueur, et fais appelle au fonction
-- 'cases_monsters_arround' pour avoir entourage du joueur qu'on vas afficher sur la fenetre,
-- avec la taille 630 x 630 pixels (on obtient 70x70 pixels pour chaque case) (cf ligne 17).
-- 'cases_monsters_arround' retourne une matrice que on parcours pour afficher sur fenetre case par case.
-- fonction regarde lastKeyCode du modele courrante pour savoir dans quelle 
-- direction afficher joueur.
draw :: Renderer -> TextureMap -> Etat -> IO ()
draw rdr tmap etat = do
    let env = envi (modele etat)
    let j = joueur etat
    let c = carte (modele etat)
    let evts = events (modele etat)
    let Just coord = get_pos_joueur env  -- recupere position du joueur
    draw_aux rdr tmap etat 0 0 (cases_monsters_arround j c env coord)
    draw_events rdr tmap env evts
    draw_statistique rdr tmap etat j -- desiigner la statistique du joueur
    where
        -- fonction auxilliere qui parcours matrice element par element et les affiche sur la
        -- bonne partie du fenetre
        draw_aux :: Renderer -> TextureMap -> Etat -> CInt -> CInt -> [[Maybe (Case, [Entite])]] -> IO ()
        draw_aux _ _ _ x y [] = return ()
        draw_aux rdr tmap etat x y ([]:xs) = draw_aux rdr tmap etat 0 (y+70) xs
        -- l'affichage su case vide (en dehors du carte)
        draw_aux rdr tmap etat x y ((Nothing:xs):xss) = 
            let txt = fetchTexture (TextureId "void") tmap
            in 
                do 
                    copy rdr txt Nothing (Just (mkArea x y 70 70))  -- affichage du case
                    draw_aux rdr tmap etat (x+70) y (xs:xss)
        -- l'affichage de cases sur la carte avec ses entites
        draw_aux rdr tmap etat x y (((Just (caase,entites)):xs):xss) =
            let txt = case caase of
                        Mur -> fetchTexture (TextureId "wall") tmap
                        Carte.Normal -> fetchTexture (TextureId "floor") tmap
                        Entree -> fetchTexture (TextureId "enter") tmap
                        Sortie -> if (level etat) == 5 
                                  then fetchTexture (TextureId "cup") tmap
                                  else fetchTexture (TextureId "exit") tmap
                        Porte NS Fermee -> fetchTexture (TextureId "door_ns_closed") tmap
                        Porte NS Ouverte -> fetchTexture (TextureId "door_ns_opened") tmap
                        Porte EO Fermee -> fetchTexture (TextureId "door_we_closed") tmap
                        Porte EO Ouverte -> fetchTexture (TextureId "door_we_opened") tmap
            in 
                do
                    copy rdr txt Nothing (Just (mkArea x y 70 70)) -- affichage du case
                    draw_entites rdr tmap etat x y entites -- affichage des entites
                    draw_aux rdr tmap etat (x+70) y (xs:xss)
                where
                    -- fonction auxilliere permetant afficher les entites (et joueur aussi) sur la case specifié.
                    draw_entites :: Renderer -> TextureMap -> Etat -> CInt -> CInt -> [Entite] -> IO ()
                    draw_entites _ _ _ _ _ [] = return ()
                    draw_entites rdr tmap etat x y (ent:xs) = do
                        let env = envi (modele etat)
                        let lkc = lastKeyCode (modele etat)
                        let evenements = events (modele etat)
                        let txt = if isJoueur ent 
                                  then case lkc of -- on verifie lastKeyCode du modele pour afficher joueur dans bonne direction
                                            KeycodeZ -> fetchTexture (TextureId "player_back") tmap 
                                            KeycodeD -> fetchTexture (TextureId "player_right") tmap 
                                            KeycodeS -> fetchTexture (TextureId "player_front") tmap 
                                            KeycodeQ -> fetchTexture (TextureId "player_left") tmap 
                                            KeycodeUp -> fetchTexture (TextureId "player_back") tmap 
                                            KeycodeRight -> fetchTexture (TextureId "player_right") tmap 
                                            KeycodeDown -> fetchTexture (TextureId "player_front") tmap 
                                            KeycodeLeft -> fetchTexture (TextureId "player_left") tmap
                                  else fetchTexture (TextureId (nom ent)) tmap
                        courrent_time <- time
                        if isJoueur ent 
                        then 
                            if find (\evenement -> (isJoueurAttack evenement) && ((begin_time evenement) + 0.1 >= courrent_time)) evenements == Nothing -- to modify
                            then do
                                copy rdr txt Nothing (Just (mkArea x y 70 70)) -- affichage d'entite
                                draw_entites rdr tmap etat x y xs
                            else draw_entites rdr tmap etat x y xs
                        else 
                            if find (\evenement -> (isMonsterAttack ent evenement) && ((begin_time evenement) + 0.1 >= courrent_time)) evenements == Nothing
                            then do
                                copy rdr txt Nothing (Just (mkArea x y 70 70)) -- affichage d'entite
                                draw_entites rdr tmap etat x y xs
                            else draw_entites rdr tmap etat x y xs

draw_waiting_background :: Renderer -> TextureMap -> IO ()
draw_waiting_background rdr tmap = do
    copy rdr (fetchTexture (TextureId "waiting") tmap) Nothing (Just (mkArea 0 0 630 630))

draw_statistique :: Renderer -> TextureMap -> Etat -> Entite -> IO ()
draw_statistique rdr tmap etat (Joueur _ pvTotal pv (Stat dmg attSpeed vis)) = do
    let barreVie = let x = (fromIntegral pv)/(fromInteger pvTotal) 
                   in
                        if isPerdu etat
                        then "barre_vie_0/10"
                        else if x <= 0.1
                        then "barre_vie_1/10"
                        else if x <= 0.2
                        then "barre_vie_2/10"
                        else if x <= 0.3
                        then "barre_vie_3/10"
                        else if x <= 0.4
                        then "barre_vie_4/10"
                        else if x <= 0.5
                        then "barre_vie_5/10"
                        else if x <= 0.6
                        then "barre_vie_6/10"
                        else if x <= 0.7
                        then "barre_vie_7/10"
                        else if x <= 0.8
                        then "barre_vie_8/10" 
                        else if x <= 0.9
                        then "barre_vie_9/10"               
                        else "barre_vie_10/10"
    let barreVision = case vis of
                        1 -> "barre_vision_1/4"
                        2 -> "barre_vision_2/4"
                        3 -> "barre_vision_3/4"
                        4 -> "barre_vision_4/4"         
    let barreDamage = case dmg of
                        10 -> "barre_damage_0/4"
                        20 -> "barre_damage_1/4"
                        30 -> "barre_damage_2/4"
                        40 -> "barre_damage_3/4"
                        50 -> "barre_damage_4/4"
    let barreAttackSpeed = case attSpeed of
                            2.0 -> "barre_attSpeed_0/4"
                            1.5 -> "barre_attSpeed_1/4"
                            1.1 -> "barre_attSpeed_2/4"
                            0.8 -> "barre_attSpeed_3/4"
                            0.5 -> "barre_attSpeed_4/4"
    copy rdr (fetchTexture (TextureId barreVie) tmap) Nothing (Just (mkArea 563 20 64 20))
    copy rdr (fetchTexture (TextureId barreVision) tmap) Nothing (Just (mkArea 563 90 64 20))
    copy rdr (fetchTexture (TextureId barreDamage) tmap) Nothing (Just (mkArea 563 160 64 20))
    copy rdr (fetchTexture (TextureId barreAttackSpeed) tmap) Nothing (Just (mkArea 563 230 64 20))

draw_events :: Renderer -> TextureMap -> Environnement -> [GameEvent]-> IO ()
draw_events _ _ _ [] = return ()
draw_events rdr tmap env ((JoueurAttack dir time_event):events) = do 
    courrent_time <- time
    if time_event + 0.1 < courrent_time
    then draw_events rdr tmap env events
    else do   
        case dir of
            At_Nord -> copy rdr (fetchTexture (TextureId "attack_up") tmap) Nothing (Just (mkArea 280 210 70 140))
            At_Ouest -> copy rdr (fetchTexture (TextureId "attack_left") tmap) Nothing (Just (mkArea 210 280 140 70))
            At_Sud -> copy rdr (fetchTexture (TextureId "attack_down") tmap) Nothing (Just (mkArea 280 280 70 140))
            At_Est -> copy rdr (fetchTexture (TextureId "attack_right") tmap) Nothing (Just (mkArea 280 280 140 70))
        draw_events rdr tmap env events
draw_events rdr tmap  env ((PowerUpEvent _):events) = do
    copy rdr (fetchTexture (TextureId "power_up") tmap) Nothing (Just (mkArea 210 0 210 70))
    draw_events rdr tmap env events
draw_events rdr tmap env ((MonsterAttack id dir time_event):events) = do
    courrent_time <- time
    if time_event + 0.1 < courrent_time
    then draw_events rdr tmap env events
    else do
        case trouve_id id env of
            Nothing -> draw_events rdr tmap env events
            Just (_,ent ) -> do 
                copy rdr (fetchTexture (TextureId (nom ent)) tmap) Nothing (Just (mkArea 280 280 70 70))
                draw_events rdr tmap env events

draw_game_over :: Renderer -> TextureMap -> IO ()
draw_game_over rdr tmap = do
    copy rdr (fetchTexture (TextureId "game_over") tmap) Nothing (Just (mkArea 0 0 630 630))

draw_victory :: Renderer -> TextureMap -> IO ()
draw_victory rdr tmap = do
    copy rdr (fetchTexture (TextureId "victory") tmap) Nothing (Just (mkArea 0 0 630 630)) 