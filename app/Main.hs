{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay)
import System.IO
import qualified Data.Map.Strict as M 
import qualified Data.Set as S 
import Carte
import Entite
import Modele
import Etat
import Keyboard (Keyboard)
import qualified Keyboard as K
import SDL
import Environnement
import System.Random
import TextureMap
import Graphics 
import Generators

-- fonction qui recupere la carte à partir d'un fichier
carteFromFile :: FilePath -> IO Carte
carteFromFile file = do 
    fd <- openFile file ReadMode
    str <- hGetContents fd
    return (read str :: Carte)

-- fonction qui stocke la carte dans un fichier
carteToFile :: Carte -> FilePath -> IO ()
carteToFile carte file = do 
    writeFile file (show carte)
    return ()

-- Etat initiale du jeu.
initLevel :: IO Etat
initLevel = do
    carte <- genGameCarte   -- on genere la carte
    --let env = Envi (M.fromList [(C 1 8, [Joueur 0 100]), (C 2 1, [Slime 1 20]), (C 6 7, [Slime 2 20])])  -- on construit notre environnement à partir du 2 Slimes et 1 joueur 
    courrent_time <- time
    env <- generateGameEnvironnement courrent_time VeryEasy carte joueur_init
    gen <- getStdGen    -- on recupere la genreateur standart pour l'encapsuler dans etat
    let (joueur, monstres) = info_env env
    return (Tour 1 (Cont carte env gen K.createKeyboard KeycodeS []) joueur monstres)

new_level :: Etat -> IO Etat
new_level etat = do
    carte <- genGameCarte
    courrent_time <- time
    let difficulte = case (level etat) of
                        1 -> Easy
                        2 -> Mediocre
                        3 -> Hard
                        4 -> VeryHard
    env <- generateGameEnvironnement courrent_time difficulte carte (joueur etat)
    let (joueur, monstres) = info_env env
    return $ Tour ((level etat)+1) (Cont carte env (genEtat etat) K.createKeyboard KeycodeS []) joueur monstres

-- le boucle principale du jeu. Se tourne tant que on ne tombe pas sur situation gagnant ou perdant. 
gameLoop :: Double -> Renderer -> TextureMap -> Keyboard -> Etat -> IO ()
gameLoop frameRate renderer tmap kbd etat = do
    startTime <- time
    events <- pollEvents
    let kbd' = K.handleEvents events kbd   -- handle les events du clavier
    clear renderer      -- effacer le fenetre
    draw renderer tmap etat -- dessigner jeu
    present renderer   -- faire apparaitre le jeu sur fenetre
    endTime <- time
    let refreshTime = endTime - startTime
    let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
    --threadDelay $ delayTime * 1000   
    let etat' = etat_tour endTime etat kbd' -- passage au nouvelle etat du jeu
    case etat' of
        Gagne level _ _-> 
                         if level == 5
                         then do
                             clear renderer
                             draw_victory renderer tmap --afficher victoire
                             present renderer
                             threadDelay 4000000
                             return ()
                         else do
                            clear renderer
                            draw_waiting_background renderer tmap
                            present renderer
                            new_etat <- new_level etat'
                            gameLoop frameRate renderer tmap K.createKeyboard new_etat
        Perdu _ _ _ _ -> do
            clear renderer      -- effacer le fenetre
            draw renderer tmap etat' -- dessigner jeu
            present renderer
            threadDelay 1000000
            clear renderer
            draw_game_over renderer tmap
            present renderer -- afficher game over
            threadDelay 4000000
            return ()
        _ -> gameLoop frameRate renderer tmap K.createKeyboard etat'   -- on vide le keyboard et on passe au prochaine tour du jeu

main :: IO ()
main = do
    initializeAll   
    window <- createWindow "Dungeon Crawling" $ defaultWindow { windowInitialSize = V2 630 630 }    
    renderer <- createRenderer window (-1) defaultRenderer
    tmap <- initTexturesMap renderer    -- initialisation de tous les textures dissponibles
    draw_waiting_background renderer tmap
    present renderer 
    etat <- initLevel        -- creation d'etat initial
    let kbd = K.createKeyboard      -- initialisation du clavier
    gameLoop 60 renderer tmap kbd etat    -- passage au boucle du jeu
