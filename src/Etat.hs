module Etat where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Modele 
import Environnement
import Entite
import Keyboard 
import Events
import System.Random
{- *************** Etat ****************** -}

-- etat du jeu est soit perdu soit gagne soit en tour
data Etat = Perdu {
                     level :: Int,   -- numero du niveau du perte
                     modele :: Modele,  -- modele du jeu au moment du perte
                     joueur :: Entite,  -- etc
                     monstres :: S.Set Entite  -- etc
                  }-- etat perdu
            | Gagne {level::Int, genEtat :: StdGen, joueur :: Entite} -- etat gagne du niveau
            | Tour {    -- tour du jeu, qui est composé du
                     level :: Int,   -- numero du niveau
                     modele :: Modele,  -- modele courrante du jeu
                     joueur :: Entite, -- joueur
                     monstres :: S.Set Entite -- les mobs
                    } deriving (Show)

-- propriete 1
-- tous les monstres et joueur dans champs 'monstres' et 'joueur' existent dans l'environnemment
-- et l'inverse (tous les entites dans environnement sont present dans les champs 'joueur' et 'monstres')
monstres_et_joueur_coherence_prop :: Etat -> Bool
monstres_et_joueur_coherence_prop (Gagne _ _ _) = True
monstres_et_joueur_coherence_prop etat =
    let env = envi (modele etat)
        j = joueur etat
        mtrs = monstres etat
    in
    env_fit_etat env (S.insert j mtrs)    -- environnement à jour avec etat
    && etat_fit_env (S.insert j mtrs) env    -- etat à jour avec environnement
    where 
        -- tous les entites qui sont presentent dans un environnement sont aussi presentent dans les champs
        -- 'joueur' et 'monstres' d'etat
        env_fit_etat:: Environnement -> S.Set Entite -> Bool
        env_fit_etat (Envi content) entites =
            all (\ env_entites -> all (\ ent -> elem ent entites) env_entites) (M.elems content)
        -- tous les entites qui sont presentent dans les champs 'joueur' et 'monstres' d'etat 
        -- sont aussi presentent dans environnement
        etat_fit_env:: S.Set Entite -> Environnement -> Bool 
        etat_fit_env entites env =
            all (\ ent -> trouve_id (iden ent) env /= Nothing) entites

-- invariant d'etat.
etat_inv :: Etat -> Bool
etat_inv (Gagne lev _ j) = lev >=1 && entite_inv j && isJoueur j
etat_inv etat =
    ((level etat) >= 1)  -- le numero du niveau doit etre positive
    && isJoueur (joueur etat)    -- le champs 'joueur' doit contenir entite joueur
    && entite_inv (joueur etat)    -- joueur respecte son invariant
    && modele_inv (modele etat)     -- modele d'etat doit respecter son invariant
    && all entite_inv (monstres etat)   -- tous les monstres doivent respecter ses invariants
    && all (not.isJoueur) (monstres etat)  -- tous les entites du champ 'monstre' doivent etre NPC (pas de joueur)
    && monstres_et_joueur_coherence_prop etat  -- doit respecter propriete decrit en dessus


-- pre-condition sur etat_tour
-- etat specifié doit respecter son invariant
etat_tour_pre ::  Double -> Etat -> Keyboard -> Bool
etat_tour_pre time etat@(Tour _ _ _ _) kbd =
    time >= 0
    && etat_inv etat
etat_tour_pre _ _ _ = False  

-- fonction central du jeu. Elle prends le temps (relative) actuelle,
-- elle prends etat actuelle, l'etat du clavier actuelle. Et elle calcule
-- nouvelle etat du jeu. La nouvelle etat est obtenu apres avoir calculer la nouvelle modele du jeu en
-- appelant fonction 'tour' sur chaque entite du jeu (tous les entites sont presentent dans les champs 'joueur' et 'monstres')
-- Apres avoir calculer la modele, fonction teste si modele est en situation gagnant pour niveau actuelle.
-- Si une de ces test donne le resultat positive alors fonction retourne etat correspondant (Gagne ou Perdu)
-- Sinon elle construit nouvelle etat 'Tour' avec tous les mises a jours prises en compte.
-- A chaque tour la listes des evenements du modele sont filtré selon le temps actuelle.
etat_tour :: Double -> Etat -> Keyboard -> Etat
etat_tour time (Tour level modele _ _) kbd =
    let (new_joueur, new_monstres) = info_env (envi modele)
    in
        let evenements = filterEvents time (events modele) new_joueur new_monstres  
        in 
            let modele_tmp = tour time (modele {keyboard=kbd, events = evenements}) new_joueur        -- on appele 'tour' sur joueur 
            in 
                let (new_joueur1, new_monstres1) = info_env (envi modele_tmp)
                in 
                    let new_modele = S.foldr (\ ent modele_acc -> tour time modele_acc ent) modele_tmp new_monstres1
                    in
                    if level_finished new_modele      -- teste si on passe au niveu en dessus
                    then Gagne level (gene new_modele) new_joueur1
                    else if perte new_modele    -- teste si modele est en situation perdante
                    then (Perdu level new_modele new_joueur1 new_monstres1)
                    else (Tour level new_modele new_joueur1 new_monstres1)
etat_tour _ x _ = error "should not occur"

-- post-condition sur etat_tour
-- etat specifié doit respecter son invariant
etat_tour_post ::  Double -> Etat -> Keyboard -> Bool
etat_tour_post time etat kbd =
    let new_etat = etat_tour time etat kbd
        (new_joueur, new_monstres) = info_env (envi (modele etat))
    in
        etat_inv new_etat
        &&
        let evenements = filterEvents time (events (modele etat)) new_joueur new_monstres  
        in 
            case new_etat of 
                Gagne lvl _ j -> let modele_obtenu = tour time ((modele etat) {keyboard=kbd, events = evenements}) new_joueur
                                 in level_finished modele_obtenu
                Perdu lvl modele j _ -> perte modele
                Tour level new_modele _ _ -> not (level_finished new_modele) && not (perte new_modele) 

-- predicat que dit si etat est Perdu 
isPerdu :: Etat -> Bool
isPerdu (Perdu _ _ _ _) = True
isPerdu _ = False