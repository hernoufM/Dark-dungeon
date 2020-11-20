module Events where 

import Entite
import Carte
import Data.List
import qualified Data.Set as S

-- la direction du attaque
data DirectionAttack =  At_Nord | At_Sud | At_Ouest | At_Est | At_Central deriving (Eq,Show)

-- evenements permettent de savoir l'evenements se passant dans le jeu. Cette information sert pour le module Graphics.
-- Aussi l'evenements aides au jeu de compter le temps ecoulé depuis la derniere attaque du mob/joueur, notament, pour la gestion de 'cooldown' d'attaque. 
data GameEvent = JoueurAttack { direction :: DirectionAttack, begin_time :: Double}
             | MonsterAttack {monster_id :: Int, direction :: DirectionAttack, begin_time :: Double} 
             | PowerUpEvent {begin_time :: Double } deriving (Show)

instance Eq GameEvent where
    (==) (JoueurAttack dir1 beg_time1) (JoueurAttack dir2 beg_time2) = dir1 == dir2 && beg_time1 == beg_time2
    (==) (MonsterAttack id1 dir1 beg_time1) (MonsterAttack id2 dir2 beg_time2) = dir1 == dir2 && beg_time1 == beg_time2 && id1 == id2
    (==) (PowerUpEvent bt1) (PowerUpEvent bt2) = bt1==bt2
    (==) _ _ = False

-- invariant du GameEvent
-- Le temps quand une requete est produit doit etre positive
-- L'identifiant du monstre pour une MonsterAttack event doit etre strictement positive
game_event_inv :: GameEvent -> Bool
game_event_inv event =
    (begin_time event) >= 0.0
    && case event of
        MonsterAttack id _ _ -> id>0
        _ -> True

mkJoueurAttackEvent :: DirectionAttack -> Double -> GameEvent
mkJoueurAttackEvent = JoueurAttack

mkMonsterAttackEvent :: Int -> DirectionAttack -> Double -> GameEvent
mkMonsterAttackEvent = MonsterAttack

mkPowerUpEvent :: Double -> GameEvent
mkPowerUpEvent = PowerUpEvent

isJoueurAttack :: GameEvent -> Bool
isJoueurAttack (JoueurAttack _ _) = True
isJoueurAttack _ = False

isMonsterAttack :: Entite -> GameEvent -> Bool
isMonsterAttack ent (MonsterAttack id _ _) = (iden ent) == id
isMonsterAttack _ _ = False

-- pre-condition du filterEvents.
-- Le temps courrant doit etre superieure au 0
-- Tous les evenements de liste doivent respecter ces invariants
-- L'entite passé doit etre un joueur
-- L'ensemble des entites specifié doit contenir que des monstres
-- Pour chaque joueur/monstre il y a au plus une attack event 
filterEvents_pre :: Double -> [GameEvent] -> Entite -> S.Set Entite -> Bool
filterEvents_pre time events joueur monstres =
    time >= 0.0
    && all game_event_inv events
    && all (\evt -> time >= (begin_time evt)) events
    && isJoueur joueur
    && length (filter isJoueurAttack events) <= 1 
    && all (\monstre -> length (filter (isMonsterAttack monstre) events) <= 1) monstres   -- pour une monstre une event à la fois
    && all (not.isJoueur) monstres

-- fonction permetant de filtrer la liste des evenements et ne garder que recent.
-- Chaque evenement a le durée pendant laquelle il peut rester dans la liste des events
-- Pour PowerUp c'est 2 seconde (pour pouvoir afficher PowerUp dans l'interface graphique du jeu)
-- Pout une evenement d'attack du joueur/monstre c'est coolDown de ce joueur/monstre (pour interdire à ce entite d'attaquer avant que son cooldown soit ecoulé) 
filterEvents :: Double -> [GameEvent] -> Entite -> S.Set Entite -> [GameEvent]
filterEvents time events (Joueur _ _ _ (Stat _ attSpeed _)) monstres =
    filter (\event -> case event of 
                        JoueurAttack _ begin_time -> begin_time + attSpeed >= time 
                        MonsterAttack id _ begin_time -> case find (\monstre -> iden monstre == id) (S.toList monstres) of
                                                            Nothing -> False    -- si le monstre est mort on supprime son event
                                                            Just monstre -> begin_time + (attackCoolDown (stat monstre)) >= time
                        PowerUpEvent begin_time -> begin_time + 2.0 >= time) events

-- post-condition du filterEvents.
-- Tous les evenements de liste doivent respecter ces invariants
-- Pour chaque joueur/monstre il y a au plus une attack event 
-- Il n'y a pas des evenements des monstres morts
-- Il n'y a pas des evenements anciens (ceux dont le durée est ecoulé)
filterEvents_post :: Double -> [GameEvent] -> Entite -> S.Set Entite -> Bool
filterEvents_post time events joueur monstres =
    let new_events = filterEvents time events joueur monstres
    in
        all game_event_inv new_events
        && all (\evt -> time >= (begin_time evt)) new_events
        && length (filter isJoueurAttack new_events) <= 1 
        && all (\monstre -> length (filter (isMonsterAttack monstre) new_events) <= 1) monstres
        && all (\evt -> case evt of
                            JoueurAttack _ begin_time -> begin_time + (attackCoolDown (stat joueur)) >= time
                            MonsterAttack id _ begin_time -> case find (\monstre -> iden monstre == id) (S.toList monstres) of
                                                                Nothing -> False 
                                                                Just monstre -> begin_time + (attackCoolDown (stat monstre)) >= time 
                            PowerUpEvent begin_time -> begin_time + 2.0 >= time) new_events     -- on a bien filtré tous les evenements qui sont ancien ou evenements des monstres morts
