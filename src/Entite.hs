module Entite where 

{- *************** Entite ****************** -}

data Entite = -- joueur 
              Joueur {iden :: Int, pvieTotal :: Integer, pvie :: Integer, stat :: Stat}
              -- monstre qui n'attaque pas
              | PeacefullMonstre {iden :: Int, nom :: String, pvie :: Integer, cour_mouvement_coolDown :: Double, last_mouvement :: Double} 
              -- monstre qui n'attaque pas si on ne l'attaque pas
              | AgressiveMonstre {iden :: Int, nom :: String, pvie :: Integer, init_mouvement_coolDown :: Double, cour_mouvement_coolDown :: Double, last_mouvement :: Double, stat :: Stat} 
              -- monstre qui nous attaque des que il nous voit
              | NeutralMonstre {iden :: Int, nom :: String, pvieTotal :: Integer, pvie :: Integer, init_mouvement_coolDown :: Double, cour_mouvement_coolDown :: Double, last_mouvement :: Double, stat :: Stat}
            deriving (Show)

-- statistique d'attaque
data Stat = Stat { damage :: Int, attackCoolDown :: Double,  vision :: Int} deriving (Show)

-- invariant de statistique
-- La statistique ne peut exister que dans contexte du attaque, donc son damage ne peut pas etre <10 (10 - le damage minimal dans jeu)
-- CoolDown apres attaque ne peut pas etre plus rapide que 0.1 seconde.
-- La vision pour joueur signifie le nombre des cases entour qui lui est visible (le reste est affiché comme brouillard du guerre). 
-- Pour un mob c'est le nombre des cases entour que il lui faut pour detecter joueur
stat_inv :: Stat -> Bool
stat_inv (Stat dmg cd vis) =
    (dmg >= 10) && (cd >= 0.1) && (vis >= 1)

-- le joueur au debut du jeu
joueur_init :: Entite
joueur_init = Joueur 0 100 100 (Stat 10 2.0 1)

-- Deux entites sont egaux si leur identifiants sont egaux
instance Eq Entite where
    (==) ent1 ent2 = (iden ent1) == (iden ent2)

-- Ordre entre les entites corresponds au ordre entre ses identifiants  
instance Ord Entite where
    (<=) ent1 ent2 = (iden ent1) <= (iden ent2)

-- predicat qui dit si l'entite est le joueur 
isJoueur :: Entite -> Bool
isJoueur (Joueur _ _ _ _) = True
isJoueur _ = False

-- predicat qui dit si l'entite est un monstre agressive 
isAgressive :: Entite -> Bool
isAgressive (AgressiveMonstre _ _ _ _ _ _ _) = True
isAgressive _ = False

-- predicat qui dit si l'entite est un monstre 'peacefull' 
isPeacefull :: Entite -> Bool
isPeacefull (PeacefullMonstre _ _ _ _ _) = True
isPeacefull _ = False

-- predicat qui dit si l'entite est un monstre neutral 
isNeutral :: Entite -> Bool
isNeutral (NeutralMonstre _ _ _ _ _ _ _ _) = True
isNeutral _ = False

-- invariant d'entite
-- joueur doit avoir un id == 0
entite_inv :: Entite -> Bool
entite_inv (Joueur id pvT pv stat) = id==0 && pvT>0 && pv<=pvT && stat_inv stat 
entite_inv (PeacefullMonstre id _ pv mouv_cd last_mouv) = id > 0 && mouv_cd >= 0.1 && last_mouv >= 0.0
entite_inv (AgressiveMonstre id _ pv init_mouv_cd cour_mouv_cd last_mouv stat) = id > 0 && init_mouv_cd >= 0.1 && cour_mouv_cd >= 0.1 && stat_inv stat 
entite_inv (NeutralMonstre id _ pvT pv init_mouv_cd cour_mouv_cd last_mouv stat) = id > 0 && init_mouv_cd >= 0.1 && pvT>0 && pv<=pvT && cour_mouv_cd >= 0.1 && stat_inv stat 