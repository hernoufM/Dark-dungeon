module PowerUp where 

import System.Random
import Entite

-- Les PowerUps que joueur peut obtenir en tuant monstre. 
data PowerUp = Hp | Hp_Total | Vision | Dmg | SpeedAtack | No

-- la probabilité d'obtenir un powerUp apres avoir tué un monstre.
-- 60% pas de powerUp
-- 20% - +50 points de vie
-- 11% - +50 points de vie total (le nombre maximal des points de vie que joueur peut avoir au meme temps)
-- 11% - +10 damage
-- 11% - la vitesse d'ataque augmente
-- 7% - le joueur peut voir une case en plus (brouillard de gere recule sur 1 case)
powerUp_prob :: [(Int,PowerUp)]
powerUp_prob = [(60, No),(20, Hp), (11, Hp_Total), (11, Dmg), (11, SpeedAtack), (7, Vision)]

-- fonction qui renvoie un powerUp aleatoire en fonction de probabilité
random_powerUp :: StdGen -> (PowerUp,StdGen)
random_powerUp gen =
    let (nombre, new_gen) = randomR (1, 100) gen 
    in
        let res = foldr (\ (p,pU) (pacc, pUacc) -> 
                                if (p+pacc >= nombre) && (pacc < nombre) 
                                then (p+pacc, pU)
                                else (p+pacc, pUacc))
                            (0, No) powerUp_prob
        in (snd res, new_gen)

-- pre-condition du apply_powerUp.
-- Entité specifié doit respecter son invariant
-- En plus ça doit etre un joueur
apply_powerUp_pre :: Entite -> StdGen -> Bool
apply_powerUp_pre ent _ =
    entite_inv ent
    && isJoueur ent

-- fonction qui est appelé chaque fois quand joueur tue un monstre.
-- Elle prends un joueur et lui applique un powerUp, s'il est généré, et renovie joueur avec nouvelle caracteristiques.
-- Si powerUp n'etait pas généré elle renvoi Nothing  
apply_powerUp :: Entite -> StdGen -> (Maybe Entite,StdGen)
apply_powerUp joueur@(Joueur _ pvT pv stat@(Stat dmg speedAt vis)) gen =
    let (powerUp,new_gen) = random_powerUp gen
    in
        let new_joueur = case powerUp of
                            Hp -> if pvT == pv 
                                  then Nothing
                                  else Just $ joueur {pvie = min (pv +50) pvT }
                            Hp_Total -> Just $ joueur {pvieTotal = pvT +50 }
                            Vision -> if vis == 4
                                      then Nothing
                                      else Just $ joueur { stat = stat { vision = vis +1 } }
                            Dmg -> if dmg == 50
                                   then Nothing
                                   else Just $ joueur { stat = stat { damage = dmg+10} }
                            SpeedAtack -> case speedAt of
                                            2.0 -> Just $ joueur { stat = stat { attackCoolDown = 1.5} }
                                            1.5 -> Just $ joueur { stat = stat { attackCoolDown = 1.1} }
                                            1.1 -> Just $ joueur { stat = stat { attackCoolDown = 0.8} }
                                            0.8 -> Just $ joueur { stat = stat { attackCoolDown = 0.5} }
                                            0.5 -> Nothing
                            No -> Nothing 
        in (new_joueur, new_gen)

-- post-condition du apply_powerUp.
-- Si fonction a renvoiyé entite alors une de ces caracteristiques ont été augmentés
apply_powerUp_post :: Entite -> StdGen -> Bool
apply_powerUp_post ent@(Joueur _ pvT pv (Stat dmg speedAt vis)) gen =
    let (res,_) = apply_powerUp ent gen
    in
        case res of
            Nothing -> True
            Just new_entite -> (pvie new_entite) == min (pv + 50) pvT  -- HP a était appliqué
                               || (pvieTotal new_entite) == pvT + 50    -- HP_Total a était appliqué
                               || (vision (stat new_entite)) == vis + 1
                               || (damage (stat new_entite)) == dmg + 10
                               || (attackCoolDown (stat new_entite)) == case speedAt of
                                                                            2.0 -> 1.5
                                                                            1.5 -> 1.1
                                                                            1.1 -> 0.8
                                                                            0.8 -> 0.5