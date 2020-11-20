module ModeleSpec where--(modeleSpec) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Test.Hspec
import Modele
import Carte
import Entite
import Events
import Keyboard
import Environnement
import System.Random
import SDL.Input.Keyboard.Codes

{- ************** Modele ***************** -}

{- test à faire -}
{- tour d'entite mort -}
{- tour pour chaque mob -}
{- tour d un joueur -}
{- test pour attaque du joueur quand cooldown est ecoulé-}
{- test pour  "    "      "     "      "      n'est pas ecoulé  -}
{- test pour attaque du monstre quand cooldown est ecoulé-}
{- test pour  "    "      "     "      "      n'est pas ecoulé  -}



-- modele correcte avec keyboard vide
modeleCorrecte1 :: Modele
modeleCorrecte1 = 
    --
    -- carte du fichier "carte/start.map":
    -- XXXXXXXXXX
    -- X    | XSX
    -- X    X X-X
    -- XXXX X X X
    -- X    X X X
    -- X XXXX   X
    -- X    XXXXX
    -- XXX    XXX
    -- XE  X  XXX
    -- XXXXXXXXXX
    --
    -- environnement :
    -- (1,8)   --   Joueur(0)
    -- (2,1)   --   PeacefullMonstre(1)
    -- (6,7)   --   AgressiveMonstre(2)
    --
    -- etat clavier : {}
    -- events : []
    let carte = read "XXXXXXXXXX\nX    | XSX\nX    X X-X\nXXXX X X X\nX    X X X\nX XXXX   X\nX    XXXXX\nXXX    XXX\nXE  X  XXX\nXXXXXXXXXX" :: Carte 
        env = Envi (M.fromList [(C 1 8, [joueur_init]), (C 2 1, [PeacefullMonstre 1 "pink_slime" 20 1.0 1.0]), (C 6 7, [AgressiveMonstre 2 "red_slime" 60 1.0 1.0 1.0 (Stat 40 1.0 3)])])
        in 
        Cont carte env (mkStdGen 20) createKeyboard KeycodeD []

-- modele correcte avec keyboard vide
modeleCorrecteEvents :: Modele
modeleCorrecteEvents = 
    --
    -- carte du fichier "carte/start.map":
    -- XXXXXXXXXX
    -- X    | XSX
    -- X    X X-X
    -- XXXX X X X
    -- X    X X X
    -- X XXXX   X
    -- X    XXXXX
    -- XXX    XXX
    -- XE  X  XXX
    -- XXXXXXXXXX
    --
    -- environnement :
    -- (1,8)   --   Joueur(0)
    -- (2,1)   --   PeacefullMonstre(1)
    -- (6,7)   --   AgressiveMonstre(2)
    --
    -- etat clavier : {}
    -- events : [PU-1.0; J-2.0]
    let carte = read "XXXXXXXXXX\nX    | XSX\nX    X X-X\nXXXX X X X\nX    X X X\nX XXXX   X\nX    XXXXX\nXXX    XXX\nXE  X  XXX\nXXXXXXXXXX" :: Carte 
        env = Envi (M.fromList [(C 1 8, [joueur_init]), (C 2 1, [PeacefullMonstre 1 "pink_slime" 20 1.0 1.0]), (C 6 7, [AgressiveMonstre 2 "red_slime" 60 1.0 1.0 1.0 (Stat 40 1.0 3)])])
        in 
        Cont carte env (mkStdGen 20) createKeyboard KeycodeD [PowerUpEvent 1.0, JoueurAttack At_Est 2.0]

-- modele correcte avec keyboard non vide (touche "D" appuyé)
modeleCorrecte2 :: Modele
modeleCorrecte2 = 
    --
    -- carte du fichier "carte/start.map":
    -- XXXXXXXXXX
    -- X    | XSX
    -- X    X X-X
    -- XXXX X X X
    -- X    X X X
    -- X XXXX   X
    -- X    XXXXX
    -- XXX    XXX
    -- XE  X  XXX
    -- XXXXXXXXXX
    --
    -- environnement :
    -- (1,8)   --   Joueur(0)
    -- (6,1)   --   Slime(1)
    -- (1,5)   --   Slime(2)
    --
    -- etat clavier : {}
    -- events : []

    let carte = read "XXXXXXXXXX\nX    | XSX\nX    X X-X\nXXXX X X X\nX    X X X\nX XXXX   X\nX    XXXXX\nXXX    XXX\nXE  X  XXX\nXXXXXXXXXX" :: Carte 
        env = Envi (M.fromList [(C 1 8, [joueur_init]), (C 6 1, [PeacefullMonstre 1 "blue_worm" 60 0.7 1.0]), (C 1 5, [NeutralMonstre 2 "rat" 30 30 0.7 0.7 1.0 (Stat 15 0.7 5)])])
        in 
        Cont carte env (mkStdGen 30) (S.singleton KeycodeD) KeycodeD []

-- modele correcte avec keyboard non vide (touche "Q" appuyé)
modeleCorrecte3 :: Modele
modeleCorrecte3 = 
    --
    -- carte du fichier "carte/start.map":
    -- XXXXXXXXXX
    -- X    | XSX
    -- X    X X-X
    -- XXXX X X X
    -- X    X X X
    -- X XXXX   X
    -- X    XXXXX
    -- XXX    XXX
    -- XE  X  XXX
    -- XXXXXXXXXX
    --
    -- environnement :
    -- (1,8)   --   Joueur(0)
    -- (6,1)   --   Slime(1)
    -- (1,5)   --   Slime(2)
    --
    -- etat clavier : {Q}
    -- event : []
    let carte = read "XXXXXXXXXX\nX    | XSX\nX    X X-X\nXXXX X X X\nX    X X X\nX XXXX   X\nX    XXXXX\nXXX    XXX\nXE  X  XXX\nXXXXXXXXXX" :: Carte 
        env = Envi (M.fromList [(C 1 8, [joueur_init]), (C 6 1, [PeacefullMonstre 1 "worm" 20 1.5 1.0]), (C 1 5, [PeacefullMonstre 2 "blue_worm" 60 0.7 2.0])])
        in 
        Cont carte env (mkStdGen 40) (S.singleton KeycodeQ) KeycodeD []

-- constructeur des modeles libres (capables de ne pas respecter invariant)
mkModele :: Carte -> Environnement -> Keyboard -> [GameEvent] -> Modele 
mkModele carte env kbd events=
    Cont carte env (mkStdGen 30) kbd KeycodeD events

modele_invSpec = do      -- les testes sur les proprietes du modele et sur l'invariant qui verifie ces proprietes en plus
    
    describe "env_fit_carte_prop" $ do         -- testes sur prop1
        it "teste si l'environnement n'est pas en contradiction avec la carte pour modele specifié" $ do
            modeleCorrecte1 `shouldSatisfy` env_fit_carte_prop
        it "sinon prop n'est pas respecté" $ do
            -- carte du modeleFaux:
            -- XXXXX
            -- XE SX
            -- XXXXX
            --
            -- environnement du modeleFaux:
            -- (0,0)   --   Joueur(0)
            --
            -- etat du clavier : {}
            -- events : []
            let modeleFaux = mkModele (read "XXXXX\nXE SX\nXXXXX" :: Carte) (Envi (M.fromList [(C 0 0, [joueur_init])])) createKeyboard []
                in
                    modeleFaux `shouldNotSatisfy` env_fit_carte_prop
    
    describe "evenements_valides_prop" $ do         -- testes sur prop1
        it "teste si l'environnement n'est pas en contradiction avec la carte pour modele specifié" $ do
            modeleCorrecte1 `shouldSatisfy` evenements_valides_prop
            modeleCorrecteEvents `shouldSatisfy` evenements_valides_prop
        it "sinon prop n'est pas respecté" $ do
            -- carte du modeleFaux:
            -- XXXXX
            -- XE SX
            -- XXXXX
            --
            -- environnement du modeleFaux:
            -- (0,0)   --   Joueur(0)
            --
            -- etat du clavier : {}
            -- events : [J-1.0; J-2.0]
            let modeleFaux = mkModele (read "XXXXX\nXE SX\nXXXXX" :: Carte) (Envi (M.fromList [(C 0 0, [joueur_init])])) createKeyboard [JoueurAttack At_Sud 1.0, JoueurAttack At_Est 2.0]
                in
                    modeleFaux `shouldNotSatisfy` evenements_valides_prop

    describe "modele_inv" $ do        -- testes sur invariant du modele
        it "si carte et environnement respecte ses invariants et le proprité est verifié alors invariant du modele est respecté" $ do
            modeleCorrecte1 `shouldSatisfy` modele_inv
            modeleCorrecteEvents `shouldSatisfy` modele_inv

bougeSpec = do
    -- carte du modele:
    -- XXXXX        XX1XX
    -- XE SX        X234X
    -- XXXXX        XXXXX
    --
    -- environnement du modele:
    -- (1,1)   --   Joueur(0)
    -- (2,1)   --   Slime(1)
    --
    -- etat du clavier : {}

    let joueur = joueur_init
    let slime = PeacefullMonstre 1 "pink_slime" 20 1.0 1.0
    let coord1 = C 2 0    -- (1)
    let coord2 = C 1 1    -- (2)
    let coord3 = C 2 1    -- (3)
    let coord4 = C 3 1    -- (4)
    let coordNonExistant = C 20 20
    let modeleEx = mkModele (read "XXXXX\nXE SX\nXXXXX" :: Carte) (Envi (M.fromList [(C 1 1, [joueur]), (C 2 1, [slime])])) createKeyboard []
    describe "bouge" $ do -- batterie de tests sur bouge
        it "Si les pre-conditions sont verifiés" $ do   -- testes sur pre-conditions
            bouge_pre modeleEx joueur coord2 `shouldBe` True
            bouge_pre modeleEx joueur coord3 `shouldBe` True
            bouge_pre modeleEx joueur coordNonExistant `shouldBe` True
            bouge_pre modeleEx slime coord1 `shouldBe` True
            bouge_pre modeleEx slime coord2 `shouldBe` True
            bouge_pre modeleEx slime coord3 `shouldBe` True
            bouge_pre modeleEx slime coord4 `shouldBe` True
            bouge_pre modeleEx slime coordNonExistant `shouldBe` True
        it "Alors les post-conditions sont satisfaits" $ do   -- testes sur post-condition
            bouge_post modeleEx joueur coord2 `shouldBe` True  -- modele ne change pas (coords egaux)
            bouge_post modeleEx joueur coord3 `shouldBe` True  -- modele est changé (joueur -> (3))
            bouge_post modeleEx joueur coordNonExistant `shouldBe` True -- modele ne change pas (coord n'est pas traversable)
            bouge_post modeleEx slime coord1 `shouldBe` True  -- modele ne change pas (coord n'est pas traversable)
            bouge_post modeleEx slime coord2 `shouldBe` True  -- modele ne change pas (coord n'est pas frachissable dans environnement pour slime)
            bouge_post modeleEx slime coord3 `shouldBe` True  -- modele ne change pas (coords egaux) 
            bouge_post modeleEx slime coord4 `shouldBe` True  -- modele ne change pas (coord n'est pas traversable)
            bouge_post modeleEx slime coordNonExistant `shouldBe` True

prevoitSpec = do
    let slime1 = PeacefullMonstre 1 "pink_slime" 20 1.0 1.0
    let slime2 = PeacefullMonstre 2 "pink_slime" 20 1.0 1.0
    describe "prevoit" $ do -- batterie de tests sur prevoit
        it "Si les pre-conditions sont verifiés" $ do   -- testes sur pre-conditions
            prevoit_pre modeleCorrecte1 slime1 `shouldBe` True
            prevoit_pre modeleCorrecte1 slime2 `shouldBe` True
            prevoit_pre modeleCorrecte2 slime1 `shouldBe` True
            prevoit_pre modeleCorrecte2 slime2 `shouldBe` True
        it "Alors les post-conditions sont satisfaits" $ do   -- testes sur post-condition
            prevoit_post modeleCorrecte1 slime1 `shouldBe` True
            prevoit_post modeleCorrecte1 slime2 `shouldBe` True
            prevoit_post modeleCorrecte2 slime1 `shouldBe` True
            prevoit_post modeleCorrecte2 slime2 `shouldBe` True

decideSpec = do
    let slime1 = PeacefullMonstre 1 "pink_slime" 20 1.0 1.0
    let slime2 = PeacefullMonstre 2 "pink_slime" 20 1.0 1.0
    let listOrdres1 = [(1, Rien_faire )]
    let listOrdres2 = [(1, Rien_faire ), (1, Aller_Nord)]
    let listOrdres3 = [(2, Rien_faire ), (1, Aller_Nord ), (1, Aller_Sud)]
    let listOrdres4 = [(3, Rien_faire ), (1, Aller_Nord ), (1, Aller_Sud), (1, Aller_Est)]
    let listOrdres5 = [(4, Rien_faire ), (1, Aller_Nord ), (1, Aller_Sud), (1, Aller_Est), (1, Aller_Ouest)]
    describe "decide" $ do -- batterie de tests sur decide
        it "Si les pre-conditions sont verifiés" $ do   -- testes sur pre-conditions
            decide_pre listOrdres1 modeleCorrecte1 slime1 `shouldBe` True
            decide_pre listOrdres2 modeleCorrecte1 slime1 `shouldBe` True
            decide_pre listOrdres3 modeleCorrecte1 slime1 `shouldBe` True
            decide_pre listOrdres4 modeleCorrecte1 slime1 `shouldBe` True
            decide_pre listOrdres5 modeleCorrecte1 slime1 `shouldBe` True
            decide_pre listOrdres1 modeleCorrecte1 slime2 `shouldBe` True
            decide_pre listOrdres2 modeleCorrecte1 slime2 `shouldBe` True
            decide_pre listOrdres3 modeleCorrecte1 slime2 `shouldBe` True
            decide_pre listOrdres4 modeleCorrecte1 slime2 `shouldBe` True
            decide_pre listOrdres5 modeleCorrecte1 slime2 `shouldBe` True
        it "Alors les post-conditions sont satisfaits" $ do   -- testes sur post-condition
            decide_post listOrdres1 modeleCorrecte1 slime1 `shouldBe` True
            decide_post listOrdres2 modeleCorrecte1 slime1 `shouldBe` True
            decide_post listOrdres3 modeleCorrecte1 slime1 `shouldBe` True
            decide_post listOrdres4 modeleCorrecte1 slime1 `shouldBe` True
            decide_post listOrdres5 modeleCorrecte1 slime1 `shouldBe` True
            decide_post listOrdres1 modeleCorrecte1 slime2 `shouldBe` True
            decide_post listOrdres2 modeleCorrecte1 slime2 `shouldBe` True
            decide_post listOrdres3 modeleCorrecte1 slime2 `shouldBe` True
            decide_post listOrdres4 modeleCorrecte1 slime2 `shouldBe` True
            decide_post listOrdres5 modeleCorrecte1 slime2 `shouldBe` True

{- test à faire -}
{- tour d'entite mort -}
{- tour pour chaque mob -}
{- tour d un joueur -}
{- test pour attaque du joueur quand cooldown est ecoulé-}
{- test pour  "    "      "     "      "      n'est pas ecoulé  -}
{- test pour attaque du monstre quand cooldown est ecoulé-}
{- test pour  "    "      "     "      "      n'est pas ecoulé  -}

modeleJoueurFrappeMur = mkModele (read "XXXXX\nXE SX\nXXXXX" :: Carte) (Envi (M.fromList [(C 1 1, [joueur_init])])) (S.singleton KeycodeLeft) []
modeleJoueurFrappeVide = mkModele (read "XXXXX\nXE SX\nXXXXX" :: Carte) (Envi (M.fromList [(C 1 1, [joueur_init])])) (S.singleton KeycodeRight) []
modeleJoueurFrappeMonstre = mkModele (read "XXXXX\nXE SX\nXXXXX" :: Carte) (Envi (M.fromList [(C 1 1, [joueur_init]),(C 2 1, [PeacefullMonstre 1 "pink_slime" 20 1.0 1.0])])) (S.singleton KeycodeRight) []
modeleJoueurFrappeMonstreCoolDown = mkModele (read "XXXXX\nXE SX\nXXXXX" :: Carte) (Envi (M.fromList [(C 1 1, [joueur_init]),(C 2 1, [PeacefullMonstre 1 "pink_slime" 20 1.0 1.0])])) (S.singleton KeycodeRight) [mkJoueurAttackEvent At_Est 0.0]
modeleMonstreAgressiveFarAway = mkModele (read "XXXXXXXX\nXE    SX\nXXXXXXXX" :: Carte) (Envi (M.fromList [(C 1 1, [joueur_init]),(C 6 1, [AgressiveMonstre 2 "red_slime" 60 1.0 1.0 1.0 (Stat 40 1.0 3)])])) createKeyboard []
modeleMonstreAgressiveArround = mkModele (read "XXXXX\nXE SX\nXXXXX" :: Carte) (Envi (M.fromList [(C 1 1, [joueur_init]),(C 3 1, [AgressiveMonstre 2 "red_slime" 60 1.0 1.0 1.0 (Stat 40 1.0 3)])])) createKeyboard []
modeleMonstreAgressiveFrappe = mkModele (read "XXXXX\nXE SX\nXXXXX" :: Carte) (Envi (M.fromList [(C 1 1, [joueur_init]),(C 2 1, [AgressiveMonstre 2 "red_slime" 60 1.0 1.0 1.0 (Stat 40 1.0 3)])])) createKeyboard []
modeleMonstreAgressiveFrappeCooldown = mkModele (read "XXXXX\nXE SX\nXXXXX" :: Carte) (Envi (M.fromList [(C 1 1, [joueur_init]),(C 2 1, [AgressiveMonstre 2 "red_slime" 60 1.0 1.0 1.0 (Stat 40 1.0 3)])])) createKeyboard [mkMonsterAttackEvent 2 At_Est 0.0]

tourSpec = do
    -- carte du modeleEx:
    -- XXXXX        XX1XX
    -- XE|SX        X234X
    -- XXXXX        XXXXX
    --
    -- environnement du modeleEx:
    -- (1,1)   --   Joueur(0)
    --
    -- etat du clavier : {Space}
    let joueur = joueur_init
    let slime1 = PeacefullMonstre 1 "pink_slime" 20 1.0 1.0
    let slime2 = AgressiveMonstre 2 "red_slime" 60 1.0 1.0 1.0 (Stat 40 1.0 3)
    let modeleEspace = mkModele (read "XXXXX\nXE|SX\nXXXXX" :: Carte) (Envi (M.fromList [(C 1 1, [joueur])])) (S.singleton KeycodeSpace) []
    let modeleEspace = mkModele (read "XXXXX\nXE SX\nXXXXX" :: Carte) (Envi (M.fromList [(C 1 1, [joueur])])) (S.singleton KeycodeD) []
    describe "tour" $ do -- batterie de tests sur tour
        it "Si les pre-conditions sont verifiés" $ do   -- testes sur pre-conditions
            -- tests du deplacements/ouverture des portes pour joueur
            tour_pre 0.0 modeleCorrecte1 joueur `shouldBe` True
            tour_pre 0.0 modeleCorrecte2 joueur `shouldBe` True
            tour_pre 0.0 modeleCorrecte3 joueur `shouldBe` True
            tour_pre 0.0 modeleEspace joueur `shouldBe` True
            tour_pre 0.0 modeleCorrecte1 slime1 `shouldBe` True
            tour_pre 0.0 modeleCorrecte1 slime2 `shouldBe` True
            -- tests de frappe du joueur
            tour_pre 1.0 modeleJoueurFrappeMur joueur `shouldBe` True
            tour_pre 1.0 modeleJoueurFrappeVide joueur `shouldBe` True
            tour_pre 1.0 modeleJoueurFrappeMonstre joueur `shouldBe` True
            tour_pre 1.0 modeleJoueurFrappeMonstreCoolDown joueur `shouldBe` True
            -- tests de mouvement de monstre agressive
            tour_pre 1.0 modeleMonstreAgressiveFarAway slime2 `shouldBe` True
            tour_pre 1.0 modeleMonstreAgressiveArround slime2 `shouldBe` True
            --test du frappe du monstre agressive
            tour_pre 1.0 modeleMonstreAgressiveFrappe slime2 `shouldBe` True
            tour_pre 1.0 modeleMonstreAgressiveFrappeCooldown slime2 `shouldBe` True
        it "Alors les post-conditions sont satisfaits" $ do   -- testes sur post-condition
            -- tests du deplacements/ouverture des portes pour joueur
            tour_post 0.0 modeleCorrecte1 joueur `shouldBe` True    -- modele ne change pas (keyboard vide)
            tour_post 0.0 modeleCorrecte2 joueur `shouldBe` True    -- modele est changé, joueur s'est deplacé vers (2,8)
            tour_post 0.0 modeleCorrecte3 joueur `shouldBe` True    -- modele ne change pas (coord non traversable) 
            tour_post 0.0 modeleEspace joueur `shouldBe` True           -- modele est changé, la carte est modifié (porte su (2,1) devenu ouvert)
            tour_post 0.0 modeleCorrecte1 slime2 `shouldBe` True    
            tour_post 0.0 modeleCorrecte1 slime2 `shouldBe` True
            -- tests de frappe du joueur
            tour_post 1.0 modeleJoueurFrappeMur joueur `shouldBe` True
            tour_post 1.0 modeleJoueurFrappeVide joueur `shouldBe` True
            tour_post 1.0 modeleJoueurFrappeMonstre joueur `shouldBe` True
            tour_post 1.0 modeleJoueurFrappeMonstreCoolDown joueur `shouldBe` True
            -- tests de mouvement de monstre agressive
            tour_post 1.0 modeleMonstreAgressiveFarAway slime2 `shouldBe` True
            tour_post 1.0 modeleMonstreAgressiveArround slime2 `shouldBe` True
            --test du frappe du monstre agressive
            tour_post 1.0 modeleMonstreAgressiveFrappe slime2 `shouldBe` True
            tour_post 1.0 modeleMonstreAgressiveFrappeCooldown slime2 `shouldBe` True


level_finishedSpec = do
    -- carte du modeleEx1:
    -- XXXXX 
    -- XE SX  
    -- XXXXX 
    --
    -- environnement du modeleEx1:
    -- (1,1)   --   Joueur(0)
    --
    -- etat du clavier : {}
    --
    -- carte du modeleEx2:
    -- XXXXX 
    -- XE SX  
    -- XXXXX 
    --
    -- environnement du modeleEx2:
    -- (3,1)   --   Joueur(0)
    --
    -- etat du clavier : {}
    let modeleEx1 = mkModele (read "XXXXX\nXE SX\nXXXXX" :: Carte) (Envi (M.fromList [(C 1 1, [joueur_init])])) createKeyboard []
    let modeleEx2 = mkModele (read "XXXXX\nXE SX\nXXXXX" :: Carte) (Envi (M.fromList [(C 3 1, [joueur_init])])) createKeyboard []
    describe "level_finished" $ do -- batterie de tests sur level_finished
        it "Si les pre-conditions sont verifiés" $ do   -- testes sur pre-conditions
            level_finished_pre modeleEx1 `shouldBe` True
            level_finished_pre modeleEx2 `shouldBe` True
        it "Alors les post-conditions sont satisfaits" $ do   -- testes sur post-condition
            level_finished_post modeleEx1 `shouldBe` True   -- situation normal
            level_finished_post modeleEx2 `shouldBe` True   -- situiation gagnant

perteSpec = do
    let modeleNormal = mkModele (read "XXXXX\nXE SX\nXXXXX" :: Carte) (Envi (M.fromList [(C 1 1, [joueur_init])])) createKeyboard []
    let modelePerdant = mkModele (read "XXXXX\nXE SX\nXXXXX" :: Carte) (Envi (M.fromList [(C 1 1, [joueur_init {pvie=0}])])) createKeyboard []
    describe "perte" $ do -- batterie de tests sur perte
        it "Si les pre-conditions sont verifiés" $ do   -- testes sur pre-conditions
            perte_pre modeleNormal `shouldBe` True
            perte_pre modelePerdant `shouldBe` True
        it "Alors les post-conditions sont satisfaits" $ do   -- testes sur post-condition
            perte_post modeleNormal `shouldBe` True   -- situation normal
            perte_post modelePerdant `shouldBe` True   -- situiation perdant

modeleMonstreAgressiveTueJoeur = mkModele (read "XXXXX\nXE SX\nXXXXX" :: Carte) (Envi (M.fromList [(C 1 1, [joueur_init {pvie=10}]),(C 2 1, [AgressiveMonstre 2 "red_slime" 60 1.0 1.0 1.0 (Stat 40 1.0 3)])])) createKeyboard [] 
modeleJoueurTueMonstre = mkModele (read "XXXXX\nXE SX\nXXXXX" :: Carte) (Envi (M.fromList [(C 1 1, [joueur_init]),(C 2 1, [AgressiveMonstre 2 "red_slime" 5 1.0 1.0 1.0 (Stat 40 1.0 3)])])) (S.singleton KeycodeRight) [] 


frapperSpec = do
    let monstre = AgressiveMonstre 2 "red_slime" 60 1.0 1.0 1.0 (Stat 40 1.0 3)
    describe "frapper" $ do -- batterie de tests sur frapper
        it "Si les pre-conditions sont verifiés" $ do   -- testes sur pre-conditions
            -- frape du joueur sans tuer monstre
            frapper_pre 1.0 joueur_init (C 2 1) modeleJoueurFrappeMonstre `shouldBe` True
            -- frappe du joueur en tuant le monstre
            frapper_pre 1.0 joueur_init (C 2 1) modeleJoueurTueMonstre `shouldBe` True
            -- frape du monstre sans tuer joueur
            frapper_pre 1.0 monstre (C 1 1) modeleMonstreAgressiveFrappe `shouldBe` True
            -- frappe du monstre en tuant joueur
            frapper_pre 1.0 monstre (C 1 1) modeleMonstreAgressiveTueJoeur `shouldBe` True
            -- frappe du joueur dans vide
            frapper_pre 1.0 joueur_init (C 2 1) modeleJoueurFrappeVide `shouldBe` True
        it "Alors les post-conditions sont satisfaits" $ do   -- testes sur post-condition
            -- frape du joueur sans tuer monstre
            frapper_post 1.0 joueur_init (C 2 1) modeleJoueurFrappeMonstre `shouldBe` True
            -- frappe du joueur en tuant le monstre
            frapper_post 1.0 joueur_init (C 2 1) modeleJoueurTueMonstre `shouldBe` True
            -- frape du monstre sans tuer joueur
            frapper_post 1.0 monstre (C 1 1) modeleMonstreAgressiveFrappe `shouldBe` True
            -- frappe du monstre en tuant joueur
            frapper_post 1.0 monstre (C 1 1) modeleMonstreAgressiveTueJoeur `shouldBe` True
            -- frappe du joueur dans vide
            frapper_post 1.0 joueur_init (C 2 1) modeleJoueurFrappeVide `shouldBe` True
modeleSpec = do
    modele_invSpec
    bougeSpec
    prevoitSpec
    decideSpec
    tourSpec
    level_finishedSpec
    perteSpec
    frapperSpec