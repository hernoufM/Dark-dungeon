module EtatSpec where --(etatSpec) where 

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Etat
import Modele 
import Environnement
import Entite
import Keyboard 
import Carte
import System.Random
import Test.Hspec
import SDL.Input.Keyboard.Codes
{- ************** Etat ***************** -}
 
-- etat normal
etatNormal :: Etat
etatNormal =
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
    -- (2,1)   --   Slime(1)
    -- (6,7)   --   Slime(2)
    --
    -- etat clavier : {}
    -- events : []
    let carte = read "XXXXXXXXXX\nX    | XSX\nX    X X-X\nXXXX X X X\nX    X X X\nX XXXX   X\nX    XXXXX\nXXX    XXX\nXE  X  XXX\nXXXXXXXXXX" :: Carte 
        joueur = joueur_init
        slime1 = PeacefullMonstre 1 "pink_slime" 20 1.0 1.0
        slime2 = PeacefullMonstre 2 "pink_slime" 20 1.0 1.0
    in
        let env = Envi (M.fromList [(C 1 8, [joueur]), (C 2 1, [slime1]), (C 6 7, [slime2])])
        in 
            Tour 1 (Cont carte env (mkStdGen 20) createKeyboard KeycodeD []) joueur (S.fromList [slime1, slime2])

-- etat avant gagnée
etatAvantGagne :: Etat
etatAvantGagne =
    --
    -- carte du fichier "carte/start.map":
    -- XXXXXXXXXX
    -- X    | XSX
    -- X    X X\X
    -- XXXX X X X
    -- X    X X X
    -- X XXXX   X
    -- X    XXXXX
    -- XXX    XXX
    -- XE  X  XXX
    -- XXXXXXXXXX
    --
    -- environnement :
    -- (8,2)   --   Joueur(0)
    -- (2,1)   --   Slime(1)
    -- (6,7)   --   Slime(2)
    --
    -- etat clavier : {}
    -- events : []
    let carte = read "XXXXXXXXXX\nX    | XSX\nX    X X\\X\nXXXX X X X\nX    X X X\nX XXXX   X\nX    XXXXX\nXXX    XXX\nXE  X  XXX\nXXXXXXXXXX" :: Carte 
        joueur = joueur_init
        slime1 = PeacefullMonstre 1 "pink_slime" 20 1.0 1.0
        slime2 = PeacefullMonstre 2 "pink_slime" 20 1.0 1.0
    in
        let env = Envi (M.fromList [(C 8 2, [joueur]), (C 2 1, [slime1]), (C 6 7, [slime2])])
        in 
            Tour 1 (Cont carte env (mkStdGen 20) createKeyboard KeycodeD []) joueur (S.fromList [slime1, slime2])

-- etat avant perte
etatAvantPerte :: Etat
etatAvantPerte =
    --
    -- carte du fichier "carte/start.map":
    -- XXXXXXXXXX
    -- X    | XSX
    -- X    X X\X
    -- XXXX X X X
    -- X    X X X
    -- X XXXX   X
    -- X    XXXXX
    -- XXX    XXX
    -- XE  X  XXX
    -- XXXXXXXXXX
    --
    -- environnement :
    -- (8,2)   --   Joueur(0)
    -- (2,1)   --   Slime(1)
    -- (6,7)   --   Slime(2)
    --
    -- etat clavier : {}
    -- events : []
    let carte = read "XXXXXXXXXX\nX    | XSX\nX    X X\\X\nXXXX X X X\nX    X X X\nX XXXX   X\nX    XXXXX\nXXX    XXX\nXE  X  XXX\nXXXXXXXXXX" :: Carte 
        joueur = joueur_init {pvie=10}
        slime1 = AgressiveMonstre 1 "red_slime" 60 1.0 1.0 1.0 (Stat 40 1.0 3)
        slime2 = PeacefullMonstre 2 "pink_slime" 20 1.0 1.0
    in
        let env = Envi (M.fromList [(C 8 2, [joueur]), (C 8 3, [slime1]), (C 6 7, [slime2])])
        in 
            Tour 1 (Cont carte env (mkStdGen 20) createKeyboard KeycodeD []) joueur (S.fromList [slime1, slime2])

-- constructeur des etat libres (capables de ne pas respecter invariant)
mkEtat :: Carte -> Environnement -> Keyboard -> Entite -> S.Set Entite -> Etat 
mkEtat carte env kbd j mobs =
    Tour 0 (Cont carte env (mkStdGen 30) kbd KeycodeD []) j mobs

etat_invSpec = do      -- les testes sur les proprietes d'etat et sur l'invariant qui verifie ces proprietes en plus

    describe "monstres_et_joueur_coherence_prop" $ do         -- testes sur prop1
        it "teste si tous les monstres et joueur dans champs 'monstres' et 'joueur' existent dans l'environnemment et l'inverse" $ do
            etatNormal `shouldSatisfy` monstres_et_joueur_coherence_prop
            etatAvantGagne `shouldSatisfy` monstres_et_joueur_coherence_prop
            etatAvantPerte `shouldSatisfy` monstres_et_joueur_coherence_prop
        it "sinon prop n'est pas respecté" $ do
            -- etatFaux1 :
            --     modele:
            --           carte:
            --              XXXX
            --              XESX
            --              XXXX
            --           environnement:             
            --              (2,1)   --   Joueur(0)
            --              (1,1)   --   Slime(1)
            --     joueur: Joueur(5) <-- ici non coherence avec environnement
            --     monstres: {Slime(1)}
            --
            -- etatFaux2 :
            --     modele:
            --           carte:
            --              XXXX
            --              XESX
            --              XXXX
            --           environnement:             
            --              (2,1)   --   Joueur(0), Slime(2)  <-- ici non coherence avec etat
            --              (1,1)   --   Slime(1)
            --     joueur: Joueur(0)
            --     monstres: {Slime(1)}

            let joueur = joueur_init
            let slime1 = PeacefullMonstre 1 "pink_slime" 20 1.0 1.0
            let slime2 = PeacefullMonstre 2 "pink_slime" 20 1.0 1.0
            let etatFaux1 = mkEtat (read "XXXX\nXESX\nXXXX" :: Carte) 
                                   (Envi (M.fromList ([(C 1 1, [slime1]),
                                                       (C 2 1, [joueur])])))
                                   createKeyboard
                                   (joueur_init {iden=5, pvie=50})
                                   (S.fromList [slime1])
            let etatFaux2 = mkEtat (read "XXXX\nXESX\nXXXX" :: Carte) 
                                   (Envi (M.fromList ([(C 1 1, [slime1]),
                                                       (C 2 1, [joueur, slime2])])))
                                   createKeyboard
                                   joueur
                                   (S.fromList [slime1])
            etatFaux1 `shouldNotSatisfy` monstres_et_joueur_coherence_prop
            etatFaux2 `shouldNotSatisfy` monstres_et_joueur_coherence_prop

    describe "etat_inv" $ do        -- testes sur invariant du etat
        it "si modele respecte son invariant, level est >=1, le champ 'joueur' contient joueur et champ 'monstres' contient que des mobs, le proprité est verifié alors invariant du modele est respecté" $ do
            etatNormal `shouldSatisfy` etat_inv
            etatAvantGagne `shouldSatisfy` etat_inv
            etatAvantPerte `shouldSatisfy` etat_inv

etat_tourSpec = do
    -- on veux tester fonction etat_tour
    -- les cas qui sont interessant à traiter sont suivant :
    --           1) etat_tour qui produit etat Gagne (etatCorrecte2 avec le kbd qui contient touche Z)
    --           2) etat_tour qui produit etat Perdre (le keyboard passé est vide et le monstre agressive à coté a un damage >= pvie du joueur)
    --           3) etat_tour qui bouge tous le monde (etatCorrecte1 avec le time equal à 1.5 et kbd qui contient touche D)
    describe "etat_tour" $ do -- batterie de tests sur etat_tour
        it "Si les pre-conditions sont verifiés" $ do   -- testes sur pre-conditions
            etat_tour_pre 3.0 etatNormal (S.singleton KeycodeD) `shouldBe` True
            etat_tour_pre 3.0 etatAvantGagne (S.singleton KeycodeZ) `shouldBe` True
            etat_tour_pre 3.0 etatAvantPerte createKeyboard `shouldBe` True
        it "Alors les post-conditions sont satisfaits" $ do   -- testes sur post-condition
            etat_tour_post 3.0 etatNormal (S.singleton KeycodeD) `shouldBe` True
            etat_tour_post 3.0 etatAvantGagne (S.singleton KeycodeZ) `shouldBe` True
            etat_tour_post 3.0 etatAvantPerte createKeyboard `shouldBe` True

etatSpec = do
    etat_invSpec
    --etat_tourSpec