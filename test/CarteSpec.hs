module CarteSpec (carteSpec) where

import Test.Hspec
import qualified Data.Map.Strict as M
import Carte

{- ****************** Coordonnees *********************** -}
coordSpec = do      -- batterie de testes sur Coord
    
    describe "instance Ord Coord" $ do  -- teste d'instanciation du Ord
        it "allows comparison of two Coord" $ do
            let coord1 = C 5 9
                coord2 = C 5 15
                in
                coord1 <= coord2 `shouldBe` True

{- ****************** Cases *********************** -}

caseSpec = do       -- batterie de testes sur Case
    
    describe "instance Show Case" $ do  -- teste d'instanciation du Show
        it "converts Case to string" $ do
            let case1 = Porte EO Ouverte 
                in
                show case1 `shouldBe` "/"      
    
    describe "instance Read Case" $ do  -- teste d'instanciation du Read
        it "parse string and returns Case" $ do
            let str = "/"
                in
                (read str :: Case) `shouldBe` Porte EO Ouverte 

{- ****************** Carte *********************** -}


carteInstanceSpec = do      -- les tests sur tous les instanciations du carte
    
    describe "instance Show Carte" $ do     -- teste d'instanciation du Show
        it "converts Carte to string" $ do
            let carteEx = Carte 3 3 (M.fromList [ (C 0 0, Mur), -- on teste sur la carte qui contient tous les types des cases
                                                  (C 1 0, Normal),
                                                  (C 2 0, Entree),
                                                  (C 0 1, Sortie),
                                                  (C 1 1, Porte NS Fermee),
                                                  (C 2 1, Porte NS Ouverte),
                                                  (C 0 2, Porte EO Fermee),
                                                  (C 1 2, Porte EO Ouverte),
                                                  (C 2 2, Mur)])
                in
                    show carteEx `shouldBe` "X E\nS-\\\n|/X"
    
    describe "instance Read Carte" $ do     -- teste d'instanciation du Read
        it "parse string and returns Carte" $ do
            let carteEx = Carte 3 3 (M.fromList [ (C 0 0, Mur), -- on teste sur la carte qui contient tous les types des cases
                                                  (C 1 0, Normal),
                                                  (C 2 0, Entree),
                                                  (C 0 1, Sortie),
                                                  (C 1 1, Porte NS Fermee),
                                                  (C 2 1, Porte NS Ouverte),
                                                  (C 0 2, Porte EO Fermee),
                                                  (C 1 2, Porte EO Ouverte),
                                                  (C 2 2, Mur)])
                in
                    (read "X E\nS-\\\n|/X" ::Carte) `shouldBe` carteEx



-- carte du fichier "carte/start.map"
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
carteCorrecte :: Carte
carteCorrecte = read "XXXXXXXXXX\nX    | XSX\nX    X X-X\nXXXX X X X\nX    X X X\nX XXXX   X\nX    XXXXX\nXXX    XXX\nXE  X  XXX\nXXXXXXXXXX" :: Carte

carte_invSpec = do      -- les testes sur les proprietes du carte et sur l'invariant qui combine tous les proprietes
    
    describe "coords_in_rect_prop" $ do         -- testes sur prop1
        it "teste si tous les coordonnees du carte sont dans rectangle formée par deux points (0,0) et (larg-1,haut-1)" $ do
            carteCorrecte `shouldSatisfy` coords_in_rect_prop   
        it "si une des coordonnees sors du rectangle, alors la prop n'est pas respecté" $ do
            let carteFaux = Carte 3 3 (M.fromList [ (C 0 0, Mur),
                                                  (C 1 0, Normal),
                                                  (C 2 0, Entree),
                                                  (C 0 1, Sortie),
                                                  (C 1 1, Porte NS Fermee),
                                                  (C 2 1, Porte NS Ouverte),
                                                  (C 0 2, Porte EO Fermee),
                                                  (C 1 2, Porte EO Ouverte),
                                                  (C 2 2, Mur),
                                                  (C 5 5, Mur)]) -- coordonnees qui sort du rectangle
                in carteFaux `shouldNotSatisfy` coords_in_rect_prop

    describe "all_coords_exists_prop" $ do      -- testes sur prop2
        it "teste si tous les coordoonnees possibles du rectangle, formée par deux points (0,0) et (larg-1,haut-1), existent dans la carte" $ do
            carteCorrecte `shouldSatisfy` all_coords_exists_prop   
        it "si une des coordonnees n'existe pas sur la carte, alors la prop n'est pas respecté" $ do
            let carteFaux = Carte 3 3 (M.fromList [ (C 0 0, Mur),
                                                  (C 1 0, Normal),
                                                  (C 2 0, Entree),
                                                  (C 0 1, Sortie),
                                                  -- il manque coordonnees (1,1) 
                                                  (C 2 1, Porte NS Ouverte),
                                                  (C 0 2, Porte EO Fermee),
                                                  (C 1 2, Porte EO Ouverte),
                                                  (C 2 2, Mur)]) 
                in carteFaux `shouldNotSatisfy` all_coords_exists_prop

    describe "one_entry_one_exit_prop" $ do     -- testes sur prop3
        it "teste s'il existe une unique entree et une unique sortie" $ do
            carteCorrecte `shouldSatisfy` one_entry_one_exit_prop   
        it "sinon prop n'est pas respecté" $ do
            -- XXXX
            -- XSSX         
            -- XE X
            -- XXXX
            let carteFaux = read "XXXX\nXSSX\nXE X\nXXXX" :: Carte -- deux sortie
                in carteFaux `shouldNotSatisfy` one_entry_one_exit_prop

    describe "wall_surrounded_prop" $ do        -- testes sur prop4
        it "teste si les carte est entouré par des mures." $ do
            carteCorrecte `shouldSatisfy` wall_surrounded_prop   
        it "sinon prop n'est pas respecté" $ do
            -- XXXX
            -- X  X
            -- X  X
            -- X XX
            let carteFaux = read "XXXX\nX  X\nX  X\nX XX" :: Carte
                in carteFaux `shouldNotSatisfy` wall_surrounded_prop 

    describe "doors_in_wall_prop" $ do        -- testes sur prop5
        it "teste si tous les portes sur carte sont entouré par des murs de deux cotés" $ do
            carteCorrecte `shouldSatisfy` doors_in_wall_prop   
        it "sinon prop n'est pas respecté" $ do
            -- XXXX
            -- X| X
            -- X -X     
            -- XXXX
            -- porte NS n'as pas de murs à gauche et poerte EO n'as pas de mur en bas
            let carteFaux = read "XXXX\nX| X\nX -X\nXXXX" :: Carte  
                in carteFaux `shouldNotSatisfy` doors_in_wall_prop 

    describe "exit_acces_prop" $ do        -- testes sur prop6
        it "teste si la sortie est accessible à partir d'entree" $ do
            carteCorrecte `shouldSatisfy` exit_acces_prop   
        it "sinon prop n'est pas respecté" $ do
            -- XXXXXXXXXX
            -- X    | XSX   meme example que la carte correcte sauf
            -- X    X X-X
            -- XXXX X X X
            -- X    X X X
            -- X XXXX X X <- ici on a fermé le passage vers sortie
            -- X    XXXXX
            -- XXX    XXX
            -- XE  X  XXX
            -- XXXXXXXXXX
            let carteFaux = read "XXXXXXXXXX\nX    | XSX\nX    X X-X\nXXXX X X X\nX    X X X\nX XXXX X X\nX    XXXXX\nXXX    XXX\nXE  X  XXX\nXXXXXXXXXX" :: Carte
                in carteFaux `shouldNotSatisfy` exit_acces_prop 
    
    describe "carte_inv" $ do        -- testes sur invariant du carte
        it "si tous les proprité sont respecté alors l'invariant du carte est respecté" $ do
            carteCorrecte `shouldSatisfy` carte_inv  

getCaseSpec = do
    let coordExistant = C 8 1       -- coordonnees existant
    let coordNotExistant = C 20 20  -- coordonnees non existant
    describe "getCase" $ do     -- batterie de tests sur getCase
        it "Si les pre-conditions sont verifiés" $ do   -- testes sur pre-condition
            getCase_pre carteCorrecte coordExistant `shouldBe` True
            getCase_pre carteCorrecte coordNotExistant `shouldBe` True
        it "Alors les post-conditions sont satisfaits" $ do   
            getCase_post carteCorrecte coordExistant `shouldBe` True
            getCase_post carteCorrecte coordNotExistant `shouldBe` True

isTraversableSpec = do
    let coordNormal = C 1 1     -- coord avec case normal
    let coordMur = C 0 0        -- coord avec mur
    let coordPorteFermee = C 5 1       -- coord avec porte fermée
    let coordNotExistant = C 20 20      -- coord non existant
    describe "isTraversable" $ do     -- batterie de tests sur isTraversable
        it "Si les pre-conditions sont verifiés" $ do   -- testes sur pre-condition
            isTraversable_pre carteCorrecte coordNormal `shouldBe` True
            isTraversable_pre carteCorrecte coordMur `shouldBe` True
            isTraversable_pre carteCorrecte coordPorteFermee `shouldBe` True
            isTraversable_pre carteCorrecte coordNotExistant `shouldBe` True
        it "Alors les post-conditions sont satisfaits" $ do   -- testes sur post-condition
            isTraversable_post carteCorrecte coordNormal `shouldBe` True
            isTraversable_post carteCorrecte coordMur `shouldBe` True
            isTraversable_post carteCorrecte coordPorteFermee `shouldBe` True
            isTraversable_post carteCorrecte coordNotExistant `shouldBe` True

editCaseSpec = do
    -- XXXXXXXXXX                   X1XXXXXXXX 
    -- X    | XSX     les case      X    | X2X 
    -- X    X X-X     que on veut   X 4  X X-X
    -- XXXX X X X     editer        XXXX X X X
    -- X    X X X     sont noté     X    X X X
    -- X XXXX   X     avec une      X XXXX 3 X
    -- X    XXXXX     chiffre       X    XXXXX      
    -- XXX    XXX                   XXX    XXX
    -- XE  X  XXX                   XE  X  XXX
    -- XXXXXXXXXX                   XXXXXXXXXX

    let coord1 = C 1 0   -- (1)
    let coord2 = C 8 1   -- (2)
    let coord3 = C 7 5   -- (3)
    let coord4 = C 2 2   -- (4)
    let coordNotExistant = C 20 20    -- coord non existant
    describe "editCase" $ do     -- batterie de tests sur editCase
        it "Si les pre-conditions sont verifiés" $ do   -- testes sur pre-condition
            editCase_pre carteCorrecte coord1 Normal `shouldBe` True   
            editCase_pre carteCorrecte coord2 (Porte NS Fermee) `shouldBe` True 
            editCase_pre carteCorrecte coord3 (Porte EO Fermee) `shouldBe` True
            editCase_pre carteCorrecte coord3 Mur `shouldBe` True 
            editCase_pre carteCorrecte coord4 (Porte EO Fermee) `shouldBe` True
            editCase_pre carteCorrecte coord4 Mur `shouldBe` True
            editCase_pre carteCorrecte coordNotExistant Mur `shouldBe` True
        it "Alors les post-conditions sont satisfaits" $ do   -- testes sur post-condition
            editCase_post carteCorrecte coord1 Normal `shouldBe` True   -- carte ne change pas (prop4)
            editCase_post carteCorrecte coord2 (Porte NS Fermee) `shouldBe` True    -- carte ne change pas (prop3)
            editCase_post carteCorrecte coord3 (Porte EO Fermee) `shouldBe` True  -- carte est changé
            editCase_post carteCorrecte coord3 Mur `shouldBe` True      -- carte ne change pas (prop6)
            editCase_post carteCorrecte coord4 (Porte EO Fermee) `shouldBe` True    -- carte ne change pas (prop5)
            editCase_post carteCorrecte coord4 Mur `shouldBe` True       -- carte est changé
            editCase_post carteCorrecte coordNotExistant Mur `shouldBe` True     -- carte ne change pas (coord n'existe pas)

actWithDoorSpec = do
    -- XXXXXXXXXX                   XXXXXXXXXX 
    -- X    | XSX     les coords    X    2 XSX 
    -- X    X X-X     que on a      X 1  X X3X
    -- XXXX X X X     choisi        XXXX X X X
    -- X    X X X     sont noté     X    X X X
    -- X XXXX   X     avec une      X XXXX   X
    -- X    XXXXX     chiffre       X    XXXXX      
    -- XXX    XXX                   XXX    XXX
    -- XE  X  XXX                   XE  X  XXX
    -- XXXXXXXXXX                   XXXXXXXXXX

    let coord1 = C 2 2   -- (1)
    let coord2 = C 5 1   -- (2)
    let coord3 = C 8 2   -- (3)
    let coordNotExistant = C 20 20    -- coord non existant
    describe "actWithDoor" $ do     -- batterie de tests sur actWithDoor
        it "Si les pre-conditions sont verifiés" $ do   -- testes sur pre-condition
            actWithDoor_pre carteCorrecte coord1 `shouldBe` True   
            actWithDoor_pre carteCorrecte coord2 `shouldBe` True 
            actWithDoor_pre carteCorrecte coord3 `shouldBe` True
            actWithDoor_pre carteCorrecte coordNotExistant `shouldBe` True 
        it "Alors les post-conditions sont satisfaits" $ do   -- testes sur post-condition
            actWithDoor_post carteCorrecte coord1 `shouldBe` True      -- carte ne change pas (pas de porte)
            actWithDoor_post carteCorrecte coord2 `shouldBe` True      -- carte est changé (porte s'est ouverte) 
            actWithDoor_post carteCorrecte coord3 `shouldBe` True      -- carte est changé (porte s'est ouverte)
            actWithDoor_post carteCorrecte coordNotExistant `shouldBe` True     -- carte ne change pas (coord n'existe pas)

getEntreeCoordSpec = do
    describe "getEntreeCoord" $ do     -- batterie de tests sur getEntreeCoord
        it "Si le pre-conditions est verifié" $ do   
            getEntreeCoord_pre carteCorrecte `shouldBe` True
        it "Alors le post-conditions est satisfait" $ do   
            getEntreeCoord_post carteCorrecte `shouldBe` True  

portesAroundSpec = do
    --  XXXXXX                         1X2XXX
    --  XX-XSX     les coords          X37XSX 
    --  X/ |EX     que on a choisi     X65|EX
    --  XX\X-X     sont noté           XX\4-X
    --  XXXXXX     avec une chiffre    XXXXXX

    let carteEx = read "XXXXXX\nXX-XSX\nX/ |EX\nXX\\X-X\nXXXXXX" :: Carte
    let coord1 = C 0 0   -- (1) pas de portes à coté
    let coord2 = C 2 0   -- (2) 1 porte à coté
    let coord3 = C 1 1   -- (3) 2 portes à coté
    let coord4 = C 3 3   -- (4) 3 portes à coté
    let coord5 = C 2 2   -- (5) 4 portes à coté
    let coord6 = C 1 2   -- (6) on est sur porte ouvert
    let coord7 = C 2 1   -- (7) on est sur porte fermée
    let coordNotExistant = C 20 20    -- coord non existant
    describe "portesAround" $ do     -- batterie de tests sur portesAround
        it "Si les pre-conditions sont verifiés" $ do   -- testes sur pre-condition
            portesAround_pre carteEx coord1 `shouldBe` True   
            portesAround_pre carteEx coord2 `shouldBe` True   
            portesAround_pre carteEx coord3 `shouldBe` True   
            portesAround_pre carteEx coord4 `shouldBe` True
            portesAround_pre carteEx coord5 `shouldBe` True   
            portesAround_pre carteEx coord6 `shouldBe` True   
            portesAround_pre carteEx coord7 `shouldBe` True   
            portesAround_pre carteEx coordNotExistant `shouldBe` True  
        it "Alors les post-conditions sont satisfaits" $ do   -- testes sur post-condition
            portesAround_post carteEx coord1 `shouldBe` True   
            portesAround_post carteEx coord2 `shouldBe` True   
            portesAround_post carteEx coord3 `shouldBe` True   
            portesAround_post carteEx coord4 `shouldBe` True
            portesAround_post carteEx coord5 `shouldBe` True   
            portesAround_post carteEx coord6 `shouldBe` True   
            portesAround_post carteEx coord7 `shouldBe` True   
            portesAround_post carteEx coordNotExistant `shouldBe` True

carteSpec = do
    coordSpec
    caseSpec
    carteInstanceSpec
    carte_invSpec
    getCaseSpec
    isTraversableSpec
    editCaseSpec
    actWithDoorSpec
    getEntreeCoordSpec
    portesAroundSpec