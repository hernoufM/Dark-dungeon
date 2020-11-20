module EntiteSpec (arbitrary,entiteSpec) where

import Test.Hspec
import Entite

import Test.QuickCheck

{- *************** Entite ****************** -}
monstres = [ joueur_init,
             PeacefullMonstre 1 "pink_slime" 20 1.0 0.0,
             PeacefullMonstre 2 "worm" 20 1.5 1.0,
             PeacefullMonstre 3 "blue_worm" 60 1.5 2.0,
             NeutralMonstre 4 "green_slime" 40 40 1.0 1.0 3.0 (Stat 15 1.0 3),
             NeutralMonstre 5 "rat" 30 30 0.7 0.7 4.0 (Stat 15 0.7 5),
             AgressiveMonstre 6 "red_slime" 60 1.0 1.0 5.0 (Stat 40 1.0 3) ]

instance Arbitrary Entite where
    arbitrary =  do
        i <- choose (0,(length monstres) -1)
        return $ monstres !! i

entiteSpec = do     -- batterie de testes sur Entite
    describe "instance Eq Entite" $ do -- teste d'instanciation du Eq
        it "defines equality of two Entite" $ do
            let entite2 = PeacefullMonstre 0 "pink_slime" 20 1.0 0.0
                in
                joueur_init `shouldBe` entite2
    
    describe "instance Ord Entite" $ do  -- teste d'instanciation du Ord
        it "allows comparison of two Entite" $ do
            let entite1 = joueur_init
                entite2 = NeutralMonstre 5 "green_slime" 40 40 1.0 1.0 0.0 (Stat 15 1.0 3)
                in
                entite1 <= entite2 `shouldBe` True
    
    describe "entite_inv" $ do
        it "entite respecte son invariant" $
            property entite_inv 