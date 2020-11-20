module PowerUpSpec (powerUpSpec) where

import System.Random
import Test.Hspec
import PowerUp
import Entite

gen = mkStdGen 100

apply_powerUpSpec = do
    describe "apply_powerUp" $ do     
        it "Si les pre-conditions sont verifiés" $ do   
            apply_powerUp_pre joueur_init gen `shouldBe` True
        it "Alors les post-conditions sont satisfaits" $ do   
            apply_powerUp_post joueur_init gen `shouldBe` True

powerUpSpec = do
    apply_powerUpSpec
    