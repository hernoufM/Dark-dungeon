module GeneratorsSpec (generatorsSpec) where

import Test.Hspec
import qualified Data.Map.Strict as M
import Carte
import Entite
import Generators
import System.Random

larg = 10
haut = 10

gen1 = mkStdGen 50
gen2 = mkStdGen 100
gen3 = mkStdGen 200
gen4 = mkStdGen 300
gen5 = mkStdGen 400
gen6 = mkStdGen 400

map_etap1 = create_bordure larg haut
map_etap2 = fst (fill_randomly larg haut map_etap1 gen1)
map_etap3 = fst (fix_walls map_etap2 gen2)
map_etap4 = fst (join_ways larg haut map_etap3 gen3)
map_etap5 = generate_doors map_etap4 
map_etap6 = generate_entry larg haut map_etap5


create_bordureSpec = do
    describe "create_bordure" $ do     
        it "Si les pre-conditions sont verifiés" $ do   
            create_bordure_pre larg haut `shouldBe` True
        it "Alors les post-conditions sont satisfaits" $ do   
            create_bordure_post larg haut `shouldBe` True

fill_randomlySpec = do
    describe "fill_randomly" $ do     
        it "Si les pre-conditions sont verifiés" $ do   
            fill_randomly_pre larg haut map_etap1 gen1 `shouldBe` True
        it "Alors les post-conditions sont satisfaits" $ do   
            fill_randomly_post larg haut map_etap1 gen1 `shouldBe` True

all_groupsSpec = do
    describe "all_groups" $ do     
        it "Si les pre-conditions sont verifiés" $ do   
            all_groups_pre Mur map_etap2 `shouldBe` True
            all_groups_pre Normal map_etap2 `shouldBe` True
        it "Alors les post-conditions sont satisfaits" $ do   
            all_groups_post Mur map_etap2 `shouldBe` True
            all_groups_post Normal map_etap2 `shouldBe` True

fix_wallsSpec = do
    describe "fix_walls" $ do     
        it "Si les pre-conditions sont verifiés" $ do   
            fix_walls_pre map_etap2 gen2 `shouldBe` True
        it "Alors les post-conditions sont satisfaits" $ do   
            fix_walls_post map_etap2 gen2 `shouldBe` True

join_waysSpec = do
    describe "join_ways" $ do     
        it "Si les pre-conditions sont verifiés" $ do   
            join_ways_pre larg haut map_etap3 gen3 `shouldBe` True
        it "Alors les post-conditions sont satisfaits" $ do   
            join_ways_post larg haut map_etap3 gen3 `shouldBe` True

generate_doorsSpec = do
    describe "generate_doors" $ do     
        it "Si les pre-conditions sont verifiés" $ do   
            generate_doors_pre map_etap4 `shouldBe` True
        it "Alors les post-conditions sont satisfaits" $ do   
            generate_doors_post map_etap4 `shouldBe` True

generate_entrySpec = do
    describe "generate_entry" $ do     
        it "Si les pre-conditions sont verifiés" $ do   
            generate_entry_pre larg haut map_etap5 `shouldBe` True
        it "Alors les post-conditions sont satisfaits" $ do   
            generate_entry_post larg haut map_etap5 `shouldBe` True

generate_exitSpec = do
    describe "generate_exit" $ do     
        it "Si les pre-conditions sont verifiés" $ do   
            generate_exit_pre larg haut map_etap6 `shouldBe` True
        it "Alors les post-conditions sont satisfaits" $ do   
            generate_exit_post larg haut map_etap6 `shouldBe` True

genCarteSpec = do
    describe "genCarte" $ do     
        it "Si les pre-conditions sont verifiés" $ do   
            genCarte_pre larg haut gen4 `shouldBe` True
        it "Alors les post-conditions sont satisfaits" $ do   
            genCarte_post larg haut gen4 `shouldBe` True

carteEx :: Carte
carteEx = read "XXXXXXXXXX\nX    | XSX\nX    X X-X\nXXXX X X X\nX    X X X\nX XXXX   X\nX    XXXXX\nXXX    XXX\nXE  X  XXX\nXXXXXXXXXX" :: Carte

env_etap1 = add_player joueur_init carteEx

add_playerSpec = do
    describe "add_player" $ do     
        it "Si les pre-conditions sont verifiés" $ do   
            add_player_pre joueur_init carteEx `shouldBe` True
        it "Alors les post-conditions sont satisfaits" $ do   
            add_player_post joueur_init carteEx `shouldBe` True

randomEntiteSpec = do
    describe "randomEntite" $ do     
        it "Si les pre-conditions sont verifiés" $ do   
            randomEntite_pre 1.0 VeryEasy 1 gen4 `shouldBe` True
            randomEntite_pre 1.0 Easy 1 gen4 `shouldBe` True
            randomEntite_pre 1.0 Mediocre 1 gen4 `shouldBe` True
            randomEntite_pre 1.0 Hard 1 gen4 `shouldBe` True
            randomEntite_pre 1.0 VeryHard 1 gen4 `shouldBe` True
        it "Alors les post-conditions sont satisfaits" $ do   
            randomEntite_post 1.0 VeryEasy 1 gen4 `shouldBe` True
            randomEntite_post 1.0 Easy 1 gen4 `shouldBe` True
            randomEntite_post 1.0 Mediocre 1 gen4 `shouldBe` True
            randomEntite_post 1.0 Hard 1 gen4 `shouldBe` True
            randomEntite_post 1.0 VeryHard 1 gen4 `shouldBe` True

generate_monstersSpec = do
    describe "generate_monsters" $ do     
        it "Si les pre-conditions sont verifiés" $ do   
            generate_monsters_pre 1.0 VeryEasy carteEx env_etap1 gen5 `shouldBe` True
            generate_monsters_pre 1.0 Easy carteEx env_etap1 gen5 `shouldBe` True
            generate_monsters_pre 1.0 Mediocre carteEx env_etap1 gen5 `shouldBe` True
            generate_monsters_pre 1.0 Hard carteEx env_etap1 gen5 `shouldBe` True
            generate_monsters_pre 1.0 VeryHard carteEx env_etap1 gen5 `shouldBe` True
        it "Alors les post-conditions sont satisfaits" $ do   
            generate_monsters_post 1.0 VeryEasy carteEx env_etap1 gen5 `shouldBe` True
            generate_monsters_post 1.0 Easy carteEx env_etap1 gen5 `shouldBe` True
            generate_monsters_post 1.0 Mediocre carteEx env_etap1 gen5 `shouldBe` True
            generate_monsters_post 1.0 Hard carteEx env_etap1 gen5 `shouldBe` True
            generate_monsters_post 1.0 VeryHard carteEx env_etap1 gen5 `shouldBe` True

generatorsSpec = do
    create_bordureSpec
    fill_randomlySpec
    all_groupsSpec
    fix_wallsSpec
    join_waysSpec
    generate_doorsSpec
    generate_entrySpec
    generate_exitSpec
    genCarteSpec
    add_playerSpec
    randomEntiteSpec
    generate_monstersSpec