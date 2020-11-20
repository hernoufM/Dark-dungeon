module EnvironnementSpec (environnementSpec) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Test.QuickCheck
import Test.Hspec
import Environnement
import Entite
import Carte
import Generators
import CarteQuickCheckSpec
import EntiteSpec
import System.Random

{- ************** Environnement ***************** -}


--generateur difficulte
instance Arbitrary Difficulte where
    arbitrary =  do
        level <- choose (1,5) :: Gen Int
        let diff = case level of
                    1 -> Easy
                    2 -> VeryEasy
                    3 -> Mediocre
                    4 -> Hard
                    5 -> VeryHard
        return diff

-- generateur d'environnement 
generateTestEnvironnement :: Gen Environnement
generateTestEnvironnement = do
    seed <- choose (0,1000000)
    carte <- arbitrary
    time <- choose (0,10000)
    difficulte <- arbitrary
    let gen = mkStdGen seed
    let contenu = add_player joueur_init carte
    let (contenu2,_) = generate_monsters (fromInteger time) difficulte carte contenu gen
    return (Envi (fmap (\ent -> [ent]) contenu2))

instance Arbitrary Environnement where
    arbitrary = generateTestEnvironnement

prop_genTestEnvironnement_inv :: Property 
prop_genTestEnvironnement_inv = forAll generateTestEnvironnement $ environnement_inv

genTestEnvironnementSpec = do
  describe "genTestEnvironnement" $ do
    it "genere les environnements satisfiant leur invariant" $ 
      property prop_genTestEnvironnement_inv

prop_franchissable_env :: Coord -> Environnement -> Property 
prop_franchissable_env coord env =
  (franchissable_env_pre coord env)
  ==> 
  let nbEnites = foldr (\ entites acc -> (length entites) + acc) 0 (contenu_envi env)
  in
    classify (nbEnites >= 0 && nbEnites < 5) "small environment (0 <= nb_enites  < 5 )" $
    classify (nbEnites >= 5) "large environment (nb_enites >= 5 )" $
    property $ franchissable_env_post coord env

franchissable_envSpec = do
  describe "franchissable_env" $ do
    it "post condition est verifie" $
      property prop_franchissable_env 

prop_trouve_id :: Int -> Environnement -> Property 
prop_trouve_id id env =
  (trouve_id_pre id env)
  ==> 
  let nbEnites = foldr (\ entites acc -> (length entites) + acc) 0 (contenu_envi env)
  in
    classify (nbEnites >= 0 && nbEnites < 5) "small environment (0 <= nb_enites  < 5 )" $
    classify (nbEnites >= 5) "large environment (nb_enites >= 5 )" $
    property $ trouve_id_post id env

trouve_idSpec = do
  describe "trouve_id" $ do
    it "post condition est verifie" $
      property $ forAll (choose (1,20)) $ prop_trouve_id

prop_rm_env_id :: Int -> Environnement -> Property 
prop_rm_env_id id env =
  (rm_env_id_pre id env)
  ==> 
  let nbEnites = foldr (\ entites acc -> (length entites) + acc) 0 (contenu_envi env)
  in
    classify (nbEnites >= 0 && nbEnites < 5) "small environment (0 <= nb_enites  < 5 )" $
    classify (nbEnites >= 5) "large environment (nb_enites >= 5 )" $
    property $ rm_env_id_post id env

rm_env_idSpec = do
  describe "rm_env_id" $ do
    it "post condition est verifie" $
      property $ forAll (choose (1,20)) $ prop_rm_env_id

prop_bouge_id :: Int -> Coord -> Environnement -> Property 
prop_bouge_id id coord env =
  (bouge_id_pre id coord env)
  ==> 
  let nbEnites = foldr (\ entites acc -> (length entites) + acc) 0 (contenu_envi env)
  in
    classify (nbEnites >= 0 && nbEnites < 5) "small environment (0 <= nb_enites  < 5 )" $
    classify (nbEnites >= 5) "large environment (nb_enites >= 5 )" $
    property $ bouge_id_post id coord env

bouge_idSpec = do
  describe "bouge_id" $ do
    it "post condition est verifie" $
      property $ forAll (choose (1,20)) $ prop_bouge_id

prop_get_pos_joueur :: Environnement -> Property 
prop_get_pos_joueur env =
  (get_pos_joueur_pre env)
  ==> 
  let nbEnites = foldr (\ entites acc -> (length entites) + acc) 0 (contenu_envi env)
  in
    classify (nbEnites >= 0 && nbEnites < 5) "small environment (0 <= nb_enites  < 5 )" $
    classify (nbEnites >= 5) "large environment (nb_enites >= 5 )" $
    property $ get_pos_joueur_post env

get_pos_joueurSpec = do
  describe "get_pos_joueur" $ do
    it "post condition est verifie" $
      property prop_get_pos_joueur 

prop_info_env :: Environnement -> Property 
prop_info_env env =
  (info_env_pre env)
  ==> 
  let nbEnites = foldr (\ entites acc -> (length entites) + acc) 0 (contenu_envi env)
  in
    classify (nbEnites >= 0 && nbEnites < 5) "small environment (0 <= nb_enites  < 5 )" $
    classify (nbEnites >= 5) "large environment (nb_enites >= 5 )" $
    property $ info_env_post env

info_envSpec = do
  describe "info_env" $ do
    it "post condition est verifie" $
      property prop_info_env 

prop_remplace :: Entite -> Entite -> Environnement -> Property 
prop_remplace ent1 ent2 env =
  (remplace_pre ent1 ent2 env)
  ==> 
  let nbEnites = foldr (\ entites acc -> (length entites) + acc) 0 (contenu_envi env)
  in
    classify (nbEnites >= 0 && nbEnites < 5) "small environment (0 <= nb_enites  < 5 )" $
    classify (nbEnites >= 5) "large environment (nb_enites >= 5 )" $
    property $ remplace_post ent1 ent2 env

remplaceSpec = do
  describe "remplace" $ do
    it "post condition est verifie" $
      property $ prop_remplace joueur_init      -- on essaye de remplacer joueur par une autre entite

environnementSpec = do
    genTestEnvironnementSpec
    franchissable_envSpec
    trouve_idSpec
    rm_env_idSpec
    bouge_idSpec
    get_pos_joueurSpec
    info_envSpec
    remplaceSpec
