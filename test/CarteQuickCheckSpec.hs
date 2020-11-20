module CarteQuickCheckSpec (arbitrary, carteQuickCheckSpec) where

import Test.Hspec
import Test.QuickCheck

import Carte
import Generators
import System.Random



instance Arbitrary Coord where
    arbitrary =  do
        x <- choose (0,20)
        y <- choose (0,20)
        return (C x y)

case_from_id :: Int -> Int -> Case 
case_from_id num_caase type_porte =
    case num_caase of
            1 -> Entree
            2 -> Sortie
            3 -> Mur
            4 -> Normal
            5 -> do
                case type_porte of
                    1 -> (Porte NS Fermee)
                    2 -> (Porte NS Ouverte)
                    3 -> (Porte EO Fermee)
                    4 -> (Porte EO Ouverte)

instance Arbitrary Case where
    arbitrary =  do
        num_caase <- choose (1,5)
        type_porte <- choose (1,4)
        return (case_from_id num_caase type_porte)

-- generateur du carte
genTestCarte :: Gen Carte
genTestCarte = do
    seed <- choose (0,1000000)
    larg <- choose (10,15)
    haut <- choose (10,15)
    let gen = mkStdGen seed
    return (genCarte larg haut gen)

prop_genTestCarte_inv :: Property 
prop_genTestCarte_inv = forAll genTestCarte $ carte_inv

genTestCarteSpec = do
  describe "genTestCarte_QuickCheck" $ do
    it "genere les cartes satisfiant leur invariant" $ 
      property prop_genTestCarte_inv

instance Arbitrary Carte where
    arbitrary = genTestCarte

prop_getCase :: Carte -> Coord -> Property 
prop_getCase carte coord =
  (getCase_pre carte coord)
  ==> 
  let larg = largeur carte
      haut = hauteur carte
  in
    classify (larg*haut < 150) "small carte (100 <= nb_cases  < 150)" $
    classify (larg*haut >= 150 && larg*haut < 185) "medium carte (150 <= nb_cases < 185)" $
    classify (larg*haut >=185 && larg*haut <= 225) "large carte (nb_cases <= 225)" $
    property $ getCase_post carte coord

getCaseSpec = do
  describe "getCase_QuickCheck" $ do
    it "post condition est verifie" $
      property prop_getCase 

prop_isTraversable :: Carte -> Coord -> Property 
prop_isTraversable carte coord =
  (isTraversable_pre carte coord)
  ==> 
  let larg = largeur carte
      haut = hauteur carte
  in
    classify (larg*haut < 150) "small carte (100 <= nb_cases  < 150)" $
    classify (larg*haut >= 150 && larg*haut < 185) "medium carte (150 <= nb_cases < 185)" $
    classify (larg*haut >=185 && larg*haut <= 225) "large carte (nb_cases <= 225)" $
    property $ isTraversable_post carte coord

isTraversableSpec = do
  describe "isTraversable_QuickCheck" $ do
    it "post condition est verifie" $
      property prop_isTraversable

prop_editCase :: Carte -> Coord -> Case -> Property 
prop_editCase carte coord caase=
  (editCase_pre carte coord caase)
  ==> 
  let larg = largeur carte
      haut = hauteur carte
  in
    classify (larg*haut < 150) "small carte (100 <= nb_cases  < 150)" $
    classify (larg*haut >= 150 && larg*haut < 185) "medium carte (150 <= nb_cases < 185)" $
    classify (larg*haut >=185 && larg*haut <= 225) "large carte (nb_cases <= 225)" $
    property $ editCase_post carte coord caase

editCaseSpec = do
  describe "editCase_QuickCheck" $ do
    it "post condition est verifie" $
      property prop_editCase

prop_actWithDoor :: Carte -> Coord-> Property 
prop_actWithDoor carte coord =
  (actWithDoor_pre carte coord)
  ==> 
  let larg = largeur carte
      haut = hauteur carte
  in
    classify (larg*haut < 150) "small carte (100 <= nb_cases  < 150)" $
    classify (larg*haut >= 150 && larg*haut < 185) "medium carte (150 <= nb_cases < 185)" $
    classify (larg*haut >=185 && larg*haut <= 225) "large carte (nb_cases <= 225)" $
    property $ actWithDoor_post carte coord

actWithDoorSpec = do
  describe "actWithDoor_QuickCheck" $ do
    it "post condition est verifie" $
      property prop_actWithDoor

prop_getEntreeCoord :: Carte -> Property 
prop_getEntreeCoord carte =
  (getEntreeCoord_pre carte)
  ==> 
  let larg = largeur carte
      haut = hauteur carte
  in
    classify (larg*haut < 150) "small carte (100 <= nb_cases  < 150)" $
    classify (larg*haut >= 150 && larg*haut < 185) "medium carte (150 <= nb_cases < 185)" $
    classify (larg*haut >=185 && larg*haut <= 225) "large carte (nb_cases <= 225)" $
    property $ getEntreeCoord_post carte

getEntreeCoordSpec = do
  describe "getEntreeCoord_QuickCheck" $ do
    it "post condition est verifie" $
      property prop_getEntreeCoord

prop_portesAround :: Carte -> Coord-> Property 
prop_portesAround carte coord =
  (portesAround_pre carte coord)
  ==> 
  let larg = largeur carte
      haut = hauteur carte
  in
    classify (larg*haut < 150) "small carte (100 <= nb_cases  < 150)" $
    classify (larg*haut >= 150 && larg*haut < 185) "medium carte (150 <= nb_cases < 185)" $
    classify (larg*haut >=185 && larg*haut <= 225) "large carte (nb_cases <= 225)" $
    property $ portesAround_post carte coord

portesAroundSpec = do
  describe "portesAround_QuickCheck" $ do
    it "post condition est verifie" $
      property prop_portesAround

carteQuickCheckSpec = do
    genTestCarteSpec
    getCaseSpec
    isTraversableSpec
    editCaseSpec
    actWithDoorSpec
    getEntreeCoordSpec
    portesAroundSpec
