
import Test.Hspec

import CarteSpec
import EntiteSpec
import EnvironnementSpec
import ModeleSpec
import EtatSpec
import GeneratorsSpec
import CarteQuickCheckSpec
import EventsSpec
import PowerUpSpec

main :: IO ()
main = hspec $ do
    -- Carte
    carteSpec
    -- CarteQuickCheckSpec
    carteQuickCheckSpec
    -- Entite
    entiteSpec
    -- Environnement
    environnementSpec
    -- Modele
    modeleSpec
    -- Etat
    etatSpec
    -- Generators
    generatorsSpec
    -- Events
    eventsSpec
    -- PowerUp
    powerUpSpec