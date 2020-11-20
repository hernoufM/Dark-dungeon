module EventsSpec (eventsSpec) where

import Test.Hspec
import Events
import Entite
import qualified Data.Set as S

gameEventSpec = do       -- batterie de testes sur GameEvent
    
    describe "instance Eq GameEvent" $ do  -- teste d'instanciation du Eq
        it "equality of two game events" $ 
            let event1 = JoueurAttack At_Sud 1.0
                event2 = JoueurAttack At_Nord 1.0
                event3 = PowerUpEvent 2.0
                event4 = PowerUpEvent 2.0
                in do
                    (event1 == event2) `shouldBe` False
                    (event1 == event3) `shouldBe` False
                    (event3 == event4) `shouldBe` True
    
    describe "game_event_inv" $ do  -- testes sur invariant du game event
        let eventCorect = JoueurAttack At_Nord 1.0
        let eventIncorrect1 = JoueurAttack At_Nord (-1.0)
        let eventIncorrect2 = MonsterAttack 0 At_Ouest 5.0
        it "invariant du game event est respecté" $ 
            do
                eventCorect `shouldSatisfy` game_event_inv
                eventIncorrect1 `shouldNotSatisfy` game_event_inv
                eventIncorrect2 `shouldNotSatisfy` game_event_inv 

filterEventsSpec = do
    let monstres = S.fromList [ NeutralMonstre 1 "green_slime" 40 40 1.0 1.0 0.5 (Stat 15 1.0 3), 
                                AgressiveMonstre 2 "spider" 50 0.7 0.7 0.5 (Stat 30 0.7 4)]
    let liste_event = [PowerUpEvent 0.5, MonsterAttack 1 At_Sud 0.5, JoueurAttack At_Nord 0.5]
    describe "filterEvents" $ do     -- batterie de tests sur filterEvents
        it "Si les pre-conditions sont verifiés" $ do   -- testes sur pre-condition
            filterEvents_pre 0.5 [] joueur_init monstres `shouldBe` True 
            filterEvents_pre 0.5 liste_event joueur_init monstres `shouldBe` True   
            filterEvents_pre 2.0 liste_event joueur_init monstres `shouldBe` True   
            filterEvents_pre 5.0 liste_event joueur_init monstres  `shouldBe` True
        it "Alors les post-conditions sont satisfaits" $ do   
            filterEvents_post 0.5 [] joueur_init monstres `shouldBe` True 
            filterEvents_post 0.5 liste_event joueur_init monstres `shouldBe` True   
            filterEvents_post 2.0 liste_event joueur_init monstres `shouldBe` True   
            filterEvents_post 5.0 liste_event joueur_init monstres  `shouldBe` True

eventsSpec = do
    gameEventSpec
    filterEventsSpec