{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Data.List (sortBy)
import Data.Ord (comparing)

data Rune = Rune
  { runeId        :: Int  
  , runeSlot      :: Int
  , runeSetName   :: String
  , runeMainStat  :: String
  , runeMainValue :: Int
  , runeSub1      :: String
  , runeSub1Value :: Int
  , runeSub2      :: String
  , runeSub2Value :: Int
  , runeSub3      :: String
  , runeSub3Value :: Int
  , runeSub4      :: String
  , runeSub4Value :: Int
  } deriving (Show, Eq)

procura_substatus :: String -> Rune -> Int
procura_substatus stat runa = 
  if runeSub1 runa == stat then runeSub1Value runa
  else if runeSub2 runa == stat then runeSub2Value runa
  else if runeSub3 runa == stat then runeSub3Value runa
  else if runeSub4 runa == stat then runeSub4Value runa
  else 0

procura_mainstatus :: String -> Rune -> Int
procura_mainstatus stat runa = 
  if runeMainStat runa == stat then runeMainValue runa 
    else 0

somador_main_sub :: String -> Rune -> Int
somador_main_sub stat runa = procura_mainstatus stat runa + procura_substatus stat runa

separarPorSlots :: [Rune] -> [[Rune]]
separarPorSlots runas = [filter (\r -> runeSlot r == slot) runas | slot <- [1..6]]

ordena_pega10 :: String -> [Rune] -> [Rune]
ordena_pega10 stat runas = 
  take 10 $ reverse $ sortBy (comparing (somador_main_sub stat)) runas 

total_combinação :: String -> [Rune] -> Int
total_combinação stat runas = sum $ map (somador_main_sub stat) runas

-- runas aleatórias pra teste
runa1 :: Rune
runa1 = Rune 1 1 "Violent" "ATK" 100 "HP" 200 "DEF" 150 "SPD" 10 "CR" 5

runa2 :: Rune
runa2 = Rune 2 1 "Swift" "HP" 300 "ATK" 80 "DEF" 100 "SPD" 15 "CD" 12

runa3 :: Rune
runa3 = Rune 3 2 "Violent" "DEF" 120 "HP" 250 "ATK" 90 "SPD" 8 "ACC" 20

spec :: Spec
spec = do
  describe "procura_substatus" $ do
    it "valor substat 1" $ do
      procura_substatus "HP" runa1 `shouldBe` 200
    
    it "valor substat 2" $ do
      procura_substatus "DEF" runa1 `shouldBe` 150
    
    it "valor substat 3" $ do
      procura_substatus "SPD" runa1 `shouldBe` 10
    
    it "valor substat 4" $ do
      procura_substatus "CR" runa1 `shouldBe` 5

  describe "procura_mainstatus" $ do
    it "encontra mainstat correto" $ do
      procura_mainstatus "ATK" runa1 `shouldBe` 100
    
    it "retorna 0 para mainstat diferente" $ do
      procura_mainstatus "DEF" runa1 `shouldBe` 0

  describe "somador_main_sub" $ do
    it "soma main + sub" $ do
      somador_main_sub "ATK" runa2 `shouldBe` 80
    
    it "retorna só a main" $ do
      somador_main_sub "ATK" runa1 `shouldBe` 100
    
    it "retorna só o sub" $ do
      somador_main_sub "HP" runa1 `shouldBe` 200
    
    it "não retorna nada" $ do
      somador_main_sub "INEXISTENTE" runa1 `shouldBe` 0

  describe "separarPorSlots" $ do
    it "separar em 6 listas" $ do
      let resultado = separarPorSlots [runa1, runa2, runa3]
      length resultado `shouldBe` 6
    
    it "coloca runas nos slots corretos" $ do
      let resultado = separarPorSlots [runa1, runa2, runa3]
      length (resultado !! 0) `shouldBe` 2  -- 2 tipo 1
      length (resultado !! 1) `shouldBe` 1  -- 1 tipo 2
      length (resultado !! 2) `shouldBe` 0  -- nehnuma tipo 3
    
  describe "ordena_pega10" $ do
    it "ordenação dos status" $ do
      let resultado = ordena_pega10 "ATK" [runa1, runa2]
      head resultado `shouldBe` runa1
    
    it "verificação se tá pegando 10 runas" $ do
      let runas = replicate 15 runa1
      let resultado = ordena_pega10 "ATK" runas
      length resultado `shouldBe` 10

  describe "total_combinação" $ do
    it "soma dos valores" $ do
      let resultado = total_combinação "ATK" [runa1, runa2]
      resultado `shouldBe` 180  

main :: IO ()
main = hspec spec