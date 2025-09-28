  {-# LANGUAGE OverloadedStrings #-}

  import Database.PostgreSQL.Simple
  import Database.PostgreSQL.Simple.FromRow
  import Web.Scotty
  import Network.Wai.Middleware.Cors
  import Network.Wai.Middleware.RequestLogger (logStdoutDev)
  import qualified Data.Text as T
  import qualified Data.Text.Lazy as TL
  import Data.Aeson (object, (.=), ToJSON(..), FromJSON)
  import Control.Monad.IO.Class (liftIO)
  import Data.List (sortBy)
  import Data.Ord (comparing)
  import Data.List (maximumBy)

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

  instance FromRow Rune where
    fromRow = Rune
      <$> field <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field <*> field

  instance ToJSON Rune where
    toJSON rune = object
      [ "id" .= runeId rune
      , "slot" .= runeSlot rune
      , "setName" .= runeSetName rune
      , "mainStat" .= runeMainStat rune
      , "mainValue" .= runeMainValue rune
      , "sub1" .= runeSub1 rune
      , "sub1Value" .= runeSub1Value rune
      , "sub2" .= runeSub2 rune
      , "sub2Value" .= runeSub2Value rune
      , "sub3" .= runeSub3 rune
      , "sub3Value" .= runeSub3Value rune
      , "sub4" .= runeSub4 rune
      , "sub4Value" .= runeSub4Value rune
      ]

  -- conectar no banco de dados
  conectarBanco :: IO Connection
  conectarBanco = do
      conn <- connect defaultConnectInfo
          { connectHost = "localhost"
          , connectPort = 5432
          , connectUser = "postgres"
          , connectPassword = "30abril01"
          , connectDatabase = "trabandrea"
          }
      putStrLn "Conseguiu conectar no banco de dados"
      return conn

  busca_runa :: Connection -> IO [Rune]
  busca_runa conn = do
      runes <- query_ conn 
        "SELECT id, slot, set_name, main_stat, main_stat_value, \
        \substat_1, substat_1_value, substat_2, substat_2_value, \
        \substat_3, substat_3_value, substat_4, substat_4_value \
        \FROM runes" :: IO [Rune]
      return runes

  -- função para separar runas por slots
  separarPorSlots :: [Rune] -> [[Rune]]
  separarPorSlots runas = [filter (\r -> runeSlot r == slot) runas | slot <- [1..6]]
  -- tá, depois disso a gente fica então com 6 listas, cada uma com as runas do slot correspondente

  -- Aqui eu mando uma runa indiivdual e ela procura o valor certo e retorna um inteiro
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

  -- aqui eu ordeno do maior pro menor, inverto, pego os 10 primeiros e retorno
  ordena_pega10 :: String -> [Rune] -> [Rune]
  ordena_pega10 stat runas = 
    take 10 $ reverse $ sortBy (comparing (somador_main_sub stat)) runas 

  -- aqui é o nosso controle, ele q vai chamar as coisas dessa parte do programa
  controle_top10 :: String -> [Rune] -> [[Rune]]
  controle_top10 stat runas = map (ordena_pega10 stat) (separarPorSlots runas)
  
  -- AQUI TEMOS TODA A GERAÇÃO, precismaos de um controle do mob pra chamar o controle de runa
  controle_tipo_mob :: String -> [Rune] -> String -> [[Rune]]
  controle_tipo_mob stat runas seet =
    let runas_filtroseet = if seet == "Sem_seet" then runas else filter(\r -> runeSetName r == seet) runas
    in 
      if stat == "ATK" then controle_top10 "ATK" runas_filtroseet
      else if stat == "DEF" then controle_top10 "DEF" runas_filtroseet
      else if stat == "HP" then controle_top10 "HP" runas_filtroseet
      else if stat == "SPD" then controle_top10 "SPD" runas_filtroseet
      else separarPorSlots runas -- isso aqui não acontece nunca, mas dá erro se tirar
  -- aqui se tudo der certo eu retorno 6 listas com as 10 runas com atributo maior daquele substatus

  total_combinação :: String -> [Rune] -> Int
  total_combinação stat runas  = sum $ map (somador_main_sub stat) runas

  realiza_combinação :: [[Rune]] -> String -> [Rune]  
  realiza_combinação runas stat = 
    let combinações = sequence runas
        combinacoes2 = [(combinacao,total_combinação stat combinacao) | combinacao <- combinações]
        (melhor_combinacao, _) = maximumBy (comparing snd) combinacoes2
    in melhor_combinacao

  main :: IO ()
  main = do
      conn <- conectarBanco
      putStrLn "conectado ao postgre"

      -- puxar as runas do banco de dados
      runas <- busca_runa conn
      
      -- realizar a conexão com o webservice
      scotty 3000 $ do
        middleware logStdoutDev
        
        -- Ligação com o elm
        middleware simpleCors

        -- rota -> tem q tiar isso deois eu acho
        get "/requisitos/:seet_escolha/:objetivoMOB" $ do
          runas_atual <- liftIO $ busca_runa conn
          seet_escolha <- pathParam "seet_escolha"
          objetivoMOB <- pathParam "objetivoMOB"

          let runas_filtradas = controle_tipo_mob objetivoMOB runas_atual seet_escolha  
          let melhor_combinacao = realiza_combinação runas_filtradas objetivoMOB
          let valor_total = total_combinação objetivoMOB melhor_combinacao

          json $ object 
            [ "tipoRuna" .= seet_escolha
            , "stat_escolhido" .= objetivoMOB
            , "valor_total" .= valor_total 
            , "melhor_combinação" .= melhor_combinacao
            ]