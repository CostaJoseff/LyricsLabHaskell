{-# LANGUAGE DeriveGeneric #-}
module Objetos.Banda where

import Data.Aeson
import GHC.Generics



data Banda = Banda {

    nome:: String,
    composicaoAtual :: [String],
    artistasAnteriores :: [String],
    musicas :: [String],
    instrumentos :: [String],
    dataFundacao :: String,
    genero :: String


}deriving (Generic)

instance FromJSON Banda
instance ToJSON Banda
instance Show Banda where
    show (Banda nome composicaoAtual artistasAnteriores musicas instrumentos dataFundacao genero) = 
        "___________________________________\n" ++
        "Nome: " ++ nome ++ "\n" ++
        "Composicao Atual: " ++ show composicaoAtual ++ "\n" ++
        "Artistas Anteriores: " ++ show artistasAnteriores ++ "\n" ++ 
        "Musicas: " ++ show musicas ++ "\n" ++
        "Instrumentos: " ++ show instrumentos ++ "\n" ++
        "dataFundacao: " ++ dataFundacao ++ "\n" ++
        "Genero: " ++ genero ++ "\n" 
