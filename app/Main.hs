module Main (main) where

import Lib

import LyricsLibService as LLS
import Control.Exception
import Text.Read (readMaybe)
import Control.Concurrent (threadDelay)
import Data.Char
import Data.List.Split

main::IO()
main = do
  apresentacaoInicial
  fluxoPrincipal

fluxoPrincipal:: IO()
fluxoPrincipal = do
  resultado <- menuInicial
  if resultado == "0" then print ("Até mais!")
  else do
    case resultado of
      "11" -> do --Cadastrar Artista
        menuCadastrarArtista
        fluxoPrincipal
      "12" -> do --Cadastrar Banda
        menuCadastrarBanda
        fluxoPrincipal
      "13" -> do --Cadastrar Musica
        print ("###")
      "14" -> do --Adicionar Integrantes na Banda
        print ("###")
      "15" -> do --Remover Integrantes na Banda
        print ("###")
      "21" -> do --Buscar Artista
        putStrLn("Informe o nome do artista:")
        nome <- getLine
        resultado <- LLS.buscarArtistaPorNome nome
        putStrLn (show resultado)
        fluxoPrincipal
      "22" -> do --Buscar Banda
        putStrLn("Informe o nome da banda:")
        nome <- getLine
        resultado <- LLS.buscarBanda nome
        putStrLn (resultado)
        fluxoPrincipal
      "23" -> do --Buscar Musica
        putStrLn("Informe o nome da musica:")
        nome <- getLine
        resultado <- LLS.buscarMusica nome
        putStrLn (resultado)
        fluxoPrincipal
      "24" -> do --Filtrar Artistas
        menuFiltrarArtistas
        fluxoPrincipal
      "25" -> do --Filtrar Bandas
        menuFiltrarBandas
        fluxoPrincipal
      "26" -> do --Filtrar Musicas
        menuFiltrarMusicas
        fluxoPrincipal
      "27" -> do --Top Artistas
        putStrLn("Informe a quantidade de artistas que devem aparecer no TOP:")
        quantidade <- getLine
        let quantidadeInt = readMaybe quantidade :: Maybe Int
        case quantidadeInt of
          Just numero -> do
            artistasTop <- LLS.topArtistas numero
            putStrLn (show artistasTop)
          Nothing -> do
            putStrLn("\n\nOpcao invalida!\n\n")
            fluxoPrincipal
      "28" -> do --Top Bandas
        putStrLn("Informe a quantidade de bandas que devem aparecer no TOP:")
        quantidade <- getLine
        let quantidadeInt = readMaybe quantidade :: Maybe Int
        case quantidadeInt of
          Just numero -> do
            bandasTop <- LLS.topBandas numero
            putStrLn (show bandasTop)
          Nothing -> do
            putStrLn("\n\nOpcao invalida!\n\n")
            fluxoPrincipal
      "29" -> do --Top Musicas
        putStrLn("Informe a quantidade de musicas que devem aparecer no TOP:")
        quantidade <- getLine
        let quantidadeInt = readMaybe quantidade :: Maybe Int
        case quantidadeInt of
          Just numero -> do
            musicasTop <- LLS.topMusicas numero
            putStrLn (show musicasTop)
          Nothing -> do
            putStrLn("\n\nOpcao invalida!\n\n")
            fluxoPrincipal

apresentacaoInicial:: IO()
apresentacaoInicial = do
  print("Bem vindo ao Lyrics-LIB")
  threadDelay (2 * 1000000)
  putStrLn $ replicate 50 '\n'

menuInicial:: IO (String)
menuInicial = do
  putStrLn("----------")
  putStrLn("1 - Cadastros")
  putStrLn("2 - Buscas")
  putStrLn("3 - DashBoard")
  putStrLn("0 - Sair")
  putStrLn("----------\n\n")
  entrada <- getLine
  let entradaInt = readMaybe entrada :: Maybe Int
  case entradaInt of
    Just numero -> do
      case numero of
        1 -> do
          menuCadastros "1"
        2 -> do
          menuBuscas "2"
        3 -> do
          menuDashBoard
        0 -> return ("0")
        _ -> do
          putStrLn("\n\nOpcao invalida!\n\n")
          menuInicial
    Nothing -> do
      putStrLn("\n\nOpcao invalida!\n\n")
      menuInicial

menuCadastros:: String -> IO (String)
menuCadastros num1 = do
  putStrLn("----------")
  putStrLn("1 - Cadastrar Artista")
  putStrLn("2 - Cadastrar Banda")
  putStrLn("3 - Cadastrar Musica")
  putStrLn("----------")
  putStrLn("4 - Adicionar Integrantes na Banda")
  putStrLn("5 - Remover Integrantes na Banda")
  putStrLn("----------\n\n")
  entrada <- getLine
  let entradaInt = readMaybe entrada :: Maybe Int
  case entradaInt of
    Just numero -> do
      case numero of
        1 -> return (num1++"1")
        2 -> return (num1++"2")
        3 -> return (num1++"3")
        4 -> return (num1++"4")
        5 -> return (num1++"5")
        _ -> do
          putStrLn("\n\nOpcao invalida!\n\n")
          menuCadastros num1
    Nothing -> do
      putStrLn("\n\nOpcao invalida!\n\n")
      menuCadastros num1

menuBuscas:: String -> IO (String)
menuBuscas num1 = do
  putStrLn("----------")
  putStrLn("1 - Buscar Artista")
  putStrLn("2 - Buscar Banda")
  putStrLn("3 - Buscar Musica")
  putStrLn("----------")
  putStrLn("4 - Filtrar artistas")
  putStrLn("5 - Filtrar Bandas")
  putStrLn("6 - Filtrar Musicas")
  putStrLn("----------")
  putStrLn("7 - Top Artistas")
  putStrLn("8 - Top Bandas")
  putStrLn("9 - Top Musicas")
  putStrLn("----------\n\n")
  entrada <- getLine
  let entradaConvertida = readMaybe entrada :: Maybe Int
  case entradaConvertida of
    Just numero -> do
      case numero of
        1 -> return (num1++"1")
        2 -> return (num1++"2")
        3 -> return (num1++"3")
        4 -> return (num1++"4")
        5 -> return (num1++"5")
        6 -> return (num1++"6")
        7 -> return (num1++"7")
        8 -> return (num1++"8")
        9 -> return (num1++"9")
        _ -> do
          putStrLn("\n\nOpcao invalida!\n\n")
          menuBuscas num1

    Nothing -> do
      putStrLn("\n\nOpcao invalida!\n\n")
      menuBuscas num1


menuDashBoard:: IO (String)
menuDashBoard = return ("Em Breve!")

menuFiltrarArtistas:: IO()
menuFiltrarArtistas = do
  putStrLn("-----Filtrar Artistas-----")
  putStrLn("Opcao unica - filtrar artista por funcao")
  putStrLn("\nIndique a funcao a ser filtrada:")
  funcao <- getLine
  resultado <- LLS.filtrarArtistasPorFuncao funcao
  putStrLn (show resultado)

menuFiltrarBandas:: IO()
menuFiltrarBandas = do
  putStrLn("-----Filtrar Bandas-----")
  putStrLn("1 - Por genero")
  putStrLn("2 - Por instrumento")
  entrada <- getLine
  let entradaInt = readMaybe entrada :: Maybe Int
  case entradaInt of
    Just numero -> do
      case numero of
        1 -> do
          putStrLn("Indique o genero a ser filtrado:")
          genero <- getLine
          resultado <- LLS.filtrarBandasPorGenero genero
          putStrLn (show resultado)
        2 -> do
          putStrLn("Indique o instrumento a ser filtrado:")
          genero <- getLine
          resultado <- LLS.filtrarBandasPorInstrumento genero
          putStrLn (show resultado)
        _ -> do
          putStrLn("\n\nOpcao invalida!\n\n")
          menuFiltrarBandas
    Nothing -> do
      putStrLn("\n\nOpcao invalida!\n\n")
      menuFiltrarBandas

menuFiltrarMusicas:: IO ()
menuFiltrarMusicas = do
  putStrLn("-----Filtrar Musicas-----")
  putStrLn("1 - Por ritmo")
  putStrLn("2 - Por instrumento")
  putStrLn("3 - Por trecho")
  entrada <- getLine
  let entradaInt = readMaybe entrada :: Maybe Int
  case entradaInt of
    Just numero -> do
      case numero of
        1 -> do
          putStrLn("Indique o ritmo a ser filtrado:")
          ritmo <-getLine
          resultado <- LLS.filtrarMusicasPorRitmo ritmo
          putStrLn (show resultado)
        2 -> do
          putStrLn("Indique o instrumento a ser filtrado:")
          instrumento <-getLine
          resultado <- LLS.filtrarMusicasPorInstrumento instrumento
          putStrLn (show resultado)
        3 -> do
          putStrLn("Indique o trecho a ser filtrado:")
          trecho <-getLine
          resultado <- LLS.filtrarMusicasPorTrecho trecho
          putStrLn (show resultado)
        _ -> do
          putStrLn("\n\nOpcao invalida!\n\n")
          menuFiltrarMusicas

    Nothing -> do
      putStrLn("\n\nOpcao invalida!\n\n")
      menuFiltrarMusicas

menuCadastrarArtista:: IO ()
menuCadastrarArtista = do
  putStrLn("Nome do artista")
  nome <- getLine
  putStrLn("Nome da Banda Atual")
  nomeBanda <- getLine
  putStrLn("Participou de outras Bandas? (s/n)")
  resposta <- getLine
  case (map toUpper resposta) of
    "S" -> do
      putStrLn("Digite o nome das bandas e ao acabar digite ~~fim (com ~~):")
      bandasAnteriores <- obterListaDeBandas [] 1
      putStrLn("Informe a funcao do artista:")
      funcao <- getLine
      LLS.cadastrarArtista [nome, nomeBanda, funcao] bandasAnteriores
    "N" -> do
      putStrLn("Informe a funcao do artista:")
      funcao <- getLine
      LLS.cadastrarArtista [nome, nomeBanda, funcao] []
    _ -> do
      putStrLn("Opcao invalida, abortando cadastro!")

obterListaDeBandas:: [String] -> Int -> IO [String]
obterListaDeBandas resultado indice = do
  putStrLn("Banda " ++ (show indice) ++ ":")
  banda <- getLine
  if banda == "~~fim" then return (resultado)
  else obterListaDeBandas (banda:resultado) (indice+1)

menuCadastrarBanda:: IO ()
menuCadastrarBanda = do
  putStrLn("Nome da banda:")
  nome <- getLine
  artistasAtuais <- obterComposicaoAtual
  artistasAnteriores <- menuArtistasAnteriores
  musicas <- menuMusicas
  instrumentos <- obterInstrumentos
  putStrLn("Data da fundacao da banda:")
  dataFund <- getLine
  putStrLn("Genero geral da banda:")
  genero <- getLine
  LSS.cadastrarBanda [nome, dataFund, genero] artistasAtuais artistasAnteriores musicas instrumentos


obterComposicaoAtual::[String]
obterComposicaoAtual = do
  putStrLn("Insira os ID's dos artistas separados por espaco")
  idsArtistas <- getLine
  return (splitOn " " idsArtistas)

obterInstrumentos:: IO [String]
obterInstrumentos = do
  putStrLn("Insira os instrumentos utilizados pela banda separados por espaco")
  instrumentos <- getLine
  return (splitOn " " instrumentos)

menuArtistasAnteriores:: IO [String]
menuArtistasAnteriores = do
  putStrLn("A banda ja possuiu outros artistas alem dos atuais? (s/n)")
  resposta <- getLine
  case (map toUpper resposta) of
    "S" -> do
      putStrLn("Insira os ID's dos artistas separados por espaco")
      idsArtistas <- getLine
      return (splitOn " " idsArtistas)
    "N" -> do
      return []
    _ -> do
      putStrLn("Opcao invalida!")
      menuArtistasAnteriores

menuMusicas:: IO [String]
menuMusicas = do
  putStrLn("A banda possui musicas? (s/n)")
  resposta <- getLine
  case (map toUpper resposta) of
    "S" -> do
      putStrLn("Insira os ID's das musicas separadas por espaco")
      idsMusicas <- getLine
      return (splitOn " " idsMusicas)
    "N" -> do
      return []
    _ -> do
      putStrLn("Opcao invalida!")
      menuMusicas