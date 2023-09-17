module Main (main) where

import Lib

import LyricsLibService as LLS
import Control.Exception
import Text.Read (readMaybe)
import Control.Concurrent (threadDelay)
import Data.Char
import System.Process (system)

limparTerminal:: IO ()
limparTerminal = do
  system "clear"
  return ()

main::IO()
main = do
  limparTerminal
  apresentacaoInicial
  fluxoPrincipal

apresentacaoInicial:: IO()
apresentacaoInicial = do
  print("Bem vindo ao Lyrics-LIB")

fluxoPrincipal:: IO()
fluxoPrincipal = do
  limparTerminal
  resultado <- menuInicial
  if resultado == "0" then do
    print ("Ate mais!")
    threadDelay (2 * 1000000)
    limparTerminal
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
        fluxoPrincipal
      "14" -> do --Adicionar Integrantes na Banda
        print ("###")
        fluxoPrincipal
      "15" -> do --Remover Integrantes na Banda
        print ("###")
        fluxoPrincipal
      "21" -> do --Buscar Artista
        menuBuscarArtista
        fluxoPrincipal
      "22" -> do --Buscar Banda
        menuBuscarBanda
        fluxoPrincipal
      "23" -> do --Buscar Musica
        menuBuscarMusica
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
        menuTopArtistas
        fluxoPrincipal
      "28" -> do --Top Bandas
        menuTopBandas
        fluxoPrincipal
      "29" -> do --Top Musicas
        menuTopMusicas
        fluxoPrincipal

menuInicial:: IO (String)
menuInicial = do
  limparTerminal
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
          threadDelay (2 * 1000000)
          menuInicial
    Nothing -> do
      putStrLn("\n\nOpcao invalida!\n\n")
      threadDelay (2 * 1000000)
      menuInicial

menuCadastros:: String -> IO (String)
menuCadastros num1 = do
  limparTerminal
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
          threadDelay (2 * 1000000)
          menuCadastros num1
    Nothing -> do
      putStrLn("\n\nOpcao invalida!\n\n")
      threadDelay (2 * 1000000)
      menuCadastros num1

menuBuscas:: String -> IO (String)
menuBuscas num1 = do
  limparTerminal
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
          threadDelay (2 * 1000000)
          menuBuscas num1

    Nothing -> do
      putStrLn("\n\nOpcao invalida!\n\n")
      threadDelay (2 * 1000000)
      menuBuscas num1


menuDashBoard:: IO (String)
menuDashBoard = return ("Em Breve!")

menuBuscarArtista:: IO ()
menuBuscarArtista = do
  limparTerminal
  putStrLn("Informe o nome do artista:")
  nome <- getLine
  resultado <- LLS.buscarArtistaPorNome nome
  limparTerminal
  listarArtistas resultado
  putStrLn("Enter para continuar.")
  getLine
  return ()

listarArtistas:: [String] -> IO ()
listarArtistas [] = return ()
listarArtistas (artista:resto) = do
  putStrLn (artista)
  listarArtistas resto

menuBuscarBanda:: IO ()
menuBuscarBanda = do
  limparTerminal
  putStrLn("Informe o nome da banda:")
  nome <- getLine
  resultado <- LLS.buscarBanda nome
  putStrLn (resultado)

menuBuscarMusica:: IO ()
menuBuscarMusica = do
  limparTerminal
  putStrLn("Informe o nome da musica:")
  nome <- getLine
  resultado <- LLS.buscarMusica nome
  putStrLn (resultado)

menuTopArtistas:: IO ()
menuTopArtistas = do
  limparTerminal
  putStrLn("Informe a quantidade de artistas que devem aparecer no TOP:")
  quantidade <- getLine
  let quantidadeInt = readMaybe quantidade :: Maybe Int
  case quantidadeInt of
    Just numero -> do
      if numero <= 0 then do
        putStrLn("\n\nOpcao invalida!\n\n")
        threadDelay (2 * 1000000)
        menuTopArtistas
      else do
        artistasTop <- LLS.topArtistas numero
        putStrLn (show artistasTop)
    Nothing -> do
      putStrLn("\n\nOpcao invalida!\n\n")
      threadDelay (2 * 1000000)
      menuTopArtistas

menuTopBandas:: IO ()
menuTopBandas = do
  limparTerminal
  putStrLn("Informe a quantidade de bandas que devem aparecer no TOP:")
  quantidade <- getLine
  let quantidadeInt = readMaybe quantidade :: Maybe Int
  case quantidadeInt of
    Just numero -> do
      if numero <= 0 then do
        putStrLn("\n\nOpcao invalida!\n\n")
        threadDelay (2 * 1000000)
        menuTopMusicas
      else do
        bandasTop <- LLS.topBandas numero
        putStrLn (show bandasTop)
    Nothing -> do
      putStrLn("\n\nOpcao invalida!\n\n")
      threadDelay (2 * 1000000)
      menuTopBandas

menuTopMusicas:: IO ()
menuTopMusicas = do
  limparTerminal
  putStrLn("Informe a quantidade de musicas que devem aparecer no TOP:")
  quantidade <- getLine
  let quantidadeInt = readMaybe quantidade :: Maybe Int
  case quantidadeInt of
    Just numero -> do
      if numero <= 0 then do
        putStrLn("\n\nOpcao invalida!\n\n")
        threadDelay (2 * 1000000)
        menuTopMusicas
      else do
        musicasTop <- LLS.topMusicas numero
        putStrLn (show musicasTop)
    Nothing -> do
      putStrLn("\n\nOpcao invalida!\n\n")
      threadDelay (2 * 1000000)
      menuTopMusicas

menuFiltrarArtistas:: IO()
menuFiltrarArtistas = do
  limparTerminal
  putStrLn("-----Filtrar Artistas-----")
  putStrLn("Opcao unica - filtrar artista por funcao")
  putStrLn("\nIndique a funcao a ser filtrada:")
  funcao <- getLine
  resultado <- LLS.filtrarArtistasPorFuncao funcao
  listarArtistas resultado
  putStrLn("Enter para continuar.")
  getLine
  return ()

menuFiltrarBandas:: IO()
menuFiltrarBandas = do
  limparTerminal
  putStrLn("-----Filtrar Bandas-----")
  putStrLn("1 - Por genero")
  putStrLn("2 - Por instrumento")
  entrada <- getLine
  let entradaInt = readMaybe entrada :: Maybe Int
  case entradaInt of
    Just numero -> do
      case numero of
        1 -> do
          limparTerminal
          putStrLn("Indique o genero a ser filtrado:")
          genero <- getLine
          resultado <- LLS.filtrarBandasPorGenero genero
          putStrLn (show resultado)
        2 -> do
          limparTerminal
          putStrLn("Indique o instrumento a ser filtrado:")
          genero <- getLine
          resultado <- LLS.filtrarBandasPorInstrumento genero
          putStrLn (show resultado)
        _ -> do
          putStrLn("\n\nOpcao invalida!\n\n")
          threadDelay (2 * 1000000)
          menuFiltrarBandas
    Nothing -> do
      putStrLn("\n\nOpcao invalida!\n\n")
      threadDelay (2 * 1000000)
      menuFiltrarBandas

menuFiltrarMusicas:: IO ()
menuFiltrarMusicas = do
  limparTerminal
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
          limparTerminal
          putStrLn("Indique o ritmo a ser filtrado:")
          ritmo <-getLine
          resultado <- LLS.filtrarMusicasPorRitmo ritmo
          putStrLn (show resultado)
        2 -> do
          limparTerminal
          putStrLn("Indique o instrumento a ser filtrado:")
          instrumento <-getLine
          resultado <- LLS.filtrarMusicasPorInstrumento instrumento
          putStrLn (show resultado)
        3 -> do
          limparTerminal
          putStrLn("Indique o trecho a ser filtrado:")
          trecho <-getLine
          resultado <- LLS.filtrarMusicasPorTrecho trecho
          putStrLn (show resultado)
        _ -> do
          putStrLn("\n\nOpcao invalida!\n\n")
          threadDelay (2 * 1000000)
          menuFiltrarMusicas

    Nothing -> do
      putStrLn("\n\nOpcao invalida!\n\n")
      threadDelay (2 * 1000000)
      menuFiltrarMusicas

menuCadastrarArtista:: IO ()
menuCadastrarArtista = do
  limparTerminal
  putStrLn("Nome do artista")
  nome <- getLine
  limparTerminal
  putStrLn("Nome da Banda Atual")
  nomeBanda <- getLine
  bandasAnteriores <- obterListaDeBandas
  limparTerminal
  putStrLn("Informe a funcao do artista:")
  funcao <- getLine
  LLS.cadastrarArtista [nome, nomeBanda, funcao] bandasAnteriores

obterListaDeBandas:: IO [String]
obterListaDeBandas = do
  limparTerminal
  putStrLn("Participou de outras Bandas? (s/n)")
  resposta <- getLine
  case (map toUpper resposta) of
    "S" -> do
      limparTerminal
      putStrLn("Insira os nomes das bandas separados por espaco")
      bandas <- getLine
      return (words bandas)
    "N" -> do
      return []
    _ -> do
      putStrLn("\n\nOpcao invalida!\n\n")
      threadDelay (2 * 1000000)
      obterListaDeBandas

menuCadastrarBanda:: IO ()
menuCadastrarBanda = do
  limparTerminal
  putStrLn("Nome da banda:")
  nome <- getLine
  artistasAtuais <- obterComposicaoAtual
  artistasAnteriores <- menuArtistasAnteriores
  musicas <- menuMusicas
  instrumentos <- obterInstrumentos
  limparTerminal
  putStrLn("Data da fundacao da banda:")
  dataFund <- getLine
  limparTerminal
  putStrLn("Genero geral da banda:")
  genero <- getLine
  LLS.cadastrarBanda [nome, dataFund, genero] artistasAtuais artistasAnteriores musicas instrumentos


obterComposicaoAtual:: IO [String]
obterComposicaoAtual = do
  limparTerminal
  putStrLn("Insira os ID's dos artistas separados por espaco")
  idsArtistas <- getLine
  return (words idsArtistas)

obterInstrumentos:: IO [String]
obterInstrumentos = do
  limparTerminal
  putStrLn("Insira os instrumentos utilizados pela banda separados por espaco")
  instrumentos <- getLine
  return (words instrumentos)

menuArtistasAnteriores:: IO [String]
menuArtistasAnteriores = do
  limparTerminal
  putStrLn("A banda ja possuiu outros artistas alem dos atuais? (s/n)")
  resposta <- getLine
  case (map toUpper resposta) of
    "S" -> do
      limparTerminal
      putStrLn("Insira os ID's dos artistas separados por espaco")
      idsArtistas <- getLine
      return (words idsArtistas)
    "N" -> do
      return []
    _ -> do
      putStrLn("Opcao invalida!")
      menuArtistasAnteriores

menuMusicas:: IO [String]
menuMusicas = do
  limparTerminal
  putStrLn("A banda possui musicas? (s/n)")
  resposta <- getLine
  case (map toUpper resposta) of
    "S" -> do
      limparTerminal
      putStrLn("Insira os ID's das musicas separadas por espaco")
      idsMusicas <- getLine
      return (words idsMusicas)
    "N" -> do
      return []
    _ -> do
      putStrLn("Opcao invalida!")
      menuMusicas
