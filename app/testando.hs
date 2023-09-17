import LyricsLibService
import Objetos.Banda
import Bandas.BandaService

main :: IO()
main = do
    --banda <- buscarBanda "Salvo3333"

    --print banda

    --salvaBanda (Banda "Salvo2" ["Eu2","tu2"] ["Eu2"] ["dea2"]  ["Guitarra"] "12/12/12" "rock1")


    --bandas <- filtrarBandaPorArtista "Eu1"

    --print bandas

    resultado <- adicionarInstrumentoEmBanda  "NOVOINSTRUMENTO" "Salvo2"

    print resultado