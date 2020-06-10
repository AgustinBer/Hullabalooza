module Lib where
import Text.Show.Functions
import Data.List

type Genero = Festival -> Festival
data Festival = UnFestival {lugar::String,cantidadPersonas::Float,estadoInicial::String,bandas :: [Banda]} deriving (Show)
data Banda = UnaBanda {descripcionExpertos :: [String],db::Int, genero::Genero}deriving (Show)

hullabalooza = UnFestival {lugar="Argentina",cantidadPersonas=10000,estadoInicial= "Indiferente",bandas=[miranda,losRedondos,metallica,soda]}

losRedondos = UnaBanda {descripcionExpertos=["legendaria","pogosa"],db=45,genero=rockNacional}
soda= UnaBanda {descripcionExpertos=["irrepetible"],db=40,genero=rockNacional}
miranda= UnaBanda {descripcionExpertos=["insipida","incolora","inodora"],db=60,genero=pop}
metallica = UnaBanda{descripcionExpertos=["legendaria","vendida"],db=60,genero=heavyMetal}

modificarEstado :: String -> Festival -> Festival
modificarEstado nuevoEstado festival = festival {estadoInicial = nuevoEstado}

aumentarPublicoEn:: Float -> Festival -> Festival
aumentarPublicoEn n festival = festival {cantidadPersonas= cantidadPersonas festival + n}

rockNacional :: Genero
rockNacional festival =  aumentarPublicoEn 100 festival

pop:: Genero
pop festival | (=="Indiferente"). estadoInicial $ festival = aumentarPublicoEn (cantidadPersonas festival).modificarEstado "euforico" $ festival
             | otherwise = festival 

heavyMetal :: Genero
heavyMetal festival = aumentarPublicoEn (cantidadPersonas festival * 0.01).modificarEstado (estadoInicial festival ++ " pesado") $ festival

trashMetal :: Genero
trashMetal festival = aumentarPublicoEn (cantidadPersonas festival * 0.01).modificarEstado (estadoInicial festival ++ " basura") $ festival

tocar :: Banda -> Genero
tocar banda festival = (genero banda) festival

--The strokes, que toca a 45 decibeles y está descripta como “suicidio asistido”, “emocional” y “linda”. No pertenece 
--a ninguno de los géneros conocidos, pero hay expertos que afirman que es una fusión musical entre el pop y el heavy metal.

theStrokes = UnaBanda {descripcionExpertos=["suicidio asistido","emocional","linda"],db=45,genero = pop.heavyMetal}

suceder :: Festival->Festival
suceder festival = foldr tocar festival (bandas festival)

clasificar :: Banda -> [String]
clasificar banda = esAcustica banda ++ esVendida banda ++ esLegendaria banda
type Clasificaciones = [String]
type Clasificar = Banda -> Clasificaciones


esVendida :: Clasificar
esVendida banda | tieneMasDe3Criticas banda || descriptaComo "vendida" banda = ["vendida"]
                | otherwise = []

tieneMasDe3Criticas banda = length (descripcionExpertos banda) > 3 
descriptaComo descripcion banda= elem descripcion (descripcionExpertos banda) 

tocaAMasDB cant banda = db banda > cant

esAcustica :: Clasificar
esAcustica banda | tocaAMasDB 55 banda = ["acustica"]
                 | otherwise = []

esLegendaria :: Clasificar
esLegendaria banda | descriptaComo "legendaria" banda && tocaAMasDB 40 banda = ["legendaria"]
                   | otherwise = []


popularidad :: Banda -> Clasificaciones -> Int
popularidad banda clasificaciones = length (intersect clasificaciones (clasificar banda)) * 100


buenFest :: Festival -> Clasificaciones -> Bool
buenFest festival clasificaciones = popularidadTotal clasificaciones festival > 1000 && bandasSegunPopularidad (bandas festival) clasificaciones

bandasSegunPopularidad :: [Banda] -> Clasificaciones -> Bool
bandasSegunPopularidad (banda1:banda2:resto) clasificaciones = popularidad banda1 clasificaciones < popularidad banda2 clasificaciones && bandasSegunPopularidad (banda2:resto) clasificaciones
bandasSegunPopularidad [banda] _ = True


popularidadTotal clasificaciones festival =sum.map (flip popularidad clasificaciones) $ (bandas festival)
