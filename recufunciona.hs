import Data.Data (Data)
import Text.Show.Functions () 
 
data Aventurero = UnAventurero{
    nombre :: String,
    carga :: Int,
    salud :: Int,
    coraje :: Bool,
    criterioDeSeleccionDeEncuentros :: [Criterio]
} deriving Show
type Criterio = Aventurero -> Bool --[conformista, valiente, lightpacker]

modificarCarga :: (Int -> Int) -> Aventurero -> Aventurero
modificarCarga unaFuncion unAventurero= unAventurero { carga = unaFuncion . carga $ unAventurero }

modificarSalud :: (Int -> Int) -> Aventurero -> Aventurero
modificarSalud unaFuncion unAventurero= unAventurero { salud = unaFuncion . salud $ unAventurero }

modificarCoraje :: (Bool -> Bool) -> Aventurero -> Aventurero
modificarCoraje unaFuncion unAventurero= unAventurero { coraje = unaFuncion . coraje $ unAventurero }

--despues de aplciar el encuentro al aventurero: (luego modelo el encuentro y tengo q agregarlo)
--creoq tendria q pasarle un encuentro (que es aventurero -> aventurero)
conformista :: Aventurero -> Bool
conformista unAventurero = True

valiente :: Aventurero -> Bool
valiente unAventurero = coraje unAventurero || ((>100) . salud) unAventurero

type Carga = Int
ligthpacker :: Carga -> Aventurero -> Bool
ligthpacker unaCargaParticular unAventurero = ((>unaCargaParticular) . carga) unAventurero   

pepita = UnAventurero "pepita" 30 100 False [conformista, valiente]
jose = UnAventurero "jose" 41 200 True [conformista]


-- punto 2
type Condicion = Aventurero -> Bool
existeAlgunAventureroQue ::[Aventurero] -> Bool
existeAlgunAventureroQue unosAventureros = any tieneNombreMas5Letras unosAventureros

tieneNombreMas5Letras :: Aventurero -> Bool
tieneNombreMas5Letras unAventurero =  (>5). length . nombre $ unAventurero

cargaTotalSegun :: [Aventurero] -> Int
cargaTotalSegun unosAventureros = sum . map carga . filter tieneCargaPar $ unosAventureros

tieneCargaPar :: Aventurero -> Bool
tieneCargaPar unAventurero = even . carga $ unAventurero


--punto 3
type Encuentro = Aventurero -> Aventurero

curandero :: Encuentro
curandero unAventurero = modificarSalud (+(20 * salud unAventurero `div` 100)). modificarCarga ((`div` 2).subtract 1) $ unAventurero

inspirador :: Encuentro
inspirador unAventurero = modificarSalud (+(10 * salud unAventurero `div` 100)). modificarCoraje (const True) $ unAventurero

embaucador :: Encuentro
embaucador unAventurero = modificarCriterio ((ligthpacker 10) :). modificarSalud (`div` 2) . modificarCarga ((+10).subtract 1) . modificarCoraje (const False) $ unAventurero

modificarCriterio :: ([Criterio] -> [Criterio]) -> Aventurero -> Aventurero
modificarCriterio unaFuncion unAventurero = unAventurero { criterioDeSeleccionDeEncuentros = unaFuncion . criterioDeSeleccionDeEncuentros $ unAventurero }


--punto 4

-- enfrentaEncuentro :: Aventurero -> [Encuentro] -> Bool
-- enfrentaEncuentro unAventurero listaDeEncuentros =  map($ unAventurero) listaDeEncuentros


{-
criterioDeSeleccionDeEncuentros :: [Criterio]
} deriving Show
type Criterio = Aventurero -> Bool --[conformista, valiente, lightpacker]
    Encuentro = Aventurero -> Aventurero [curandero, inspirador]

pepito = UnAventurero "pepito" 6 50 False [valiente]

enfrentaEncuentro pepito [curandero, inspirador]
    pepito cumple con curandero (energia >50)
    pepito cumple con inspirador 
    pepito no cumple (false ) con (corta)


-}


