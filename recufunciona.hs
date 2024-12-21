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
ligthpacker unaCargaParticular = (>unaCargaParticular) . carga

pepita = UnAventurero "pepita" 30 100 False [conformista, valiente]
jose = UnAventurero "jose" 41 200 True [conformista]


-- punto 2
type Condicion = Aventurero -> Bool
existeAlgunAventureroQue ::[Aventurero] -> Bool
existeAlgunAventureroQue = any tieneNombreMas5Letras

tieneNombreMas5Letras :: Aventurero -> Bool
tieneNombreMas5Letras = (>5). length . nombre

cargaTotalSegun :: [Aventurero] -> Int
cargaTotalSegun = sum . map carga . filter tieneCargaPar

tieneCargaPar :: Aventurero -> Bool
tieneCargaPar = even . carga


--punto 3
type Encuentro = Aventurero -> Aventurero

curandero :: Encuentro
curandero unAventurero = modificarSalud (+(20 * salud unAventurero `div` 100)). modificarCarga ((`div` 2).subtract 1) $ unAventurero

inspirador :: Encuentro
inspirador unAventurero = modificarSalud (+(10 * salud unAventurero `div` 100)). modificarCoraje (const True) $ unAventurero

embaucador :: Encuentro
embaucador = modificarCriterio (ligthpacker 10 :). modificarSalud (`div` 2) . modificarCarga ((+10).subtract 1) . modificarCoraje (const False)

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


