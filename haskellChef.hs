import Text.Show.Functions()

data Participante = UnParticipante {
    nombre       :: String,
    trucos       :: [Truco],
    especialidad :: Plato
} deriving Show

data Plato = UnPlato {
    dificultad  :: Int,
    componentes :: [Componente]
} deriving Show

chocotorta :: Plato
chocotorta = UnPlato {
    dificultad  = 4,
    componentes = [("harina",4),("chocolate",10), ("sal", 2)]
}

canelones :: Plato
canelones = UnPlato {
    dificultad  = 3,
    componentes = [("harina",12),("salsa",20),("ricota",4),("verdura",5),("agua",1),("salsaBlanca",20), ("sal", 4)]
}

type Truco = Plato -> Plato

type Componente = (Ingrediente, Int)
type Ingrediente = String

cambiarComponentes :: ([Componente] -> [Componente]) -> Plato -> Plato
cambiarComponentes fn plato = plato {componentes = fn.componentes $ plato}

cambiarDificultad :: (Int -> Int) -> Plato -> Plato
cambiarDificultad fn plato = plato {dificultad = fn.dificultad $ plato}

{-Parte A-}
endulzar :: Int -> Truco
endulzar gramos plato = cambiarComponentes (armarComponente "azucar" gramos:) plato

salar :: Int -> Truco
salar gramos plato = cambiarComponentes (armarComponente "sal" gramos:) plato

armarComponente :: Ingrediente -> Int -> Componente
armarComponente ingrediente gramos = (ingrediente,gramos)

darSabor :: Int -> Int -> Truco
darSabor gramosSal gramosAzucar = endulzar gramosAzucar.salar gramosSal

duplicarPorcion :: Truco
duplicarPorcion plato = cambiarComponentes duplicarGramos plato

duplicarGramos :: [Componente] -> [Componente]
duplicarGramos componentes = map duplicarGramosComponente componentes

duplicarGramosComponente :: Componente -> Componente
duplicarGramosComponente (ingrediente,gramos) = armarComponente ingrediente (gramos*2)

simplificar :: Truco
simplificar plato
    | esComplejo plato = simplificarPlato plato
    | otherwise        = plato 

simplificarPlato :: Truco
simplificarPlato = quitarComponentesLivianos.dificultadEn5

dificultadEn5 :: Truco
dificultadEn5 plato = cambiarDificultad asignarDificultadCinco plato

asignarDificultadCinco :: Int -> Int
asignarDificultadCinco dificultad = 5

quitarComponentesLivianos :: Truco
quitarComponentesLivianos plato = cambiarComponentes filtrarComponentesPesados plato

filtrarComponentesPesados :: [Componente] -> [Componente]
filtrarComponentesPesados componentes = filter componentePesado componentes

componentePesado :: Componente -> Bool
componentePesado (_,gramos) = gramos > 10

esVegano :: Plato -> Bool
esVegano plato = analizarCriterio componenteVegano (componentes plato)

analizarCriterio :: Foldable t => (a -> Bool) -> t a -> Bool
analizarCriterio criterio componentes = all criterio componentes

componenteVegano :: Componente -> Bool
componenteVegano ("carne",_) = False 
componenteVegano ("huevos",_) = False
componenteVegano ("leche",_) = False
componenteVegano ("manteca",_) = False
componenteVegano ("yogur",_) = False
componenteVegano ("queso",_) = False
componenteVegano _ = True

esSinTacc :: Plato -> Bool
esSinTacc plato = analizarCriterio componenteSinTacc (componentes plato)

componenteSinTacc :: Componente -> Bool
componenteSinTacc ("harina",_) = False
componenteSinTacc _ = True

esComplejo :: Plato -> Bool
esComplejo plato = length (componentes plato) > 5 && dificultad plato > 7

noAptoHipertension :: Plato -> Bool
noAptoHipertension plato = any componenteDaHipertension (componentes plato) 

componenteDaHipertension :: Componente -> Bool
componenteDaHipertension ("sal",gramos) = gramos > 2
componenteDaHipertension _ = False
{-Parte A-}

{-Parte B-}
pepeRonccino :: Participante
pepeRonccino = UnParticipante {
    nombre = "Pepe Ronccino",
    trucos = [darSabor 2 5, simplificar,duplicarPorcion],
    especialidad = canelones
}
{-Parte B-}

{-Parte C-}
cocinar :: Participante -> Plato
cocinar participante = foldr ($) (especialidad participante) $ trucos participante

esMejorQue :: Plato -> Plato -> Bool
esMejorQue plato1 plato2 =  dificultad plato1 > dificultad plato2 && sumatoriaDePesosMenor plato1 plato2

sumatoriaDePesosMenor :: Plato -> Plato -> Bool
sumatoriaDePesosMenor plato1 plato2 = sumarPesos (componentes plato1) < sumarPesos (componentes plato2)

obtenerPeso :: Componente -> Int
obtenerPeso (_,peso) = peso

obtenerIngrediente :: Componente -> String
obtenerIngrediente (ingrediente,_) = ingrediente

sumarPesos :: [Componente] -> Int
sumarPesos componentes = sum (map obtenerPeso componentes)

participanteEstrella :: [Participante] -> Participante
participanteEstrella participantes = foldr1 mejorParticipante participantes

mejorParticipante :: Participante -> Participante -> Participante
mejorParticipante unParticipante otroParticipante
  | esMejorQue (cocinar unParticipante) (cocinar otroParticipante) = unParticipante
  | otherwise                                                      = otroParticipante
{-Parte C-}

{-Parte D-}
platinum :: Plato 
platinum = UnPlato {
    dificultad = 10,
    componentes = componentesInfinitos
}

componentesInfinitos :: [Componente]
componentesInfinitos = map (\unNumero -> ("Ingrediente" ++ show unNumero, unNumero)) [1..]
{-Parte D-}