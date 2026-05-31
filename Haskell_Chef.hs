import Data.List

-- Parte A --

data Ingrediente = UnIngrediente {
    nombreIngrediente::String,
    pesoIngrediente::Double
} deriving (Eq, Show)

data Plato = UnPlato {
    dificultad::Int,
    componentes::[Ingrediente]
} deriving(Eq, Show)

type Truco = Plato -> Plato

data Participante = UnParticipante {
    nombre::String,
    trucos::[Truco],
    especialidad::Plato
}

crearIngrediente::String -> Double -> Ingrediente
crearIngrediente = UnIngrediente

agregarComponente::Ingrediente -> Plato -> Plato
agregarComponente nuevoIng unPlato = unPlato {componentes = nuevoIng : componentes unPlato}

endulzar::Double -> Truco
endulzar gramos = agregarComponente (crearIngrediente "azucar" gramos)

salar::Double -> Truco
salar gramos = agregarComponente (crearIngrediente "sal" gramos)

darSabor::Double -> Double -> Truco
darSabor gramosSal gramosAzucar = salar gramosSal . endulzar gramosAzucar

duplicarPeso::Ingrediente -> Ingrediente
duplicarPeso unIng = unIng {pesoIngrediente = pesoIngrediente unIng * 2}

duplicarPorcion::Truco
duplicarPorcion unPlato = unPlato {componentes = map duplicarPeso (componentes unPlato)}

agruparSiEsPesado::Ingrediente -> [Ingrediente] -> [Ingrediente]
agruparSiEsPesado unIng acumulador
    | pesoIngrediente unIng >= 10 = unIng : acumulador
    | otherwise = acumulador

dejarPesados::[Ingrediente] -> [Ingrediente]
dejarPesados = foldr agruparSiEsPesado []

esBardo::Plato -> Bool
esBardo unPlato = length (componentes unPlato) > 5 && dificultad unPlato > 7

simplificar::Truco
simplificar unPlato 
    | esBardo unPlato = unPlato {dificultad = 5, componentes = dejarPesados (componentes unPlato)}
    | otherwise = unPlato

esVegano::Plato -> Bool
esVegano unPlato = not (any (\ing -> nombreIngrediente ing `elem` ["carne", "huevo", "leche", "crema", "queso"]) (componentes unPlato))

esSinTacc::Plato -> Bool
esSinTacc unPlato = not (any (\ing -> nombreIngrediente ing == "harina") (componentes unPlato))

esComplejo::Plato -> Bool
esComplejo = esBardo

acumularSal::Ingrediente -> Double -> Double
acumularSal unIng acc
    | nombreIngrediente unIng == "sal" = pesoIngrediente unIng + acc
    | otherwise = acc

totalSal::Plato -> Double
totalSal unPlato = foldr acumularSal 0 (componentes unPlato)

noAptoHipertension::Plato -> Bool
noAptoHipertension unPlato = totalSal unPlato > 2

-- Parte B --

platoPepe::Plato
platoPepe = UnPlato {
    dificultad = 8,
    componentes = [
        UnIngrediente "carne" 200, 
        UnIngrediente "harina" 50, 
        UnIngrediente "sal" 5, 
        UnIngrediente "papa" 100, 
        UnIngrediente "tomate" 80, 
        UnIngrediente "cebolla" 15
    ]
}

pepe::Participante
pepe = UnParticipante {
    nombre = "Pepe Ronccino",
    trucos = [darSabor 2 5, simplificar, duplicarPorcion],
    especialidad = platoPepe
}

-- Parte C --

cocinar::Participante -> Plato
cocinar unParticipante = foldr (\unTruco unPlato -> unTruco unPlato) (especialidad unParticipante) (trucos unParticipante)

pesoTotal::Plato -> Double
pesoTotal unPlato = foldr ((+) . pesoIngrediente) 0 (componentes unPlato)

esMejorQue::Plato -> Plato -> Bool
esMejorQue plato1 plato2 = dificultad plato1 > dificultad plato2 && pesoTotal plato1 < pesoTotal plato2

obtenerMejor::Participante -> Participante -> Participante
obtenerMejor p1 p2
    | esMejorQue (cocinar p1) (cocinar p2) = p1
    | otherwise = p2

participanteEstrella::[Participante] -> Participante
participanteEstrella listaParticipantes = foldr1 obtenerMejor listaParticipantes
