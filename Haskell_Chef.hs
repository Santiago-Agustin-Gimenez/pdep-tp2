import Data.List

type Componente = (String, Double)

data Plato = UnPlato {
    dificultad::Int,
    componentes::[Componente]
} deriving(Eq, Show)

type Truco = Plato -> Plato

data Participante = UnParticipante {
    nombre::String,
    trucos::[Truco],
    especialidad::Plato
}

endulzar::Double -> Truco
endulzar gramos unPlato = unPlato {componentes = ("azucar", gramos):componentes unPlato}

salar::Double -> Truco
salar gramos unPlato = unPlato {componentes = ("sal", gramos):componentes unPlato}

darSabor::Double -> Double -> Truco
darSabor gramosSal gramosAzucar = endulzar gramosAzucar . salar gramosSal

duplicarPorcion::Truco
duplicarPorcion unPlato = unPlato {componentes = map (\(ingrediente, peso) -> (ingrediente, peso * 2)) (componentes unPlato)}

dejarPesados::[Componente] -> [Componente]
dejarPesados[] = []
dejarPesados ((ingrediente, peso):xs)
    | peso >= 10 = (ingrediente, peso):dejarPesados xs
    | otherwise = dejarPesados xs

esBardo::Plato -> Bool
esBardo unPlato = length (componentes unPlato) > 5 && dificultad unPlato > 7

simplificar::Truco
simplificar unPlato 
    | esBardo unPlato = unPlato {dificultad = 5, componentes = dejarPesados (componentes unPlato)}
    | otherwise = unPlato

esIngredienteNoVegano::String -> Bool
esIngredienteNoVegano unIngrediente = unIngrediente `elem` ["carne", "huevo", "leche", "crema", "queso"]

esVegano::Plato -> Bool
esVegano unPlato = not (any (\(ingrediente, _) -> esIngredienteNoVegano ingrediente) (componentes unPlato))

esSinTacc::Plato -> Bool
esSinTacc unPlato = not (any (\(ingrediente, _) -> ingrediente == "harina") (componentes unPlato))

esComplejo::Plato -> Bool
esComplejo unPlato = esBardo unPlato

sumarSal::[Componente] -> Double
sumarSal[] = 0
sumarSal ((ingrediente, peso):xs)
    | ingrediente == "sal" = peso + sumarSal xs
    | otherwise = sumarSal xs

totalSal::Plato -> Double
totalSal unPlato = sumarSal (componentes unPlato)

noAptoHipertension::Plato -> Bool
noAptoHipertension unPlato = totalSal unPlato > 2