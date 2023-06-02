import Text.Show.Functions

data Personaje = UnPersonaje
  { nombre :: String,
    dinero :: Int,
    felicidad :: Int
  }
  deriving (Show, Eq)

--Punto 1

type Actividad = Personaje -> Personaje

irEscuela :: Actividad
irEscuela pers
  | nombre pers == "lisa" = alterarFelicidad 20 pers
  | otherwise = alterarFelicidad (-20) pers

alterarFelicidad :: Int -> Actividad
alterarFelicidad num pers = pers {felicidad = max (felicidad pers + num) 0}

comerDonas :: Int -> Actividad
comerDonas cant = alterarDinero (-10) . alterarFelicidad (10 * cant)

alterarDinero :: Int -> Actividad
alterarDinero num pers = pers {dinero = dinero pers + num}

irTrabajo :: String -> Actividad
irTrabajo trabajo = alterarDinero (cuantoDinero trabajo)

--irTrabajo' :: String -> Actividad
--irTrabajo' trabajo

-- | trabajo == "escuela elemental" = alterarFelicidad (-20= . alterarDinero (cuantoDinero trabajo)
-- | otherwise = alterarDinero (cuantoDinero trabajo)
trabajarDirector :: Actividad
trabajarDirector = alterarFelicidad (-20) . alterarDinero (cuantoDinero "escuela elemental")

cuantoDinero :: [a] -> Int
cuantoDinero = length

irAEscuela :: Actividad
irAEscuela = alterarFelicidad (-20)

--Inventado
tomarMedicamento :: Actividad
tomarMedicamento = alterarFelicidad (-20) . alterarDinero (-50)

homero = UnPersonaje "homero" 100 100

skinner = UnPersonaje "skinner" 50 15

lisa = UnPersonaje "lisa" 80 50

-- * Main> comerDonas 12 homero
-- UnPersonaje {nombre = "homero", dinero = 90, felicidad = 220}

-- * Main> trabajarDirector skinner
--  UnPersonaje {nombre = "skinner", dinero = 67, felicidad = 0}

-- *Main> tomarMedicamento (irEscuela lisa)
-- UnPersonaje {nombre = "lisa", dinero = 30, felicidad = 50}
