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

homero :: Personaje
homero = UnPersonaje "homero" 100 100

skinner :: Personaje
skinner = UnPersonaje "skinner" 50 15

lisa :: Personaje
lisa = UnPersonaje "lisa" 80 50

-- * Main> comerDonas 12 homero

-- UnPersonaje {nombre = "homero", dinero = 90, felicidad = 220}

-- * Main> trabajarDirector skinner

--  UnPersonaje {nombre = "skinner", dinero = 67, felicidad = 0}

-- * Main> tomarMedicamento (irEscuela lisa)

-- UnPersonaje {nombre = "lisa", dinero = 30, felicidad = 50}

-- Punto 2

type Logro = Personaje -> Bool

srBurns :: Personaje
srBurns = UnPersonaje "Sr. Burns" 10000 5

serMillonario :: Logro
serMillonario pers = dinero pers > dinero srBurns

alegrarse :: Int -> Logro
alegrarse num pers = felicidad pers > num

verKrosti :: Logro
verKrosti pers = dinero pers > 10

--Inventado
ganarLoteria :: Logro
ganarLoteria pers = serMillonario pers && verKrosti pers

--A

bart1 :: Personaje
bart1 = UnPersonaje "bart" 6 20

bart2 :: Personaje
bart2 = UnPersonaje "bart" 11 20

mafia :: Actividad
mafia = irTrabajo "mafia"

esDecisiva :: Personaje -> Actividad -> Logro -> Bool
esDecisiva pers actividad logro = (logro pers == False) && (logro (actividad pers) == True)

-- *Main> esDecisiva bart1 mafia verKrosti
-- True
-- *Main> esDecisiva bart2 mafia verKrosti
-- False

--B