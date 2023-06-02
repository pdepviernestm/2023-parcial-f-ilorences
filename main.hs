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
-- | trabajo == "escuela elemental" = alterarFelicidad (-20). alterarDinero (cuantoDinero trabajo)
-- | otherwise = alterarDinero (cuantoDinero trabajo)

trabajarDirector :: Actividad
trabajarDirector = irEscuela . irTrabajo "escuela elemental"
cuantoDinero :: [a] -> Int
cuantoDinero = length

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
srBurns = UnPersonaje "Sr. Burns" 120 5

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
bart1 = UnPersonaje "bart" 6 2

bart2 :: Personaje
bart2 = UnPersonaje "bart" 11 20

bart3 :: Personaje
bart3 = UnPersonaje "bart" 5 10

mafia :: Actividad
mafia = irTrabajo "mafia"

esDecisiva :: Personaje -> Logro -> Actividad -> Bool
esDecisiva pers logro actividad = (logro pers == False) && (logro (actividad pers) == True)

-- *Main> esDecisiva bart1 verKrosti mafia
-- True
-- *Main> esDecisiva bart2 verKrosti mafia
-- False
-- esDecisiva bart3 verKrosti mafia
-- False
--B

-- Prueba
maggie :: Personaje
maggie = UnPersonaje "maggie" 100 100

actividades1 :: [Actividad]
actividades1 = [irEscuela,trabajo1, tomarMedicamento,trabajo2, mafia]

actividades2 :: [Actividad]
actividades2 = [irEscuela,tomarMedicamento,trabajo2,trabajo1, mafia]

actividades3 :: [Actividad]
actividades3 = [irEscuela, tomarMedicamento, mafia]

actividades4 :: [Actividad]
actividades4 = [irEscuela, tomarMedicamento, mafia, comerDonas 200]

trabajo1 :: Actividad
trabajo1 = irTrabajo "masdeveintecaracteres01"

-- *Main> trabajo1 maggie
-- UnPersonaje {nombre = "maggie", dinero = 123, felicidad = 100}

trabajo2 :: Actividad
trabajo2 = irTrabajo "masdeveintecaracteres002"

-- trabajo2 maggie
-- UnPersonaje {nombre = "maggie", dinero = 124, felicidad = 100}

--
realizarPrimeraDecisiva :: Personaje -> Logro -> [Actividad] -> Personaje
realizarPrimeraDecisiva pers logro acts
  | null (primeraDecisiva pers logro acts) = pers
  | otherwise = head(primeraDecisiva pers logro acts) pers

primeraDecisiva :: Personaje -> Logro -> [Actividad] -> [Actividad]
primeraDecisiva pers logro = filter (esDecisiva pers logro)

-- *Main> realizarPrimeraDecisiva maggie serMillonario actividades1
-- UnPersonaje {nombre = "maggie", dinero = 123, felicidad = 100}

-- *Main> realizarPrimeraDecisiva maggie serMillonario actividades2
-- UnPersonaje {nombre = "maggie", dinero = 124, felicidad = 100}

-- *Main> realizarPrimeraDecisiva maggie serMillonario actividades3
-- UnPersonaje {nombre = "maggie", dinero = 100, felicidad = 100} (no cambia porque ninguna es decisiva)

-- *Main> realizarPrimeraDecisiva maggie (alegrarse 105) actividades4
-- UnPersonaje {nombre = "maggie", dinero = 90, felicidad = 2100}

-- *Main> realizarPrimeraDecisiva maggie (alegrarse 90) actividades4
-- UnPersonaje {nombre = "maggie", dinero = 100, felicidad = 100} (no cambia porque el valor a superar es 95 y su felicidad ya lo superaba, no tiene decisivas)

-- C
actividadInfinita1 :: [Actividad]
actividadInfinita1 = repeat (irTrabajo "aaaaaaaaaaaaaaaaaaaaa")

actividadInfinita2 :: [Actividad]
actividadInfinita2 = repeat (irTrabajo "bbbbbbbbbbb")

actividadInfinita3 :: [Actividad]
actividadInfinita3 = repeat (irEscuela)

-- *Main> irTrabajo "aaaaaaaaaaaaaaaaaaaaa" maggie
-- UnPersonaje {nombre = "maggie", dinero = 121, felicidad = 100}

-- *Main> realizarPrimeraDecisiva maggie serMillonario actividadInfinita1
-- UnPersonaje {nombre = "maggie", dinero = 121, felicidad = 100} :: Al encontrar la primera actividad decisiva,
-- no evalua toda la lista infinita (lazy) por lo que no queda recorriendola
------------------------------------------------------------------------------------------------------------------
-- *Main> irTrabajo "bbbbbbbbbbb" maggie
-- UnPersonaje {nombre = "maggie", dinero = 111, felicidad = 100}

-- *Main> realizarPrimeraDecisiva maggie serMillonario actividadInfinita2 :: El programa se cuelga ya que recorre todas
-- las actividades hasta encontrar la primera que permita "serMillonario" pero no existe ninguna

-- Con actividadInfinita3 pasa lo mismo, pero nunca lo encontraría porque irEscuela no es decisiva para el logro serMillonario
