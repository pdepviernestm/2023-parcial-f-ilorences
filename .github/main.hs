import Text.Show.Functions

data Personaje = UnPersonaje
  { nombre :: String,
    dinero :: Int,
    felicidad :: Int
  }
  deriving (Show, Eq)

type Actividad = Personaje -> Personaje

irEscuela :: Actividad
irEscuela pers
  |nombre pers == "lisa" = alterarFelicidad 20 pers
  |otherwise = alterarFelicidad (-20) pers

alterarFelicidad :: Int -> Personaje -> Personaje
alterarFelicidad num pers = pers{felicidad = felicidad pers + num}


