import Text.Show.Functions ()

data Usuario = Usuario {
    nickname :: String,
    indiceFelicidad :: Float,
    librosAdquiridos :: [Libro],
    librosLeidos :: [Libro]
} deriving (Show)

data Libro = Libro {
    titulo :: String,
    autor :: String,
    paginas :: Int,
    genero :: Genero
} deriving (Show)

type Genero = Usuario -> Usuario

micaela :: Usuario
micaela = Usuario {
    nickname = "micaelap",
    indiceFelicidad = 70,
    librosAdquiridos = [elResplandor, cienAnios],
    librosLeidos = [cienAnios]
}

elResplandor, cienAnios :: Libro
elResplandor = Libro {
    titulo = "El Resplandor",
    autor = "Stephen King",
    paginas = 500,
    genero = terror
}

cienAnios = Libro {
    titulo = "Cien Anios de Soledad",
    autor = "Garcia Marquez",
    paginas = 600,
    genero = comediaDramatica
}