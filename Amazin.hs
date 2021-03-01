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

comediaDramatica, comediaAbsurda, comediaSatirica, otraComedia, cienciaFiccion, terror :: Genero

comediaDramatica usuario = usuario

comediaAbsurda usuario = aumentarFelicidad usuario 5

comediaSatirica usuario = usuario {indiceFelicidad = 2 * indiceFelicidad usuario}

otraComedia usuario = aumentarFelicidad usuario 10

aumentarFelicidad :: Usuario -> Float -> Usuario
aumentarFelicidad usuario cantidad = usuario {indiceFelicidad = cantidad + indiceFelicidad usuario}

cienciaFiccion usuario = usuario {nickname = reverse (nickname usuario)}

terror usuario = usuario {librosAdquiridos = []}

leerLibro :: Usuario -> Libro -> Usuario
leerLibro usuario libro = (genero libro . agregarLibro libro) usuario

agregarLibro :: Libro -> Usuario -> Usuario
agregarLibro libro usuario = usuario {librosLeidos = libro:librosLeidos usuario}
