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
    genero :: Genero,
    tipo :: Tipo
} deriving (Show)

type Genero = Usuario -> Usuario
type Tipo = Libro -> Bool

micaela :: Usuario
micaela = Usuario {
    nickname = "micaelap",
    indiceFelicidad = 70,
    librosAdquiridos = [cienAnios],
    librosLeidos = [cienAnios]
}

elResplandor, cienAnios :: Libro
elResplandor = Libro {
    titulo = "El Resplandor",
    autor = "Stephen King",
    paginas = 150,
    genero = terror,
    tipo = novelaCorta
}

cienAnios = Libro {
    titulo = "Cien Anios de Soledad",
    autor = "Garcia Marquez",
    paginas = 600,
    genero = comediaDramatica,
    tipo = novela
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

ponerseAlDia :: Usuario -> Usuario
ponerseAlDia usuario = foldl leerLibro usuario (librosQueNoLeyo usuario)

libroLeido :: Usuario -> Libro -> Bool -- Determina si un libro fue leido
libroLeido usuario libro = any (esElMismo libro) (librosLeidos usuario)

esElMismo :: Libro -> Libro -> Bool
esElMismo libro otroLibro = (titulo libro == titulo otroLibro) && (autor libro == autor otroLibro)

librosQueNoLeyo :: Usuario -> [Libro]
librosQueNoLeyo usuario = filter (not . libroLeido usuario) (librosAdquiridos usuario)

esFanaticoDe :: Usuario -> String -> Bool
esFanaticoDe usuario unAutor = all (fueEscritoPor unAutor) (librosLeidos usuario)

fueEscritoPor :: String -> Libro -> Bool
fueEscritoPor unAutor libro = autor libro == unAutor

cuento, novelaCorta, novela :: Tipo

cuento libro = paginas libro < 100

novelaCorta libro = (paginas libro >= 100) && (paginas libro <= 200)

novela libro = paginas libro > 200

librosDelTipo :: Tipo -> Usuario -> [Libro]
librosDelTipo unTipo usuario = filter unTipo (librosAdquiridos usuario)

-- Pregunta teorica:

-- En este caso, no podria ponerse al dia el usuario ya que para realizar esto, lee los libros adquiridos que no haya leido antes. 
-- Si la cantidad de libros adquiridos es infinita, la cantidad de libros que no leyo, es infinita tambien. Se deberia aplicar otra funcion que limite la cantidad
-- de libros adquiridos a leer, por ejemplo la funcion "take". De esta forma, podria ejecutarse la funcion "ponerseAlDia", aunque la cantidad de libros adquirida sea infinita.
