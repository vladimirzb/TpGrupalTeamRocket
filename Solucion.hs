module Solucion where
----No importar librerias, no se puede
-- Completar con los datos del grupo
-- Nombre de Grupo: Team Rocket
-- Integrante 1: Daniel Bulacio, db2166419@gmail.com, 41561111
-- Integrante 2: Antú Gonzalo Eyaralar, antuelbolson@gmail.com, 38431966
-- Integrante 3: Vladimir Zantleifer Barreda, zantleifer.vladimir@gmail.com 44599438
-- Integrante 4: Santiago Agustín Oviedo, santyoviedo1@gmail.com, 44379544

type Usuario = (Integer, String) -- (id, nombre/user)
type Relacion = (Usuario, Usuario) -- (usuarios que se relacionan)
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Ejercicios

--Hecha por Santi
-- Función: nombresDeUsuario, dada una red social, devuelve una lista con los nombres de usuario de todos los usuarios en la red.
-- Función: proyectarNombres, dada una lista de usuarios, devuelve una lista con los nombres de usuarios correspondientes.

proyectarNombres :: [Usuario] ->  [String] 
proyectarNombres [] = []
proyectarNombres (x:xs) | sinRepetidos (x:xs) = [nombreDeUsuario x] ++ proyectarNombres xs

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red | redSocialValida red = proyectarNombres (usuarios red) 

--proyectarNombres: Dada una lista de usuarios -users-, creo una lista con el nombre de usuario del primer elemento de users y lo voy concatenando con los nombres de usuarios siguientes.  
--nombresDeUsuarios: Dada una red social valida -red- llama a la funcion proyectarNombres utilizando la lista de usuarios de red. 

--Test de la catedra: aprobado.
--Faltan tests extras.



--Hecha por Vladimir
--Paso test de la catedra sin errores: Paso el test 
--Paso test extras: Pendiente
-- AmigosDe: Dada la red y un usuario(x) devuelve una secuencia que contiene todos los amigos del usuario(x)
--Estaba pensando lo siguiente con RedSocial consigo todas las relaciones, entonces primero busco en cuales relaciones aparece el usuario dado y luego las filtro y devuelvo esa secuencia de relaciones (que seria el usuario con su amigo en una tupla)

--con la lista de relaciones filtrada con solo las relaciones que contienen al usuario, voy a recorrer recurisvamente las tuplas en relaciones y agarrar el elemento que no es el usuario
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red u = auxAmigosDe (obtenerRelacionesConUsuario red u) u

auxAmigosDe :: [Relacion] -> Usuario -> [Usuario]
auxAmigosDe [] _ = []
auxAmigosDe (r:rs) u
    |u == fst r = snd r : auxAmigosDe rs u 
    |u == snd r = fst r : auxAmigosDe rs u 



---Me armo una lista desde cero con las relaciones que esta el usuario
obtenerRelacionesConUsuario :: RedSocial -> Usuario -> [Relacion]
obtenerRelacionesConUsuario red u = auxObtenerRelacionesConUsuario (relaciones(red)) u

auxObtenerRelacionesConUsuario :: [Relacion] -> Usuario -> [Relacion]
auxObtenerRelacionesConUsuario [] _ = []
auxObtenerRelacionesConUsuario (r:rs) u
                                | usuarioEstaEnRelacion u r = r : auxObtenerRelacionesConUsuario rs u
                                | otherwise= auxObtenerRelacionesConUsuario rs u
                                    where usuarioEstaEnRelacion u (r1, r2) = u==r1 || u==r2
--Hecha por Vladimir
--Paso test de la catedra sin errores: Paso el test 
--Paso test extras: Pendiente
-- describir qué hace la función: Dada la redsocial y un usuario devuelvo la cantidad de amigos del usuario. Utilizo la funcion amigosDe para obtener directamente la secuencia con los amigos del usuario y con una funcion axuliar cuento los elementos de la secuencia
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red u = cantidadDeAmigosAux (amigosDe red u)

cantidadDeAmigosAux :: [Usuario] -> Int
cantidadDeAmigosAux []  = 0
cantidadDeAmigosAux (u:us)= cantidadDeAmigosAux(us) + 1


-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = usuarioConMasAmigosAux red (usuarios red)

usuarioConMasAmigosAux :: RedSocial -> [Usuario] -> Usuario
usuarioConMasAmigosAux _ [x] = x
usuarioConMasAmigosAux red (x:xs) | cantidadDeAmigos red x > cantidadDeAmigos red (head xs) = usuarioConMasAmigosAux red (x:(tail xs))
                                  | cantidadDeAmigos red x == cantidadDeAmigos red (head xs) = usuarioConMasAmigosAux red xs
                                  | otherwise = usuarioConMasAmigosAux red xs
--Hecha por Santi

--Función: estaRobertoCarlos, dada una red social válida, decide si existe un usuario con más de un millón de amigos en ella.
--Función: estaRobertoCarlosAux, dada una red social y una lista de usuarios, decide si uno de los usuarios tiene más de un millón de amigos en esa red.
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red | redSocialValida red = estaRobertoCarlosAux red (usuarios red)

estaRobertoCarlosAux :: RedSocial -> [Usuario] -> Bool
estaRobertoCarlosAux red [] = False
estaRobertoCarlosAux red (x:xs) | cantidadDeAmigos red x > 1000000 = True
                                | otherwise = estaRobertoCarlosAux red xs

--Test de la cátedra: aprobado.
--Faltan test extras.

        
                                    
-- describir qué hace la función: .....
-- Casos que deberían funcionar
-- publicacionesDe ([(1,"Dani"),(2,"Antu"),(3, "Santi"),(4,"Vladi")], [((1,"Dani"),(2,"Antu")), ((2,"Antu"),(3,"Santi"))], [((3, "Santi"), "Publicacion 1", []), ((2, "Antu"), "Publicacion 2", []), ((3, "Santi"), "Publicacion 3", [])]) (3, "Santi")
-- publicacionesDe ([(1,"Dani"),(2,"Antu"),(3, "Santi"),(4,"Vladi")], [((1,"Dani"),(2,"Antu")), ((2,"Antu"),(3,"Santi"))], [((3, "Santi"), "Publicacion 1", []), ((2, "Antu"), "Publicacion 2", []), ((3, "Santi"), "Publicacion 3", [])]) (2, "Antu")
-- publicacionesDe ([(1,"Dani"),(2,"Antu"),(3, "Santi"),(4,"Vladi")], [((1,"Dani"),(2,"Antu")), ((2,"Antu"),(3,"Santi"))], [((3, "Santi"), "Publicacion 1", []), ((2, "Antu"), "Publicacion 2", []), ((3, "Santi"), "Publicacion 3", [])]) (1, "Dani")
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red user = filtrarPublicacionesPorUsuario (publicaciones red) user

filtrarPublicacionesPorUsuario :: [Publicacion] -> Usuario -> [Publicacion]
filtrarPublicacionesPorUsuario [] u     = []
filtrarPublicacionesPorUsuario (x:xs) u | u == usuarioDePublicacion x = x : filtrarPublicacionesPorUsuario xs u
                                        | otherwise = filtrarPublicacionesPorUsuario xs u

-- describir qué hace la función: .....
-- publicacionesQueLeGustanA ([(1,"Dani"),(2,"Antu"),(3, "Santi"),(4,"Vladi")], [((1,"Dani"),(2,"Antu")), ((2,"Antu"),(3,"Santi"))], [((3, "Santi"), "Publicacion 1", [(1, "Dani"), (2, "Antu"), (4, "Vladi")]), ((2, "Antu"), "Publicacion 2", [(2, "Antu"), (3, "Santi")]), ((3, "Santi"), "Publicacion 3", [(1, "Dani"), (2, "Antu"), (3, "Santi")])]) (1, "Dani")
-- publicacionesQueLeGustanA ([(1,"Dani"),(2,"Antu"),(3, "Santi"),(4,"Vladi")], [((1,"Dani"),(2,"Antu")), ((2,"Antu"),(3,"Santi"))], [((3, "Santi"), "Publicacion 1", [(1, "Dani"), (2, "Antu"), (4, "Vladi")]), ((2, "Antu"), "Publicacion 2", [(2, "Antu"), (3, "Santi")]), ((3, "Santi"), "Publicacion 3", [(1, "Dani"), (2, "Antu"), (3, "Santi")])]) (4, "Vladi")
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red user = publicacionesConLikesDe (publicaciones red) user

publicacionesConLikesDe :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesConLikesDe [] u = []
publicacionesConLikesDe (x:xs) u | pertenece u (likesDePublicacion x) == True = x : publicacionesConLikesDe xs u
                                           | otherwise = publicacionesConLikesDe xs u

-- describir qué hace la función: .....
-- CASOS QUE DAN FALSE
-- lesGustanLasMismasPublicaciones ([(1,"Dani"),(2,"Antu"),(3, "Santi"),(4,"Vladi")], [((1,"Dani"),(2,"Antu")), ((2,"Antu"),(3,"Santi"))], [((3, "Santi"), "Publicacion 1", [(1, "Dani"), (2, "Antu"), (4, "Vladi")]), ((2, "Antu"), "Publicacion 2", [(2, "Antu"), (3, "Santi"), (4, "Vladi")]), ((3, "Santi"), "Publicacion 3", [(1, "Dani"), (2, "Antu"), (3, "Santi")])]) (1, "Dani") (4, "Vladi")
-- lesGustanLasMismasPublicaciones ([(1,"Dani"),(2,"Antu"),(3, "Santi"),(4,"Vladi")], [((1,"Dani"),(2,"Antu")), ((2,"Antu"),(3,"Santi"))], [((3, "Santi"), "Publicacion 1", [(1, "Dani"), (2, "Antu"), (4, "Vladi")]), ((2, "Antu"), "Publicacion 2", [(2, "Antu"), (3, "Santi")]), ((3, "Santi"), "Publicacion 3", [(1, "Dani"), (2, "Antu"), (3, "Santi")])]) (1, "Dani") (4, "Vladi")
-- CASO TRUE
-- lesGustanLasMismasPublicaciones ([(1,"Dani"),(2,"Antu"),(3, "Santi"),(4,"Vladi")], [((1,"Dani"),(2,"Antu")), ((2,"Antu"),(3,"Santi"))], [((3, "Santi"), "Publicacion 1", [(1, "Dani"), (2, "Antu"), (4, "Vladi")]), ((2, "Antu"), "Publicacion 2", [(2, "Antu"), (3, "Santi")]), ((3, "Santi"), "Publicacion 3", [(2, "Antu"), (3, "Santi")])]) (1, "Dani") (4, "Vladi")
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = mismosElementos (publicacionesQueLeGustanA red u1) (publicacionesQueLeGustanA red u2)

-- describir qué hace la función: .....
--CASO F
-- tieneUnSeguidorFiel ([(1,"Dani"),(2,"Antu"),(3, "Santi"),(4,"Vladi")], [((1,"Dani"),(2,"Antu")), ((2,"Antu"),(3,"Santi"))], [((3, "Santi"), "Publicacion 1", [(1, "Dani"), (2, "Antu"), (4, "Vladi")]), ((3, "Santi"), "Publicacion 2", [(3, "Santi")]), ((3, "Santi"), "Publicacion 3", [(2, "Antu"), (3, "Santi")])]) (3, "Santi")
--CASO V
-- tieneUnSeguidorFiel ([(1,"Dani"),(2,"Antu"),(3, "Santi"),(4,"Vladi")], [((1,"Dani"),(2,"Antu")), ((2,"Antu"),(3,"Santi"))], [((3, "Santi"), "Publicacion 1", [(1, "Dani"), (2, "Antu"), (4, "Vladi")]), ((3, "Santi"), "Publicacion 2", [(2, "Antu"), (3, "Santi")]), ((3, "Santi"), "Publicacion 3", [(2, "Antu"), (3, "Santi")])]) (3, "Santi")
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red u = auxSFielPasoLikesDePrimeraPublicacion (publicacionesDe red u)

-- Recibe todas las publicaciones que hizo el usuario ingresado (inputUser). Le pasa a auxSFielDioLikeAPrimeraPublicacion el array de likes de la primer publicación y todo el resto de publicaciones.
-- Esto es porque si el inputUser tiene un seguidor fiel (sf) me basta con chequear que los likes de la primer publicación, ya que un usuario no puede ser sf de otro si no likeo todas las publicaciones, incluida la primera.
auxSFielPasoLikesDePrimeraPublicacion :: [Publicacion] ->  Bool
auxSFielPasoLikesDePrimeraPublicacion (x:xs) | auxSFielDioLikeAPrimeraPublicacion (likesDePublicacion x) (x:xs) == False = False
                                             | otherwise = True

-- auxSFielDioLikeAPrimeraPublicacion itera sobre los usuarios que le dieron like a la primer publicación y le pasa uno por uno a auxSFielLeDioLikeATodasLasPublicaciones, junto con el array de publicaciones.
auxSFielDioLikeAPrimeraPublicacion :: [Usuario] -> [Publicacion] -> Bool
auxSFielDioLikeAPrimeraPublicacion [] _ = False
auxSFielDioLikeAPrimeraPublicacion (x:xs) pubs | auxSFielLeDioLikeATodasLasPublicaciones x pubs == False = auxSFielDioLikeAPrimeraPublicacion xs pubs
                                               | otherwise = True
          
-- auxSFielLeDioLikeATodasLasPublicaciones itera sobre las likes de todas las publicaciones menos la primera, y devuelve True si uno de los usuarios que le dio like a la primera, le dio like también a todo el resto.
auxSFielLeDioLikeATodasLasPublicaciones :: Usuario -> [Publicacion] -> Bool
auxSFielLeDioLikeATodasLasPublicaciones u []     = True
auxSFielLeDioLikeATodasLasPublicaciones u (x:xs) | pertenece u (likesDePublicacion x) == True = auxSFielLeDioLikeATodasLasPublicaciones u xs
                                                 | otherwise = False

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined


--Predicados Auxiliares

---------- Comienzo Predicados Vladi-------------
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece x [] = False
pertenece x (y:ys) | x==y = True
                   |otherwise = pertenece x ys

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos l1 l2 = mismosElementosAux l1 l2 && mismosElementosAux l2 l1

mismosElementosAux :: (Eq t) => [t] -> [t] -> Bool
mismosElementosAux [] _     = True
mismosElementosAux (x:xs) ys | pertenece x ys == True = mismosElementosAux xs ys
                             | otherwise=False

{- Inputs con lo que probe para que fijarme si devuelve los valores correctos El primer ejemplo todas las publicaciones estan la lista de usuarios, en el segundo no:
usuariosDePublicacionSonUsuariosDeRed [(1, "Vladi"), (2, "Santi"), (3, "Antu")] [((1, "Vladi"), "Publicación 1", []), ((2, "Santi"), "Publicación 2", []), ((3, "Antu"), "Publicación 3", [])]
usuariosDePublicacionSonUsuariosDeRed [(1, "Vladi"), (2, "Santi"), (3, "Antu")] [((1, "Vladi"), "Publicación 1", []), ((2, "Santi"), "Publicación 2", []), ((4, "Dani"), "Publicación 4", [])]
usuariosDePublicacionSonUsuariosDeRed [(1, "Vladi"), (2, "Santi"), (3, "Antu"), (4, "Dani"), (5, "Eve")] [((1, "Vladi"), "Publicación 1", []), ((2, "Santi"), "Publicación 2", []), ((3, "Antu"), "Publicación 3", []), ((4, "Dani"), "Publicación 4", []), ((5, "Eve"), "Publicación 5", [])]
usuariosDePublicacionSonUsuariosDeRed [(1, "Vladi"), (2, "Santi"), (3, "Antu"), (4, "Dani"), (5, "Eve")] [((1, "Vladi"), "Publicación 1", []), ((2, "Santi"), "Publicación 2", []), ((3, "Antu"), "Publicación 3", []), ((4, "Dani"), "Publicación 4", []), ((6, "Frank"), "Publicación 6", [])]
-}
usuariosDePublicacionSonUsuariosDeRed :: [Usuario] ->[Publicacion] -> Bool 
usuariosDePublicacionSonUsuariosDeRed _ [] = True
usuariosDePublicacionSonUsuariosDeRed usuarios (pub:publicaciones) 
                        | pertenece (usuarioDePublicacion(pub)) usuarios  == True = usuariosDePublicacionSonUsuariosDeRed usuarios publicaciones
                        | otherwise = False 


--Desarmo la tupla para poder usar los usuarios
{-Inputs de prueba
Valor esperado true = usuariosDeRelacionValidos [(1, "Vladi"), (2, "Santi"), (3, "Antu"), (4, "Dani")] [((1, "Vladi"), (2, "Santi")), ((2, "Santi"), (3, "Antu")), ((3, "Antu"), (4, "Dani"))]
Valor esperado false = usuariosDeRelacionValidos [(1, "Vladi"), (2, "Santi"), (3, "Antu"), (4, "Dani")] [((1, "Vladi"), (2, "Santi")), ((2, "Santi"), (3, "Antu")), ((3, "Antu"), (3, "Antu"))]
Valor esperado false = usuariosDeRelacionValidos [(1, "Vladi"), (2, "Santi"), (3, "Antu"), (4, "Dani")] [((1, "Vladi"), (2, "Santi")), ((2, "Santi"), (3, "Antu")), ((5, "Extra"), (4, "Dani"))]

-}
usuariosDeRelacionValidos :: [Usuario] -> [Relacion] -> Bool
usuariosDeRelacionValidos _ [] = True
usuariosDeRelacionValidos usuarios (((us1, us2):relaciones))
                        | pertenece us1 usuarios && pertenece us2 usuarios && us1/=us2 = usuariosDeRelacionValidos usuarios relaciones
                        | otherwise = False 

{- inputs de prueba
Valor esperado true = relacionesAsimetricas [((1, "A"), (2, "B")), ((2, "B"), (3, "C")), ((3, "C"), (4, "D"))]
Valor esperado false = relacionesAsimetricas [((1, "A"), (2, "B")), ((2, "B"), (1, "A")), ((3, "C"), (4, "D"))]

-}
relacionesAsimetricas :: [Relacion] -> Bool
relacionesAsimetricas [] = True
relacionesAsimetricas (((us1, us2):relaciones)) |  not (pertenece (us2,us1) relaciones) == True = relacionesAsimetricas relaciones 
                                                | otherwise = False


sonDeLaRed :: RedSocial -> [Usuario] -> Bool
sonDeLaRed _ [] = True
sonDeLaRed red (u:us) | pertenece u (usuarios(red)) = sonDeLaRed red us
                      | otherwise = False


----------------- Fin Predicados Vladi-----------------

--Predicados Dani
usuarioValido :: Usuario -> Bool
usuarioValido u = (idDeUsuario u) > 0 && longitud (nombreDeUsuario u) > 0

usuariosValidos :: [Usuario] -> Bool
usuariosValidos []     = True
usuariosValidos (u:us) | noHayIdsRepetidos (u:us) == True && usuarioValido u == True = usuariosValidos us
                       | otherwise = False


-- Función auxiliar para determinar la longitud de una sequencia.
longitud :: [t] -> Integer
longitud []     = 0
longitud (x:xs) = 1 + longitud xs


-- aux ((1,"Dani"),(4,"Vladi")) [((1,"Dani"),(2,"Antu")), ((1,"Dani"),(3,"Fer"))]
auxiliarRelacionesRepetidas :: Relacion -> [Relacion] -> Bool
auxiliarRelacionesRepetidas (u1, u2) []     = True
auxiliarRelacionesRepetidas (u1, u2) (x:xs) | x == (u1, u2) = False
                                            | otherwise = auxiliarRelacionesRepetidas (u1, u2) xs

noHayPublicacionesRepetidas :: [Publicacion] -> Bool
noHayPublicacionesRepetidas []     = True
noHayPublicacionesRepetidas (x:xs) | auxiliarPublicacionesRepetidas x xs == True = noHayPublicacionesRepetidas xs
                                   | otherwise = False

-- auxiliarPublicacionesRepetidas ((1, "Daniel"), "Hola", []) [((1, "Daniel"), "Hola", []), ((2, "Antu"), "estas", [])]
auxiliarPublicacionesRepetidas :: Publicacion -> [Publicacion] -> Bool
auxiliarPublicacionesRepetidas (p1, p2, _) [] = True
auxiliarPublicacionesRepetidas pub (x:xs) | pub == x = False
                                          | otherwise = auxiliarPublicacionesRepetidas pub xs

-- Casos validos:
-- usuariosLikeValidos [(1,"Dani"),(2,"Antu"),(3, "Santi"),(4, "Vladi")] []
-- usuariosLikeValidos [(1,"Dani"),(2,"Antu"),(3, "Santi"),(4, "Vladi")] [((2,"Antu")),(3,"Santi")]
-- Casos falsos:
-- usuariosLikeValidos [(1,"Dani"),(2,"Antu"),(3, "Santi"),(4, "Vladi")] [(2,"Antu"),(4,"Santi")] 
usuariosLikeValidos :: [Usuario] -> [Usuario] -> Bool
usuariosLikeValidos users []     = True
usuariosLikeValidos users (x:xs) | pertenece x users == True = usuariosLikeValidos users xs
                                 | otherwise = False

-- Caso valido: usuariosDeLikeDePublicacionSonUsuariosDeRed [(1, "Vladi"), (2, "Santi"), (3, "Antu"), (4, "Dani"), (5, "Eve")] [((1, "Vladi"), "Publicación 1", [(3, "Antu"), (4, "Dani"), (5, "Eve")]), ((2, "Santi"), "Publicación 2", [(1, "Vladi"), (2, "Santi"), (3, "Antu")]), ((3, "Antu"), "Publicación 3", [])]
-- Caso falso: usuariosDeLikeDePublicacionSonUsuariosDeRed [(1, "Vladi"), (2, "Santi"), (3, "Antu"), (4, "Dani"), (5, "Eve")] [((1, "Vladi"), "Publicación 1", [(3, "Antu"), (4, "Dani"), (5, "Eve")]), ((2, "Santi"), "Publicación 2", [(1, "Vladi"), (2, "Santi"), (3, "Antu")]), ((3, "Antu"), "Publicación 3", [(69, "bot")])]
usuariosDeLikeDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
usuariosDeLikeDePublicacionSonUsuariosDeRed users []     = True
usuariosDeLikeDePublicacionSonUsuariosDeRed users (x:xs) | usuariosLikeValidos users (likesDePublicacion x) == True = usuariosDeLikeDePublicacionSonUsuariosDeRed users xs
                                                         | otherwise = False

-- Casos validos: Cualquier caso valido de las funciones que utilizo
-- Casos falso: Cualquier caso invalido de las funciones que utilizo
publicacionesValidas :: [Usuario] -> [Publicacion] -> Bool
publicacionesValidas users pubs = usuariosDePublicacionSonUsuariosDeRed users pubs && usuariosDeLikeDePublicacionSonUsuariosDeRed users pubs && noHayPublicacionesRepetidas pubs

-- Falta testear
relacionesValidas :: [Usuario] -> [Relacion] -> Bool
relacionesValidas users rel = usuariosDeRelacionValidos users rel && relacionesAsimetricas rel && noHayRelacionesRepetidas rel

-- Falta testear
redSocialValida :: RedSocial -> Bool
redSocialValida red = usuariosValidos (usuarios red) && relacionesValidas (usuarios red) (relaciones red) && publicacionesValidas (usuarios red) (publicaciones red)

----------------- Fin Predicados Dani-----------------

-- Predicados Antú

noHayIdsRepetidos :: [Usuario] -> Bool
noHayIdsRepetidos []     = True
noHayIdsRepetidos (x:xs) | auxiliarIdsRepetidos (idDeUsuario x) xs == False = noHayIdsRepetidos xs
                         | otherwise = False  

auxiliarIdsRepetidos :: Integer -> [Usuario] -> Bool
auxiliarIdsRepetidos x [] = False
auxiliarIdsRepetidos x (y:ys) | x == idDeUsuario y = True
                      | otherwise = auxiliarIdsRepetidos x ys

noHayRelacionesRepetidas :: [Relacion] -> Bool
noHayRelacionesRepetidas []     = True
noHayRelacionesRepetidas (x:xs) | auxiliarRelacionesRepetidas x xs == True = noHayRelacionesRepetidas xs
                                | otherwise = False


--Test 
--Caso valido: (1,"Dani") (2,"Antu") ([(1,"Dani"),(2,"Antu"),(4,"Vladi")], [((1,"Dani"),(4,"Vladi")), ((1,"Dani"),(2,"Antu")), ((2,"Antu"),(4,"Vladi"))], [])
--Caso falso: (1,"Dani") (2,"Antu") ([(1,"Dani"),(2,"Antu"),(4,"Vladi")], [((1,"Dani"),(4,"Vladi")), ((2,"Antu"),(4,"Vladi"))], [])


relacionadosDirecto :: Usuario -> Usuario -> RedSocial -> Bool
relacionadosDirecto u1 u2 red = auxiliarRelacionadosDirecto u1 u2 (relaciones red)
                              

auxiliarRelacionadosDirecto :: Usuario -> Usuario -> [Relacion] -> Bool
auxiliarRelacionadosDirecto u1 u2 []     = False
auxiliarRelacionadosDirecto u1 u2 (x:xs) | (u1, u2) == x || (u2, u1) == x = True
                                         |  otherwise = auxiliarRelacionadosDirecto u1 u2 xs


--Test
-- Caso valido: [(1,"Dani"),(2,"Antu"),(3, "Santi")] ([(1,"Dani"),(2,"Antu"),(3, "Santi"),(4,"Vladi")], [((1,"Dani"),(2,"Antu")), ((2,"Antu"),(3,"Santi"))], [])
-- Caso falso: [(1,"Dani"),(2,"Antu"),(3, "Santi")] ([(1,"Dani"),(2,"Antu"),(3, "Santi"),(4,"Vladi")], [((1,"Dani"),(2,"Antu")), ((2,"Antu"),(4,"Vladi"))], []) 
-- Caso verdadero [(1,"Dani"),(2,"Antu"),(3, "Santi"),(4, "Vladi")] ([(1,"Dani"),(2,"Antu"),(3, "Santi"),(4,"Vladi")], [((1,"Dani"),(2,"Antu")), ((2,"Antu"),(3,"Santi")), ((3,"Santi"),(4, "Vladi"))], [])
-- Caso falso: [(1,"Dani"),(2,"Antu"),(3, "Santi"),(4, "Vladi")] ([(1,"Dani"),(2,"Antu"),(3, "Santi"),(4,"Vladi")], [((1,"Dani"),(2,"Antu")), ((2,"Antu"),(3,"Santi")), ((1,"Dani"),(4, "Vladi"))], [])

cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool
cadenaDeAmigos [x] red     = True
cadenaDeAmigos (x:xs) red | relacionadosDirecto x (head xs) red == True = cadenaDeAmigos xs red
                          | otherwise = False
                              
----------------- Fin Predicados Antu-----------------

-- Predicados Santi

--Función que dada una lista, devuelve su primer elemento.
empiezaCon :: [t] -> t
empiezaCon (x:xs) = x

--Test
--Caso de prueba --[(1,"Santi"),(2,"Antu")]--, --["a","b","c"]--, --[1,2,3]--

--Función que dada una lista, devuelve otra lista con su último elemento.
terminaCon :: [t] -> [t]
terminaCon [x] = [x]
terminaCon (x:xs) = terminaCon xs

--Test
--Caso de prueba --[(1,"Santi"),(2,"Antu")]--, --["a","b","c"]--, --[1,2,3]--

--Función que decide si una función tiene repetidos o no.

sinRepetidos :: (Eq t) => [t] -> Bool
sinRepetidos [] = True
sinRepetidos (x:xs) | pertenece x xs == True = False
                    | pertenece x xs == False = sinRepetidos xs

--Test
--Casos válidos --[(1,"Santi"),(2,"Antu")]--, --["a","b","c"]--, --[1,2,3]--
--Casos falsos --[(1,"Santi"),(2,"Antu"),(2,"Antu")]--, --["a","b","a"]--, --[2,2,3]--

----------------- Fin Predicados Santi -----------------