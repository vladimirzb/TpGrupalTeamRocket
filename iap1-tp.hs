----No importar librerias, no se puede
-- Completar con los datos del grupo
-- Nombre de Grupo: Team Rocket
-- Integrante 1: Daniel Bulacio, db2166419@gmail.com, 41561111
-- Integrante 2: Antú Gonzalo Eyaralar, antuelbolson@gmail.com, 38431966
-- Integrante 3: Vladimir Zantleifer Barreda, zantleifer.vladimir@gmail.com 
-- Integrante 4: Santiago Agustín Oviedo, santyoviedo1@gmail.com, 44379544

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
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

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios = undefined

-- describir qué hace la función: .....
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe = undefined

-- describir qué hace la función: .....
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos = undefined

-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos = undefined

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos = undefined

-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe = undefined

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA = undefined

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

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
usuariosDeRelacionValidos [(1, "Vladi"), (2, "Santi"), (3, "Antu"), (4, "Dani")] [((1, "Vladi"), (2, "Santi")), ((2, "Santi"), (3, "Antu")), ((3, "Antu"), (4, "Dani"))]
usuariosDeRelacionValidos [(1, "Vladi"), (2, "Santi"), (3, "Antu"), (4, "Dani")] [((1, "Vladi"), (2, "Santi")), ((2, "Santi"), (3, "Antu")), ((3, "Antu"), (3, "Antu"))]
usuariosDeRelacionValidos [(1, "Vladi"), (2, "Santi"), (3, "Antu"), (4, "Dani")] [((1, "Vladi"), (2, "Santi")), ((2, "Santi"), (3, "Antu")), ((5, "Extra"), (4, "Dani"))]

-}
usuariosDeRelacionValidos :: [Usuario] -> [Relacion] -> Bool
usuariosDeRelacionValidos _ [] = True
usuariosDeRelacionValidos usuarios (((us1, us2):relaciones))
                        | pertenece us1 usuarios && pertenece us2 usuarios && us1/=us2 = usuariosDeRelacionValidos usuarios relaciones
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