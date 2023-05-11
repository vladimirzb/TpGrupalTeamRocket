-- Vladi: Va a tirar error la primera vez que lo ejecutes y despues copia este codigo :set -package base y luego volver a loadear el archivo para que funcione y correr main
import Test.HUnit
import Solucion

main = runTestTT tests
--Vladi: Vamos a hacer una secuencia de test para cada ejercicios y luegos vamos a poner estas secuencias en el main para que corran todos los test para todos los ejercios
tests = test [
    " nombresDeUsuarios 1" ~: (nombresDeUsuarios redA) ~?= ["Juan","Natalia","Pedro","Mariela"],

    testsSuiteAmigosDe,

    " cantidadDeAmigos 1" ~: (cantidadDeAmigos redA usuario1) ~?= 2,

    " usuarioConMasAmigos 1" ~: expectAny (usuarioConMasAmigos redA) [usuario2, usuario4],

    " estaRobertoCarlos 1" ~: (estaRobertoCarlos redA) ~?= False,

    " publicacionesDe 1" ~: (publicacionesDe redA usuario2) ~?= [publicacion2_1, publicacion2_2],

    " publicacionesQueLeGustanA 1" ~: (publicacionesQueLeGustanA redA usuario1) ~?= [publicacion2_2, publicacion4_1],

    " lesGustanLasMismasPublicaciones 2" ~: (lesGustanLasMismasPublicaciones redB usuario1 usuario3) ~?= True,

    " tieneUnSeguidorFiel 1" ~: (tieneUnSeguidorFiel redA usuario1) ~?= True,
    
    " existeSecuenciaDeAmigos 1" ~: (existeSecuenciaDeAmigos redA usuario1 usuario3) ~?= True

 ]
--Comentario Vladi: Ya prepare todo para que funcionen los suit cases

--Test de vladi: amigosDe

{- 
Paso 1: Descomponer la solución informática en unidades funcionales
En este caso, tenemos una única unidad funcional:
    amigosDe
Paso 2: Elegir una unidad funcional
La unidad funcional a testear será:
    amigosDe
Paso 3: Identificar factores
Los factores en este caso son los parámetros del problema:
    red: RedSocial
    u: Usuario
Paso 4: Determinar categorías
Para cada parámetro, determinamos las siguientes características:
    red: 
        Tiene usuarios?
        Tiene relaciones?
        Tiene publicaciones? (NO SON RELEVANTES PERO LO PONGO PORQUE HAY QUE PONERLO, NO LAS UTILIZO PARA NADA)
    u: Usuario
        Tiene relacion?
        No tiene relacion?
Paso 5: Determinar elecciones
Para cada categoría, determinamos sus elecciones o choices:
    red: 
        Tiene usuarios? (Si,no)
        Tiene relaciones?(Si,no) 
    u: Usuario
        Tiene relacion? (Si,no)
            -Tiene 3
            -Tiene 1
            -Tiene 0
Paso 6: Clasificar las elecciones
Consideraremos las siguientes clasificaciones:
    red: 
        Tiene usuarios? (Si,no)
        Tiene relaciones?(Si,no)
    u: Usuario
        Tiene relacion? (Si,no)
            -Tiene 3
            -Tiene 1
            -Tiene 0
Paso 7: Armar los casos de test
Red con usuarios, sin relaciones, usuario sin relación.
Red con usuarios, con relaciones, usuario sin relación.
Red con usuarios, con relaciones, usuario con una relación.
Red con usuarios, con relaciones, usuario con tres relaciones.
Red sin usuarios, sin relaciones, usuario sin relación. (Este caso no es válido debido a que el usuario debe pertenecer a la red, pero lo menciono para completar las combinaciones)

-}

--Caso de prueba 1:Red social válida, Con relaciones, Usuario válido,Usuario pertenece a la red
red1 = [(1, [2, 3]), (2, [1]), (3, [1])]
u1 = 1
testsSuiteAmigosDe = test [
    " amigosDe 1" ~: (amigosDe redA usuario1) ~?= [usuario2, usuario4], --Test de la catedra

 ]
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

-- Ejemplos

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])


usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)
