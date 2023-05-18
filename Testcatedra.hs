-- Vladi: Va a tirar error la primera vez que lo ejecutes y despues copia este codigo :set -package base y luego volver a loadear el archivo para que funcione y correr main
import Test.HUnit
import Solucion

main = runTestTT tests
--Vladi: Vamos a hacer una secuencia de test para cada ejercicios y luegos vamos a poner estas secuencias en el main para que corran todos los test para todos los ejercios
tests = test [
   
    testsSuiteNombresDeUsuarios,

    testsSuiteAmigosDe,

    testsSuiteCantidadDeAmigos,

    testsSuiteUsuarioConMasAmigos,

    testsSuiteEstaRobertoCarlos,

    testsSuitePublicacionesDe,

    testsSuitepublicacionesQueLeGustanA,
    
    testSuitelesGustanLasMismasPublicaciones,

    testsSuiteTieneUnSeguidorFiel,
    
    testsSuiteexisteSecuenciaDeAmigose

 ]
--Comentario Vladi: Ya prepare todo para que funcionen los suit cases

------------------------------------Test de Santi: nombresDeUsuarios--------------------------------------------------
{-Paso 1: Descomponer la solución informática en unidades funcionales
En este caso, tenemos dos unidades funcionales:
  nombresDeUsuarios
  proyectarNombres
Paso 2: Elegir una unidad funcional
La primer unidad funcional a testear será:
  proyectarNombres (debido a que nombresDeUsuarios depende ella)
Paso 3: Identificar factores
Los factores en este caso son los parámetros del problema:
  us: [Usuario]
Paso 4: Determinar categorías
Para cada parámetro, determinamos las siguientes características:
  us:
    Tiene usuarios?
Paso 5: Determinar elecciones
Para cada categoría, determinamos sus elecciones o choices:
  us:
    Tiene usuarios?
      -Tiene 0
      -Tiene 1
      -Tiene 5 con nombres de usuario sin repetir
      -Tiene 5 con nombres de usuarios repetidos
Paso 6: Clasificar las elecciones
Consideraremos las siguientes clasificaciones:
  us:
    Tiene usuarios? 
      -Tiene 0, tipo único
      -Tiene 1, tipo único
      -Tiene 5 (sin repetidos), tipo único
      -Tiene 5 (con repetidos), tipo único
Paso 7: Armar los casos de test
Caso 1: Lista sin usuarios. Resultado esperado: Lista vacía.
Caso 2: Lista con 1 usuario. Resultado esperado: Lista con el único nombre de usuario.
Caso 3: Lista con 5 usuarios, sin repetidos. Resultado esperado: Lista con los nombres de usuario.
Caso 4: Lista con 5 usuarios, con repetidos. Resultado esperado: Lista con los nombres de usuario sin repetidos.
-}
--Defino los usuarios:
proyectarNombres_usuario1 = (1,"Santi")
proyectarNombres_usuario2 = (2,"Vladi")
proyectarNombres_usuario3 = (3,"Dani")
proyectarNombres_usuario4 = (4,"Antú")
proyectarNombres_usuario5 = (5,"DiCaprio")
proyectarNombres_usuario6 = (6,"DiCaprio")
proyectarNombres_usuario7 = (7,"Vladi")


--Defino las listas de usuarios:
proyectarNombres_lista_usuarios1 = []
proyectarNombres_lista_usuarios2 = [(3,"Dani")]
proyectarNombres_lista_usuarios3 = [(1,"Santi"), (2,"Vladi"), (3,"Dani"), (4,"Antu"), (5,"DiCaprio")]
proyectarNombres_lista_usuarios4 = [(5,"DiCaprio"), (7,"Vladi"), (2, "Vladi"), (4,"Antu"), (6,"DiCaprio")]

testsSuiteProyectarNombres = test [
    "proyectarNombres - Caso 1: Lista sin usuarios" ~: (proyectarNombres proyectarNombres_lista_usuarios1) ~?= [],
    "proyectarNombres - Caso 2: Lista con un usuario" ~: (proyectarNombres proyectarNombres_lista_usuarios2) ~?= ["Dani"],
    "proyectarNombres - Caso 3: Lista con cinco usuarios, sin repetidos" ~: (proyectarNombres proyectarNombres_lista_usuarios3) ~?= ["Santi", "Vladi", "Dani", "Antu", "DiCaprio"],
    "proyectarNombres - Caso 4: Lista con cinco usuarios, con repetidos" ~: (proyectarNombres proyectarNombres_lista_usuarios4) ~?= ["Vladi", "Antu", "DiCaprio"]
  ]

{-Paso 8: Terminé de testear ProyectarNombres, vuelvo al paso 2.
Paso 2: Paso 2: Elegir una unidad funcional
La segunda unidad funcional a testear será:
  nombresDeUsuarios
Paso 3: Identificar factores
Los factores en este caso son los parámetros del problema:
  red: RedSocial
Paso 4: Determinar categorías
Para cada parámetro, determinamos las siguientes características:
  red:
    Tiene usuarios?
Paso 5: Determinar elecciones
Para cada categoría, determinamos sus elecciones o choices:
  red:
    Tiene usuarios?
      -Tiene 0
      -Tiene 1
      -Tiene 5 con nombres de usuario sin repetir
      -Tiene 5 con nombres de usuarios repetidos
Paso 6: Clasificar las elecciones
Consideraremos las siguientes clasificaciones:
  red:
    Tiene usuarios? 
      -Tiene 0, tipo único
      -Tiene 1, tipo único
      -Tiene 5 (sin repetidos), tipo único
      -Tiene 5 (con repetidos), tipo único
Paso 7: Armar los casos de test
Caso 1: Red sin usuarios. Resultado esperado: Lista vacía.
Caso 2: Red con 1 usuario. Resultado esperado: Lista con el nombre de usuario del usuario.
Caso 3: Red con 5 usuarios, sin repetidos. Resultado esperado: Lista con los nombres de usuario de los usuarios.
Caso 4: Red con 5 usuarios, con repetidos. Resultado esperado: Lista con los nombres de usuarios sin repetidos.
-}

--Defino los usuarios:
nombresDeUsuarios_usuario1 = (1,"Santi")
nombresDeUsuarios_usuario2 = (2,"Vladi")
nombresDeUsuarios_usuario3 = (3,"Dani")
nombresDeUsuarios_usuario4 = (4,"Antú")
nombresDeUsuarios_usuario5 = (5,"DiCaprio")
nombresDeUsuarios_usuario6 = (6,"DiCaprio")
nombresDeUsuarios_usuario7 = (7,"Vladi")


--Defino las redes:
nombresDeUsuarios_red1 = ([],[],[])
nombresDeUsuarios_red2 = ([(3,"Dani")],[],[])
nombresDeUsuarios_red3 = ([(1,"Santi"), (2,"Vladi"), (3,"Dani"), (4,"Antu"), (5,"DiCaprio")], [], [])
nombresDeUsuarios_red4 = ([(5,"DiCaprio"), (7,"Vladi"), (2, "Vladi"), (4,"Antu"), (6,"DiCaprio")], [], [])

testsSuiteNombresDeUsuarios = test [
    "nombresDeUsuarios - Caso 0" ~: (nombresDeUsuarios redA) ~?= ["Juan","Natalia","Pedro","Mariela"], --Test de la cátedra
    "nombresDeUsuarios - Caso 1: Red sin usuarios" ~: (nombresDeUsuarios nombresDeUsuarios_red1) ~?= [],
    "nombresDeUsuarios - Caso 2: Red con un usuario" ~: (nombresDeUsuarios nombresDeUsuarios_red2) ~?= ["Dani"],
    "nombresDeUsuarios - Caso 3: Red con cinco usuarios, sin repetidos" ~: (nombresDeUsuarios nombresDeUsuarios_red3) ~?= ["Santi", "Vladi", "Dani", "Antu", "DiCaprio"],
    "nombresDeUsuarios - Caso 4: Red con cinco usuarios, con repetidos" ~: (nombresDeUsuarios nombresDeUsuarios_red4) ~?= ["Vladi", "Antu", "DiCaprio"]
  ]
------------------------------------Fin Test de Santi: nombresDeUsuarios--------------------------------------------------


------------------------------------Test de Santi: estaRobertoCarlos--------------------------------------------------
{-Paso 1: Descomponer la solución informática en unidades funcionales
En este caso, tenemos una única unidad funcional:
  estaRobertoCarlos
Paso 2: Elegir una unidad funcional
La unidad funcional a testear será:
  estaRobertoCarlos
Paso 3: Identificar factores
Los factores en este caso son los parámetros del problema:
  red: RedSocial
Paso 4: Determinar categorías
Para cada parámetro, determinamos las siguientes características:
  red:
    Tiene usuarios?
    Tiene relaciones?
Paso 5: Determinar elecciones
Para cada categoría, determinamos sus elecciones o choices:
  red:
    Tiene usuarios?
      -Tiene 0
      -Tiene 2
      -Tiene 12
    Tiene relaciones?
      -Tiene 0
      -Tiene 1
      -Tiene 11
Paso 6: Clasificar las elecciones
Consideraremos las siguientes clasificaciones:
  red:
    Tiene usuarios? 
      -Tiene 0, tipo único
      -Tiene 2, tipo único
      -Tiene 12, tipo combinable
    Tiene relaciones?
      -Tiene 0, tipo único
      -Tiene 2, tipo único
      -Tiene 12, tipo combinable
        Entre quienes son la relaciones?
          -12 relaciones de diferentes usuarios
          -11 relaciones del mismo usuario, una de otro usuario
Paso 7: Armar los casos de test
Caso 1: Red sin usuarios. Resultado esperado: False.
Caso 2: Red con 2 usuarios. Resultado esperado: False.
Caso 3: Red con 12 usuarios, con 0 relaciones. Resultado esperado: False.
Caso 4: Red con 12 usuarios, con 2 relaciones. Resultado esperado: False.
Caso 5: Red con 12 usuarios, con 12 relaciones de diferentes usuarios. Resultado esperado: False.
Caso 6: Red con 12 usuarios, con 12 relaciones y 11 realciones del mismo usuario. Resultado esperado: True.
-}

--e_r_c es abreviación de estaRobertoCarlos
--Defino los usuarios:
e_r_c_usuario1 = (1,"Santi")
e_r_c_usuario2 = (2,"Vladi")
e_r_c_usuario3 = (3,"Dani")
e_r_c_usuario4 = (4,"Antú")
e_r_c_usuario5 = (5,"Santi")
e_r_c_usuario6 = (6,"Roberto Carlos")
e_r_c_usuario7 = (7,"Messi")
e_r_c_usuario8 = (8,"Scaloni")
e_r_c_usuario9 = (9,"Palermo")
e_r_c_usuario10 = (10,"Riquelme")
e_r_c_usuario11 = (11,"Maradona")
e_r_c_usuario12 = (12,"Bianchi")

--Defino las relaciones:
e_r_c_relacion_6_1 = (e_r_c_usuario6, e_r_c_usuario1)
e_r_c_relacion_6_2 = (e_r_c_usuario6, e_r_c_usuario2)
e_r_c_relacion_6_3 = (e_r_c_usuario6, e_r_c_usuario3)
e_r_c_relacion_6_4 = (e_r_c_usuario6, e_r_c_usuario4)
e_r_c_relacion_6_5 = (e_r_c_usuario6, e_r_c_usuario5)
e_r_c_relacion_6_7 = (e_r_c_usuario6, e_r_c_usuario7)
e_r_c_relacion_6_8 = (e_r_c_usuario6, e_r_c_usuario8)
e_r_c_relacion_6_9 = (e_r_c_usuario6, e_r_c_usuario9)
e_r_c_relacion_6_10 = (e_r_c_usuario6, e_r_c_usuario10)
e_r_c_relacion_6_11 = (e_r_c_usuario6, e_r_c_usuario11)
e_r_c_relacion_6_12 = (e_r_c_usuario6, e_r_c_usuario12)
e_r_c_relacion_7_9 = (e_r_c_usuario7, e_r_c_usuario9)
e_r_c_relacion_10_12 = (e_r_c_usuario10, e_r_c_usuario12)


--Defino las redes, una por cada caso:
estaRobertoCarlos_red_1 = ([],[],[])
estaRobertoCarlos_red_2 = ([e_r_c_usuario1,e_r_c_usuario3],[],[])
estaRobertoCarlos_red_3 =
    ([e_r_c_usuario1, e_r_c_usuario2, e_r_c_usuario3, e_r_c_usuario4,
      e_r_c_usuario5, e_r_c_usuario6, e_r_c_usuario7, e_r_c_usuario8,
      e_r_c_usuario9, e_r_c_usuario10, e_r_c_usuario11, e_r_c_usuario12],

      [],
      
      [])
estaRobertoCarlos_red_4 =
    ([e_r_c_usuario1, e_r_c_usuario2, e_r_c_usuario3, e_r_c_usuario4,
      e_r_c_usuario5, e_r_c_usuario6, e_r_c_usuario7, e_r_c_usuario8,
      e_r_c_usuario9, e_r_c_usuario10, e_r_c_usuario11, e_r_c_usuario12],

     [e_r_c_relacion_6_5,e_r_c_relacion_10_12],

      [])
estaRobertoCarlos_red_5 =
    ([e_r_c_usuario1, e_r_c_usuario2, e_r_c_usuario3, e_r_c_usuario4,
      e_r_c_usuario5, e_r_c_usuario6, e_r_c_usuario7, e_r_c_usuario8,
      e_r_c_usuario9, e_r_c_usuario10, e_r_c_usuario11, e_r_c_usuario12],

     [e_r_c_relacion_6_1, e_r_c_relacion_6_2, e_r_c_relacion_6_3, e_r_c_relacion_6_4,
      e_r_c_relacion_6_5, e_r_c_relacion_6_7, e_r_c_relacion_6_8, e_r_c_relacion_6_9,
      e_r_c_relacion_6_10, e_r_c_relacion_6_11, e_r_c_relacion_7_9, e_r_c_relacion_10_12],

      []) 
estaRobertoCarlos_red_6 = 
    ([e_r_c_usuario1, e_r_c_usuario2, e_r_c_usuario3, e_r_c_usuario4,
      e_r_c_usuario5, e_r_c_usuario6, e_r_c_usuario7, e_r_c_usuario8,
      e_r_c_usuario9, e_r_c_usuario10, e_r_c_usuario11, e_r_c_usuario12],

     [e_r_c_relacion_6_1, e_r_c_relacion_6_2, e_r_c_relacion_6_3, e_r_c_relacion_6_4,
      e_r_c_relacion_6_5, e_r_c_relacion_6_7, e_r_c_relacion_6_8, e_r_c_relacion_6_9,
      e_r_c_relacion_6_10, e_r_c_relacion_6_11, e_r_c_relacion_6_12],

      []) 

testsSuiteEstaRobertoCarlos = test [
    "estaRobertoCarlos - Caso 0" ~: (estaRobertoCarlos redA) ~?= False, --Test de la cátedra
    "estaRobertoCarlos - Caso 1: Red sin usuarios" ~: (estaRobertoCarlos estaRobertoCarlos_red_1) ~?= False,
    "estaRobertoCarlos - Caso 2: Red con dos usuarios" ~: (estaRobertoCarlos estaRobertoCarlos_red_2) ~?= False,
    "estaRobertoCarlos - Caso 3: Red con doce usuarios, cero relaciones" ~: (estaRobertoCarlos estaRobertoCarlos_red_3) ~?= False,
    "estaRobertoCarlos - Caso 4: Red con doce usuarios, dos relaciones" ~: (estaRobertoCarlos estaRobertoCarlos_red_4) ~?= False,
    "estaRobertoCarlos - Caso 5: Red con doce usuarios, doce relaciones de diferentes usuarios" ~: (estaRobertoCarlos estaRobertoCarlos_red_5) ~?= False,
    "estaRobertoCarlos - Caso 6: Red con doce usuarios, once relaciones del mismo usuario" ~: (estaRobertoCarlos estaRobertoCarlos_red_6) ~?= True
 ]

------------------------------------Fin Test de Santi: estaRobertoCarlos--------------------------------------------------

------------------------------------Test de vladi: amigosDe--------------------------------------------------

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
Caso 1:Red con usuarios, sin relaciones, usuario sin relación.
Caso 2:Red con usuarios, con relaciones, usuario sin relación.
Caso 3:Red con usuarios, con relaciones, usuario con una relación.
Caso 4:Red con usuarios, con relaciones, usuario con tres relaciones.
Caso 5:Red sin usuarios, sin relaciones, usuario sin relación. (Este caso no es válido debido a que el usuario debe pertenecer a la red, pero lo menciono para completar las combinaciones)

-}
--Definimos usuarios
amigosDe_usuario1 = (1, "Vladimir")
amigosDe_usuario2 = (2, "Lionel Messi")
amigosDe_usuario3 = (3, "Sergio Agüero")
amigosDe_usuario4 = (4, "Paulo Dybala")
amigosDe_usuario5 = (5, "Ángel Di Maria") -- Este usuario no pertenece a ninguna red

--Definimos redes
amigosDe_red1 = ([amigosDe_usuario1, amigosDe_usuario2, amigosDe_usuario3], [(amigosDe_usuario1, amigosDe_usuario2), (amigosDe_usuario1, amigosDe_usuario3)], []) -- Red con relaciones
amigosDe_red2 = ([amigosDe_usuario2], [], []) -- Red sin relaciones
amigosDe_red3 = ([amigosDe_usuario2, amigosDe_usuario3], [(amigosDe_usuario2, amigosDe_usuario3)], []) -- Red con una relacion
amigosDe_red4 = ([amigosDe_usuario1, amigosDe_usuario2, amigosDe_usuario3, amigosDe_usuario4], [(amigosDe_usuario1, amigosDe_usuario2), (amigosDe_usuario1, amigosDe_usuario3), (amigosDe_usuario1, amigosDe_usuario4)], []) -- Red con tres relaciones
amigosDe_red5 = ([amigosDe_usuario2], [], []) -- Red sin el usuario que se prueba

testsSuiteAmigosDe = test [
    "amigosDe 1" ~: (amigosDe redA usuario1) ~?= [usuario2, usuario4], --Test de la catedra
   "amigosDe - Caso 1: Red con relaciones, usuario con dos relaciones" ~: (amigosDe amigosDe_red1 amigosDe_usuario1) ~?= [amigosDe_usuario2, amigosDe_usuario3],
    "amigosDe - Caso 2: Red sin relaciones, usuario sin relaciones" ~: (amigosDe amigosDe_red2 amigosDe_usuario2) ~?= [],
    "amigosDe - Caso 3: Red con una relacion, usuario con una relacion" ~: (amigosDe amigosDe_red3 amigosDe_usuario2) ~?= [amigosDe_usuario3],
    "amigosDe - Caso 4: Red con tres relaciones, usuario con tres relaciones" ~: (amigosDe amigosDe_red4 amigosDe_usuario1) ~?= [amigosDe_usuario2, amigosDe_usuario3, amigosDe_usuario4],
    "amigosDe - Caso 5: Red sin usuario en prueba, usuario no existente en la red" ~: (amigosDe amigosDe_red5 amigosDe_usuario5) ~?= []
 ]
-------------------------------------Fin Test de vladi: amigosDe--------------------------------------------------

------------------------------------Test de vladi: cantidadDeAmigos--------------------------------------------------
{-Paso 1: Descomponer la solución informática en unidades funcionales
En este caso, tenemos una única unidad funcional:
cantidadDeAmigos
Paso 2: Elegir una unidad funcional
La unidad funcional a testear será:
cantidadDeAmigos
Paso 3: Identificar factores
Los factores en este caso son los parámetros del problema:
red: RedSocial
u: Usuario
Paso 4: Determinar categorías
Para cada parámetro, determinamos las siguientes características:
red:
Tiene usuarios?
Tiene relaciones?
u: Usuario
Tiene relacion?
No tiene relacion?
Paso 5: Determinar elecciones
Para cada categoría, determinamos sus elecciones o choices:
red:
Tiene usuarios? (Si, no)
Tiene relaciones?(Si, no)
u: Usuario
Tiene relacion? (Si, no)
-Tiene 5
-Tiene 3
-Tiene 1
-Tiene 0
Paso 6: Clasificar las elecciones
Consideraremos las siguientes clasificaciones:
red:
Tiene usuarios? (Si, no)
Tiene relaciones? (Si, no)
u: Usuario
Tiene relacion? (Si, no)
-Tiene 5
-Tiene 3
-Tiene 1
-Tiene 0
Paso 7: Armar los casos de test
Caso 1: Red con usuarios, sin relaciones, usuario sin relación.
Caso 2: Red con usuarios, con relaciones, usuario sin relación.
Caso 3: Red con usuarios, con relaciones, usuario con una relación.
Caso 4: Red con usuarios, con relaciones, usuario con tres relaciones.
Caso 5: Red con usuarios, con relaciones, usuario con cinco relaciones.
Caso 6: Red sin usuarios, sin relaciones, usuario sin relación. (Este caso no es válido debido a que el usuario debe pertenecer a la red, pero se menciona para completar las combinaciones)
-}

------------------------------------Fin Test de vladi: cantidadDeAmigos--------------------------------------------------
--Definimos usuarios
cantidadDeAmigos_usuario1 :: (Integer, String)
cantidadDeAmigos_usuario1 = (1, "Vladimir")
cantidadDeAmigos_usuario2 = (2, "Lionel Messi")
cantidadDeAmigos_usuario3 = (3, "Sergio Agüero")
cantidadDeAmigos_usuario4 = (4, "Paulo Dybala")
cantidadDeAmigos_usuario5 = (5, "Ángel Di Maria") 
cantidadDeAmigos_usuario6 = (6, "Neymar") 

--Definimos redes
cantidadDeAmigos_red1 = ([cantidadDeAmigos_usuario1, cantidadDeAmigos_usuario2, cantidadDeAmigos_usuario3], [(cantidadDeAmigos_usuario1, cantidadDeAmigos_usuario2), (cantidadDeAmigos_usuario1, cantidadDeAmigos_usuario3)], []) -- Red con relaciones
cantidadDeAmigos_red2 = ([cantidadDeAmigos_usuario2], [], []) -- Red sin relaciones
cantidadDeAmigos_red3 = ([cantidadDeAmigos_usuario2, cantidadDeAmigos_usuario3], [(cantidadDeAmigos_usuario2, cantidadDeAmigos_usuario3)], []) -- Red con una relacion
cantidadDeAmigos_red4 = ([cantidadDeAmigos_usuario1, cantidadDeAmigos_usuario2, cantidadDeAmigos_usuario3, cantidadDeAmigos_usuario4], [(cantidadDeAmigos_usuario1, cantidadDeAmigos_usuario2), (cantidadDeAmigos_usuario1, cantidadDeAmigos_usuario3), (cantidadDeAmigos_usuario1, cantidadDeAmigos_usuario4)], []) -- Red con tres relaciones
cantidadDeAmigos_red5 = ([cantidadDeAmigos_usuario1, cantidadDeAmigos_usuario2, cantidadDeAmigos_usuario3, cantidadDeAmigos_usuario4, cantidadDeAmigos_usuario5, cantidadDeAmigos_usuario6], [(cantidadDeAmigos_usuario1, cantidadDeAmigos_usuario2), (cantidadDeAmigos_usuario1, cantidadDeAmigos_usuario3), (cantidadDeAmigos_usuario1, cantidadDeAmigos_usuario4), (cantidadDeAmigos_usuario1, cantidadDeAmigos_usuario5), (cantidadDeAmigos_usuario1, cantidadDeAmigos_usuario6)], []) -- Red con cinco relaciones
cantidadDeAmigos_red6 = ([], [], []) -- Red sin el usuario que se prueba


testsSuiteCantidadDeAmigos  = test [
    "cantidadDeAmigos 1" ~: (cantidadDeAmigos redA usuario1) ~?= 2, --Test de la catedra
    "cantidadDeAmigos - Caso 1: Red con relaciones, usuario con dos relaciones" ~: (cantidadDeAmigos cantidadDeAmigos_red1 cantidadDeAmigos_usuario1) ~?= 2,
    "cantidadDeAmigos - Caso 2: Red sin relaciones, usuario sin relaciones" ~: (cantidadDeAmigos cantidadDeAmigos_red2 cantidadDeAmigos_usuario2) ~?= 0,
    "cantidadDeAmigos - Caso 3: Red con una relacion, usuario con una relacion" ~: (cantidadDeAmigos cantidadDeAmigos_red3 cantidadDeAmigos_usuario2) ~?= 1,
    "cantidadDeAmigos - Caso 4: Red con tres relaciones, usuario con tres relaciones" ~: (cantidadDeAmigos cantidadDeAmigos_red4 cantidadDeAmigos_usuario1) ~?= 3,
    "cantidadDeAmigos - Caso 5: Red con cinco relaciones, usuario con cinco relaciones" ~: (cantidadDeAmigos cantidadDeAmigos_red5 cantidadDeAmigos_usuario1) ~?= 5
 ]



-------------------------------Test de vladi: existeSecuenciaDeAmigos-----------------------------------------
{-
Paso 1: Descomponer la solución informática en unidades funcionales
Tenemos varias unidades funcionales en este caso:
- existeSecuenciaDeAmigos
- esAmigoQueBuscamos
- usuarioYaVisitado

Paso 2: Elegir una unidad funcional
La unidad funcional a testear será:
- existeSecuenciaDeAmigos

Paso 3: Identificar factores
Los factores en este caso son los parámetros del problema:
- red: RedSocial
- u1: Usuario
- u2: Usuario

Paso 4: Determinar categorías
Para cada parámetro, determinamos las siguientes características:
- red:
- Tiene usuarios?
- Tiene relaciones?
- u1: Usuario
- Tiene relaciones?
- u2: Usuario
- Tiene relaciones?
- Relación entre Usuario1 y Usuario2?

Paso 5: Determinar elecciones
Para cada categoría, determinamos sus elecciones:
- red:
- Tiene usuarios? (Si, no)
- Tiene relaciones? (Si, no)
- u1: Usuario
- Tiene relaciones? (Si, no)
- u2: Usuario
- Tiene relaciones? (Si, no)
- Relación entre Usuario1 y Usuario2? (Están conectados directamente, Están conectados a través de un amigo en común, No están conectados)

Paso 6: Clasificar las elecciones
Las clasificaciones son las mismas que las elecciones detalladas en el Paso 5.

Paso 7: Armar los casos de test

Caso 1: Esto no cumple con el requisito porque la red no es válida, ya que no tiene usuarios.
Red sin usuarios, sin relaciones.
Usuario1 y Usuario2 sin relaciones.
Resultado esperado: False.
Caso 2: Esto no cumple con el requisito ya que los usuarios no están en la red.
Red con usuarios pero sin relaciones.
Usuario1 y Usuario2 sin relaciones.
Resultado esperado: False.
Caso 3:False.
Red con usuarios y relaciones.
Usuario1 con relaciones, Usuario2 sin relaciones.
Caso 4: False.
Red con usuarios y relaciones.
Usuario1 con una relación, Usuario2 con una relación, pero no están conectados.
Caso 5:True.
Red con usuarios y relaciones.
Usuario1 con una relación, Usuario2 con una relación, y están conectados directamente. 
Caso 6:True.
Red con usuarios y relaciones.
Usuario1 con varias relaciones, Usuario2 con una relación, y están conectados a través de un amigo en común.
Caso 7: True.
Red con usuarios y relaciones.
Usuario1 y Usuario2 son la misma persona. 


---Casos extras que pense que son mas complicados de resolver
Caso 8: La red social es un grafo no conectado. No hay un camino entre el Usuario1 y Usuario2.

Caso 9: Hay múltiples caminos posibles entre el Usuario1 y Usuario2.
Caso 10: False.
Red con usuarios y relaciones.
Usuario1 y Usuario2 están en la misma red, pero están en subgrafos separados, lo que significa que no hay un camino que conecte directamente a Usuario1 con Usuario2.
-}
-- Usuarios
vladimir = (1, "Vladimir")
messi = (2, "Messi")
pedro = (3, "Pedro")
lucas = (4, "Lucas")
juan = (5, "Juan")
-- Usuarios adicionales
maria = (6, "Maria")
diego = (7, "Diego")
pepe = (8, "Pepe")
-- Usuarios adicionales para el caso 10
aguero = (9, "Aguero")
dybala = (10, "Dybala")
martinez = (11, "Martinez")
correa = (12, "Correa")
pereyra = (13, "Pereyra")
papu = (14, "Papu")
otamendi = (15, "Otamendi")
tagliafico = (16, "Tagliafico")
paredes = (17, "Paredes")
dimaria = (18, "Di Maria")
-- Usuarios adicionales para el caso 11
mbappe = (19, "Mbappe")
pogba = (20, "Pogba")
kante = (21, "Kante")
varane = (22, "Varane")
giroud = (23, "Giroud")
lloris = (24, "Lloris")
dembele = (25, "Dembele")
griezmann = (26, "Griezmann")
-- Usuarios adicionales para el caso 12
modric = (27, "Modric")
rakitic = (28, "Rakitic")
perisic = (29, "Perisic")
rebic = (30, "Rebic")
mandzukic = (31, "Mandzukic")
vida = (32, "Vida")
lovren = (33, "Lovren")
brozovic = (34, "Brozovic")
kovacic = (35, "Kovacic")
kramaric = (36, "Kramaric")
vrsaljko = (37, "Vrsaljko")
jedvaj = (38, "Jedvaj")
brekalo = (39, "Brekalo")
barisic = (40, "Barisic")
-- Usuarios adicionales para el caso 13
badelj = (41, "Badelj")
pjaca = (42, "Pjaca")
kalinic = (43, "Kalinic")
-- Usuarios adicionales para el caso 14
neymar = (44, "Neymar")
alves = (45, "Alves")
silva = (46, "Silva")
coutinho = (47, "Coutinho")
-- Relaciones
relacion_vladimir_messi = (vladimir, messi)
relacion_messi_pedro = (messi, pedro)
relacion_vladimir_juan = (vladimir, juan)
relacion_messi_juan = (messi, juan)
relacion_lucas_juan = (lucas, juan)

-- Relaciones adicionales
relacion_vladimir_maria = (vladimir, maria)
relacion_maria_diego = (maria, diego)
relacion_diego_pepe = (diego, pepe)
relacion_pepe_juan = (pepe, juan)

-- Relaciones adicionales para el caso 10
relacion_aguero_dybala = (aguero, dybala)
relacion_dybala_martinez = (dybala, martinez)
relacion_martinez_correa = (martinez, correa)
relacion_correa_pereyra = (correa, pereyra)
relacion_pereyra_papu = (pereyra, papu)
relacion_papu_otamendi = (papu, otamendi)
relacion_otamendi_tagliafico = (otamendi, tagliafico)
relacion_tagliafico_paredes = (tagliafico, paredes)
relacion_paredes_dimaria = (paredes, dimaria)

-- Relaciones adicionales para el caso 11
relacion_mbappe_pogba = (mbappe, pogba)
relacion_pogba_kante = (pogba, kante)
relacion_kante_varane = (kante, varane)
relacion_kante_giroud = (kante, giroud)
relacion_varane_lloris = (varane, lloris)
relacion_varane_giroud = (varane, giroud)
relacion_lloris_dembele = (lloris, dembele)
relacion_giroud_dembele = (giroud, dembele)
relacion_dembele_griezmann = (dembele, griezmann)
-- Relaciones adicionales para el caso 12
relacion_modric_rakitic = (modric, rakitic)
relacion_modric_perisic = (modric, perisic)
relacion_rakitic_rebic = (rakitic, rebic)
relacion_rakitic_mandzukic = (rakitic, mandzukic)
relacion_perisic_vida = (perisic, vida)
relacion_perisic_lovren = (perisic, lovren)
relacion_rebic_brozovic = (rebic, brozovic)
relacion_rebic_kovacic = (rebic, kovacic)
relacion_mandzukic_kramaric = (mandzukic, kramaric)
relacion_mandzukic_vrsaljko = (mandzukic, vrsaljko)
relacion_kovacic_jedvaj = (kovacic, jedvaj)
relacion_kovacic_brekalo = (kovacic, brekalo)
relacion_vrsaljko_barisic = (vrsaljko, barisic)
-- Relaciones adicionales para el caso 13
relacion_modric_badelj = (modric, badelj)
relacion_mandzukic_pjaca = (mandzukic, pjaca)
relacion_kovacic_kalinic = (kovacic, kalinic)
relacion_lovren_brozovic = (lovren, brozovic)
relacion_badelj_kramaric = (badelj, kramaric)
relacion_brekalo_mandzukic = (brekalo, mandzukic)
-- Relaciones adicionales para el caso 14
relacion_neymar_alves = (neymar, alves)
relacion_alves_silva = (alves, silva)
relacion_silva_coutinho = (silva, coutinho)
-- Publicaciones
publicacionesTest = [] -- no las estamos utilizando en estos casos de prueba asi que no es relevante las publicaciones

-- Redes Sociales
red_sin_conexiones = ([vladimir, juan], [], publicacionesTest)
red_con_conexion_directa = ([vladimir, juan], [relacion_vladimir_juan], publicacionesTest)
red_con_conexion_indirecta = ([vladimir, messi, juan], [relacion_vladimir_messi, relacion_messi_juan], publicacionesTest)
red_con_multiples_conexiones = ([vladimir, messi, pedro, juan], [relacion_vladimir_messi, relacion_messi_pedro, relacion_lucas_juan], publicacionesTest)

-- Redes Sociales adicionales
red_no_conectada = ([vladimir, messi, maria, diego, pepe, juan], [relacion_vladimir_messi, relacion_maria_diego], publicacionesTest)
red_con_multiples_caminos = ([vladimir, messi, pedro, lucas, maria, diego, pepe, juan], [relacion_vladimir_messi, relacion_messi_pedro, relacion_lucas_juan, relacion_vladimir_maria, relacion_maria_diego, relacion_diego_pepe, relacion_pepe_juan], publicacionesTest)
--Red social de la seleccion
red_seleccion_argentina = 
  ( [vladimir, messi, pedro, lucas, maria, diego, pepe, juan, aguero, dybala, martinez, correa, pereyra, papu, otamendi, tagliafico, paredes, dimaria]
  , [ relacion_vladimir_messi
    , relacion_messi_pedro
    , relacion_lucas_juan
    , relacion_vladimir_maria
    , relacion_maria_diego
    , relacion_diego_pepe
    , relacion_pepe_juan
    , relacion_aguero_dybala
    , relacion_dybala_martinez
    , relacion_martinez_correa
    , relacion_correa_pereyra
    , relacion_pereyra_papu
    , relacion_papu_otamendi
    , relacion_otamendi_tagliafico
    , relacion_tagliafico_paredes
    , relacion_paredes_dimaria
    ]
  , publicacionesTest
  )
-- Red social adicional para el caso 11
red_francia = 
  ( [mbappe, pogba, kante, varane, giroud, lloris, dembele, griezmann]
  , [ relacion_mbappe_pogba
    , relacion_pogba_kante
    , relacion_kante_varane
    , relacion_kante_giroud
    , relacion_varane_lloris
    , relacion_varane_giroud
    , relacion_lloris_dembele
    , relacion_giroud_dembele
    , relacion_dembele_griezmann
    ]
  , publicacionesTest
  )
  -- Red social adicional para el caso 12
red_croacia = 
  ( [modric, rakitic, perisic, rebic, mandzukic, vida, lovren, brozovic, kovacic, kramaric, vrsaljko, jedvaj, brekalo, barisic]
  , [ relacion_modric_rakitic
    , relacion_modric_perisic
    , relacion_rakitic_rebic
    , relacion_rakitic_mandzukic
    , relacion_perisic_vida
    , relacion_perisic_lovren
    , relacion_rebic_brozovic
    , relacion_rebic_kovacic
    , relacion_mandzukic_kramaric
    , relacion_mandzukic_vrsaljko
    , relacion_kovacic_jedvaj
    , relacion_kovacic_brekalo
    , relacion_vrsaljko_barisic
    ]
  , publicacionesTest
  )
-- Red social adicional para el caso 13
red_croacia_compleja = 
  ( [modric, rakitic, perisic, rebic, mandzukic, vida, lovren, brozovic, kovacic, kramaric, vrsaljko, jedvaj, brekalo, barisic, badelj, pjaca, kalinic]
  , [ relacion_modric_rakitic
    , relacion_modric_perisic
    , relacion_modric_badelj
    , relacion_rakitic_rebic
    , relacion_rakitic_mandzukic
    , relacion_perisic_vida
    , relacion_perisic_lovren
    , relacion_rebic_brozovic
    , relacion_rebic_kovacic
    , relacion_mandzukic_kramaric
    , relacion_mandzukic_vrsaljko
    , relacion_mandzukic_pjaca
    , relacion_kovacic_jedvaj
    , relacion_kovacic_brekalo
    , relacion_kovacic_kalinic
    , relacion_vrsaljko_barisic
    , relacion_lovren_brozovic
    , relacion_badelj_kramaric
    , relacion_brekalo_mandzukic
    ]
  , publicacionesTest
  )
  -- Red social adicional para el caso 14
red_brasil = 
  ( [neymar, alves, silva, coutinho]
  , [ relacion_neymar_alves
    , relacion_alves_silva
    , relacion_silva_coutinho
    ]
  , publicacionesTest
  )
testsSuiteexisteSecuenciaDeAmigose = test [
        " existeSecuenciaDeAmigos 1" ~: (existeSecuenciaDeAmigos redA usuario1 usuario3) ~?= True, 
        "Caso 3" ~: existeSecuenciaDeAmigos red_sin_conexiones vladimir juan ~?= False,
        "Caso 4" ~: existeSecuenciaDeAmigos red_con_multiples_conexiones vladimir lucas ~?= False,
        "Caso 5" ~: existeSecuenciaDeAmigos red_con_conexion_directa vladimir juan ~?= True,
        "Caso 6" ~: existeSecuenciaDeAmigos red_con_conexion_indirecta vladimir juan ~?= True,
        "Caso 7" ~: existeSecuenciaDeAmigos red_con_conexion_directa vladimir vladimir ~?= True,
        "Caso 8" ~: existeSecuenciaDeAmigos red_no_conectada vladimir diego ~?= False,
        "Caso 9" ~: existeSecuenciaDeAmigos red_con_multiples_caminos vladimir juan ~?= True,
        "Caso 10" ~: existeSecuenciaDeAmigos red_seleccion_argentina vladimir dimaria ~?= False,
        "Caso 11" ~: existeSecuenciaDeAmigos red_francia mbappe griezmann ~?= True,
        "Caso 12" ~: existeSecuenciaDeAmigos red_croacia modric barisic ~?= True,
        "Caso 13" ~: existeSecuenciaDeAmigos red_croacia_compleja modric kalinic ~?= True,
        "Caso 14" ~: existeSecuenciaDeAmigos red_brasil neymar coutinho ~?= True
 ]

-------------------------------------------Tests usuarioConMasAmigos----------------------------------------------------------
--Definimos Usuarios

usuarioConMasAmigos_usuario1 = (1,"Dani")
usuarioConMasAmigos_usuario2 = (2,"Antu")
usuarioConMasAmigos_usuario3 = (3, "Santi")
usuarioConMasAmigos_usuario4 = (4,"Vladi")

--Definimos Relaciones
usuarioConMasAmigos_relacion1 = (usuarioConMasAmigos_usuario1,usuarioConMasAmigos_usuario2)
usuarioConMasAmigos_relacion2 = (usuarioConMasAmigos_usuario2,usuarioConMasAmigos_usuario3)
usuarioConMasAmigos_relacion3 = (usuarioConMasAmigos_usuario1,usuarioConMasAmigos_usuario4)

--Definimos Redes (no consideramos las publicaciones ya que no son reelevantes)
usuarioConMasAmigos_usuarios = [usuarioConMasAmigos_usuario1,usuarioConMasAmigos_usuario2,usuarioConMasAmigos_usuario3,usuarioConMasAmigos_usuario4]

usuarioConMasAmigos_red1_relacion1 = []
usuarioConMasAmigos_red2_relacion2 = [usuarioConMasAmigos_relacion1, usuarioConMasAmigos_relacion2]
usuarioConMasAmigos_red3_relacion3 = [usuarioConMasAmigos_relacion1, usuarioConMasAmigos_relacion2,usuarioConMasAmigos_relacion3]

usuarioConMasAmigos_red1 = (usuarioConMasAmigos_usuarios, usuarioConMasAmigos_red1_relacion1, [])
usuarioConMasAmigos_red2 = (usuarioConMasAmigos_usuarios, usuarioConMasAmigos_red2_relacion2, [])
usuarioConMasAmigos_red3 = (usuarioConMasAmigos_usuarios, usuarioConMasAmigos_red3_relacion3, [])

testsSuiteUsuarioConMasAmigos = test [
  " usuarioConMasAmigos 1" ~: expectAny (usuarioConMasAmigos redA) [usuario2, usuario4], 
  "Caso 1 : Red sin relaciones" ~: usuarioConMasAmigos usuarioConMasAmigos_red1 ~?= usuarioConMasAmigos_usuario4, --Devuelve el último usuario porque la cantidad de amigos de todos es igual (0) entonces itera hasta llegar al ultimo
  "Caso 2 : Red con alguna relacion" ~: usuarioConMasAmigos usuarioConMasAmigos_red2 ~?= usuarioConMasAmigos_usuario2,
  "Caso 3 : Red con mas relaciones" ~: usuarioConMasAmigos usuarioConMasAmigos_red3  ~?= usuarioConMasAmigos_usuario2 --Igual que en el caso 1, hay dos usuarios con la misma cantidad de amigos y por como itero, devuelvo el último.
        
 ]

--------------------------------------------Test publicacionesDe-------------------------------------------------------------
-- Definimos usuarios
publicacionesDe_usuario1 = (1,"Dani")
publicacionesDe_usuario2 = (2,"Antu")
publicacionesDe_usuario3 = (3, "Santi")

--Definimos Publicaciones (no definimos me gusta porque no son reelevantes)
publicacionesDe_publicacion1 = (publicacionesDe_usuario3,"Publicacion 1",[])
publicacionesDe_publicacion2 = (publicacionesDe_usuario2,"Publicacion 2",[])
publicacionesDe_publicacion3 = (publicacionesDe_usuario3,"Publicacion 3",[])

--Definimos Redes (No definimos relaciones porque no son reelevantes)
publicacionesDe_usuarios = [publicacionesDe_usuario1, publicacionesDe_usuario2, publicacionesDe_usuario3]

publicacionesDe_publicaciones_red2 = [publicacionesDe_publicacion1, publicacionesDe_publicacion2, publicacionesDe_publicacion3]

publicacionesDe_red1 = (publicacionesDe_usuarios,[],[])
publicacionesDe_red2 = (publicacionesDe_usuarios,[],publicacionesDe_publicaciones_red2)

testsSuitePublicacionesDe = test [
  " publicacionesDe 1"                      ~: publicacionesDe redA usuario2                                 ~?= [publicacion2_1, publicacion2_2],
  "Caso 1 Red sin publicaciónes : "         ~: publicacionesDe publicacionesDe_red1 publicacionesDe_usuario1 ~?= [],
  "Caso 2 Usuario con una publicación : "   ~: publicacionesDe publicacionesDe_red2 publicacionesDe_usuario2 ~?= [publicacionesDe_publicacion2],
  "Caso 3 Usuario con dos publicaciónes : " ~: publicacionesDe publicacionesDe_red2 publicacionesDe_usuario3 ~?= [publicacionesDe_publicacion1, publicacionesDe_publicacion3],
  "Caso 4 Usuario sin publcaciónes : "      ~: publicacionesDe publicacionesDe_red2 publicacionesDe_usuario1 ~?= [] 
  ]

----------------------------------------------------------TEST SEGUIDOR FIEL---------------------------------------------------------------------------------------------------------
 --CASO F
-- tieneUnSeguidorFiel ([(1,"Dani"),(2,"Antu"),(3, "Santi"),(4,"Vladi")], [], [((3, "Santi"), "Publicacion 1", [(1, "Dani"), (2, "Antu"), (4, "Vladi")]), ((3, "Santi"), "Publicacion 2", [(3, "Santi")]), ((3, "Santi"), "Publicacion 3", [(2, "Antu"), (3, "Santi")])]) (3, "Santi")

-- tieneUnSeguidorFiel ([(1,"Dani"),(2,"Antu"),(3, "Santi"),(4,"Vladi")], [], [((3, "Santi"), "Publicacion 1", []), ((3, "Santi"), "Publicacion 2", []), ((3, "Santi"), "Publicacion 3", [])]) (3, "Santi")

-- tieneUnSeguidorFiel ([(1,"Dani"),(2,"Antu"),(3, "Santi"),(4,"Vladi")], [], []) (3, "Santi")
--CASO V
-- tieneUnSeguidorFiel ([(1,"Dani"),(2,"Antu"),(3, "Santi"),(4,"Vladi")], [], [((3, "Santi"), "Publicacion 1", [(1, "Dani"), (2, "Antu"), (4, "Vladi")]), ((3, "Santi"), "Publicacion 2", [(2, "Antu"), (3, "Santi")]), ((3, "Santi"), "Publicacion 3", [(2, "Antu"), (3, "Santi")])]) (3, "Santi")
-- Idem si ninguna publicación tiene likes.

seguidorFiel_usuario1 = (1,"Dani")
seguidorFiel_usuario2 = (2,"Antu")
seguidorFiel_usuario3 = (3, "Santi")
seguidorFiel_usuario4 = (4,"Vladi")

-- Todas las publicaciones que usamos para testear son del usuario input porque el resto no son relevantes al funcionamiento del ejercicio.
seguidorFiel_publicacion1_red1 = (seguidorFiel_usuario3, "Publicacion 1", [seguidorFiel_usuario1, seguidorFiel_usuario2, seguidorFiel_usuario4])
seguidorFiel_publicacion2_red1 = (seguidorFiel_usuario3, "Publicacion 2", [seguidorFiel_usuario3])
seguidorFiel_publicacion3_red1 = (seguidorFiel_usuario3, "Publicacion 3", [seguidorFiel_usuario2, seguidorFiel_usuario3])

seguidorFiel_publicacion1_red2 = (seguidorFiel_usuario3, "Publicacion 1", [])
seguidorFiel_publicacion2_red2 = (seguidorFiel_usuario3, "Publicacion 2", [])
seguidorFiel_publicacion3_red2 = (seguidorFiel_usuario3, "Publicacion 3", [])

seguidorFiel_publicacion1_red3 = (seguidorFiel_usuario3, "Publicacion 1", [seguidorFiel_usuario1, seguidorFiel_usuario2, seguidorFiel_usuario4])
seguidorFiel_publicacion2_red3 = (seguidorFiel_usuario3, "Publicacion 2", [seguidorFiel_usuario2, seguidorFiel_usuario3])
seguidorFiel_publicacion3_red3 = (seguidorFiel_usuario3, "Publicacion 3", [seguidorFiel_usuario2, seguidorFiel_usuario3])

seguidorFiel_usuarios = [seguidorFiel_usuario1, seguidorFiel_usuario2, seguidorFiel_usuario3, seguidorFiel_usuario4]
seguidorFiel_publicaciones_red1 = [seguidorFiel_publicacion1_red1, seguidorFiel_publicacion2_red1, seguidorFiel_publicacion3_red1]
seguidorFiel_publicaciones_red2 = [seguidorFiel_publicacion1_red2, seguidorFiel_publicacion2_red2, seguidorFiel_publicacion3_red2]
seguidorFiel_publicaciones_red3 = [seguidorFiel_publicacion1_red3, seguidorFiel_publicacion2_red3, seguidorFiel_publicacion3_red3]

-- No usamos redes con relaciones porque no son relevantes al funcionamiento del ejercicio.
seguidorFiel_red1 = (seguidorFiel_usuarios, [], seguidorFiel_publicaciones_red1)
seguidorFiel_red2 = (seguidorFiel_usuarios, [], seguidorFiel_publicaciones_red2)
seguidorFiel_red3 = (seguidorFiel_usuarios, [], seguidorFiel_publicaciones_red3)
seguidorFiel_red4 = (seguidorFiel_usuarios, [], [])

testsSuiteTieneUnSeguidorFiel = test [
    " tieneUnSeguidorFiel 1"                ~: tieneUnSeguidorFiel redA usuario1                           ~?= True,
    "SF Caso 1: No tiene SF : "             ~: tieneUnSeguidorFiel seguidorFiel_red1 seguidorFiel_usuario3 ~?= False,
    "SF Caso 2: Publicaciones sin likes : " ~: tieneUnSeguidorFiel seguidorFiel_red2 seguidorFiel_usuario3 ~?= False,
    "SF Caso 3: Tiene un SF : "             ~: tieneUnSeguidorFiel seguidorFiel_red3 seguidorFiel_usuario3 ~?= True,
    "SF Caso 4: Red sin publicaciones : "   ~: tieneUnSeguidorFiel seguidorFiel_red4 seguidorFiel_usuario3 ~?= False 
  ]
----------------------------------------------------------Test publicacionesQueLeGustanA------------------------------------------------------

-- Funcion publicacionesQueLeGustanA recibe una RedSocial y un Usuario y le pasa la lista de publicaciones de la red y el usuario a publicacionesConLikesDe
-- Funcion publicacionesConLikesDe itera sobre las publicaciones y chequea si el usuario ingresado le dio like. En caso afirmativo, lo agrega a una lista, sino hace el chequeo con la publicación siguiente.
-- publicacionesQueLeGustanA ([(1,"Dani"),(2,"Antu"),(3, "Santi"),(4,"Vladi")], [], [((3, "Santi"), "Publicacion 1", [(1, "Dani"), (2, "Antu"), (4, "Vladi")]), ((2, "Antu"), "Publicacion 2", [(2, "Antu"), (3, "Santi")]), ((3, "Santi"), "Publicacion 3", [(1, "Dani"), (2, "Antu"), (3, "Santi")])]) (1, "Dani")
-- publicacionesQueLeGustanA ([(1,"Dani"),(2,"Antu"),(3, "Santi"),(4,"Vladi")], [], [((3, "Santi"), "Publicacion 1", [(1, "Dani"), (2, "Antu"), (4, "Vladi")]), ((2, "Antu"), "Publicacion 2", [(2, "Antu"), (3, "Santi")]), ((3, "Santi"), "Publicacion 3", [(1, "Dani"), (2, "Antu"), (3, "Santi")])]) (4, "Vladi")
-- publicacionesQueLeGustanA ([(1,"Dani"),(2,"Antu"),(3, "Santi"),(4,"Vladi")], [], [((3, "Santi"), "Publicacion 1", [(1, "Dani"), (2, "Antu")]), ((2, "Antu"), "Publicacion 2", [(2, "Antu"), (3, "Santi")]), ((3, "Santi"), "Publicacion 3", [(1, "Dani"), (2, "Antu"), (3, "Santi")])]) (4, "Vladi")
-- publicacionesQueLeGustanA ([(1,"Dani"),(2,"Antu"),(3, "Santi"),(4,"Vladi")], [], []) (4, "Vladi")
-- No usamos tests con relaciones porque no son relevantes al funcionamiento del ejercicio.

--Definimos Usuarios
publicacionesQueLeGustanA_usuario1 = (1,"Dani")
publicacionesQueLeGustanA_usuario2 = (2,"Antu")
publicacionesQueLeGustanA_usuario3 = (3,"Santi")
publicacionesQueLeGustanA_usuario4 = (4,"Vladi")

--Definimos Publicaciones
publicacionesQueLeGustanA_publicacion1 = (publicacionesQueLeGustanA_usuario3,"Publicacion 1",[publicacionesQueLeGustanA_usuario1,publicacionesQueLeGustanA_usuario2,publicacionesQueLeGustanA_usuario4])
publicacionesQueLeGustanA_publicacion2 = (publicacionesQueLeGustanA_usuario2,"Publicacion 2",[publicacionesQueLeGustanA_usuario2,publicacionesQueLeGustanA_usuario3])
publicacionesQueLeGustanA_publicacion3 = (publicacionesQueLeGustanA_usuario3,"Publicacion 3",[publicacionesQueLeGustanA_usuario1,publicacionesQueLeGustanA_usuario2,publicacionesQueLeGustanA_usuario3])
publicacionesQueLeGustanA_publicacion4 = (publicacionesQueLeGustanA_usuario3,"Publicacion 4",[publicacionesQueLeGustanA_usuario1,publicacionesQueLeGustanA_usuario2])

--Definimos Redes (No definimos relaciones porque no son reelevantes)

publicacionesQueLeGustanA_usuarios = [publicacionesQueLeGustanA_usuario1,publicacionesQueLeGustanA_usuario2,publicacionesQueLeGustanA_usuario3,publicacionesQueLeGustanA_usuario4]

publicacionesQueLeGustanA_red1 = (publicacionesQueLeGustanA_usuarios,[],[])
publicacionesQueLeGustanA_red2 = (publicacionesQueLeGustanA_usuarios,[],[publicacionesQueLeGustanA_publicacion1,publicacionesQueLeGustanA_publicacion2,publicacionesQueLeGustanA_publicacion3])
publicacionesQueLeGustanA_red3 = (publicacionesQueLeGustanA_usuarios,[],[publicacionesQueLeGustanA_publicacion4,publicacionesQueLeGustanA_publicacion2,publicacionesQueLeGustanA_publicacion3])

testsSuitepublicacionesQueLeGustanA = test [ 
     " publicacionesQueLeGustanA 1" ~: publicacionesQueLeGustanA redA usuario1 ~?= [publicacion2_2, publicacion4_1],
     "Caso 1 La red no contiene Publicaciones : " ~: publicacionesQueLeGustanA publicacionesQueLeGustanA_red1 publicacionesQueLeGustanA_usuario1 ~?=  [],
     "Caso 2 El usuario dio like a publicaciones : " ~: publicacionesQueLeGustanA publicacionesQueLeGustanA_red2 publicacionesQueLeGustanA_usuario1 ~?= [publicacionesQueLeGustanA_publicacion1,publicacionesQueLeGustanA_publicacion3],
     "Caso 3 Otro usuario dio like a publicacion : " ~: publicacionesQueLeGustanA publicacionesQueLeGustanA_red2 publicacionesQueLeGustanA_usuario4 ~?= [publicacionesQueLeGustanA_publicacion1],
     "Caso 4 El usuario no dio likes : " ~: publicacionesQueLeGustanA publicacionesQueLeGustanA_red3 publicacionesQueLeGustanA_usuario4 ~?= []
  ]

---------------------------------------------------Test leGustanLasMismasPublicaciones---------------------------------------
--- Funcion lesGustanLasMismasPublicaciones reutiliza el ejercicio publicacionesQueLeGustanA para traer el array de las publicaciones likeadas por ambos usuarios ingresados. Luego, usando mismosElementos chequeo que ambos array sean iguales, lo cual devolvería True. Caso contrario devuelve False
-- CASOS QUE DAN FALSE
-- lesGustanLasMismasPublicaciones ([(1,"Dani"),(2,"Antu"),(3, "Santi"),(4,"Vladi")], [], [((3, "Santi"), "Publicacion 1", [(1, "Dani"), (2, "Antu"), (4, "Vladi")]), ((2, "Antu"), "Publicacion 2", [(2, "Antu"), (3, "Santi"), (4, "Vladi")]), ((3, "Santi"), "Publicacion 3", [(1, "Dani"), (2, "Antu"), (3, "Santi")])]) (1, "Dani") (4, "Vladi")
-- lesGustanLasMismasPublicaciones ([(1,"Dani"),(2,"Antu"),(3, "Santi"),(4,"Vladi")], [], [((3, "Santi"), "Publicacion 1", [(1, "Dani"), (2, "Antu"), (4, "Vladi")]), ((2, "Antu"), "Publicacion 2", [(2, "Antu"), (3, "Santi")]), ((3, "Santi"), "Publicacion 3", [(1, "Dani"), (2, "Antu"), (3, "Santi")])]) (1, "Dani") (4, "Vladi")
-- CASO TRUE
-- lesGustanLasMismasPublicaciones ([(1,"Dani"),(2,"Antu"),(3, "Santi"),(4,"Vladi")], [], [((3, "Santi"), "Publicacion 1", []), ((2, "Antu"), "Publicacion 2", []), ((3, "Santi"), "Publicacion 3", [])]) (1, "Dani") (4, "Vladi")
-- lesGustanLasMismasPublicaciones ([(1,"Dani"),(2,"Antu"),(3, "Santi"),(4,"Vladi")], [], []) (1, "Dani") (4, "Vladi")
-- lesGustanLasMismasPublicaciones ([(1,"Dani"),(2,"Antu"),(3, "Santi"),(4,"Vladi")], [], [((3, "Santi"), "Publicacion 1", [(1, "Dani"), (2, "Antu"), (4, "Vladi")]), ((2, "Antu"), "Publicacion 2", [(2, "Antu"), (3, "Santi")]), ((3, "Santi"), "Publicacion 3", [(2, "Antu"), (3, "Santi")])]) (1, "Dani") (4, "Vladi")
-- Si no hay publicaciones siempre da true, porque los dos usuarios ingresados le dieron like a la misma cantidad de publicaciones: 0.
-- Idem si ninguna publicación tiene likes.
--No usamos tests con relaciones porque no son relevantes al funcionamiento del ejercicio.

--Definimos usuarios
lesGustanLasMismasPublicaciones_usuario1 = (1,"Dani")
lesGustanLasMismasPublicaciones_usuario2 = (2,"Antu")
lesGustanLasMismasPublicaciones_usuario3 = (3,"Santi")
lesGustanLasMismasPublicaciones_usuario4 = (4,"Vladi")

--Definimos publicaciones 
lesGustanLasMismasPublicaciones_publicacion1 = (lesGustanLasMismasPublicaciones_usuario3,"Publicacion 1",[lesGustanLasMismasPublicaciones_usuario1,lesGustanLasMismasPublicaciones_usuario2,lesGustanLasMismasPublicaciones_usuario4])
lesGustanLasMismasPublicaciones_publicacion2 = (lesGustanLasMismasPublicaciones_usuario2,"Publicacion 2",[lesGustanLasMismasPublicaciones_usuario3,lesGustanLasMismasPublicaciones_usuario2,lesGustanLasMismasPublicaciones_usuario4])
lesGustanLasMismasPublicaciones_publicacion3 = (lesGustanLasMismasPublicaciones_usuario3,"Publicacion 3",[lesGustanLasMismasPublicaciones_usuario1,lesGustanLasMismasPublicaciones_usuario2,lesGustanLasMismasPublicaciones_usuario3])
lesGustanLasMismasPublicaciones_publicacion4 = (lesGustanLasMismasPublicaciones_usuario4,"Publicacion 4",[lesGustanLasMismasPublicaciones_usuario1,lesGustanLasMismasPublicaciones_usuario2,lesGustanLasMismasPublicaciones_usuario4])
lesGustanLasMismasPublicaciones_publicacion5 = (lesGustanLasMismasPublicaciones_usuario2,"Publicacion 5",[])
lesGustanLasMismasPublicaciones_publicacion6 = (lesGustanLasMismasPublicaciones_usuario3,"Publicacion 6",[])
lesGustanLasMismasPublicaciones_publicacion7 = (lesGustanLasMismasPublicaciones_usuario3,"Publicacion 7",[lesGustanLasMismasPublicaciones_usuario2,lesGustanLasMismasPublicaciones_usuario3])

--Definimos Redes (No definimos relaciones porque no son reelevantes)
lesGustanLasMismasPublicaciones_usuarios = [lesGustanLasMismasPublicaciones_usuario1,lesGustanLasMismasPublicaciones_usuario2,lesGustanLasMismasPublicaciones_usuario3,lesGustanLasMismasPublicaciones_usuario4]

lesGustanLasMismasPublicaciones_red1 = (lesGustanLasMismasPublicaciones_usuarios,[],[lesGustanLasMismasPublicaciones_publicacion2,lesGustanLasMismasPublicaciones_publicacion3]) --Caso False Vladi Dani les gustan distintas
lesGustanLasMismasPublicaciones_red3 = (lesGustanLasMismasPublicaciones_usuarios,[],[lesGustanLasMismasPublicaciones_publicacion1,lesGustanLasMismasPublicaciones_publicacion2,lesGustanLasMismasPublicaciones_publicacion3]) --Caso False Vladi Dani les gusta una igual pero otras no en simultaneo
lesGustanLasMismasPublicaciones_red4 = (lesGustanLasMismasPublicaciones_usuarios,[],[lesGustanLasMismasPublicaciones_publicacion5,lesGustanLasMismasPublicaciones_publicacion6]) -- Caso verdadero porque no hay likes Dani Vladi
lesGustanLasMismasPublicaciones_red5 = (lesGustanLasMismasPublicaciones_usuarios,[],[]) --Caso verdadero porque no hay publicaciones, por ende les gustan a ambos 0 Dani Vladi
lesGustanLasMismasPublicaciones_red6 = (lesGustanLasMismasPublicaciones_usuarios,[],[lesGustanLasMismasPublicaciones_publicacion1,lesGustanLasMismasPublicaciones_publicacion7])--Caso verdadero Dani Vladi

testSuitelesGustanLasMismasPublicaciones = test [
    " lesGustanLasMismasPublicaciones 2" ~: (lesGustanLasMismasPublicaciones redB usuario1 usuario3) ~?= True,
    "Caso 1 Les gustan distintas : " ~: lesGustanLasMismasPublicaciones lesGustanLasMismasPublicaciones_red1 lesGustanLasMismasPublicaciones_usuario1 lesGustanLasMismasPublicaciones_usuario4 ~?= False,
    "Caso 2 Les gusta una igual pero no todas : " ~: lesGustanLasMismasPublicaciones lesGustanLasMismasPublicaciones_red3 lesGustanLasMismasPublicaciones_usuario1 lesGustanLasMismasPublicaciones_usuario4 ~?= False,
    "Caso 3 Publicaciones sin like : " ~: lesGustanLasMismasPublicaciones lesGustanLasMismasPublicaciones_red4 lesGustanLasMismasPublicaciones_usuario1 lesGustanLasMismasPublicaciones_usuario4 ~?= True,
    "Caso 4 No hay publicaciones : " ~: lesGustanLasMismasPublicaciones lesGustanLasMismasPublicaciones_red5 lesGustanLasMismasPublicaciones_usuario1 lesGustanLasMismasPublicaciones_usuario4 ~?= True,
    "Caso 5 Les gustan las mismas publicaciones : " ~: lesGustanLasMismasPublicaciones lesGustanLasMismasPublicaciones_red6 lesGustanLasMismasPublicaciones_usuario1 lesGustanLasMismasPublicaciones_usuario4 ~?= True


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
