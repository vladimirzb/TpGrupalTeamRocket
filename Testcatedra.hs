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
    
    testsSuiteexisteSecuenciaDeAmigose

 ]
--Comentario Vladi: Ya prepare todo para que funcionen los suit cases

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
usuario1 = (1, "Juan")
usuario2 = (2, "Lionel Messi")
usuario3 = (3, "Sergio Agüero")
usuario4 = (4, "Paulo Dybala")
usuario5 = (5, "Ángel Di Maria") -- Este usuario no pertenece a ninguna red

--Definimos redes
red1 = ([usuario1, usuario2, usuario3], [(usuario1, usuario2), (usuario1, usuario3)], []) -- Red con relaciones
red2 = ([usuario2], [], []) -- Red sin relaciones
red3 = ([usuario2, usuario3], [(usuario2, usuario3)], []) -- Red con una relacion
red4 = ([usuario1, usuario2, usuario3, usuario4], [(usuario1, usuario2), (usuario1, usuario3), (usuario1, usuario4)], []) -- Red con tres relaciones
red5 = ([usuario2], [], []) -- Red sin el usuario que se prueba
testsSuiteAmigosDe = test [
    "amigosDe 1" ~: (amigosDe redA usuario1) ~?= [usuario2, usuario4], --Test de la catedra
     "amigosDe - Caso 1: Red con relaciones, usuario con dos relaciones" ~: (amigosDe red1 usuario1) ~?= [usuario2, usuario3],
    "amigosDe - Caso 2: Red sin relaciones, usuario sin relaciones" ~: (amigosDe red2 usuario2) ~?= [],
    "amigosDe - Caso 3: Red con una relacion, usuario con una relacion" ~: (amigosDe red3 usuario2) ~?= [usuario3],
    "amigosDe - Caso 4: Red con tres relaciones, usuario con tres relaciones" ~: (amigosDe red4 usuario1) ~?= [usuario2, usuario3, usuario4],
    "amigosDe - Caso 5: Red sin usuario en prueba, usuario no existente en la red" ~: (amigosDe red5 usuario5) ~?= []
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
