{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}



{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
import Data.List

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}
data Target = Target {
    position :: Position,
    behavior :: Behavior
}

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.

    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)

{-
    *** TODO ***

    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}
data Game = Game {  targetList :: [Target]
                    , hunter :: Position
                    , obstacleList :: [Position]
                    , gatewayList :: [(Position, Position)]
                    , dimensions :: Position
                } deriving (Eq, Ord)
{-
    *** Optional ***

    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}

{-
    *** TODO ***

    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.

    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}
gameAsString :: Game -> String
gameAsString g@(Game targetList hunter obstacleList gatewayList dimensions) =
    foldr (\(a, b) acc  ->
                            if a /= fst dimensions - 1 && b == snd dimensions - 1
                                then cell g (a, b) ++ "\n" ++ acc
                                else cell g (a, b) ++ acc)
                            "" [(a, b) | a <- [0..fst dimensions - 1], b <- [0..snd dimensions - 1]]
    where cell g pos
                | hunter == pos = "!"
                | [position | (Target position _) <- targetList, pos == position] /= []  = "*"
                | pos `elem` obstacleList = "@"
                | [(p1, p2) | (p1, p2) <- gatewayList, pos == p1 || pos == p2] /= [] = "#"
                | otherwise = " "

instance Show Game where
    show = gameAsString

{-
    *** TODO ***

    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}
emptyGame :: Int -> Int -> Game
emptyGame noLines noCols = Game { targetList = []
                            , hunter = (1, 1)
                            , obstacleList = createMargins noLines noColsisTargetKilled hunter x || acc
                            , gatewayList = []
                            , dimensions = (noLines, noCols)
                            } where createMargins x y = [(a, b) | a <- [0..x - 1], b <- [0..y - 1], a == x - 1 || b == y - 1 || a == 0 || b == 0]
{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}
addHunter :: Position -> Game -> Game
addHunter p g@(Game targetList hunter obstacleList gatewayList dimensions) =
                if cell g p == " " && fst p <= fst dimensions - 1 && snd p <= snd dimensions - 1 && fst p >= 0 && snd p >= 0
                    then Game targetList p obstacleList gatewayList dimensions
                    else g
                    where cell g pos
                            | hunter == pos = "!"
                            | [position | (Target position behavior) <- targetList, pos == position] /= []  = "*"
                            | pos `elem` obstacleList = "@"
                            | [(p1, p2) | (p1, p2) <- gatewayList, pos == p1 || pos == p2] /= [] = "#"
                            | otherwise = " "

{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Hunter-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}
addTarget :: Behavior -> Position -> Game -> Game
addTarget b p g@(Game targetList hunter obstacleList gatewayList dimensions) =
                                Game (Target p b : targetList) hunter obstacleList gatewayList dimensions

{-
    *** TODO ***

    Primește o pereche de poziții și un joc și întoarce un nou (Position, Position)joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}
addGateway :: (Position, Position) -> Game -> Game
addGateway (p1, p2) g@(Game targetList hunter obstacleList gatewayList dimensions) =
                                Game targetList hunter obstacleList (gatewayList ++ [(p1,p2)]) dimensions

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în c(Position, Position)re a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}
addObstacle :: Position -> Game -> Game
addObstacle p g@(Game targetList hunter obstacleList gatewayList dimensions) =
                                Game targetList hunter (obstacleList ++ [p]) gatewayList dimensions

{-
    *** TODO ***

    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}
attemptMove :: Position -> Game -> Maybe Position
attemptMove p g@(Game targetList hunter obstacleList gatewayList dimensions)
                        | cell g p == "@" = Nothing
                        | cell g p == " " = Just p
                        | fst p < 0 || snd p < 0 || fst p >= fst dimensions || snd p >= snd dimensions = Nothing
                        | p == pos1 = Just pos2
                        | otherwise = Just pos1
                        where
                            (pos1, pos2) = head [(p1, p2) | (p1, p2) <- gatewayList, p == p1 || p == p2]
                            cell g pos
                                        | hunter == pos = "!"
                                        | [position | (Target position behavior) <- targetList, pos == position] /= []  = "*"
                                        | pos `elem` obstacleList = "@"
                                        | [(p1, p2) | (p1, p2) <- gatewayList, pos == p1 || pos == p2] /= [] = "#"
                                        | otherwise = " "


{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre est.
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne
    pe loc.

    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.

    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}

goSomewhere :: Position -> Position -> Behavior -> Game -> Target
goSomewhere initial final b g@(Game targetList hunter obstacleList gatewayList dimensions)
        | isNothing (attemptMove final g) = Target initial b
        | checkPosIsGateway final gatewayList && pos1 == final = Target pos2 b
        | checkPosIsGateway final gatewayList && pos2 == final = Target pos1 b
        | otherwise = Target final b
        where (pos1, pos2) = head [(p1, p2) | (p1, p2) <- gatewayList, final == p1 || final == p2]

checkPosIsGateway :: Position -> [(Position, Position)] -> Bool
checkPosIsGateway pos [] = False
checkPosIsGateway pos gatewayList = [(p1, p2) | (p1, p2) <- gatewayList, pos == p1 || pos == p2] /= []

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

goEast :: Behavior
goEast p g@(Game targetList hunter obstacleList gatewayList dimensions) = goSomewhere p (fst p, snd p + 1) goEast g

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest.
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne
    pe loc.
-}
goWest :: Behavior
goWest p g@(Game targetList hunter obstacleList gatewayList dimensions) = goSomewhere p (fst p, snd p - 1) goWest g

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord.
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne
    pe loc.
-}
goNorth :: Behavior
goNorth p g@(Game targetList hunter obstacleList gatewayList dimensions) = goSomewhere p (fst p - 1, snd p) goNorth g

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud.
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne
    pe loc.
-}
goSouth :: Behavior
goSouth p g@(Game targetList hunter obstacleList gatewayList dimensions) = goSomewhere p (fst p + 1, snd p) goSouth g

{-
    *** TODO ***

    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud.
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}

bounce :: Int -> Behavior
bounce d p g@(Game targetList hunter obstacleList gatewayList dimensions)
        | d == -1 && isNothing (attemptMove (fst p - 1, snd p) g) = Target (fst p + 1, snd p) (bounce 1)
        | d == 1 && isNothing (attemptMove (fst p + 1, snd p) g) = Target (fst p - 1, snd p) (bounce (-1))
        | d == 1 = goSomewhere p (fst p + 1, snd p) (bounce 1) g
        | d == -1 = goSomewhere p (fst p - 1, snd p) (bounce (-1)) g
        | otherwise = Target p (bounce (-d))


{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.

-}
moveTargets :: Game -> Game
moveTargets g@(Game targetList hunter obstacleList gatewayList dimensions) =
                            Game (foldl (\acc b -> acc ++ [behavior b (position b) g]) [] targetList) hunter obstacleList gatewayList dimensions

{-
    *** TODO ***

    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}
isTargetKilled :: Position -> Target -> Bool
isTargetKilled p t@(Target position behavior)
                | fst p == fst position + 1 && snd p == snd position = True
                | fst p == fst position - 1 && snd p == snd position = True
                | snd p == snd position - 1 && fst p == fst position = True
                | snd p == snd position + 1 && fst p == fst position = True
                | otherwise = False


{-
    *** TODO ***

    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.

    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}
advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState = undefined

{-
    ***  TODO ***

    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft game@(Game targetList hunter obstacleList gatewayList dimensions) = null targetList

{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}
circle :: Position -> Int -> Behavior
circle = undefined


instance ProblemState Game Direction where
    {-
        *** TODO ***

        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors = undefined

    {-
        *** TODO ***

        Verifică dacă starea curentă este un în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}
    isGoal g@(Game targetList hunter obstacleList gatewayList dimensions) = foldl (\acc x -> isTargetKilled hunter x || acc) False targetList

    {-
        *** TODO ***

        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}
    h = undefined

{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ (x1 - x2) ^ pow + (y1 - y2) ^ pow
  where
    pow = 2 :: Int

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică.
-}
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors = undefined

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal = undefined

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h = undefined
