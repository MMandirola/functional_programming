import System.Random
readMany::IO [String]
readMany = do
  line <- getLine
  if line == "" then return [] else do
    lines <- readMany
    return (line:lines)

randomStr:: Int -> String -> IO String
randomStr largo cadena = do
    let range = length cadena
    if largo == 0 then return "" else do
      g<-newStdGen
      let posiciones = take largo (randomRs (0,(range-1)) g)
      return (map (\x -> cadena !! x) posiciones)

checkStr:: String -> IO [Int]
checkStr right = do
  guess <- getLine
  let pertenencia = map (\x -> if (elem x right) then 1 else 0) guess
  let minimo = min (length guess) (length right)
  let ordenacion = map (\x -> if right !! x == guess !! x then 1 else 0)[0..(minimo-1)]
  return (zipWith (+) pertenencia ordenacion)
