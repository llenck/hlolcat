import System.IO
import System.Environment

interkek :: [[a]] -> [a] -> [a]
interkek (a:as) (b:bs) = a ++ b : interkek as bs
interkek _ _ = []

curry3 f (a, b, c) = f a b c


freq = 0.1

color :: Int -> Int -> (Int,Int,Int)
color x y = (r n, g n, b n)
    where n = x + y
          sin_arg off = freq * (fromIntegral n) + pi / 1.5 * off
          single_color off n = floor $ sin (sin_arg off) * 127 + 128
          r = single_color 0
          g = single_color 1
          b = single_color 2

color_string r g b = "\ESC[38;2;" ++ (show r) ++ ";" ++ (show g) ++ ";" ++ (show b) ++ "m"
color_esc x y = curry3 color_string $ color x y

escapes :: Int -> [[Char]]
escapes = flip map [0..] . flip color_esc

format_line :: Int -> [Char] -> [Char]
format_line off = interkek $ escapes off

enum_lines :: String -> [(String, Int)]
enum_lines s = zip (map (++ "\n") $ lines s) [0..]

streams :: [String] -> IO [Handle]
streams [] = return $ return stdin
streams files = sequence $ map (flip openFile ReadMode) files

interact_handles :: [Handle] -> (String -> String) -> IO ()
interact_handles hs fn = (sequence $ map hGetContents hs) >>= (putStr . fn . concat)

lolcat = concatMap (uncurry $ flip format_line) . enum_lines

main = getArgs >>= streams >>= flip interact_handles lolcat
