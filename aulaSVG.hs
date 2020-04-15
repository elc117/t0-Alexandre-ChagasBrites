import Text.Printf

type Point     = (Float,Float)
type Rect      = (Point,Float,Float)
type Color     = (Int,Int,Int)


-------------------------------------------------------------------------------
-- Retângulos e cores
-------------------------------------------------------------------------------

-- Lista de retângulos (constante)
listOfRects :: [Rect]
listOfRects = 
  [((  0.0,0.0),50.0,50.0),
   (( 60.0,0.0),50.0,50.0),
   ((120.0,0.0),50.0,50.0),
   ((180.0,0.0),50.0,50.0),
   ((240.0,0.0),50.0,50.0),
   ((300.0,0.0),50.0,50.0)]
   

-- Lista de cores (constante)
listOfColors :: [Color]
listOfColors =
  [(255,0,0),
   (0,255,0),
   (0,0,255),
   (255,0,0),
   (0,255,0),
   (0,0,255)]


-- Obs.: Criar listas "hard-coded" não é muito legal. Imagine uma lista com 100 retângulos: inviável!
-- Tem um jeito muito melhor de fazer isso: programando uma função geradora!!!!
-- Em Haskell, podemos gerar listas facilmente usando list comprehension
-- https://wiki.haskell.org/List_comprehension
-- http://zvon.org/other/haskell/Outputsyntax/listQcomprehension_reference.html

-------------------------------------------------------------------------------
-- Strings SVG
-------------------------------------------------------------------------------

-- String inicial do SVG
svgBegin :: Float -> Float -> String
svgBegin w h = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h 

-- String final do SVG
svgEnd :: String
svgEnd = "</svg>"

-- Gera string representando retângulo SVG 
-- dadas coordenadas e dimensoes do retângulo e uma string com atributos de estilo
svgRect :: Rect -> Color -> String 
svgRect ((x,y),w,h) color = 
  printf "<rect x='%.3f' y='%.3f' width='%.2f' height='%.2f' style='%s' />\n" x y w h style
  where style = svgStyle color
  
-- Gera string com atributos de estilo para uma dada cor
-- Atributo mix-blend-mode permite misturar cores
svgStyle :: Color -> String
svgStyle (r,g,b) = printf "fill:rgb(%d,%d,%d); mix-blend-mode: screen;" r g b


-------------------------------------------------------------------------------
-- Função principal que gera arquivo com imagem SVG
-------------------------------------------------------------------------------

--take 6 [ ((i,j),x,y) | k <- [0..], i <- [k*60], j <- [0], x <- [50], y <- [50] ]
--myIndex n = map (\x -> map (\y -> (x, y)) [1..n]) [1..n]
--myIndex n = [ [(x, y) | y <- [1..n]] | x <- [1..n] ]

main :: IO ()
main = do
  writeFile "rects.svg" $ svgstrs
  where svgstrs = svgBegin w h ++ svgFigs ++ svgEnd
        size = 16
        svgRects = [ ((x * size, y * size), size, size) | x <- [0..31], y <- [0..31] ]
        svgColors = [ (r * 8, (31 - g) * 8, 0) | r <- [0..31], g <- [0..31] ]
        svgFigs = concat (zipWith svgRect svgRects svgColors)
        --svgfigs = concat (zipWith svgRect listOfRects listOfColors)
        (w, h) = (32 * size, 32 * size) -- width, height da imagem SVG