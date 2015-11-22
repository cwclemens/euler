import Text.Parsec (many1, sepEndBy, char, digit, ParseError,
                    eof, parse, Parsec)
import Control.Applicative ((<$>),(<*>),(*>),(<*),(<|>))

type Point = (Float, Float)
type Line = (Float, Float, Float) -- (a,b,c) in ax+by=c
type Triangle = (Point, Point, Point)

main = do Right ts <- parseFile "p102_triangles.txt"
          print $ count containsOrigin ts
  where count p = length . filter p

-- containsOrigin -------

containsOrigin :: Triangle -> Bool
containsOrigin t = containsPoint t (0,0)

containsPoint :: Triangle -> Point -> Bool
containsPoint t p = sameSide (line p1 p2) p3 p &&
                    sameSide (line p2 p3) p1 p &&
                    sameSide (line p1 p3) p2 p
  where (p1,p2,p3) = t

line :: Point -> Point -> Line
line (x1,y1) (x2,y2) = case (x1==x2, y1==y2) of
    (True,True)   -> error "Degenerate Triangle"
    (True,False)  -> (1,0,x1)
    (False,True)  -> (0,1,y1)
    (False,False) -> (-m,1,b)
  where m = (y2-y1)/(x2-x1)
        b = y1 - m*x1

sameSide :: Line -> Point -> Point -> Bool
sameSide l p1 p2 = above1 == above2
  where (a,b,c) = l
        ((x1,y1),(x2,y2)) = (p1,p2)
        above1 = a*x1 + b*y1 > c
        above2 = a*x2 + b*y2 > c

-- Parsing --------------

type Parser = Parsec String ()

triangles :: Parser [Triangle]
triangles = triangle `sepEndBy` (char '\n') <* eof

triangle = (,,) <$> point <* comma <*> point <* comma <*> point

point = (,) <$> float <* comma <*> float

comma :: Parser Char
comma = char ','

float :: Parser Float
float = read <$> (minus <|> number)
  where number = many1 digit
        minus = (:) <$> char '-' <*> number

parseFile :: String -> IO (Either ParseError [Triangle])
parseFile filename = parse triangles filename <$> readFile filename

