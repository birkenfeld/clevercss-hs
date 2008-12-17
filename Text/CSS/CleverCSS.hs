------------------------------------------------------------------------------------------
-- CleverCSS in Haskell, (c) 2007, 2008 Georg Brandl. Licensed under the BSD license.
--
-- Text.CSS.CleverCSS module: main parsing and evaluation.
--
-- TODO: properly parse selectors before splitting them
------------------------------------------------------------------------------------------
{-# LANGUAGE PatternGuards, FlexibleInstances #-}

module Text.CSS.CleverCSS (cleverCSSConvert) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Arrow ((***))
import Control.Monad.Error
import Control.Monad.RWS
import Data.Char (toUpper, toLower)
import Data.List (findIndex)
import Text.Printf (printf)
import Text.ParserCombinators.Parsec hiding (newline)
import qualified Data.Map as Map

import Text.CSS.CleverCSSUtil

-- Applicative instance for Parsec parsers: not needed for parsec 3.0
instance Applicative (GenParser a b) where
  pure = return
  (<*>) = ap

css_functions = ["url", "attr", "counter"] -- rgb() is special

------------------------------------------------------------------------------------------
-- "AST" for clevercss templates

type CSSNumber = (Rational, String)  -- number, unit
type CSSColor = Either String Color  -- color name or RGB
data AssignType = Always | IfNotAssigned  deriving Eq

data Topl = Assign !AssignType !Line !String [Expr]
          | Import !Line [Expr]
          | Macro  !Line !String ![String] [Item]
          | Block  !Line ![String] [Item]
            deriving Eq
data Item = Property !Line !String [Expr]      -- prop: val
          | UseMacro !Line !String [Expr]      -- %macro(...)
          | SubBlock !Line ![String] [Item]    -- sel:
          | SubGroup !Line !String [Item]      -- prop->
            deriving Eq
data Expr = Plus Expr Expr                     -- x + y
          | Minus Expr Expr                    -- x - y
          | Mul Expr Expr                      -- x * y
          | Divide Expr Expr                   -- x / y
          | Modulo Expr Expr                   -- x % y
          | ExprListCons Expr Expr             -- x, xs
          | ExprList [Expr]                    -- x, y, z
          | Subseq [Expr]                      -- x y z
          | Call Expr !String (Maybe Expr)     -- x.y([z])
          | Var !String                        -- $x
          | Bare !String                       -- x
          | String !String                     -- "x"
          | CSSFunc !String Expr               -- url(x)
          | Number !Rational                   -- 42
          | Dim !CSSNumber                     -- 42px
          | Color !CSSColor                    -- #fff
          | Rgb Expr Expr Expr                 -- rgb(1,0,0)
          | Error !String                      -- evaluation error
          | NoExpr                             -- special case in macro calls
            deriving Eq

instance Show Topl where
  show (Assign Always _ name exprs) = name ++ " = " ++ joinShow " " exprs
  show (Assign IfNotAssigned _ name exprs) = name ++ " ?= " ++ joinShow " " exprs
  show (Import _ exprs) = "@import " ++ joinShow " " exprs
  show (Macro _ sel args items) = "@define " ++ sel ++ "(" ++ joinStr ", " args ++
                                  "):\n" ++ unlines (map ("  "++) (map show items))
  show (Block _ sels items) = (joinStr ", " sels) ++ ":\n" ++
                              unlines (map ("  "++) (map show items))

instance Show Item where
  show (Property _ name exprs) = name ++ ": " ++ joinShow " " exprs
  show (UseMacro _ name args)  = "%" ++ name ++ "(" ++ joinShow ", " args ++ ")"
  show (SubGroup _ name items) = name ++ "->\n" ++
                                 unlines (map ("    "++) (map show items))
  show (SubBlock _ sels items) = (joinStr ", " sels) ++ ":\n" ++
                                 unlines (map ("  "++) (map show items))

instance Show Expr where
  -- things that shouldn't remain when evaluated
  show (Plus a b)          = printf "<Plus %s %s>" (show a) (show b)
  show (Minus a b)         = printf "<Minus %s %s>" (show a) (show b)
  show (Mul a b)           = printf "<Mul %s %s>" (show a) (show b)
  show (Divide a b)        = printf "<Divide %s %s>" (show a) (show b)
  show (Modulo a b)        = printf "<Modulo %s %s>" (show a) (show b)
  show (ExprListCons a b)  = printf "<ExprListCons %s %s>" (show a) (show b)
  show (Call e n Nothing)  = printf "<Call %s.%s()>" (show e) n
  show (Call e n (Just a)) = printf "<Call %s.%s(%s)>" (show e) n (show a)
  show (Var a)             = printf "<Var %s>" a
  show (Rgb r g b)         = printf "<Rgb %s %s %s>" (show r) (show g) (show b)
  show (Error e)           = printf "<Error: %s>" e
  show NoExpr              = "<NoExpr>"
  -- things that can remain and need to be pretty-printed
  show (ExprList l)        = joinShow ", " l
  show (Subseq l)          = joinShow " " l
  show (String s)          = cssShow s
  show (Number n)          = show (fromRational n)
  show (Dim (n, u))        = show (fromRational n) ++ u
  show (CSSFunc name args) = name ++ "(" ++ show args ++ ")"
  show (Color (Left n))    = n
  show (Color (Right (r,g,b))) = case Map.lookup (r,g,b) reverse_colors of
                                   Just name -> name
                                   Nothing -> printf "#%02x%02x%02x" r g b
  show (Bare s)            = s

------------------------------------------------------------------------------------------
-- the tokenizer and parser

-- helpers for toplevel and expression parsers
nl           = char '\n' <?> "end of line"
ws           = many (char ' ' <?> "whitespace")
comment      = string "//" >> many (noneOf "\n") <?> "comment"
wscomment    = ws >> option "" (try comment)
emptyLine    = wscomment >> nl <?> "empty line"
-- a newline always consumes empty lines too  -- different from many1 emptyLine!
newline      = emptyLine >> many (try emptyLine) <?> "newline"
pws parser   = parser ~>> wscomment
-- a CSS identifier
ident        = (pws $ try $ (perhaps $ char '-') +++
                      ((char '_' <|> letter <|> escape) +:+
                       many (char '_' <|> char '-' <|> alphaNum <|> escape)))
               <?> "identifier"
escape       = char '\\' >> (uniescape <|> charescape) where
  uniescape    = (varCount 1 6 hexDigit ~>> perhaps (oneOf " \n"))
                 >>= (return . hexToString)
  charescape   = noneOf ("\n" ++ ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F'])
at_ident     = (char '@' >> ident) <?> "at-identifier"
varname      = (char '_' <|> letter) +:+ many (char '_' <|> alphaNum)

-- top-level parser
parser :: GenParser Char [Int] [Topl]
parser = many emptyLine >> many (atclause <|> assign <|> cassign <|> block) ~>> end
  where
    atclause = do
      atid <- at_ident
      if atid == "import"
        then Import <$> getline <*> exprseq_nl
        else if atid == "define"
             then define
             else unexpected "at-identifier, expecting @import or @define"
    assign  = Assign Always <$> getline <*> varassign <*> exprseq_nl
    cassign = Assign IfNotAssigned <$> getline <*> cvarassign <*> exprseq_nl
    blockitems = do
      firstitem <- subblock True <|> subgroup True <|> defsubst True <|> property True
      restitems <- many $ (subblock False <|> subgroup False <|>
                           defsubst False <|> property False)
      return (firstitem:restitems)
    define = Macro <$> getline <*> defname <*> defargs
                   <*> blockitems ~>> updateState tail
    block = Block <$> getline <*> selector False
                  <*> blockitems ~>> updateState tail
    subblock fst = SubBlock <$> getline <*> selector fst
                            <*> blockitems ~>> updateState tail
    subgroup fst = do
      line <- getline
      groupname <- grpname fst
      firstitem <- property True
      restitems <- many $ property False
      updateState tail
      return $! SubGroup line groupname (firstitem:restitems)
    defsubst fst = do
      line <- getline
      macro <- macname fst
      args <- macargs
      return $! UseMacro line macro (exprs2list args)
    property fst = Property <$> getline <*> propname fst <*> exprseq_nl
    selector fst = do
      names <- selnames fst
      return $! map trim (split "," names) -- XXX: breaks with fancy selectors

    -- position helper
    getline      = sourceLine <$> getPosition

    -- exprlist helper
    exprs2list (ExprListCons a b) = a : exprs2list b
    exprs2list c                  = [c]

    -- toplevel variable assignment
    varassign    = (try $ varname ~>> (ws >> char '=' >> ws)) <?> "assignment"
    cvarassign   = (try $ varname ~>> (ws >> string "?=" >> ws)) <?> "assignment"

    -- grouping constructs -- fst means, first thing in a block
    selnames fst = (try $ indented fst $ --string "def " >> ws >>
                        noneOf "\n" `manyTill` (try $ char ':' >> newline))
                   <?> "selectors"
    defname      = varname <?> "macro name"
    defargs      = (char '(' >> (varname `sepBy` (char ',' >> ws)) ~>>
                    (pws $ char ')') ~>> char ':' ~>> newline)
                   <?> "macro arguments"
    macname fst  = (try $ indented fst $ char '%' >> varname) <?> "macro substitution"
    macargs      = (char '(' >> option NoExpr expression ~>> char ')' ~>> newline)
                   <?> "macro arguments"
    propname fst = (try $ indented fst $ ident ~>> char ':') <?> "property name"
    grpname  fst = (try $ indented fst $ grpident ~>> (string "->" >> newline))
                   <?> "group name"
    grpident     = (pws $ try $ many1 letter +++
                          manyConcat (try $ char '-' +:+ many1 letter))
    manyConcat   = liftM concat . many
    end          = ((try $ string "__END__" >> nl) >> return ()) <|> eof

    -- indentation handling: for the first thing in a block, need a new indentation
    -- level, after that, the same -- the new level is popped from the stack in the
    -- individual block parsers
    indented fst parser = do
      ind <- ws
      tok <- parser
      state <- getState
      let ilen = length ind
          olen = head state
      if fst then if ilen > olen then do
                              setState (length ind : state)
                              return tok
                            else unexpected "indentation"
             else if ilen == olen then return tok else unexpected "indentation level"

    exprseq_nl = exprseq ~>> (option ';' (char ';') >> newline)

-- expression parser
exprseq :: GenParser Char [Int] [Expr]
exprseq = ws >> many1 expression
expression = plusExpr `chainr1` listOp
  where
    listOp = op ',' >> return ExprListCons
    plusExpr = mulExpr `chainl1` plusOp
    plusOp = (op '+' >> return Plus) <|> (op '-' >> return Minus)
    mulExpr = primary `chainl1` mulOp
    mulOp = (op '*' >> return Mul) <|> (op '/' >> return Divide) <|>
            (op '%' >> return Modulo)
    primary = do
      object <- parenthesized <|> str <|> dimension <|> number <|> color <|>
                func <|> rgb <|> var <|> bare
      calltails object
    parenthesized = Subseq <$> between (op '(') (op ')') (many1 expression)
    str = String <$> (sqstring <|> dqstring)
    number = (Number . readNum) <$> num
    dimension = (Dim . readDim) <$> dim
    color = (Color . Right . hexToColor) <$> hexcolor
    var = Var <$> varref
    bare = do
      name <- ident
      if Map.member name colors then return $ Color (Left name)
                                else return $ Bare name
    func = CSSFunc <$> choice (map funcall css_functions) <*> expression ~>> op ')'
    rgb = do
      funcall "rgb"
      channels <- expression -- r, g, b
      op ')'
      case channels of (ExprListCons _ (ExprListCons _ (ExprListCons _ _))) ->
                         fail "too many arguments for rgb()"
                       (ExprListCons a (ExprListCons b c)) ->
                         return $! Rgb a b c
                       _ -> fail "not enough expressions for rgb()"
    calltails object = do
      calltail <- option Nothing (call object)
      case calltail of
        Nothing     -> return object
        Just called -> calltails called
    call object = do
      methname <- methcall
      callexpr <- option Nothing (Just <$> expression)
      op ')'
      return $! Just (Call object methname callexpr)

    -- tokens appearing within expressions, always parsed with trailing
    -- whitespace discarded
    varref       = (pws $ char '$' >> varname) <?> "variable reference"
    funcall   fn = (pws $ try $ string fn ~>> char '(') <?> "function " ++ fn
    methcall     = (pws $ char '.' >> varname ~>> (ws >> char '(')) <?> "method call"
    num          = (pws $ (perhaps $ char '-') +++ many1 digit +++
                        option "" (char '.' +:+ many1 digit)) <?> "number"
    dim          = (pws $ try $ num +++ ws +++ unit) <?> "dimension"
    unit         = choice (map string
                           ["em", "ex", "px", "cm", "mm", "in", "pt", "pc", "deg",
                            "rad", "grad", "ms", "s", "Hz", "kHz", "%"])
    dqstring     = (pws $ char '"' >> many1 (noneOf "\n\\\"" <|> escape) ~>> char '"')
                   <?> "string"
    sqstring     = (pws $ char '\'' >> many1 (noneOf "\n\\'" <|> escape) ~>> char '\'')
                   <?> "string"
    hexcolor     = (pws $ char '#' >> ((try $ count 6 hexDigit) <|> count 3 hexDigit))
                   <?> "color"
    op c         = (pws $ char c) <?> "operator " ++ [c]

------------------------------------------------------------------------------------------
-- the evaluator

data EvalError = EvalErr !Line !String  -- line, message
type Dict cont = Map.Map String cont
type EvalMonad = RWST (Dict Expr, Dict (Line, [String], [Item])) ()
                      [Topl] (Either EvalError) ()

instance Error EvalError where
  strMsg s = EvalErr 0 s

instance Show EvalError where
  show (EvalErr 0 msg) = ": " ++ msg
  show (EvalErr l msg) = "(line " ++ show l ++ "):\n" ++ msg

translate :: [Topl] -> [(String, String)] -> Either EvalError [Topl]
translate toplevels initial_map =
  fmap fst $ execRWST (resolve_toplevels toplevels)
                      (eval_map Map.empty initial_map, Map.empty) []
  where
  -- evaluate items in the initial map
  eval_map :: Dict Expr -> [(String, String)] -> Dict Expr
  eval_map map [] = map
  eval_map map ((n,v):ds) = eval_map (Map.insert n
                                           (evaluate map "initial variables" v) map) ds

  -- this keeps a state of all collected blocks.
  -- it returns (Left error) or (Right ([evaluated blocks], ()))
  resolve_toplevels :: [Topl] -> EvalMonad
  resolve_toplevels (Block line sels items : ts) = do
    resolve_block line sels items
    resolve_toplevels ts
  resolve_toplevels (Macro line sel args items : ts) = do
    local (id *** Map.insert sel (line, args, items)) (resolve_toplevels ts)
  resolve_toplevels (Import line exprseq : ts) = do
    exprs <- eval_exprseq line exprjoin exprseq
    case exprs of
      CSSFunc "url" u -> modify (Import line [CSSFunc "url" u] : )
      v -> throwError $ EvalErr line $
           "invalid thing to import, should be url(): " ++ show v
    resolve_toplevels ts
  resolve_toplevels (Assign how line name exprseq : ts) = do
    ispresent <- asks (Map.member name . fst)
    if ispresent && how == IfNotAssigned then resolve_toplevels ts else do
      exprs <- eval_exprseq line exprjoin exprseq
      local (Map.insert name exprs *** id) (resolve_toplevels ts)
  resolve_toplevels [] = return ()

  resolve_block line sels items = do
    props <- mapM (resolve_item sels) items
    modify (Block line sels (concat props) : )

  resolve_item _ (Property line name exprseq) = do
    expr <- eval_exprseq line exprjoin exprseq
    return [Property line name [expr]]
  resolve_item sels (UseMacro line name args) = do
    defmap <- asks snd
    case Map.lookup name defmap of
      Nothing -> throwError $ EvalErr line ("macro " ++ name ++ " is not defined")
      Just (_, argnames, items) -> do
        let numargs = length argnames
            given   = length args
        if numargs /= given
          then throwError $ EvalErr line ("wrong number of arguments for macro " ++
                                          name ++ ": given " ++ show given ++
                                          ", should be " ++ show numargs)
          else do
            evargs <- eval_exprseq line id args
            let updfunc old = (foldl (flip $ uncurry Map.insert)
                               (fst old) (zip argnames evargs), snd old)
            local updfunc (fmap concat $ mapM (resolve_item sels) items)
  resolve_item sels (SubBlock line subsels items) =
    resolve_block line (combine_sels sels subsels) items >> return []
  resolve_item _ (SubGroup _ name items) = mapM (resolve_group name) items

  resolve_group name (Property line prop exprs) =
    fmap head $ resolve_item [] (Property line (name ++ "-" ++ prop) exprs)
  resolve_group _ _ = error "impossible item in group"

  combine_sels sels subsels = [comb s1 s2 | s1 <- sels, s2 <- subsels]
    where comb s1 s2 = maybe (s1 ++ " " ++ s2)
                       (\i -> (take i s2) ++ s1 ++ (drop (i+1) s2))
                       (findIndex (=='&') s2)

  exprjoin [e] = e
  exprjoin es  = Bare $ joinShow " " es

  eval_exprseq _    cons []  = return $ cons [String ""]
  eval_exprseq line cons seq = do
    varmap <- asks fst
    case findError (map (eval varmap) seq) of
      [Error err] -> throwError $ EvalErr line err
      result      -> return $ cons result

  -- evaluate an Expr
  eval varmap exp = let eval' = eval varmap in case exp of
    Var a -> case Map.lookup a varmap of
      Just val -> val
      Nothing  -> Error $ "variable " ++ a ++ " is not defined"
    Plus a b -> case (eval' a, eval' b) of
      (String s, String t) -> String (s ++ t)
      (Number n, Number m) -> Number (n + m)
      (Dim n, Dim m) | Just (n1, m1, w) <- unitconv n m -> Dim (n1 + m1, w)
      (Dim (n, u), Number m) -> Dim (n + m, u)
      (Number m, Dim (n, u)) -> Dim (m + n, u)
      (Color col, Number n) -> Color $ Right $ modifyChannels (+) (rgbColor col) n
      (CSSFunc "url" (String url), String s) -> CSSFunc "url" (String $ url ++ s)
      (e@(Error _), _) -> e
      (_, e@(Error _)) -> e
      (x, y) -> Error ("cannot add " ++ show x ++ " and " ++ show y)
    Minus a b -> case (eval' a, eval' b) of
      (Number n, Number m) -> Number (n - m)
      (Dim n, Dim m) | Just (n1, m1, w) <- unitconv n m -> Dim (n1 - m1, w)
      (Dim (n, u), Number m) -> Dim (n - m, u)
      (Number m, Dim (n, u)) -> Dim (m - n, u)
      (Color col, Number n) -> Color $ Right $ modifyChannels (-) (rgbColor col) n
      (e@(Error _), _) -> e
      (_, e@(Error _)) -> e
      (x, y) -> Error ("cannot subtract " ++ show x ++ " and " ++ show y)
    Mul a b -> case (eval' a, eval' b) of
      (String s, Number n) -> String $ concat (replicate (floor n) s)
      (Number n, String s) -> String $ concat (replicate (floor n) s)
      (Number n, Number m) -> Number (n * m)
      (Dim (n, u), Number m) -> Dim (n * m, u)
      (Number m, Dim (n, u)) -> Dim (m * n, u)
      (e@(Error _), _) -> e
      (_, e@(Error _)) -> e
      (x, y) -> Error ("cannot multiply " ++ show x ++ " and " ++ show y)
    Divide a b -> case (eval' a, eval' b) of
      (Number n, Number m) -> case m of
                                    0 -> Error "divide by zero"
                                    m -> Number (n / m)
      (Dim (n, u), Number m) -> case m of
                                        0 -> Error "divide by zero"
                                        m -> Dim (n / m, u)
      (e@(Error _), _) -> e
      (_, e@(Error _)) -> e
      (x, y) -> Error ("cannot divide " ++ show x ++ " by " ++ show y)
    Modulo a b -> case (eval' a, eval' b) of
      (Number n, Number m) -> case m of
                                    0 -> Error "modulo by zero"
                                    m -> Number (n `ratMod` m)
      (Dim (n, u), Number m) -> case m of
                                        0 -> Error "modulo by zero"
                                        m -> Dim (n `ratMod` m, u)
      (e@(Error _), _) -> e
      (_, e@(Error _)) -> e
      (x, y) -> Error ("cannot calculate modulus of " ++ show x ++ " and " ++ show y)
    ExprListCons a b -> case (eval' a, eval' b) of
      (_, e@(Error _)) -> e
      (e@(Error _), _) -> e
      (e, ExprList es) -> ExprList (e:es)
      (e1, e2) -> ExprList [e1, e2]
    Subseq es -> findError2 Subseq (map eval' es)
    CSSFunc name args -> case (name, eval' args) of
      ("url", String url) -> CSSFunc "url" (String url)
      ("attr", args) -> CSSFunc "attr" args
      ("counter", args) -> CSSFunc "counter" args
      (name, args) -> Error $ printf "invalid CSS function: %s(%s)" name (show args)
    Call exp name arg -> case (name, eval' exp, fmap eval' arg) of
      (_, e@(Error _), _) -> e
      (_, _, Just e@(Error _)) -> e
      -- String methods
      ("bare", String s, Nothing) -> Bare s
      ("string", String s, Nothing) -> String s
      ("string", v, Nothing) -> String (show v)
      ("length", String s, Nothing) -> Number $ toRational (length s)
      ("upper", String s, Nothing) -> String (map toUpper s)
      ("lower", String s, Nothing) -> String (map toLower s)
      ("strip", String s, Nothing) -> String (trim s)
      ("split", String s, Just (String delim)) ->
        ExprList (map String (split delim s))
      ("eval", String s, Nothing) -> evaluate varmap "evaled string" s
      -- Number methods
      ("round", Number n, Just (Number p)) -> Number (roundRat n p)
      ("round", Number n, Nothing) -> Number (roundRat n 0)
      ("round", Dim (n, u), Just (Number p)) -> Dim (roundRat n p, u)
      ("round", Dim (n, u), Nothing) -> Dim (roundRat n 0, u)
      ("abs", Number n, Nothing) -> Number (abs n)
      ("abs", Dim (n, u), Nothing) -> Dim (abs n, u)
      -- List and sequence methods
      ("length", ExprList l, Nothing) -> Number $ toRational (length l)
      ("length", Subseq l, Nothing) -> Number $ toRational (length l)
      ("join", ExprList l, Nothing) -> String $ joinShow ", " l
      ("join", Subseq l, Nothing) -> String $ joinShow " " l
      ("join", ExprList l, Just (String delim)) -> String $ joinShow delim l
      ("join", Subseq l, Just (String delim)) -> String $ joinShow delim l
      ("list", l@(ExprList _), Nothing) -> l
      ("list", Subseq l, Nothing) -> ExprList l
      ("seq", l@(Subseq _), Nothing) -> l
      ("seq", ExprList l, Nothing) -> Subseq l
      -- Color methods
      ("brighten", Color col, arg) ->
        Color $ Right $ brightenColor (rgbColor col) (getAmount arg)
      ("darken", Color col, arg) ->
        Color $ Right $ darkenColor (rgbColor col) (getAmount arg)
      -- All else is invalid
      (name, exp, arg) ->
        Error $ printf "cannot call method %s(%s) on %s" name (jshow arg) (show exp)
    Rgb r g b -> case (eval' r, eval' g, eval' b) of
      (Number r', Number g', Number b') -> Color $ Right (cx r', cx g', cx b')
      (Dim (r', "%"), Dim (g', "%"), Dim (b', "%")) ->
        Color $ Right (cx (r' * 2.55), cx (g' * 2.55), cx (b' * 2.55))
      _ -> Error "rgb() arguments must be numbers or percentages"
      where cx = inrange 0 255 . floor
    -- all other cases are primitive
    atom -> atom

  -- evaluate a string
  evaluate varmap source string = case runParser exprseq [] "" string of
    Left err  -> Error $ showWithoutPos ("in " ++ source ++ ":") err
    Right []  -> String ""
    Right [e] -> eval varmap e
    Right seq -> findError2 Subseq $ map (eval varmap) seq

  -- return either the first Error in xs, or else xs
  findError xs  = head $ [[Error e] | Error e <- xs] ++ [xs]
  -- return either the first Error in xs, or else (cons xs)
  findError2 cons xs = head $ [Error e | Error e <- xs] ++ [cons xs]

  jshow Nothing  = ""
  jshow (Just a) = show a

  rgbColor = either (colors Map.!) id

  getAmount arg = case arg of
    Nothing -> 0.1
    Just (Number am) -> (fromRational am) / 100
    Just (Dim (am, "%")) -> (fromRational am) / 100
    _ -> 0

------------------------------------------------------------------------------------------
-- main conversion function

format :: [Topl] -> String
format blocks = concatMap format_block blocks where
  format_block (Block _ sels props) =
    joinStr ", " sels ++ " {\n" ++ unlines (map format_prop props) ++ "}\n\n"
  format_block (Import _ exprs) = "@import " ++ joinShow " " exprs ++ ";\n"
  format_block (Macro _ _ _ _) = error "remaining definition in eval result"
  format_block (Assign _ _ _ _) = error "remaining assignment in eval result"
  format_prop (Property _ name [val]) = "    " ++ name ++  ": " ++ show val ++ ";"
  format_prop (Property _ _ _) = error "property has not exactly one value"
  format_prop _ = error "remaining subitems in block"

cleverCSSConvert :: SourceName -> String -> [(String, String)] -> Either String String
cleverCSSConvert name input initial_map =
  case runParser parser [0] name (preprocess input) of
      Left err    -> Left $ "Parse error " ++ show err
      Right parse -> case translate parse initial_map of
        Left evalerr -> Left $ "Evaluation error " ++ show evalerr
        Right blocks -> Right $ format (reverse blocks)
