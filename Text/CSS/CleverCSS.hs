------------------------------------------------------------------------------------------
-- CleverCSS in Haskell, (c) 2007, 2008 Georg Brandl. Licensed under the BSD license.
--
-- Text.CSS.CleverCSS module: main parsing and evaluation.
--
-- TODO: properly parse selectors before splitting them
------------------------------------------------------------------------------------------
{-# LANGUAGE PatternGuards, TypeSynonymInstances #-}

module Text.CSS.CleverCSS (cleverCSSConvert) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Monad.Error
import Control.Monad.RWS
import Data.Char (toUpper, toLower)
import Data.List (findIndex)
import Data.Sequence (Seq, singleton)
import Text.Printf (printf)
import Text.ParserCombinators.Parsec hiding (newline)
import qualified Data.Foldable as F
import qualified Data.Map as Map

import Text.CSS.CleverCSSUtil

-- Applicative instance for Parsec parsers: not needed for parsec 3.0
--instance Applicative (GenParser a b) where
--  pure = return
--  (<*>) = ap

css_functions = ["url", "attr", "counter"] -- rgb() is special

------------------------------------------------------------------------------------------
-- "AST" for clevercss templates

type CSSNumber = (Rational, String)  -- number, unit
type CSSColor = Either String Color  -- color name or RGB
data AssignType = Always | IfNotAssigned  deriving Eq

data Topl = Assign !AssignType !Line !String [Expr]  -- a [?]= b
          | Import !Line [Expr]                      -- @import url(...)
          | Include !Line [Expr]                     -- @include "..."
          | Macro  !Line !String ![String] [Item]    -- @define mac(...):
          | Block  !Line ![String] [Item]            -- sel:
          | SetFilename !String                      -- pseudo item
            deriving Eq
data Item = Property !Line !String [Expr]            -- prop: val
          | UseMacro !Line !String [Expr]            -- %macro(...)
          | SubBlock !Line ![String] [Item]          -- subsel:
          | SubGroup !Line !String [Item]            -- prop->
            deriving Eq
data Expr = Plus Expr Expr                           -- x + y
          | Minus Expr Expr                          -- x - y
          | Mul Expr Expr                            -- x * y
          | Divide Expr Expr                         -- x / y
          | Modulo Expr Expr                         -- x % y
          | ExprListCons Expr Expr                   -- x, xs
          | ExprList [Expr]                          -- x, y, z
          | Subseq [Expr]                            -- x y z
          | Call Expr !String (Maybe Expr)           -- x.y([z])
          | Var !String                              -- $x
          | Bare !String                             -- x
          | String !String                           -- "x"
          | CSSFunc !String Expr                     -- url(x)
          | Number !Rational                         -- 42
          | Dim !CSSNumber                           -- 42px
          | Color !CSSColor                          -- #fff
          | Rgb Expr Expr Expr                       -- rgb(1,0,0)
          | Error !String                            -- evaluation error
          | NoExpr                                   -- no arg in macro calls
            deriving Eq

instance Show Topl where
  show (Assign Always _ name exprs) = name ++ " = " ++ joinShow " " exprs
  show (Assign IfNotAssigned _ name exprs) = name ++ " ?= " ++ joinShow " " exprs
  show (Import _ exprs) = "@import " ++ joinShow " " exprs
  show (Include _ exprs) = "@include " ++ joinShow " " exprs
  show (Macro _ sel argnames items) = "@define " ++ sel ++ "(" ++ joinStr ", " argnames ++
                                  "):\n" ++ unlines (map ("  "++) (map show items))
  show (Block _ sels items) = (joinStr ", " sels) ++ ":\n" ++
                              unlines (map ("  "++) (map show items))
  show (SetFilename s) = "<SetFilename " ++ s ++ ">"

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
      case atid of
        "import" -> Import <$> getline <*> exprseq_nl
        "include" -> Include <$> getline <*> exprseq_nl
        "define" -> Macro <$> getline <*> defname <*> defargs <*> blockitems
        _        -> unexpected "at-identifier, expecting @import, @include or @define"
    assign  = Assign Always <$> getline <*> varassign <*> exprseq_nl
    cassign = Assign IfNotAssigned <$> getline <*> cvarassign <*> exprseq_nl
    blockitems = do
      firstitem <- subblock True <|> subgroup True <|> defsubst True <|> property True
      restitems <- many $ (subblock False <|> subgroup False <|>
                           defsubst False <|> property False)
      updateState tail  -- remove indentation level from stack
      return (firstitem:restitems)
    block = Block <$> getline <*> selector False <*> blockitems
    subblock fst = SubBlock <$> getline <*> selector fst <*> blockitems
    subgroup fst = do
      line <- getline
      groupname <- grpname fst
      firstitem <- property True
      restitems <- many $ property False
      updateState tail
      return $! SubGroup line groupname (firstitem:restitems)
    defsubst fst = UseMacro <$> getline <*> macname fst <*> (exprs2list <$> macargs)
    property fst = Property <$> getline <*> propname fst <*> exprseq_nl
    selector fst = do
      names <- selnames fst
      return $! map trim (split "," names) -- XXX: breaks with fancy selectors

    -- position helper
    getline      = sourceLine <$> getPosition

    -- exprlist helper for macro arguments
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
expression :: GenParser Char [Int] Expr
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

data EvalError = EvalErr !SourceName !Line !String  -- filename, line, message
type Dict cont = Map.Map String cont
data Env = Env { vars :: Dict Expr, macros :: Dict (Line, [String], [Item]) }
type Eval res  = RWST Env (Seq Topl) SourceName (ErrorT EvalError IO) res

instance Error EvalError where
  strMsg s = EvalErr "" 0 s

instance Show EvalError where
  show (EvalErr f 0 msg) = "(file " ++ show f ++ "): " ++ msg
  show (EvalErr f l msg) = "(file " ++ show f ++ ", line " ++ show l ++ "):\n" ++ msg

evalErr line err = do
  fname <- get
  throwError $ EvalErr fname line err

updateVars   f r = r { vars   = f (vars r)   }
updateMacros f r = r { macros = f (macros r) }

translate :: String -> [Topl] -> Dict Expr -> IO (Either EvalError (Seq Topl))
translate filename toplevels varmap = do
  let initialEnv = Env { vars = varmap, macros = Map.empty }
  res <- runErrorT $ execRWST (resolveToplevels toplevels) initialEnv filename
  return (snd <$> res)
  where
  emitBlock = tell . singleton
  -- resolve a list of toplevels -- this "writes" collected blocks
  -- returns (Left error) or (Right (filename, collected blocks))
  resolveToplevels :: [Topl] -> Eval ()
  resolveToplevels (SetFilename filename : ts) = do
    put filename
    resolveToplevels ts
  resolveToplevels (Block line sels items : ts) = do
    -- block: resolve it and continue
    resolveBlock line sels items
    resolveToplevels ts
  resolveToplevels (Macro line sel argnames items : ts) = do
    -- macro definition: store the items in the macro map and continue
    local (updateMacros $ Map.insert sel (line, argnames, items)) (resolveToplevels ts)
  resolveToplevels (Import line exprseq : ts) = do
    -- import: just append it to the collected blocks
    exprs <- evalExprseq line exprjoin exprseq
    case exprs of
      CSSFunc "url" u -> emitBlock $ Import line [CSSFunc "url" u]
      v -> evalErr line $ "invalid thing to import, should be url(): " ++ show v
    resolveToplevels ts
  resolveToplevels (Include line exprseq : ts) = do
    -- include: read and append it
    exprs <- evalExprseq line exprjoin exprseq
    case exprs of
      String filename -> do
        oldfilename <- get
        contents <- liftIO $ readFile filename
        case runParser parser [0] filename (preprocess contents) of
          Left err -> evalErr line $ "Parse error in @include " ++ show err
          Right parse -> resolveToplevels ((SetFilename filename : parse) ++
                                           (SetFilename oldfilename : ts))
      v -> evalErr line $ "invalid thing to include, should be a string: " ++ show v
  resolveToplevels (Assign how line name exprseq : ts) = do
    -- assignment: store it in the variable map and continue
    ispresent <- asks (Map.member name . vars)
    if ispresent && how == IfNotAssigned then resolveToplevels ts else do
      exprs <- evalExprseq line exprjoin exprseq
      local (updateVars $ Map.insert name exprs) (resolveToplevels ts)
  resolveToplevels [] = return ()

  resolveBlock :: Line -> [String] -> [Item] -> Eval ()
  resolveBlock line sels items = do
    props <- mapM (resolveItem sels) items
    emitBlock $ Block line sels (concat props)

  resolveItem :: [String] -> Item -> Eval [Item]
  resolveItem _ (Property line name exprseq) = do
    expr <- evalExprseq line exprjoin exprseq
    return [Property line name [expr]]
  resolveItem sels (UseMacro line name args) = do
    lookresult <- asks (Map.lookup name . macros)
    case lookresult of
      Nothing -> evalErr line ("macro " ++ name ++ " is not defined")
      Just (_, argnames, items) -> do
        let numargs = length argnames
            given   = length args
        if numargs /= given
          then evalErr line ("wrong number of arguments for macro " ++ name ++
                             ": given " ++ show given ++ ", should be " ++ show numargs)
          else do
            evaledargs <- evalExprseq line id args
            -- update locals with evaluated arguments
            let updfunc = updateVars $ Map.union (Map.fromList $ zip argnames evaledargs)
            local updfunc (concat <$> mapM (resolveItem sels) items)
  resolveItem sels (SubBlock line subsels items) =
    resolveBlock line (combineSels sels subsels) items >> return []
  resolveItem _ (SubGroup _ name items) = mapM (resolveGroup name) items

  resolveGroup name (Property line prop exprs) =
    head <$> resolveItem [] (Property line (name ++ "-" ++ prop) exprs)
  resolveGroup _ _ = error "impossible item in group"

  combineSels sels subsels = [comb s1 s2 | s1 <- sels, s2 <- subsels]
    where comb s1 s2 = maybe (s1 ++ " " ++ s2)
                       (\i -> (take i s2) ++ s1 ++ (drop (i+1) s2))
                       (findIndex (=='&') s2)

  exprjoin [e] = e
  exprjoin es  = Bare $ joinShow " " es

  evalExprseq _    cons []  = return $ cons [String ""]
  evalExprseq line cons seq = do
    varmap <- asks vars
    case findError (map (eval varmap) seq) of
      [Error err] -> evalErr line err
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
      ("eval", String s, Nothing) -> evalString varmap "evaled string" s
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
  where
    jshow Nothing  = ""
    jshow (Just a) = show a

    rgbColor = either (colors Map.!) id

    getAmount arg = case arg of
      Nothing -> 0.1
      Just (Number am) -> (fromRational am) / 100
      Just (Dim (am, "%")) -> (fromRational am) / 100
      _ -> 0


-- return either the first Error in xs, or else xs
findError xs  = head $ [[Error e] | Error e <- xs] ++ [xs]
-- return either the first Error in xs, or else (cons xs)
findError2 cons xs = head $ [Error e | Error e <- xs] ++ [cons xs]

-- evaluate a string
evalString varmap source string = case runParser exprseq [] "" string of
  Left err  -> Error $ showWithoutPos ("in " ++ source ++ ":") err
  Right []  -> String ""
  Right [e] -> eval varmap e
  Right seq -> findError2 Subseq $ map (eval varmap) seq

-- evaluate expressions into a map
evalMap :: Dict Expr -> [(String, String)] -> Dict Expr
evalMap map [] = map
evalMap map ((n,v):ds) = evalMap (Map.insert n
                                  (evalString map "initial variables" v) map) ds

------------------------------------------------------------------------------------------
-- main conversion function

format :: Seq Topl -> String
format blocks = F.foldl (\x y -> x ++ formatBlock y) "" blocks where
  formatBlock (Block _ sels props) =
    joinStr ", " sels ++ " {\n" ++ unlines (map formatProp props) ++ "}\n\n"
  formatBlock (Import _ exprs) = "@import " ++ joinShow " " exprs ++ ";\n"
  formatBlock (Include _ _) = error "remaining include in eval result"
  formatBlock (Macro _ _ _ _) = error "remaining definition in eval result"
  formatBlock (Assign _ _ _ _) = error "remaining assignment in eval result"
  formatBlock (SetFilename _) = error "remaining filename in eval result"
  formatProp (Property _ name [val]) = "    " ++ name ++  ": " ++ show val ++ ";"
  formatProp (Property _ _ _) = error "property has not exactly one value"
  formatProp _ = error "remaining subitems in block"

cleverCSSConvert :: SourceName -> String -> [(String, String)] -> IO (Either String String)
cleverCSSConvert name input initial_map =
  case runParser parser [0] name (preprocess input) of
      Left err    -> return . Left $ "Parse error " ++ show err
      Right parse -> do
        result <- translate name parse (evalMap Map.empty initial_map)
        case result of
          Left evalerr -> return . Left $ "Evaluation error " ++ show evalerr
          Right blocks -> return . Right $ format blocks
