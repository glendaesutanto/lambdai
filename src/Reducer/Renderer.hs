module Reducer.Renderer
  (
    render,

    RendererSpec (RendererSpec),
    clRendererSpec,
    latexRendererSpec
  )
where

import Prelude hiding (Left, Right)

import LambdaAST
import LambdaAST.Path
import Reducer.Step

type Renderer = Trace -> String

data RendererSpec = RendererSpec { therefore :: String,
                                   beta :: String,
                                   delta :: String,
                                   lambda :: String,
                                   firstLinePrefix :: String,
                                   linePrefix :: String,
                                   lineEnd :: String,
                                   underlineBegin :: String,
                                   underlineEnd :: String,
                                   openParen :: String,
                                   closeParen :: String,
                                   period :: String,
                                   application :: String,
                                   definition :: String,
                                   alignStep :: String -> String,
                                   start :: String,
                                   end :: String,
                                   timeout :: String }

consoleWidth = 80

clRendererSpec = RendererSpec
  {
    therefore = "∴",
    beta = "β",
    delta = "δ",
    lambda = "λ",
    firstLinePrefix = "  ",
    linePrefix = "= ",
    lineEnd = "\n",
    underlineBegin = "\x1B[4m",
    underlineEnd = "\x1B[0m",
    openParen = "(",
    closeParen = ")",
    period = ".",
    application = " ",
    definition = " = ",
    alignStep = alignStep,
    start = "",
    end = "",
    timeout = "..."
  }
  where
    alignStep :: String -> String
    alignStep s | '\x1B' `elem` s = alignLeft (consoleWidth + 3) s ++ " "
                | otherwise = alignLeft (consoleWidth - 5) s ++ " "

latexRendererSpec = RendererSpec
  {
    therefore = "&\\quad \\therefore ",
    beta = "\\beta ",
    delta = "\\delta ",
    lambda = "\\lambda ",
    firstLinePrefix = "&",
    linePrefix = "=\\;&",
    lineEnd = "\\\\\n",
    underlineBegin = "\\underline{",
    underlineEnd = "}",
    openParen = "(",
    closeParen = ")",
    period = ".",
    application = "\\;",
    definition = " = ",
    alignStep = id,
    start = "\\begin{aligned}\n",
    end = "\\end{aligned}\n",
    timeout = "\\ldots"
  }

render :: RendererSpec -> Renderer
render s (Trace init []) =
  start s ++ firstLinePrefix s ++
  renderTerm s init ++ lineEnd s ++ end s
render s (Trace init (step:steps)) =
  start s ++ firstLinePrefix s ++
  renderTermWithHighlight (Just $ path step) s init ++
  lineEnd s ++
  renderSteps s (reductionType s step) (Trace (newTerm step) steps) ++
  end s

renderSteps :: RendererSpec -> String -> Trace -> String
renderSteps s rt (Trace init []) =
  linePrefix s ++ renderStep s rt init (NoReduction init) ++ lineEnd s
renderSteps s rt (Trace _ (Timeout:_)) = linePrefix s ++ (alignStep s $ timeout s) ++ lineEnd s
renderSteps s rt (Trace init (step:steps)) =
  linePrefix s ++ renderStep s rt init step ++ lineEnd s ++
  renderSteps s (reductionType s step) (Trace (newTerm step) steps)

renderStep :: RendererSpec -> String -> LambdaTerm -> ReductionStep -> String
renderStep s rt t NoReduction {} =
  (alignStep s $ renderTerm s t) ++ therefore s ++ rt ++ renderNumber s (convertToNumber t)
renderStep s rt t step =
  (alignStep s $ renderTermWithHighlight (Just $ path step) s t) ++ therefore s ++ rt

convertToNumber :: LambdaTerm -> Maybe Int
convertToNumber (Lambda s (Lambda z remainingTerm)) = convertToNumberHelper remainingTerm
  where
    convertToNumberHelper (Variable v) | v == z = Just 0
    convertToNumberHelper (Application (Variable v) f) | v == s = (+1) <$> convertToNumberHelper f
    convertToNumberHelper _ = Nothing 
convertToNumber _ = Nothing

renderNumber :: RendererSpec -> Maybe Int -> String
renderNumber s n = case n of
  Nothing -> ""
  Just n -> lineEnd s ++ linePrefix s ++ show n ++ lineEnd s

reductionType :: RendererSpec -> ReductionStep -> String
reductionType s Beta {} = beta s
reductionType s Delta {} = delta s
reductionType _ _ = ""

renderTerm :: RendererSpec -> LambdaTerm -> String
renderTerm = renderTermWithHighlight Nothing

renderTermWithHighlight :: Maybe Path -> RendererSpec -> LambdaTerm -> String
renderTermWithHighlight (Just (Path [])) s t = underlineBegin s ++ renderTerm s t ++ underlineEnd s
renderTermWithHighlight p s (Lambda parameter term) =
  openParen s ++ lambda s ++ parameter ++ period s ++
  renderTermWithHighlight (descend p Right) s term ++ closeParen s
renderTermWithHighlight p s (Application function argument) =
  inParensIfNecessary (descend p Left) s Left function ++ application s ++
  inParensIfNecessary (descend p Right) s Right argument
renderTermWithHighlight p s (Variable var) = var
renderTermWithHighlight p s (Definition name value) = name ++ definition s ++
  renderTermWithHighlight (descend p Right) s value

inParensIfNecessary :: Maybe Path -> RendererSpec -> Branch -> LambdaTerm -> String
inParensIfNecessary p s Left t = renderTermWithHighlight p s t
inParensIfNecessary p s Right t@Application {} = openParen s ++ renderTermWithHighlight p s t ++ closeParen s
inParensIfNecessary p s Right t = renderTermWithHighlight p s t

alignLeft :: Int -> String -> String
alignLeft n s = s ++ replicate (n - (length s)) ' '
