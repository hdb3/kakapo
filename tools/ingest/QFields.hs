{-# LANGUAGE OverloadedStrings #-}
module QFields where

import qualified Data.Text as T
import qualified Data.Text.IO as T

-- qFields : quotedFields
-- decompose a Text string into multiple strings based on comma delimiters
-- additionally, ensure that text within quotation marks is not separated on commas....

-- complexities:
--   (only) leading and trailing space outside quotes is removed
--   quotation marks are removed
--   text before or after quoted text is retained
--   multiple quoted strings are retained (without the quotaion marks!)

-- solution:
--   consume text until a breaking condition (',' or END when in normal mode, END in quoted mode)
--   whilst consuming text look for quotation marks, and switch state when detected
--   add the accumulated string to the list on reaching break condition

type QuoteMote = T.Text -> T.Text -> [ T.Text ]

parse :: T.Text -> [T.Text]
parse = normalMode T.empty 
    where

    isQuote = ( '"' == )
    isSeparator = ( ',' == )
    
    
    -- this assignment hardwirse the behaviour for keeping/dropping quotes
    -- a flexible option is easy to build but i wonder how optimal is it when compiled!!!
    -- so, for now, there is always some dead code in the module
    quoteMode :: QuoteMote
    quoteMode = removeQuoteMode
    
    normalMode precedingString content =
        let ( nextChunk, remainder ) = T.break pNormal content
            acc = precedingString `T.append` ( T.strip nextChunk )
        in if T.null remainder then
            [ acc ]
        else if isSeparator ( T.head remainder ) then
            acc :  normalMode T.empty ( T.tail remainder )
        else
            quoteMode acc remainder
        where
        pNormal c = isQuote c || isSeparator c
    
    keepQuoteMode :: QuoteMote
    -- retains the enclosing quote characters
    keepQuoteMode precedingString content =
        let Just ( first , rest ) = T.uncons content
            ( nextChunk, remainder ) = T.break isQuote rest
            acc = precedingString `T.snoc` first `T.append` nextChunk
        in if T.null remainder then
            [ acc ]
        else
            normalMode ( T.snoc acc (T.head remainder) ) ( T.tail remainder )
    
    removeQuoteMode :: QuoteMote
    -- strips the enclosing quote characters
    removeQuoteMode precedingString content =
        let rest  = T.tail content
            ( nextChunk, remainder ) = T.break isQuote rest
            acc = precedingString `T.append` nextChunk
        in if T.null remainder then
            [ acc ]
        else
            normalMode acc ( T.tail remainder )
    
    
main = do
    let test t = T.putStrLn $ "[" `T.append` t `T.append` "] -> " `T.append` ( T.pack $ show $ parse t)
    test ""
    test "123"
    test "123,456"
    test "\"123,456\""
    test "\"hellow wrorld\",123"
    test "\"hellow, wrorld\",123"
