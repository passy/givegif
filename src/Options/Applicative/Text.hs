module Options.Applicative.Text
  ( text
  , textOption
  , textArgument ) where

import qualified Data.Text                 as T
import qualified Options.Applicative       as Opt
import qualified Options.Applicative.Types as Opt

text :: Opt.ReadM T.Text
text = T.pack <$> Opt.readerAsk

textOption :: Opt.Mod Opt.OptionFields T.Text -> Opt.Parser T.Text
textOption = Opt.option text

textArgument :: Opt.Mod Opt.ArgumentFields T.Text -> Opt.Parser T.Text
textArgument = Opt.argument text
