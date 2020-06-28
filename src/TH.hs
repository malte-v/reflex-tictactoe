{-# LANGUAGE TemplateHaskell #-}

module TH
  ( includeFileInSource,
  )
where

import Language.Haskell.TH
import RIO
import RIO.ByteString (pack, readFile, unpack)

includeFileInSource ::
  -- | Path to file
  FilePath ->
  -- | Haskell value name
  String ->
  Q [Dec]
includeFileInSource fp n = do
  byteString <- runIO $ readFile fp
  tWord8 <- [t|Word8|]
  tByteString <- [t|ByteString|]
  fnPack <- [|pack|]
  return
    [ SigD (mkName n) tByteString,
      FunD
        (mkName n)
        [ Clause
            []
            ( NormalB
                $ AppE (SigE fnPack $ ArrowT `AppT` (ListT `AppT` tWord8) `AppT` tByteString)
                $ ListE
                $ fmap (LitE . IntegerL . fromIntegral) (unpack byteString)
            )
            []
        ]
    ]
