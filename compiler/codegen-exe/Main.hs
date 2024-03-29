{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}

module Main (main) where

import           Control.Monad
import           Control.Monad.Writer.Lazy
import qualified Data.ByteString           as BS

import           Data.Bits                 (shiftL)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           Data.Word                 (Word8)
import           GHC.Exts                  (toList)
import           Text.Printf
import           Text.RawString.QQ         (r)

import           Data.Elf
import           Debug.Trace
import qualified Options.Applicative       as O
import           Text.Regex.TDFA

-- Cmdline parsing

data Command = Command String String

inputFilePath :: O.Parser String
inputFilePath =
  O.argument O.str (O.metavar "INPUT" <> O.help "Input non-executable TinyRAM elf file")

outputFilePath :: O.Parser String
outputFilePath =
  O.argument O.str (O.metavar "OUTPUT" <> O.help "Output init TinyRAM assembly code")

command :: O.Parser Command
command = Command <$> inputFilePath <*> outputFilePath

commandInfo :: O.ParserInfo Command
commandInfo =
  O.info (command O.<**> O.helper)
    (   O.fullDesc
     <> O.progDesc "Generate startup code for Harvard machine that initializes .data and .rodata sections"
     <> O.header "codegen"
    )

parseCommand :: MonadIO m => m Command
parseCommand = liftIO $ O.execParser commandInfo

-- Code generation

newtype Offset = Offset Integer deriving (Num)
newtype Constant = Constant Integer

chopByteString :: Offset -> [Word8] -> [(Offset, Constant)]
chopByteString offset list =
    case list of
        [] -> []
        a:b:c:d:xs -> (offset, toConstant a b c d) : chopByteString (offset + 4) xs
        _ -> error "List not multiple of 4"
    where
        toConstant a b c d = Constant
          (  fromIntegral a
          + (fromIntegral b `shiftL` 8)
          + (fromIntegral c `shiftL` 16)
          + (fromIntegral d `shiftL` 24)
          )

init' :: T.Text
init' = [r|
.section ".text"
.file   "data_and_rodata_section_init.s"
.globl  data_and_rodata_section_init
.align  32 #5
.type   data_and_rodata_section_init,@function
data_and_rodata_section_init:
|]

generate :: ElfSection -> Writer T.Text ()
generate section = do
    -- this comment is important as it is used by finish-building.sh script
    sequence_ (toInst <$> chopByteString (Offset 0) asList)
    where
      toInst :: (Offset, Constant) -> Writer T.Text ()
      toInst (Offset offset, Constant constant) =
          tell (T.pack $ printf "mov %%r0, %d\nstore.w %d, %%r0\n" constant (addr + fromIntegral offset))

      asList :: [Word8]
      asList = let l = toList content'
                   m = length l `mod` 4
                in if m == 0
                     then l
                     else l ++ replicate (4 - m) 0

      content' = elfSectionData section

      addr = elfSectionAddr section

createSInit :: MonadFail m => Elf -> String -> m [ElfSection]
createSInit elf sectionName = do
   let sections = findSections elf sectionName
   sequence $ createSInit' <$> sections
   where
    createSInit' section = do
        validateSection section
        return section

main :: IO ()
main = do
    (Command input output) <- parseCommand
    file <- BS.readFile input
    let elf = parseElf file
    validateElf elf
    mData <- createSInit elf ".data"
    mRodata <- createSInit elf ".rodata"
    let (_, codegen) = runWriter $ do
            tell init'
            sequence_ $ generate <$> mData
            sequence_ $ generate <$> mRodata
            tell $ T.pack "jmp %lr\n"
    TIO.writeFile output codegen

validateSection :: MonadFail m => ElfSection -> m ()
validateSection section = do
    when (elfSectionAddrAlign section /= 8)     (fail "Unexpected alignment")
    when (len1 /= fromIntegral len2)            (fail "Non-equal sizes")
    -- Length of the section is not necessarily multiple of word size!
    -- Since whole words are written by generator one has to ensure
    -- that an adjacent section is not overwritten.
    -- It is ensured by enforcing alignment of all sections to at least word size.
    -- It is done in finish-building.sh script and linker script.
    where
        len1 = elfSectionSize section
        len2 = BS.length (elfSectionData section)

findSections ::  Elf -> String -> [ElfSection]
findSections elf name = trace ("trace: " ++ show (elfSectionName <$> sections)) sections
  where
    regex = "^" ++ name ++ "[^ ]*$"
    sections = (\s -> elfSectionName s =~ regex) `filter` elfSections elf

validateElf :: MonadFail m => Elf -> m ()
validateElf elf = do
    when (elfClass      elf /= ELFCLASS32)    (fail "ELF32")
    when (elfData       elf /= ELFDATA2MSB)   (fail "ELFDATA2MSB")
    when (elfType       elf /= ET_EXEC)       (fail "ET_EXEC")
    when (elfMachine    elf /= EM_EXT 253)    (fail "EM_EXT")

