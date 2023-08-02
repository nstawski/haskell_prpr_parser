{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Types where

    data PlateConfig = PlateConfig
        { plateName :: String
        , plateId :: String
        , plateCapacity :: (Int, Int)
        } deriving (Show, Eq)

    data Location = Location
        { plate :: String
        , wells :: [(Char, Int)]
        } deriving (Show, Eq)

    data TransferMethod = LC_W_Bot_Bot
        | LC_W_Bot_Lev
        | LC_W_Bot_Air
        | LC_W_Lev_Bot
        | LC_W_Lev_Lev
        | LC_W_Lev_Air 
        deriving (Show, Eq)

    data Component = Component 
        { componentName :: String
        , componentLocation :: Location
        , componentMethod :: TransferMethod
        } deriving (Show, Eq)

    data Volume = Volume 
        { volumeName :: String
        , volumeValue :: Int
        } deriving (Show, Eq)

    data Recipe = Recipe
        { rname :: String,
        ingredients :: [(String, Int)]
        } deriving (Show, Eq)

    data Make = Make
        { mname :: String,
        mlocation :: Location,
        mmethod :: String,
        moptions :: String
        } deriving (Show, Eq)

    data Spread = Spread
        { scomponent :: String,
        sdestination :: Location,
        svolume :: String,
        smethod :: TransferMethod,
        soptions :: String
        } deriving (Show, Eq)

    data SpreadConfig = SpreadConfig
        { sccomponent :: Component,
        scdestination :: Location,
        scvolume :: Volume,
        scmethod :: TransferMethod,
        scoptions :: String
        } deriving (Show, Eq)

    data Transfer = Transfer
        { tsource :: Location,
        tdestination :: Location,
        tvolume :: String,
        tmethod :: TransferMethod,
        toptions :: String
        } deriving (Show, Eq)

    data TransferConfig = TransferConfig
        { tcsource :: Location,
        tcdestination :: Location,
        tcvolume :: Volume,
        tcmethod :: TransferMethod,
        tcoptions :: String
        } deriving (Show, Eq)

    data Config = Config
        { plates     :: [PlateConfig]
        , components :: [Component]
        , volumes    :: [Volume]
        , recipes    :: [Recipe]
        , makes      :: [Make]
        , spreads    :: [Spread]
        , transfers  :: [Transfer]
        } deriving (Show, Eq)

    data Value
        = PlateValue PlateConfig
        | ComponentValue Component
        | VolumeValue Volume
        | RecipeValue Recipe
        | SpreadValue SpreadConfig
        | TransferValue TransferConfig
        deriving (Show, Eq)

    data Action = MakeAction Make
        | SpreadAction Spread
        | TransferAction Transfer
        deriving (Show, Eq)
    
    data ParsedLine = Method TransferMethod
        | WellInfo (Char, [Int])
        | PlateConfigLine Value
        | ComponentConfigLine Value
        | VolumeConfigLine Value
        | LocationLine Location
        | SpreadConfigLine Action
        | TransferConfigLine Action
        deriving (Show, Eq)
