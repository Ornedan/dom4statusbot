module Model.Dominions4 where

data Era = Early
         | Middle
         | Late
         deriving (Eq, Read, Show)

nationName :: Int -> String
-- Internals block
nationName 0 = "Game internal 0"
nationName 1 = "Game internal 1"
nationName 2 = "Game internal 2"
nationName 3 = "Game internal 3"
nationName 4 = "Game internal 4"
-- EA block
nationName 5  = "EA Arcoscephale"
nationName 6  = "EA Ermor"
nationName 7  = "EA Ulm"
nationName 8  = "EA Marverni"
nationName 9  = "EA Sauromatia"
nationName 10 = "EA T'ien Ch'i"
nationName 11 = "EA Machaka"
nationName 12 = "EA Mictlan"
nationName 13 = "EA Abysia"
nationName 14 = "EA Caelum"
nationName 15 = "EA C'tis"
nationName 16 = "EA Pangaea"
nationName 17 = "EA Agartha"
nationName 18 = "EA Tir na n'Og"
nationName 19 = "EA Fomoria"
nationName 20 = "EA Vanheim"
nationName 21 = "EA Helheim"
nationName 22 = "EA Niefelheim"
nationName 25 = "EA Kailasa"
nationName 26 = "EA Lanka"
nationName 27 = "EA Yomi"
nationName 28 = "EA Hinnom"
nationName 29 = "EA Ur"
nationName 30 = "EA Berytos"
nationName 31 = "EA Xibalba"
nationName 83 = "EA Atlantis"
nationName 84 = "EA R'lyeh"
nationName 85 = "EA Pelagia"
nationName 86 = "EA Oceania"
-- MA block
nationName 33 = "MA Arcoscephale"
nationName 34 = "MA Ermor"
nationName 35 = "MA Sceleria"
nationName 36 = "MA Pythium"
nationName 37 = "MA Man"
nationName 38 = "MA Eriu"
nationName 39 = "MA Ulm"
nationName 40 = "MA Marignon"
nationName 41 = "MA Mictlan"
nationName 42 = "MA T'ien Ch'i"
nationName 43 = "MA Machaka"
nationName 44 = "MA Agartha"
nationName 45 = "MA Abysia"
nationName 46 = "MA Caelum"
nationName 47 = "MA C'tis"
nationName 48 = "MA Pangaea"
nationName 49 = "MA Asphodel"
nationName 50 = "MA Vanheim"
nationName 51 = "MA Jotunheim"
nationName 52 = "MA Vanarus"
nationName 53 = "MA Bandar Log"
nationName 54 = "MA Shinuyama"
nationName 55 = "MA Ashdod"
nationName 57 = "MA Nazca"
nationName 58 = "MA Xibalba"
nationName 87 = "MA Atlantis"
nationName 88 = "MA R'lyeh"
nationName 89 = "MA Pelagia"
nationName 90 = "MA Oceania"
-- LA block
nationName 60 = "LA Arcoscephale"
nationName 61 = "LA Pythium"
nationName 62 = "LA Lemur"
nationName 63 = "LA Man"
nationName 64 = "LA Ulm"
nationName 65 = "LA Marignon"
nationName 66 = "LA Mictlan"
nationName 67 = "LA T'ien Ch'i"
nationName 69 = "LA Jomon"
nationName 70 = "LA Agartha"
nationName 71 = "LA Abysia"
nationName 72 = "LA Caelum"
nationName 73 = "LA C'tis"
nationName 74 = "LA Pangaea"
nationName 75 = "LA Midgård"
nationName 76 = "LA Utgård"
nationName 77 = "LA Bogarus"
nationName 78 = "LA Patala"
nationName 79 = "LA Gath"
nationName 80 = "LA Ragha"
nationName 81 = "LA Xibalba"
nationName 91 = "LA Atlantis"
nationName 92 = "LA R'lyeh"
-- Unknown
nationName n  = "Nation " ++ show n