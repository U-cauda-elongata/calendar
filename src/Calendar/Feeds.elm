module Calendar.Feeds exposing (Feed, preset)

import Dict exposing (Dict)


type alias Feed =
    { title : String
    , alternate : String
    , icon : String
    }


preset : Dict String Feed
preset =
    Dict.fromList
        [ ( "UCEOugXOAfa-HRmRjKbH8z3Q.xml"
          , Feed "けものフレンズプロジェクト公式"
                "https://www.youtube.com/channel/UCEOugXOAfa-HRmRjKbH8z3Q"
                "https://yt3.ggpht.com/ryEtopDlUPjueM1j3UufZ3UrGCpuYxc5tdeX5-pTlkjygXqbw7j29bFIPu8uCy4NzHAM1EetmLM=s60"
          )
        , ( "UCEcMIuGR8WO2TwL9XIpjKtw.xml"
          , Feed "ケープペンギン / African Penguin"
                "https://www.youtube.com/channel/UCEcMIuGR8WO2TwL9XIpjKtw"
                "https://yt3.ggpht.com/ytc/AKedOLSiSzCCyj5TBipvNgNcz0NrPZbvJZmZQU9JUFE-=s60"
          )
        , ( "UCmYO-WfY7Tasry4D1YB4LJw.xml"
          , Feed "フンボルトペンギン / Humboldt Penguin"
                "https://www.youtube.com/channel/UCmYO-WfY7Tasry4D1YB4LJw"
                "https://yt3.ggpht.com/ytc/AKedOLSr75ivVQI4bHcaoMOaYxPjbRnL3-2VCSNuHbJ-=s60"
          )
        , ( "UCMpw36mXEu3SLsqdrJxUKNA.xml"
          , Feed "シマハイイロギツネ / Island Fox"
                "https://www.youtube.com/channel/UCMpw36mXEu3SLsqdrJxUKNA"
                "https://yt3.ggpht.com/2ohbFqFqLbEw66rWMhTjb-wpa5X9APonb1KZiiBJbmGcS69yKUwtmLSHfhPUSF4snFp1O9r_=s60"
          )
        , ( "UCabMjG8p6G5xLkPJgEoTnDg.xml"
          , Feed "コヨーテ / Coyote"
                "https://www.youtube.com/channel/UCabMjG8p6G5xLkPJgEoTnDg"
                "https://yt3.ggpht.com/VwLc2w11lh_JlrsDB9P4OvBJpqaoAZdS08gqQx7vtJ5-4DjsEiP5Un6xmT0q8VE6zr8uXYEnTqg=s60"
          )
        , ( "UCdNBhcAohYjXlUVYsz8X2KQ.xml"
          , Feed "ダイアウルフ / Dire Wolf"
                "https://www.youtube.com/channel/UCdNBhcAohYjXlUVYsz8X2KQ"
                "https://yt3.ggpht.com/yJUytRAl-MwBrGmIXMdctRgcNuAborghz1SGt2o6KDewrB1aP6saZLdX9HWBPdF5JEutZ6aBKNY=s60"
          )
        ]
