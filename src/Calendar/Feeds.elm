module Calendar.Feeds exposing (Feed, preset)

import Dict exposing (Dict)


type alias Feed =
    { title : String
    , alternate : String
    , icon : String
    , checked : Bool
    }


preset : Dict String Feed
preset =
    Dict.fromList
        [ ( "https://www.youtube.com/feeds/videos.xml?channel_id=UCEcMIuGR8WO2TwL9XIpjKtw"
          , Feed "ケープペンギン / African Penguin"
                "https://www.youtube.com/channel/UCEcMIuGR8WO2TwL9XIpjKtw"
                "https://yt3.ggpht.com/ytc/AKedOLSiSzCCyj5TBipvNgNcz0NrPZbvJZmZQU9JUFE-=s60"
                True
          )
        , ( "https://www.youtube.com/feeds/videos.xml?channel_id=UCmYO-WfY7Tasry4D1YB4LJw"
          , Feed "フンボルトペンギン / Humboldt Penguin"
                "https://www.youtube.com/channel/UCmYO-WfY7Tasry4D1YB4LJw"
                "https://yt3.ggpht.com/ytc/AKedOLSr75ivVQI4bHcaoMOaYxPjbRnL3-2VCSNuHbJ-=s60"
                True
          )
        , ( "https://www.youtube.com/feeds/videos.xml?channel_id=UCMpw36mXEu3SLsqdrJxUKNA"
          , Feed "シマハイイロギツネ / Island Fox"
                "https://www.youtube.com/channel/UCMpw36mXEu3SLsqdrJxUKNA"
                "https://yt3.ggpht.com/2ohbFqFqLbEw66rWMhTjb-wpa5X9APonb1KZiiBJbmGcS69yKUwtmLSHfhPUSF4snFp1O9r_=s60"
                True
          )
        , ( "https://www.youtube.com/feeds/videos.xml?channel_id=UCabMjG8p6G5xLkPJgEoTnDg"
          , Feed "コヨーテ / Coyote"
                "https://www.youtube.com/channel/UCabMjG8p6G5xLkPJgEoTnDg"
                "https://yt3.ggpht.com/VwLc2w11lh_JlrsDB9P4OvBJpqaoAZdS08gqQx7vtJ5-4DjsEiP5Un6xmT0q8VE6zr8uXYEnTqg=s60"
                True
          )
        , ( "https://www.youtube.com/feeds/videos.xml?channel_id=UCdNBhcAohYjXlUVYsz8X2KQ"
          , Feed "ダイアウルフ / Dire Wolf"
                "https://www.youtube.com/channel/UCdNBhcAohYjXlUVYsz8X2KQ"
                "https://yt3.ggpht.com/yJUytRAl-MwBrGmIXMdctRgcNuAborghz1SGt2o6KDewrB1aP6saZLdX9HWBPdF5JEutZ6aBKNY=s60"
                True
          )
        ]
