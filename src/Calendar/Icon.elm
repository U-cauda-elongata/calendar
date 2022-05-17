module Calendar.Icon exposing (clear, gitHub, hamburger, search)

import Calendar.Attributes exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (cx, cy, d, fill, r, stroke, strokeLinecap, viewBox, x1, x2, y1, y2)


clear : Svg msg
clear =
    svg
        [ xmlns "http://www.w3.org/2000/svg"
        , viewBox "0 0 15 15"
        , role "img"
        ]
        [ circle [ cx "50%", cy "50%", r "50%", stroke "none" ] []
        , line [ x1 "25%", y1 "25%", x2 "75%", y2 "75%" ] []
        , line [ x1 "25%", y1 "75%", x2 "75%", y2 "25%" ] []
        ]


gitHub : Svg msg
gitHub =
    svg
        [ xmlns "http://www.w3.org/2000/svg"
        , viewBox "0 0 24 28"
        , ariaLabel "GitHub"
        , role "img"
        ]
        [ path [ d "M12 2c6.625 0 12 5.375 12 12 0 5.297-3.437 9.797-8.203 11.391-.609.109-.828-.266-.828-.578 0-.391.016-1.687.016-3.297 0-1.125-.375-1.844-.812-2.219 2.672-.297 5.484-1.313 5.484-5.922 0-1.313-.469-2.375-1.234-3.219.125-.313.531-1.531-.125-3.187-1-.313-3.297 1.234-3.297 1.234a11.28 11.28 0 00-6 0S6.704 6.656 5.704 6.969c-.656 1.656-.25 2.875-.125 3.187-.766.844-1.234 1.906-1.234 3.219 0 4.594 2.797 5.625 5.469 5.922-.344.313-.656.844-.766 1.609-.688.313-2.438.844-3.484-1-.656-1.141-1.844-1.234-1.844-1.234-1.172-.016-.078.734-.078.734.781.359 1.328 1.75 1.328 1.75.703 2.141 4.047 1.422 4.047 1.422 0 1 .016 1.937.016 2.234 0 .313-.219.688-.828.578C3.439 23.796.002 19.296.002 13.999c0-6.625 5.375-12 12-12zM4.547 19.234c.031-.063-.016-.141-.109-.187-.094-.031-.172-.016-.203.031-.031.063.016.141.109.187.078.047.172.031.203-.031zm.484.532c.063-.047.047-.156-.031-.25-.078-.078-.187-.109-.25-.047-.063.047-.047.156.031.25.078.078.187.109.25.047zm.469.703c.078-.063.078-.187 0-.297-.063-.109-.187-.156-.266-.094-.078.047-.078.172 0 .281s.203.156.266.109zm.656.656c.063-.063.031-.203-.063-.297-.109-.109-.25-.125-.313-.047-.078.063-.047.203.063.297.109.109.25.125.313.047zm.891.391c.031-.094-.063-.203-.203-.25-.125-.031-.266.016-.297.109s.063.203.203.234c.125.047.266 0 .297-.094zm.984.078c0-.109-.125-.187-.266-.172-.141 0-.25.078-.25.172 0 .109.109.187.266.172.141 0 .25-.078.25-.172zm.906-.156c-.016-.094-.141-.156-.281-.141-.141.031-.234.125-.219.234.016.094.141.156.281.125s.234-.125.219-.219z" ] [] ]


search : Svg msg
search =
    svg
        [ xmlns "http://www.w3.org/2000/svg"
        , viewBox "0 0 15 15"
        , ariaLabel "検索"
        , role "img"
        ]
        [ circle [ cx "50%", cy "50%", r "50%", fill "none" ] []
        , circle [ cx "37.5%", cy "37.5%", r "12.5%", fill "none" ] []
        , line [ x1 "50%", y1 "50%", x2 "75%", y2 "75%" ] []
        ]


hamburger : Svg msg
hamburger =
    svg
        [ xmlns "http://www.w3.org/2000/svg"
        , viewBox "0 0 24 24"
        , role "img"
        , strokeLinecap "round"
        ]
        [ line [ x1 "6", x2 "18", y1 "6", y2 "6" ] []
        , line [ x1 "6", x2 "18", y1 "12", y2 "12" ] []
        , line [ x1 "6", x2 "18", y1 "18", y2 "18" ] []
        ]
