(ns evo.distances)

;; data from http://www.codeproject.com/Articles/259926/Introduction-to-Genetic-Algorithm-Encoding-Camel
;; licensed under the CPOL: http://www.codeproject.com/info/cpol10.aspx
(def distances [[ 0   28  57  72  81  85  80  113 89  80  ]
                [ 28  0   28  45  54  57  63  85  63  63  ]
                [ 57  28  0   20  30  28  57  57  40  57  ]
                [ 72  45  20  0   10  20  72  45  20  45  ]
                [ 81  54  30  10  0   22  81  41  10  41  ]
                [ 85  57  28  20  22  0   63  28  28  63  ]
                [ 80  63  57  72  81  63  0   80  89  113 ]
                [ 113 85  57  45  41  28  80  0   40  80  ]
                [ 89  63  40  20  10  28  89  40  0   40  ]
                [ 80  63  57  45  41  63  113 80  40  0   ]])

(defn get-distance [[a b]]
  (get-in distances [a b]))
