## Nested scopes

```lisp
(define x 1)                     ; Program-level
(define y 2)

(let ((x 10))                   ; Level 1: shadows program
  (let ((x 20))                 ; Level 2: shadows level 1
    (lambda () x)))              ; Resolves to 20 (innermost)
```


```gram
{ kind: "Pattern Lisp" }

[e0:Env |
  [],                        // first element is reference to parent, which is empty
  [ :Binding {name: "x"} |
    [:Number {value: 1}]
  ],
  [ :Binding {name: "y"} |
    [:Number {value: 2}]
  ]
]

[:Closure |
  [e1:Env |
    e0,                       // first element is reference to parent
    [ :Binding {name: "x"} |  // shadows "x" from parent
      [:Number {value: 10}]
    ],
  ],
  [:Lambda |
    [:Parameters],
    [:Body |
      [:Let |
        [e2:Env |
          e1,                       // first element points to parent env
          [ :Binding {name: "x"} |  // new binding
            [:Number {value: 20}]
          ]
        ],
        [:Body |
          [:Closure |
            [e3:Env | e2,           // first element points to parent env
            ],
            [:Lambda |
              [:Parameters],
              [:Body |
                x_level2            // Resolves to 20 (innermost)
              ]
            ]
          ]
        ]
      ]
    ]
  ]
]
```

