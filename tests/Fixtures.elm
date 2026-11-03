module Fixtures exposing
    ( authorEmail
    , authorGithub
    , authorName
    , breakDescription
    , breakSeconds
    , duffPlanYaml
    , effortDescription
    , firstEffortName
    , firstEffortSeconds
    , invalidYaml
    , mondayRounds
    , planDescription
    , planMissingDescriptionYaml
    , planWithoutDifficultyYaml
    , remotePlanJson
    , remotePlanMissingFieldJson
    , remotePlansJson
    , secondEffortName
    , secondEffortSeconds
    , treeEmptyJson
    , treeNestedJson
    , unnamedBreakSeconds
    , wednesdayRounds
    )

{-| Shared, deterministic Simpsons-themed fixtures used across test modules.
-}


planDescription : String
planDescription =
    "Duff-powered HIIT"


authorName : String
authorName =
    "Homer Simpson"


authorGithub : String
authorGithub =
    "chunkylover53"


authorEmail : String
authorEmail =
    "homer@springfieldnuclear.com"


firstEffortName : String
firstEffortName =
    "Donut lifts"


firstEffortSeconds : Int
firstEffortSeconds =
    40


effortDescription : String
effortDescription =
    "Lift the pink frosted donut above your head, mmm donuts."


secondEffortName : String
secondEffortName =
    "Moe's bar crawl"


secondEffortSeconds : Int
secondEffortSeconds =
    30


breakSeconds : Int
breakSeconds =
    15


breakDescription : String
breakDescription =
    "Take a sip of Duff."


unnamedBreakSeconds : Int
unnamedBreakSeconds =
    20


mondayRounds : Int
mondayRounds =
    3


wednesdayRounds : Int
wednesdayRounds =
    2


{-| A plan scheduled on Monday and Wednesday. Monday's steps are, in order:
an effort with a description, a named break with a description, an unnamed
break, and an effort with an explicit `normal` type.
-}
duffPlanYaml : String
duffPlanYaml =
    """description: Duff-powered HIIT
difficulty: hard
author:
  github: chunkylover53
  name: Homer Simpson
  email: homer@springfieldnuclear.com

planning:
  days:
    monday:
      rounds: 3
      steps:
        - name: Donut lifts
          seconds: 40
          description: Lift the pink frosted donut above your head, mmm donuts.

        - name: Duff break
          type: break
          seconds: 15
          description: Take a sip of Duff.

        - type: break
          seconds: 20

        - type: normal
          name: Moe's bar crawl
          seconds: 30

    wednesday:
      rounds: 2
      steps:
        - name: Moe's bar crawl
          seconds: 30

    krustyday:
      rounds: 1
      steps:
        - name: Krusty pratfalls
          seconds: 10
"""


planMissingDescriptionYaml : String
planMissingDescriptionYaml =
    """author:
  name: Homer Simpson
planning:
  days:
    monday:
      rounds: 1
      steps:
        - name: Donut lifts
          seconds: 40
"""


invalidYaml : String
invalidYaml =
    """description: [Duff-powered HIIT
author: {name: Homer Simpson
"""


remotePlanJson : String
remotePlanJson =
    """{
  "description": "Duff-powered HIIT",
  "author_name": "Homer Simpson",
  "download_url": "https://raw.githubusercontent.com/chunkylover53/plans/main/duff.yml",
  "sha": "742evergreenterrace",
  "path": "plans/duff.yml",
  "name": "duff.yml",
  "difficulty": "extreme"
}"""


remotePlanMissingFieldJson : String
remotePlanMissingFieldJson =
    """{
  "description": "Duff-powered HIIT",
  "author_name": "Homer Simpson"
}"""


remotePlansJson : String
remotePlansJson =
    """[
  {
    "description": "Duff-powered HIIT",
    "author_name": "Homer Simpson",
    "download_url": "https://raw.githubusercontent.com/chunkylover53/plans/main/duff.yml",
    "sha": "742evergreenterrace",
    "path": "plans/duff.yml",
    "name": "duff.yml",
    "difficulty": "extreme"
  },
  {
    "description": "Saxophone cardio",
    "author_name": "Lisa Simpson",
    "download_url": "https://raw.githubusercontent.com/chunkylover53/plans/main/sax.yml",
    "sha": "bleedinggumsmurphy",
    "path": "plans/sax.yml",
    "name": "sax.yml"
  }
]"""


treeEmptyJson : String
treeEmptyJson =
    """{ "files": {}, "folders": {} }"""


{-| A tree with one file at the root, two in `springfield`, and one in
`springfield/kwik-e-mart`: four files in total.
-}
treeNestedJson : String
treeNestedJson =
    """{
  "files": { "duff.yml": "Homer Simpson" },
  "folders": {
    "springfield": {
      "files": {
        "sax.yml": "Lisa Simpson",
        "skate.yml": "Bart Simpson"
      },
      "folders": {
        "kwik-e-mart": {
          "files": { "squishee.yml": "Apu Nahasapeemapetilon" },
          "folders": {}
        }
      }
    }
  }
}"""


{-| A minimal plan that omits the optional difficulty field.
-}
planWithoutDifficultyYaml : String
planWithoutDifficultyYaml =
    """description: Duff-powered HIIT
author:
  name: Homer Simpson
planning:
  days:
    friday:
      rounds: 1
      steps:
        - name: Donut lifts
          seconds: 40
"""
