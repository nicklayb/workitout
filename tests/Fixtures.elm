module Fixtures exposing
    ( audiusPlaylistJson
    , authorEmail
    , authorGithub
    , authorName
    , breakDescription
    , breakSeconds
    , breakTracks
    , duffPlanYaml
    , effortDescription
    , effortTracks
    , effortWithSecondsAndCountYaml
    , effortWithoutAmountYaml
    , everyDayPlanYaml
    , everyDayRounds
    , firstEffortName
    , firstEffortSeconds
    , invalidYaml
    , mondayRounds
    , planDescription
    , planMissingDescriptionYaml
    , planWithoutDifficultyYaml
    , planWithoutScheduleYaml
    , planningAndWorkoutYaml
    , playlistHandle
    , playlistSlug
    , playlistUrl
    , remotePlanJson
    , remotePlanMissingFieldJson
    , remotePlansJson
    , repetitionsEffortCount
    , repetitionsPlanYaml
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


repetitionsEffortCount : Int
repetitionsEffortCount =
    12


{-| A Tuesday made of an effort counted in repetitions followed by a break.
-}
repetitionsPlanYaml : String
repetitionsPlanYaml =
    """description: Duff can curls
author:
  name: Barney Gumble
planning:
  days:
    tuesday:
      rounds: 2
      steps:
        - name: Duff can curls
          count: 12
        - type: break
          seconds: 15
"""


{-| A plan whose effort has both `seconds` and `count`.
-}
effortWithSecondsAndCountYaml : String
effortWithSecondsAndCountYaml =
    """description: Confused Ralph
author:
  name: Ralph Wiggum
planning:
  days:
    monday:
      rounds: 1
      steps:
        - name: Eating paste
          seconds: 30
          count: 10
"""


{-| A plan whose effort has neither `seconds` nor `count`.
-}
effortWithoutAmountYaml : String
effortWithoutAmountYaml =
    """description: Lazy Homer
author:
  name: Homer Simpson
planning:
  days:
    monday:
      rounds: 1
      steps:
        - name: Napping
"""


everyDayRounds : Int
everyDayRounds =
    4


{-| A plan with the same workout every day: an effort counted in repetitions
followed by a break.
-}
everyDayPlanYaml : String
everyDayPlanYaml =
    """description: Daily Squishee brain freeze
author:
  name: Apu Nahasapeemapetilon
workout:
  rounds: 4
  steps:
    - name: Squishee squats
      count: 15
    - type: break
      seconds: 20
"""


{-| A plan having both a weekly planning and a workout for every day.
-}
planningAndWorkoutYaml : String
planningAndWorkoutYaml =
    """description: Indecisive Ned
author:
  name: Ned Flanders
planning:
  days:
    sunday:
      rounds: 1
      steps:
        - name: Praying
          seconds: 60
workout:
  rounds: 1
  steps:
    - name: Diddly squats
      seconds: 30
"""


{-| A plan with neither a weekly planning nor a workout.
-}
planWithoutScheduleYaml : String
planWithoutScheduleYaml =
    """description: Lazy Barney
author:
  name: Barney Gumble
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


playlistHandle : String
playlistHandle =
    "besharps"


playlistSlug : String
playlistSlug =
    "barbershop-hits"


playlistUrl : String
playlistUrl =
    "https://audius.co/besharps/playlist/barbershop-hits"


{-| Upbeat tracks, as decoded from the Audius API.
-}
effortTracks : List { id : String, title : String, artist : String, permalink : String }
effortTracks =
    [ { id = "babyOnBoard", title = "Baby on Board", artist = "The Be Sharps", permalink = "/besharps/baby-on-board" }
    , { id = "monorail", title = "Monorail Song", artist = "Lyle Lanley", permalink = "/lanley/monorail-song" }
    , { id = "spiderPig", title = "Spider Pig", artist = "Homer Simpson", permalink = "/homer/spider-pig" }
    ]


{-| Calm tracks, as decoded from the Audius API.
-}
breakTracks : List { id : String, title : String, artist : String, permalink : String }
breakTracks =
    [ { id = "jazzman", title = "Jazzman", artist = "Bleeding Gums Murphy", permalink = "/bleedinggums/jazzman" }
    , { id = "sadSax", title = "Sax on the Beach", artist = "Lisa Simpson", permalink = "/lisa/sax-on-the-beach" }
    ]


{-| Response of the playlist endpoint: one streamable track, one gated track,
one not streamable track and one without the optional flags.
-}
audiusPlaylistJson : String
audiusPlaylistJson =
    """
{ "data": [ { "playlist_name": "Barbershop hits", "tracks": [
  { "id": "babyOnBoard", "title": "Baby on Board", "permalink": "/besharps/baby-on-board",
    "is_streamable": true, "is_stream_gated": false, "user": { "name": "The Be Sharps" } },
  { "id": "gated", "title": "Bart's Secret Song", "permalink": "/bart/secret",
    "is_streamable": true, "is_stream_gated": true, "user": { "name": "Bart Simpson" } },
  { "id": "offline", "title": "Flaming Moe", "permalink": "/moe/flaming-moe",
    "is_streamable": false, "is_stream_gated": false, "user": { "name": "Moe Szyslak" } },
  { "id": "monorail", "title": "Monorail Song", "permalink": "/lanley/monorail-song",
    "is_stream_gated": null, "user": { "name": "Lyle Lanley" } }
] } ] }
"""
