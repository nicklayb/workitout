import { Elm } from './Main.elm'
import "./app.css"

const LAST_PLAN_KEY = "last_plan"

const app = Elm.Main.init({
  node: document.getElementById('elmRoot'),
  flags: {
    lastPlan: localStorage.getItem(LAST_PLAN_KEY)
  }
});

app.ports && app.ports.storeLastPlan && app.ports.storeLastPlan.subscribe(function(value) {
  if (value === null) {
    localStorage.removeItem(LAST_PLAN_KEY)
  } else {
    localStorage.setItem(LAST_PLAN_KEY, value)
  }
})

app.ports.playSound.subscribe(function(message) {
  const audio = new Audio(`/sounds/${message}.mp3`)
  audio.play()
});
