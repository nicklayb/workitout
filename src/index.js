import { Elm } from './Main.elm'
import "./app.css"

const LAST_PLAN_KEY = "last_plan"
const SETTINGS_KEY = "settings"
const CUSTOM_PLANS_KEY = "custom_plans"

const app = Elm.Main.init({
  node: document.getElementById('elmRoot'),
  flags: {
    lastPlan: localStorage.getItem(LAST_PLAN_KEY),
    settings: localStorage.getItem(SETTINGS_KEY),
    customPlans: localStorage.getItem(CUSTOM_PLANS_KEY),
    weekday: (new Date()).getDay()
  }
});

function storeOrRemove(key, value) {
  if (value === null) {
    localStorage.removeItem(key)
  } else {
    localStorage.setItem(key, value)
  }
}

app.ports && app.ports.storeLastPlan && app.ports.storeLastPlan.subscribe(function(value) {
  storeOrRemove(LAST_PLAN_KEY, value)
})

app.ports && app.ports.storeSettings && app.ports.storeSettings.subscribe(function(value) {
  storeOrRemove(SETTINGS_KEY, value)
})

app.ports && app.ports.storeCustomPlans && app.ports.storeCustomPlans.subscribe(function(value) {
  storeOrRemove(CUSTOM_PLANS_KEY, value)
})

app.ports.playSound.subscribe(function(message) {
  const audio = new Audio(`/sounds/${message}.mp3`)
  audio.play()
});
