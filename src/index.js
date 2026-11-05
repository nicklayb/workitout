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
    weekday: (new Date()).getDay(),
    // Replaced at build time with the content of the VERSION file.
    version: APP_VERSION
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

// Background music: a single audio element, so the user's click on Start
// unlocks playback for every later track. The position of the track left by
// each channel is kept to resume it when the channel plays it again.
const music = new Audio()
const savedTrackByChannel = {}
let playingTrack = null

function playMusic({ channel, url, volume }) {
  music.volume = volume
  if (playingTrack === null || playingTrack.url !== url) {
    if (playingTrack !== null) {
      savedTrackByChannel[playingTrack.channel] = { url: playingTrack.url, time: music.currentTime }
    }
    const savedTrack = savedTrackByChannel[channel]
    music.src = url
    if (savedTrack && savedTrack.url === url) {
      music.currentTime = savedTrack.time
    }
  }
  playingTrack = { channel, url }
  music.play().catch(function() {})
}

music.addEventListener("ended", function() {
  playingTrack && app.ports.musicTrackEnded.send(playingTrack.url)
})

music.addEventListener("error", function() {
  playingTrack && app.ports.musicTrackFailed.send(playingTrack.url)
})

app.ports.controlMusic.subscribe(function(command) {
  if (command.action === "play") {
    playMusic(command)
  } else {
    music.pause()
  }
})

// Screen wake lock: kept while a workout runs. Browsers release it when the
// page gets hidden, so it is requested again once the page is visible.
let screenWakeLock = null
let isScreenWakeLockWanted = false
let isScreenWakeLockPending = false

async function requestScreenWakeLock() {
  if (!("wakeLock" in navigator) || screenWakeLock !== null || isScreenWakeLockPending) return
  isScreenWakeLockPending = true
  try {
    const lock = await navigator.wakeLock.request("screen")
    lock.addEventListener("release", function() {
      if (screenWakeLock === lock) screenWakeLock = null
    })
    screenWakeLock = lock
    if (!isScreenWakeLockWanted) releaseScreenWakeLock()
  } catch (error) {
    // Denied, for instance when the page is hidden or the battery is low.
  } finally {
    isScreenWakeLockPending = false
  }
}

function releaseScreenWakeLock() {
  if (screenWakeLock !== null) {
    screenWakeLock.release()
    screenWakeLock = null
  }
}

app.ports.setScreenWakeLock.subscribe(function(isWanted) {
  isScreenWakeLockWanted = isWanted
  if (isWanted) {
    requestScreenWakeLock()
  } else {
    releaseScreenWakeLock()
  }
})

document.addEventListener("visibilitychange", function() {
  if (isScreenWakeLockWanted && document.visibilityState === "visible") {
    requestScreenWakeLock()
  }
})
