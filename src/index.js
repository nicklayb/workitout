import { Elm } from './Main.elm'
import "./app.css"

const app = Elm.Main.init({
  node: document.getElementById('elmRoot'),
  flags: {}
});

app.ports.playSound.subscribe(function(message) {
  const audio = new Audio(`/dist/sounds/${message}.mp3`)
  audio.play()
});
