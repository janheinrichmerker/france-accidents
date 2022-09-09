import {Elm} from "./Main.elm";
import {registerCustomElement, registerPorts} from "elm-mapbox";

registerCustomElement();
Elm.Main.init({
    node: document.getElementById("main"),
});


const app = Elm.MyApp.init();
registerPorts(app);
