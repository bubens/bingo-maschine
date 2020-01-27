//@ts-ignore
import { Elm } from "./elm";
import * as Config from "./config";

const getStoredModel = (key: string, fallback: Config.Model): Config.Model => {
    const rawString = window.localStorage.getItem(key);

    if (rawString === null) {
        return fallback;
    }
    else {
        const model =
            JSON.parse(rawString);

        return { ...fallback, ...model };
    }
};


type SaveStateMessage =
    { SaveState: Config.Model }

type IncomingMessage =
    SaveStateMessage

const portRouter = (data: IncomingMessage): void => {
    if (data.hasOwnProperty("SaveState")) {
        const dataString = JSON.stringify(data.SaveState);
        localStorage.setItem(Config.storageKey, dataString);
        localStorage.getItem(Config.storageKey);
    }
};


function main(elm: Elm, initModel: Config.Model): void {
    const model = getStoredModel(Config.storageKey, initModel);

    const app = elm.init({
        node: document.querySelector(Config.elmElementId),
        flags: model
    });

    app.ports.outbox.subscribe(portRouter);
}

main(Elm.Main, Config.initModel);