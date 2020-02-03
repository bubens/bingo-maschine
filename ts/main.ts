//@ts-ignore
import { Elm } from "./elm";
import * as Config from "./config";
import * as Cards from "./cards";

const getStoredModel = (key: string, fallback: Config.Model): Config.Model => {
    const rawString = window.localStorage.getItem(key);

    if (rawString === null) {
        return fallback;
    }
    else {
        try {
            const model =
                JSON.parse(rawString);

            return { ...fallback, ...model };
        }
        catch (_) {
            return fallback;
        }


    }
};

type MessageType =
    "SaveState"
    | "CreateCards"

type IncomingMessage = {
    type: MessageType,
    payload: any
}


const portRouter = (msg: IncomingMessage): void => {
    if (msg.type === "SaveState") {
        const model = msg.payload;
        const dataString = JSON.stringify(model);
        localStorage.setItem(Config.storageKey, dataString);
        localStorage.getItem(Config.storageKey);
    }
    else if (msg.type === "CreateCards") {
        const cards = msg.payload;
        Cards.create(cards);
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



main(<Elm>Elm.Main, Config.initModel);