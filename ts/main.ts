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
    | "SendJoker"

type IncomingMessage = {
    type: MessageType,
    payload: any
}

const saveState = (model: Config.Model): void => {
    const modelStr = JSON.stringify(model);
    const key = Config.storageKeys.model;
    localStorage.setItem(key, modelStr);
    localStorage.getItem(key);
};

const createCards = (cards: Cards.Cards): void => {
    const joker = localStorage.getItem(Config.storageKeys.imageData);
    if (joker === null) {
        throw new Error("Something went wrong while loading the joker.");
    }
    else {
        const pdf = Cards.create(cards, joker);

        pdf.save("bingo-cards.pdf");
    }

};

const saveJoker = (imageData: string): void => {
    const key = Config.storageKeys.imageData;
    localStorage.setItem(key, imageData);
    localStorage.getItem(key);
}

const portRouter = (msg: IncomingMessage): void => {
    switch (msg.type) {
        case "SaveState":
            saveState(msg.payload);
            break;
        case "CreateCards":
            createCards(msg.payload);
            break;
        case "SendJoker":
            saveJoker(msg.payload);
            break;
    }
};


function main(elm: Elm, initModel: Config.Model): void {
    const model = getStoredModel(Config.storageKeys.model, initModel);

    const app = elm.init({
        node: document.querySelector(Config.elmElementId),
        flags: model
    });

    app.ports.outbox.subscribe(portRouter);
}



main(<Elm>Elm.Main, Config.initModel);