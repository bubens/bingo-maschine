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


function main(elm: Elm, initModel: Config.Model): void {
    const model = getStoredModel(Config.storageKey, initModel);

    const app = elm.init({
        node: document.querySelector(Config.elmElementId),
        flags: model
    });
}

main(Elm.Main, Config.initModel);