//@ts-ignore
import { Elm } from "./elm";

const parameters = {
    node: "#here_goes_app",
    flag: {
        int: 5,
        string: "string"
    }
};

const app = Elm.Main.init(parameters);