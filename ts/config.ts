type TypeOfBingo
    = "Strings"
    | "Numbers";

// TYPES
export interface Model {
    title: string,
    size: number,
    rangeMinimum: number,
    rangeMaximum: number,
    typeOfBingo: TypeOfBingo,
    strings: string[],
    rawStringInput: "",
    rawMinimumInput: string,
    rawMaximumInput: string,
    ordered: boolean,
    joker: boolean,
    sampleSheet: String[][]
}


// VALUES
export const storageKey = "$model";

export const elmElementId = "#app_goes_here";

export const initModel: Model = {
    title: "Bingo-Maschine 4.0",
    typeOfBingo: "Numbers",
    size: 5,
    rangeMinimum: 0,
    rangeMaximum: 100,
    strings: [],
    rawStringInput: "",
    rawMinimumInput: "0",
    rawMaximumInput: "100",
    ordered: true,
    joker: false,
    sampleSheet: [[]]
};