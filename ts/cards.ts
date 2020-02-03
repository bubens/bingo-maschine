import * as Pdf from "jspdf";

type Settings = {
    offsetX: number,
    offsetY: number,
    cardWidth: number,
    cardHeight: number
}

type Card = Array<Array<string>>

type Point = { x: number, y: number };

const settings: Settings = {
    offsetX: 10,
    offsetY: 10,
    cardWidth: 133,
    cardHeight: 90
}

function drawCard({ x, y }: Point, card: Card, pdf: Pdf): Pdf {
    const realX = x + settings.offsetX;
    const realY = y + settings.offsetY;
    pdf.rect(realX, realY, settings.cardWidth, settings.cardHeight);
    return pdf;
}

export function create(cards: Array<Card>): void {
    const pdf = new Pdf({
        orientation: "landscape"
    });

    drawCard({ x: 0, y: 0 }, [[]], pdf)
        .save("bingo_karten.pdf");

}